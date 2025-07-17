# Charger les packages nécessaires
# Assurez-vous de les installer d'abord si ce n'est pas déjà fait :
# install.packages(c("shiny", "googlesheets4", "ggplot2", "dplyr", "tidyr", "readr", "shinydashboard", "DT", "scales")) # Ajout de "scales"

library(shiny)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr) # Pour parse_number(), plus robuste pour la conversion numérique
library(shinydashboard) # Pour le tableau de bord, valueBox, box, etc.
library(DT) # Pour des tableaux interactifs (DataTables)
library(scales) # Ajouté pour la fonction de formatage des axes de ggplot2

# --- Configuration de la feuille Google Sheet ---
# L'URL de votre Google Sheet contenant les données de budget.
# IMPORTANT : Assurez-vous que cette feuille est partagée publiquement (ou que vous avez configuré l'authentification gs4).
google_sheet_url <- "https://docs.google.com/spreadsheets/d/1dtZGvstHeHNffm6kDeNqsJtSD_jS78z4RQWDDuUQ6J8/edit?usp=sharing"

# Dé-authentifier pour un accès public à la feuille Google Sheet
# C'est essentiel si votre feuille est partagée avec "Tout utilisateur disposant du lien"
# Exécutez ceci une seule fois au début de votre script.
gs4_deauth()

# --- Interface Utilisateur (UI) ---
ui <- dashboardPage(
  skin = "blue", # Thème du tableau de bord (par exemple, "blue", "black", "purple", "green", etc.)
  
  # En-tête du tableau de bord
  dashboardHeader(title = "Budget Familial 💰"),
  
  # Barre latérale du tableau de bord (navigation et contrôles)
  dashboardSidebar(
    sidebarMenu(
      # Éléments de menu pour naviguer entre les différentes vues
      menuItem("Vue d'ensemble", tabName = "vue_ensemble", icon = icon("dashboard")),
      menuItem("Détail des cotisations", tabName = "detail_revenus", icon = icon("money-bill-alt")),
      menuItem("Détail des Depenses", tabName = "detail_depenses", icon = icon("wallet")),
      menuItem("Bilan mensuel", tabName = "bilan_mensuel", icon = icon("balance-scale-right"))
    ),
    hr(), # Ligne de séparation visuelle
    h4("Options et Informations", style = "padding-left: 15px; color: #fff;"),
    # Bouton pour rafraîchir manuellement les données
    actionButton("rafraichir_donnees", "Rafraîchir les données", icon = icon("sync-alt"),
                 style = "margin-left: 15px; margin-bottom: 10px;"),
    # Affichage de l'heure du dernier rafraîchissement
    p("Dernier rafraîchissement: ", textOutput("last_refresh_time", inline = TRUE),
      style = "padding-left: 15px; color: #ccc; font-size: 0.8em;")
    
  ),
  
  # Corps principal du tableau de bord
  dashboardBody(
    # Styles CSS personnalisés pour l'apparence des boîtes et tableaux
    tags$head(
      tags$style(HTML("
        /* Couleurs personnalisées pour les valueBox */
        .small-box.bg-green { background-color: #28a745 !important; color: #fff !important; }
        .small-box.bg-red { background-color: #dc3545 !important; color: #fff !important; }
        .small-box.bg-aqua { background-color: #007bff !important; color: #fff !important; }
        .small-box.bg-orange { background-color: #fd7e14 !important; color: #fff !important; } /* Nouvelle couleur pour solde négatif */
        .small-box h3, .small-box p { color: #fff !important; } /* S'assurer que le texte est blanc sur les boîtes colorées */

        /* Styles des en-têtes de boîte */
        .box.box-solid.box-primary>.box-header { background-color: #3c8dbc; color: #fff; }
        .box.box-solid.box-success>.box-header { background-color: #28a745; color: #fff; }
        .box.box-solid.box-danger>.box-header { background-color: #dc3545; color: #fff; }

        /* Marge pour les boutons des tableaux DT */
        .dataTables_wrapper .dt-buttons { margin-bottom: 10px; }
      "))
    ),
    
    # Contenu des onglets (tabItems)
    tabItems(
      # --- Tab 1: Vue d'ensemble (Tableau de bord principal) ---
      tabItem(tabName = "vue_ensemble",
             
              fluidRow(
                valueBoxOutput("totalRevenusBox", width = 4),
                valueBoxOutput("totalDepensesBox", width = 4),
                valueBoxOutput("soldeDisponibleBox", width = 4)
              ),
              # Ligne de graphiques pour les revenus et Depenses
              fluidRow(
                box(
                  title = "Revenus mensuels et cumulés", status = "primary", solidHeader = TRUE, width = 6,
                  plotOutput("revenuPlot", height = "300px")
                ),
                box(
                  title = "Depenses mensuelles et cumulées", status = "danger", solidHeader = TRUE, width = 6,
                  plotOutput("depensePlot", height = "300px")
                )
              ),
              # Graphique du bilan
              fluidRow(
                box(
                  title = "Bilan mensuel et cumulé", status = "success", solidHeader = TRUE, width = 12,
                  plotOutput("bilanPlot", height = "350px")
                )
              )
      ),
      
      # --- Tab 2: Détail des Revenus (Tableau interactif) ---
      tabItem(tabName = "detail_revenus",
              h2("Détail des revenus par personne et par Mois", align = "center"),
              fluidRow(
                box(
                  title = "Tableau détaillé des cotisations", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("revenuDetailTable")
                )
              )
      ),
      
      # --- Tab 3: Détail des Depenses (Tableau interactif) ---
      tabItem(tabName = "detail_depenses",
              h2("Détail des Depenses par type et par mois", align = "center"),
              fluidRow(
                box(
                  title = "Tableau détaillé des Depenses", status = "danger", solidHeader = TRUE, width = 12,
                  DTOutput("depenseDetailTable")
                )
              )
      ),
      
      # --- Tab 4: Bilan Mensuel (Tableau interactif) ---
      tabItem(tabName = "bilan_mensuel",
              h2("Bilan financier mensuel", align = "center"),
              fluidRow(
                box(
                  title = "Synthèse du bilan par mois", status = "success", solidHeader = TRUE, width = 12,
                  DTOutput("bilanSummaryTable")
                )
              )
      )
    )
  )
)

# --- Logique du Serveur (Server) ---
server <- function(input, output, session) {
  
  # Variable réactive pour forcer le rafraîchissement des données
  # La modification de cette variable déclenchera la relecture des données.
  refresh_trigger <- reactiveVal(0)
  
  # Variable réactive pour stocker l'heure du dernier rafraîchissement.
  last_refresh_time <- reactiveVal(Sys.time())
  
  # *** CORRECTION ICI : Initialiser le déclencheur au démarrage de l'appli ***
  # Cette ligne s'exécute une seule fois par session lorsque l'application démarre.
  refresh_trigger(1) # Définit la valeur initiale à 1 pour déclencher le premier chargement.
  
  # Observer le bouton de rafraîchissement manuel
  # Lorsque le bouton est cliqué, incrémente le déclencheur pour recharger les données.
  observeEvent(input$rafraichir_donnees, {
    refresh_trigger(refresh_trigger() + 1)
    showNotification("Chargement des données depuis Google Sheets...", type = "message", duration = 3, session = session)
  })
  
  # Afficher l'heure du dernier rafraîchissement dans la barre latérale
  output$last_refresh_time <- renderText({
    format(last_refresh_time(), "%Y-%m-%d %H:%M:%S")
  })
  
  
  # --- Données réactives : lecture et traitement des Google Sheets ---
  # Cette fonction réactive lit et pré-traite toutes les données nécessaires
  # Elle se ré-exécute chaque fois que `refresh_trigger` change.
  budget_data <- reactive({
    # S'assure que la fonction s'exécute seulement quand refresh_trigger a une valeur (i.e., est initialisé ou changé)
    req(refresh_trigger())
    
    # Définition de l'ordre des mois pour les facteurs
    mois_cols <- c("JAN", "FÉV", "MAR", "AVR", "MAI", "JUIN", "JUIL", "AOÛT", "SEPT", "OCT", "NOV", "DÉC")
    
    # --- Lecture et traitement de l'onglet 'revenu' ---
    df_revenu <- tryCatch({
      read_sheet(google_sheet_url, sheet = "revenu", col_types = "c")
    }, error = function(e) {
      showNotification(paste("Erreur de lecture de l'onglet 'revenu':", e$message), type = "error", duration = NULL, session = session)
      NULL # Retourne NULL en cas d'erreur
    })
    
    # Vérification et nettoyage des données de revenu
    if (is.null(df_revenu) || ncol(df_revenu) < 2) {
      showNotification("L'onglet 'revenu' est vide ou mal formaté. Vérifiez la feuille Google.", type = "warning", duration = NULL, session = session)
      # Retourne des dataframes vides pour éviter les erreurs subséquentes
      return(list(
        revenu_detail = data.frame(Type = character(), Mois = factor(), Montant = numeric()),
        revenu_summary = data.frame(Mois = factor(), Total_Mensuel = numeric(), Total_Cumule = numeric()),
        depense_detail = data.frame(Type = character(), Mois = factor(), Montant = numeric()),
        depense_summary = data.frame(Mois = factor(), Total_Mensuel = numeric(), Total_Cumule = numeric()),
        bilan_summary = data.frame(Mois = factor(), Revenus = numeric(), Depenses = numeric(), Bilan = numeric(), Bilan_Cumule = numeric())
      ))
    }
    
    first_col_name_revenu <- names(df_revenu)[1]
    total_revenu_row_index <- which(df_revenu[[first_col_name_revenu]] == "REVENU TOTAL")
    df_revenu_clean <- if (length(total_revenu_row_index) > 0) {
      df_revenu[1:(total_revenu_row_index - 1), ]
    } else {
      df_revenu
    }
    colnames(df_revenu_clean)[1] <- "Type"
    
    available_mois_cols_revenu <- intersect(mois_cols, names(df_revenu_clean))
    if (length(available_mois_cols_revenu) == 0) {
      showNotification("Aucune colonne de mois valide trouvée dans l'onglet 'revenu'. Assurez-vous que les en-têtes sont corrects (JAN, FÉV, etc.).", type = "error", duration = NULL, session = session)
      # Gérer ce cas en retournant des données vides
      return(list(
        revenu_detail = data.frame(Type = character(), Mois = factor(), Montant = numeric()),
        revenu_summary = data.frame(Mois = factor(), Total_Mensuel = numeric(), Total_Cumule = numeric()),
        depense_detail = data.frame(Type = character(), Mois = factor(), Montant = numeric()),
        depense_summary = data.frame(Mois = factor(), Total_Mensuel = numeric(), Total_Cumule = numeric()),
        bilan_summary = data.frame(Mois = factor(), Revenus = numeric(), Depenses = numeric(), Bilan = numeric(), Bilan_Cumule = numeric())
      ))
    }
    
    # Transformation des données de revenu du format large au format long
    df_revenu_long <- df_revenu_clean %>%
      pivot_longer(
        cols = all_of(available_mois_cols_revenu),
        names_to = "Mois",
        values_to = "Montant_Text"
      ) %>%
      mutate(
        Montant = parse_number(Montant_Text, locale = locale(decimal_mark = ",", grouping_mark = " ")), # Conversion numérique robuste
        Mois = factor(Mois, levels = mois_cols) # Assure le bon ordre des mois
      ) %>%
      filter(!is.na(Montant)) %>% # Supprime les lignes où le montant n'est pas valide
      select(Type, Mois, Montant) # Sélectionne les colonnes finales
    
    # Calcul des totaux mensuels et cumulés pour les revenus
    df_revenu_summary <- df_revenu_long %>%
      group_by(Mois) %>%
      summarise(Total_Mensuel = sum(Montant, na.rm = TRUE), .groups = "drop") %>%
      mutate(Total_Cumule = cumsum(Total_Mensuel))
    
    
    # --- Lecture et traitement de l'onglet 'depense' ---
    df_depense <- tryCatch({
      read_sheet(google_sheet_url, sheet = "depense", col_types = "c")
    }, error = function(e) {
      showNotification(paste("Erreur de lecture de l'onglet 'depense':", e$message), type = "error", duration = NULL, session = session)
      NULL # Retourne NULL en cas d'erreur
    })
    
    # Vérification et nettoyage des données de dépense
    if (is.null(df_depense) || ncol(df_depense) < 2) {
      showNotification("L'onglet 'depense' est vide ou mal formaté. Vérifiez la feuille Google.", type = "warning", duration = NULL, session = session)
      # Retourne des dataframes vides pour éviter les erreurs subséquentes
      return(list(
        revenu_detail = data.frame(Type = character(), Mois = factor(), Montant = numeric()),
        revenu_summary = data.frame(Mois = factor(), Total_Mensuel = numeric(), Total_Cumule = numeric()),
        depense_detail = data.frame(Type = character(), Mois = factor(), Montant = numeric()),
        depense_summary = data.frame(Mois = factor(), Total_Mensuel = numeric(), Total_Cumule = numeric()),
        bilan_summary = data.frame(Mois = factor(), Revenus = numeric(), Depenses = numeric(), Bilan = numeric(), Bilan_Cumule = numeric())
      ))
    }
    
    first_col_name_depense <- names(df_depense)[1]
    total_depense_row_index <- which(df_depense[[first_col_name_depense]] == "Depenses TOTALES")
    df_depense_clean <- if (length(total_depense_row_index) > 0) {
      df_depense[1:(total_depense_row_index - 1), ]
    } else {
      df_depense
    }
    colnames(df_depense_clean)[1] <- "Type"
    
    available_mois_cols_depense <- intersect(mois_cols, names(df_depense_clean))
    if (length(available_mois_cols_depense) == 0) {
      showNotification("Aucune colonne de mois valide trouvée dans l'onglet 'depense'. Assurez-vous que les en-têtes sont corrects (JAN, FÉV, etc.).", type = "error", duration = NULL, session = session)
      # Gérer ce cas en retournant des données vides
      return(list(
        revenu_detail = data.frame(Type = character(), Mois = factor(), Montant = numeric()),
        revenu_summary = data.frame(Mois = factor(), Total_Mensuel = numeric(), Total_Cumule = numeric()),
        depense_detail = data.frame(Type = character(), Mois = factor(), Montant = numeric()),
        depense_summary = data.frame(Mois = factor(), Total_Mensuel = numeric(), Total_Cumule = numeric()),
        bilan_summary = data.frame(Mois = factor(), Revenus = numeric(), Depenses = numeric(), Bilan = numeric(), Bilan_Cumule = numeric())
      ))
    }
    
    # Transformation des données de dépense
    df_depense_long <- df_depense_clean %>%
      pivot_longer(
        cols = all_of(available_mois_cols_depense),
        names_to = "Mois",
        values_to = "Montant_Text"
      ) %>%
      mutate(
        Montant = parse_number(Montant_Text, locale = locale(decimal_mark = ",", grouping_mark = " ")),
        Mois = factor(Mois, levels = mois_cols)
      ) %>%
      filter(!is.na(Montant)) %>%
      select(Type, Mois, Montant)
    
    # Calcul des totaux mensuels et cumulés pour les Depenses
    df_depense_summary <- df_depense_long %>%
      group_by(Mois) %>%
      summarise(Total_Mensuel = sum(Montant, na.rm = TRUE), .groups = "drop") %>%
      mutate(Total_Cumule = cumsum(Total_Mensuel))
    
    # --- Combiner les données pour le Bilan Mensuel ---
    # Jointures des résumés de revenus et Depenses par mois
    df_bilan <- full_join(
      df_revenu_summary %>% select(Mois, Revenus = Total_Mensuel),
      df_depense_summary %>% select(Mois, Depenses = Total_Mensuel),
      by = "Mois"
    ) %>%
      # Remplacement des NA par 0 pour un calcul correct du bilan
      mutate(
        Revenus = replace_na(Revenus, 0),
        Depenses = replace_na(Depenses, 0),
        Bilan = Revenus - Depenses, # Calcul du bilan mensuel
        Bilan_Cumule = cumsum(Bilan) # Calcul du bilan cumulé
      ) %>%
      # S'assure que l'ordre des mois est correct après la jointure
      mutate(Mois = factor(Mois, levels = mois_cols)) %>%
      arrange(Mois)
    
    # Met à jour l'heure du dernier rafraîchissement des données.
    last_refresh_time(Sys.time())
    
    # Retourne toutes les données traitées sous forme de liste.
    list(
      revenu_detail = df_revenu_long, # Données détaillées par type (pour les tableaux DT)
      revenu_summary = df_revenu_summary, # Données résumées (pour les graphiques et valueBox)
      depense_detail = df_depense_long, # Données détaillées par type
      depense_summary = df_depense_summary, # Données résumées
      bilan_summary = df_bilan # Données de bilan
    )
  })
  
  
  # --- VALUE BOXES pour la Vue d'ensemble ---
  
  # Box pour les revenus totaux
  output$totalRevenusBox <- renderValueBox({
    data <- budget_data()
    # Utilise tryCatch pour gérer le cas où data$revenu_detail est NULL ou vide
    total_revenus_global <- tryCatch({
      sum(data$revenu_detail$Montant, na.rm = TRUE)
    }, error = function(e) 0) # Retourne 0 si erreur ou pas de données
    
    valueBox(
      paste(format(total_revenus_global, big.mark = " ", nsmall = 0), " FCFA"),
      "Revenus Totaux",
      icon = icon("money-check-alt"),
      color = "green"
    )
  })
  
  # Box pour les Depenses totales
  output$totalDepensesBox <- renderValueBox({
    data <- budget_data()
    total_depenses_global <- tryCatch({
      sum(data$depense_detail$Montant, na.rm = TRUE)
    }, error = function(e) 0)
    
    valueBox(
      paste(format(total_depenses_global, big.mark = " ", nsmall = 0), " FCFA"),
      "Depenses Totales",
      icon = icon("shopping-cart"),
      color = "red"
    )
  })
  
  # Box pour le solde disponible
  output$soldeDisponibleBox <- renderValueBox({
    data <- budget_data()
    total_revenus_global <- tryCatch({
      sum(data$revenu_detail$Montant, na.rm = TRUE)
    }, error = function(e) 0)
    total_depenses_global <- tryCatch({
      sum(data$depense_detail$Montant, na.rm = TRUE)
    }, error = function(e) 0)
    solde_disponible <- total_revenus_global - total_depenses_global
    
    # Détermine la couleur et l'icône en fonction du solde (positif/négatif)
    solde_color <- if (solde_disponible >= 0) "aqua" else "orange"
    solde_icon <- if (solde_disponible >= 0) "hand-holding-usd" else "exclamation-triangle"
    
    valueBox(
      paste(format(solde_disponible, big.mark = " ", nsmall = 0), " FCFA"),
      "Solde Disponible",
      icon = icon(solde_icon),
      color = solde_color
    )
  })
  
  
  # --- GRAPHIQUES (dans l'onglet Vue d'ensemble) ---
  
  # Graphique des revenus mensuels et cumulés
  output$revenuPlot <- renderPlot({
    data <- budget_data()
    req(data$revenu_summary) # S'assure que les données de résumé sont disponibles
    # Message si aucune donnée
    if (nrow(data$revenu_summary) == 0) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Aucune donnée de revenu disponible pour ce graphique.", size=5, color="grey") + theme_void())
    }
    ggplot(data$revenu_summary, aes(x = Mois)) +
      geom_col(aes(y = Total_Mensuel, fill = "Mensuel"), position = "dodge") +
      geom_line(aes(y = Total_Cumule, group = 1, color = "Cumulé"), linewidth = 1) +
      geom_point(aes(y = Total_Cumule, color = "Cumulé"), size = 2) +
      labs(title = "", x = "Mois", y = "Montant (FCFA)") +
      scale_fill_manual(name = "Type", values = c("Mensuel" = "steelblue")) +
      scale_color_manual(name = "Type", values = c("Cumulé" = "darkgreen")) +
      theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      # *** NOUVEAUTÉ : Formatage de l'axe Y ***
      scale_y_continuous(labels = scales::label_number(big.mark = " ", decimal.mark = ","))
  })
  
  # Graphique des Depenses mensuelles et cumulées
  output$depensePlot <- renderPlot({
    data <- budget_data()
    req(data$depense_summary)
    if (nrow(data$depense_summary) == 0) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Aucune donnée de dépense disponible pour ce graphique.", size=5, color="grey") + theme_void())
    }
    ggplot(data$depense_summary, aes(x = Mois)) +
      geom_col(aes(y = Total_Mensuel, fill = "Mensuel"), position = "dodge") +
      geom_line(aes(y = Total_Cumule, group = 1, color = "Cumulé"), linewidth = 1) +
      geom_point(aes(y = Total_Cumule, color = "Cumulé"), size = 2) +
      labs(title = "", x = "Mois", y = "Montant (FCFA)") +
      scale_fill_manual(name = "Type", values = c("Mensuel" = "coral")) +
      scale_color_manual(name = "Type", values = c("Cumulé" = "darkred")) +
      theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      # *** NOUVEAUTÉ : Formatage de l'axe Y ***
      scale_y_continuous(labels = scales::label_number(big.mark = " ", decimal.mark = ","))
  })
  
  # Graphique du bilan mensuel et cumulé
  output$bilanPlot <- renderPlot({
    data <- budget_data()
    req(data$bilan_summary)
    if (nrow(data$bilan_summary) == 0) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Aucune donnée de bilan disponible pour ce graphique.", size=5, color="grey") + theme_void())
    }
    ggplot(data$bilan_summary, aes(x = Mois)) +
      geom_col(aes(y = Bilan, fill = Bilan > 0)) + # Barres vertes pour bénéfice, rouges pour déficit
      geom_line(aes(y = Bilan_Cumule, group = 1, color = "Bilan Cumulé"), linewidth = 1) +
      geom_point(aes(y = Bilan_Cumule, color = "Bilan Cumulé"), size = 2) +
      scale_fill_manual(values = c("TRUE" = "#28a745", "FALSE" = "#dc3545"), name = "Statut",
                        labels = c("TRUE" = "Bénéfice", "FALSE" = "Déficit")) +
      scale_color_manual(name = "Type", values = c("Bilan Cumulé" = "purple")) +
      labs(title = "", x = "Mois", y = "Montant (FCFA)") +
      theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      # *** NOUVEAUTÉ : Formatage de l'axe Y ***
      scale_y_continuous(labels = scales::label_number(big.mark = " ", decimal.mark = ","))
  })
  
  # --- TABLEAUX INTERACTIFS (DT) ---
  
  # Tableau détaillé des revenus (dans l'onglet "Détail des Revenus")
  output$revenuDetailTable <- renderDT({
    data <- budget_data()
    req(data$revenu_detail)
    if (nrow(data$revenu_detail) == 0) {
      return(datatable(data.frame(Message = "Aucune donnée de revenu détaillée disponible."),
                       options = list(dom = 't', paging = FALSE, searching = FALSE)))
    }
    # Transformation et formatage pour l'affichage en tableau
    df_formatted <- data$revenu_detail %>%
      pivot_wider(names_from = Mois, values_from = Montant) %>% # Met les mois en colonnes
      mutate(Type = as.character(Type)) # S'assurer que 'Type' est bien un caractère
    
    # Applique le formatage numérique et remplace NA par "" AVANT formatStyle
    df_display <- df_formatted %>%
      mutate_if(is.numeric, ~format(., big.mark = " ", nsmall = 0)) %>%
      replace(is.na(.), "") # Remplace les NA par des chaînes vides pour un affichage propre
    
    datatable(df_display, # Utilise le dataframe formaté pour l'affichage
              options = list(pageLength = 10, dom = 'Bfrtip', # Active les boutons et les fonctionnalités standard
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), # Boutons d'export
              extensions = 'Buttons', # Nécessaire pour les boutons d'export
              rownames = FALSE, # Ne pas afficher les numéros de ligne
              caption = "Détail des cotisations par personne et par mois") %>%
      # *** NOUVEAUTÉ : Coloration des cellules vides en rouge ***
      formatStyle(
        columns = names(df_display)[-1], # Applique le style à toutes les colonnes sauf la première ('Type')
        target = 'cell', # Applique le style à la cellule elle-même
        backgroundColor = JS("function(data, type, row, meta) {
          if (type === 'display' && data === '') {
            return 'background-color: #ffcccc;'; /* Rouge très clair */
          }
          return '';
        }")
      )
  })
  
  # Tableau détaillé des Depenses (dans l'onglet "Détail des Depenses")
  output$depenseDetailTable <- renderDT({
    data <- budget_data()
    req(data$depense_detail)
    if (nrow(data$depense_detail) == 0) {
      return(datatable(data.frame(Message = "Aucune donnée de dépense détaillée disponible."),
                       options = list(dom = 't', paging = FALSE, searching = FALSE)))
    }
    df_formatted <- data$depense_detail %>%
      pivot_wider(names_from = Mois, values_from = Montant) %>%
      mutate(Type = as.character(Type))
    
    df_display <- df_formatted %>%
      mutate_if(is.numeric, ~format(., big.mark = " ", nsmall = 0)) %>%
      replace(is.na(.), "")
    
    datatable(df_display,
              options = list(pageLength = 10, dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
              extensions = 'Buttons',
              rownames = FALSE,
              caption = "Détail des Depenses par Type et par Mois (FCFA)") %>%
      # *** NOUVEAUTÉ : Coloration des cellules vides en rouge ***
      formatStyle(
        columns = names(df_display)[-1], # Applique le style à toutes les colonnes sauf la première ('Type')
        target = 'cell',
        backgroundColor = JS("function(data, type, row, meta) {
          if (type === 'display' && data === '') {
            return 'background-color: #ffcccc;'; /* Rouge très clair */
          }
          return '';
        }")
      )
  })
  
  # Tableau résumé du bilan (dans l'onglet "Bilan Mensuel")
  output$bilanSummaryTable <- renderDT({
    data <- budget_data()
    req(data$bilan_summary)
    if (nrow(data$bilan_summary) == 0) {
      return(datatable(data.frame(Message = "Aucune donnée de bilan disponible."),
                       options = list(dom = 't', paging = FALSE, searching = FALSE)))
    }
    df_formatted <- data$bilan_summary %>%
      mutate(
        # Formatage des colonnes numériques pour l'affichage
        Revenus = format(Revenus, big.mark = " ", nsmall = 0),
        Depenses = format(Depenses, big.mark = " ", nsmall = 0),
        Bilan = format(Bilan, big.mark = " ", nsmall = 0),
        Bilan_Cumule = format(Bilan_Cumule, big.mark = " ", nsmall = 0)
      ) %>%
      rename(
        Mois = Mois,
        `Revenus (FCFA)` = Revenus,
        `Depenses (FCFA)` = Depenses,
        `Bilan Mensuel (FCFA)` = Bilan,
        `Bilan Cumulé (FCFA)` = Bilan_Cumule
      )
    
    datatable(df_formatted,
              options = list(pageLength = 12, dom = 'Bfrtip', # Afficher tous les mois si 12 lignes
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
              extensions = 'Buttons',
              rownames = FALSE,
              caption = "Bilan Financier Mensuel (FCFA)") %>%
      # Coloration conditionnelle pour le Bilan Mensuel
      formatStyle(
        'Bilan Mensuel (FCFA)', # Nom de la colonne à styliser
        `value` = JS("function(cell, row, data, rowIdx, colIdx) {
          // Convertit la chaîne formatée en nombre pour la comparaison
          var num = parseFloat(cell.replace(/ /g, ''));
          return num < 0 ? 'color:white;background-color: #dc3545;' : 'color:white;background-color: #28a745;';
        }")
      )
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)