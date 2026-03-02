# ------------------------------------------------------------------------------
# Robust Heart Disease Analysis Dashboard
# ------------------------------------------------------------------------------

# Required Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, tidyverse, plotly, corrplot, DT, reshape2)

# 1. DATA PREPARATION ----------------------------------------------------------
# Loading data and cleaning up "?" values (common in this dataset)
load_and_clean_data <- function() {
  # Reading the CSV
  df <- read.csv("data.csv", stringsAsFactors = FALSE, na.strings = "?")
  
  # Conversion to numeric for relevant columns
  numeric_cols <- c("age", "trestbps", "chol", "thalach", "oldpeak", "ca")
  df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)
  
  # Factorizing categorical variables for better plotting
  df$sex <- factor(df$sex, levels = c(0, 1), labels = c("Female", "Male"))
  df$cp <- factor(df$cp, levels = 1:4, labels = c("Typical Angina", "Atypical Angina", "Non-anginal", "Asymptomatic"))
  df$target <- factor(df$num > 0, levels = c(FALSE, TRUE), labels = c("No Disease", "Disease"))
  
  return(df)
}

df <- load_and_clean_data()

# 2. USER INTERFACE (UI) -------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Deep-Dive Health Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Statistical Deep-Dive", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Correlation Matrix", tabName = "correlation", icon = icon("th")),
      menuItem("Raw Data Explorer", tabName = "rawdata", icon = icon("table"))
    ),
    hr(),
    h4(" Global Filters", style = "margin-left: 15px;"),
    selectInput("filter_sex", "Gender:", choices = c("All", levels(df$sex))),
    checkboxGroupInput("filter_cp", "Chest Pain Type:", choices = levels(df$cp), selected = levels(df$cp))
  ),
  
  dashboardBody(
    tabItems(
      # Tab 1: Overview and Distributions
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("vbox_avg_age", width = 3),
                valueBoxOutput("vbox_total_obs", width = 3),
                valueBoxOutput("vbox_disease_rate", width = 3),
                valueBoxOutput("vbox_missing", width = 3)
              ),
              fluidRow(
                box(title = "Variable Distribution", width = 8, status = "primary", solidHeader = TRUE,
                    selectInput("plot_var", "Select Variable to Visualize:", choices = names(df)[sapply(df, is.numeric)]),
                    plotlyOutput("distPlot", height = "400px")),
                box(title = "Demographic Split", width = 4, status = "warning", solidHeader = TRUE,
                    plotlyOutput("pieChart", height = "400px"))
              )
      ),
      
      # Tab 2: Statistical Deep-Dive
      tabItem(tabName = "stats",
              fluidRow(
                box(title = "Comparison Plot (Target vs Numeric)", width = 7, status = "info", solidHeader = TRUE,
                    selectInput("comp_var", "Select Metric:", choices = names(df)[sapply(df, is.numeric)]),
                    plotlyOutput("compPlot")),
                box(title = "Summary Statistics", width = 5, status = "info", solidHeader = TRUE,
                    tableOutput("summaryTable"))
              ),
              fluidRow(
                box(title = "Scatter Relationship", width = 12, status = "primary", solidHeader = TRUE,
                    column(4, selectInput("scat_x", "X-Axis:", choices = names(df)[sapply(df, is.numeric)], selected = "age")),
                    column(4, selectInput("scat_y", "Y-Axis:", choices = names(df)[sapply(df, is.numeric)], selected = "chol")),
                    column(12, plotlyOutput("scatterPlot"))
                )
              )
      ),
      
      # Tab 3: Correlation
      tabItem(tabName = "correlation",
              box(title = "Feature Correlation Heatmap", width = 12, status = "danger", solidHeader = TRUE,
                  plotOutput("corrPlot", height = "600px"))
      ),
      
      # Tab 4: Raw Data
      tabItem(tabName = "rawdata",
              box(title = "Dataset Browser", width = 12, status = "primary",
                  DTOutput("dataTable"))
      )
    )
  )
)

# 3. SERVER LOGIC --------------------------------------------------------------
server <- function(input, output) {
  
  # Reactive Data Filtering
  filtered_df <- reactive({
    data <- df
    if(input$filter_sex != "All") data <- data %>% filter(sex == input$filter_sex)
    data <- data %>% filter(cp %in% input$filter_cp)
    data
  })
  
  # --- Value Boxes ---
  output$vbox_avg_age <- renderValueBox({
    valueBox(round(mean(filtered_df()$age, na.rm=T), 1), "Avg Age", icon = icon("user"), color = "blue")
  })
  output$vbox_total_obs <- renderValueBox({
    valueBox(nrow(filtered_df()), "Observations", icon = icon("database"), color = "purple")
  })
  output$vbox_disease_rate <- renderValueBox({
    rate <- round(mean(filtered_df()$target == "Disease", na.rm=T)*100, 1)
    valueBox(paste0(rate, "%"), "Disease Prevalence", icon = icon("heartbeat"), color = "red")
  })
  output$vbox_missing <- renderValueBox({
    miss <- sum(is.na(filtered_df()))
    valueBox(miss, "Missing Values", icon = icon("exclamation-triangle"), color = "yellow")
  })
  
  # --- Distribution Plot ---
  output$distPlot <- renderPlotly({
    p <- ggplot(filtered_df(), aes_string(x = input$plot_var, fill = "target")) +
      geom_density(alpha = 0.5) +
      theme_minimal() +
      labs(title = paste("Distribution of", input$plot_var), x = input$plot_var, y = "Density")
    ggplotly(p)
  })
  
  # --- Pie Chart ---
  output$pieChart <- renderPlotly({
    df_pie <- filtered_df() %>% count(sex)
    plot_ly(df_pie, labels = ~sex, values = ~n, type = 'pie') %>%
      layout(title = "Gender Split in Selection")
  })
  
  # --- Comparison Plot ---
  output$compPlot <- renderPlotly({
    p <- ggplot(filtered_df(), aes_string(x = "target", y = input$comp_var, fill = "target")) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste(input$comp_var, "by Disease Status"))
    ggplotly(p)
  })
  
  # --- Summary Table ---
  output$summaryTable <- renderTable({
    filtered_df() %>%
      group_by(target) %>%
      summarise(
        Count = n(),
        Mean = mean(get(input$comp_var), na.rm=T),
        Median = median(get(input$comp_var), na.rm=T),
        SD = sd(get(input$comp_var), na.rm=T)
      )
  }, digits = 2)
  
  # --- Scatter Plot ---
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filtered_df(), aes_string(x = input$scat_x, y = input$scat_y, color = "target")) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(title = "Relationship Analysis")
    ggplotly(p)
  })
  
  # --- Correlation Plot ---
  output$corrPlot <- renderPlot({
    cor_data <- df %>% select_if(is.numeric) %>% na.omit()
    res <- cor(cor_data)
    corrplot(res, method = "color", type = "upper", order = "hclust", 
             addCoef.col = "black", tl.col = "black", tl.srt = 45)
  })
  
  # --- Data Table ---
  output$dataTable <- renderDT({
    datatable(filtered_df(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Run the Application
shinyApp(ui = ui, server = server)
