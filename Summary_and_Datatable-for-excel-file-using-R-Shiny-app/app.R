library(shiny)
library(readxl)
library(DT)

# List of built-in datasets
built_in_datasets <- c("mtcars", "iris", "faithful")


ui <- fluidPage(
  titlePanel("student survey data dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      #dataset buildin data
      radioButtons("data_source","choose data source:",
                   choices = c("Built-in Dataset","Upload excel file"),
                   selected = "Built-in Dataset"
      ),
      #Built in dataset selection
      conditionalPanel(
        condition = "input.data_source == 'Built-in Dataset'",
        selectInput("built-in","Select built in data set",choices = built_in_datasets )
      ),
      conditionalPanel(
        condition = "input.data_source == 'Upload excel file'",
        fileInput("file","Upload excel File",accept = c("xlsx")),
        uiOutput("sheet_ui")
      ),
      
      
      
      actionButton("submit", "Load Data")
    ),
    #tab layout
    tabsetPanel(
      tabPanel("summary",
               verbatimTextOutput("dataSummary")),
      tabPanel("Data Table",
               DTOutput("dataTable")
      )
    )
  )
  
  
  
  
)

server <- function(input, output, session) {
  #reactive expression  to store  uploaded file path
  file_path <- reactive({
    req(input$file)
    input$file$datapath
  })
  
  #observe the uploaded file and update the sheet
  observeEvent(input$file,{
    sheets <- excel_sheets(file_path()) 
    updateSelectInput(session,
                      "sheet",
                      "Select sheet",
                      choices = sheets,
                      selected = sheets[1])
  })
  
  #Generate UI for sheet selection 
  output$sheet_ui <- renderUI(
    {
      req(input$file)
      selectInput("sheet","Select Sheet",choices=NULL)
    }
  )
  # Reactive function to load data based on user selection
  selected_data <- eventReactive(input$submit,{
    if(input$data_source == "Built-in Dataset"){
      get(input$'built-in')
    } else{
      req(input$file,input$sheet)
      read_excel(file_path(),sheet = input$sheet)
    }
    
  })
  
  #Render dataset summary
  output$dataSummary <- renderPrint({
    req(selected_data())
    summary(selected_data())
  })
  
  
  #Render data as a table
  output$dataTable <- renderDT({
    req(selected_data())
    datatable(selected_data())
  })
  
  
  
}

shinyApp(ui, server)













