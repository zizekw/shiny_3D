library(shiny)
library(tidyverse)
library(rgl)
library(MSnbase)
library(mzR)

data_dir <- dir("./data",full.names = TRUE, all.files = FALSE, recursive = TRUE)

# Define UI for application
ui <- navbarPage(theme = shinythemes::shinytheme("cerulean"), title = "3D Plot for MS Data",
                 
                 tabPanel(title = "Home",
                          
                          sidebarLayout(
                              sidebarPanel(
                                  tags$h4(tags$b("Instructions")),
                                  tags$body("Upload a non-confidential open source Mass Spec data file (ex. `.mzML`,`.mzXML`). Once the upload is complete - tune your settings appropriately."),
                                  tags$h4(tags$b("File Input")),
                                  fileInput("file", label = NULL),
                                  tags$h4(tags$b("Settings")),
                                  # numericInput("mzmin", label = "Input your min. target m/z value", value = 183),
                                  # numericInput("mzmax", label = "Input your max. target m/z value", value = 185),
                                  sliderInput("mzrange", label = "Input your m/z range", min = 0, max = 500, value = c(184, 184.2), step = 0.1),
                                  sliderInput("rtrange", label = "Input your RT range (min)", min = 0, max = 50, value = c(5, 35)),
                                  sliderInput("res", label = "Resolution", min = 0.0001, max = 5, value = 0.005, step = 0.01),
                                  numericInput("ncols", label = "Number of Colours", value = 100000)
                              ),
                              mainPanel(h3(tags$b("3D Plot")),
                                        rglwidgetOutput("rgl", width = "512px", height = "512px") # ,
                                        # textOutput("test")
                              )
                          )
                          
                 ),
                 tags$hr(),
                 tags$body(tags$i("© 2019 Bill Zizek"))
)

# Define server logic 
server <- function(input, output) {
  options(shiny.maxRequestSize=3000*1024^2)
  # options(rgl.printRglwidget = TRUE)
  
  # data_dir <- reactive({
  #   # dir(input$file$datapath, full.names = TRUE, all.files = FALSE, recursive = TRUE)
  #   input$file$datapath
  # })
  
  rawData <- # reactive({
    readMSData(files = 
                 # input$file$datapath,
                 data_dir,
                 mode = "onDisk", verbose = TRUE)
  # }) 
  
  data <- # reactive({
    rawData %>% filterFile(file = 1) # just incase they upload multiple files, update: nvm this should be accounted for
  # })
  
  hd <- # reactive({
    header(rawData)
  # })
  
  ms1 <- # reactive({
    which(hd$msLevel == 1)
  # })
  
  rtsel <- reactive({
    
    # hd <- # reactive({
    #   header(data)
    # # })
    
    rtmin <- reactive({head(input$rtrange, 1)})
    rtmax <- reactive({tail(input$rtrange, 1)})
    
    range <- hd$retentionTime[ms1] / 60 > rtmin() &
                hd$retentionTime[ms1] / 60 < rtmax()
    
    return(range)
  })


  mzmin <- reactive({head(input$mzrange, 1)})
  mzmax <- reactive({tail(input$mzrange, 1)})
  
  res <- reactive({input$res})
 
  ncols <- reactive({input$ncols})
  

  # rtsel <- # reactive({
  #   hd$retentionTime[ms1] / 60 > rtmin() &
  #     hd$retentionTime[ms1] / 60 < rtmax()
  # })
  
  dd <- reactive({
    M <- # reactive({
      MSmap(rawData, ms1[rtsel()], mzmin(), mzmax(), res(), zeroIsNA=TRUE)
    # })
    
    df <- # reactive({
      as(M, "data.frame")
      # })
    
    return(df)
  })
  
  mz <- reactive({dd() %>% select(mz) %>% unlist() %>% as.double()})
  rt <- reactive({dd() %>% select(rt) %>% unlist() %>% as.double()})
  int <- reactive({dd() %>% select(intensity) %>% unlist() %>% as.double()})
  
  # dd <- # reactive({
  #   as(M, "data.frame")
  #   # })

  output$rgl <- renderRglwidget({
    
    # rgl.open(useNULL=T)
    
    bg3d(color = "white")
    
    par3d(mouseMode = "trackball")
    
    rgl::plot3d(x = mz(), y = rt(), z = int(), add = FALSE,
           type = "h", col = hcl.colors(ncols(), palette = "BluGrn"), # hcl.pals() to get pals
           box = FALSE, axes = TRUE, xlab = "M/Z", ylab = "Migration time", zlab = "", main = paste0("EIE for m/z ",mzmin(),"-",mzmax())
           )
    
    axes3d(box = FALSE)
    
    rglwidget(height = "900px", width = "900px",
              reuse = TRUE,
              webGLoptions = list(preserveDrawingBuffer = TRUE))

    # if(is.null(rawData)){return()}
  })
  
  # output$test <- renderPrint(hd)
  
}

# Run the application 
shinyApp(ui = ui, server = server)