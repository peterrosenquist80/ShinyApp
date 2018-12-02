## ------Text Comparison App------
##   This app is designed to collect data from surveys where the participant is 
## shown a set of texts and is asked a question about those texts.  The provided 
## example datasets show the participant 20 sets of IMDB movie reviews and 
## ask the participant to select the most positive review.  Data collected from
## this app could be used with sentiment analysis alogrithms to determine the 
## degree of positivity expressed by English emotion words.  I codeveloped this
## app as my final project for my graduate level Political Science course,
## Applied Statistical Programming. 

## install 'shiny', 'DT', and 'shinythemes' packages as needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, DT, shinythemes)
require(shiny)
require(shinythemes)
require(DT)

## !-- Need to set working directory to location of 'Movies.rds' and 'input.rds' files
Movies <- readRDS("Movies.rds")
Input <- readRDS('input.rds')

## takes inputs:  
##               dataset w/ comparison id numbers; 
##               dataset with movie review pairings(or trios, etc); 
##               Title of survey; 
##               index of the question within a dataset; 
##               survey taker ID
runShiny <- function(dataSet, dataSet2, Title, QuestionID, workerID){
  
  
  ## assign initial pair of reviews at dataset index 1 or next set if picking up where left off
  if (exists("index")) {
    samp <<- c()
    for (i in 3:ncol(dataSet2)){
      samp[i-2] <<- dataSet$Review[which(dataSet2[eval(index), i] == dataSet$ID)]
    }
  }
  ## or if starting new session, assigns set of reviews to first documents in dataset
  else {
    samp <<- c()
    for (i in 3:ncol(dataSet2)){
      samp[i-2] <<- dataSet$Review[which((dataSet2[1,i]) == dataSet$ID)]}
  }
  ## creates UI for a page with: 
  ##              radio buttons associated with individual review text, 
  ##              submit button for saving responses, 
  ##              and "placeholder" for use with 'insertUI()' function 
  ui=shinyUI(fluidPage
        (theme = shinytheme("superhero"),
             titlePanel(Title),
             ## create main html element, which contains radio buttons (with choices' text), submit button, 
             ## download button, and an optional data table
             mainPanel(
               fluidRow(
                 radioButtons("review_choice", 
                              label = h4(dataSet[,2][which(dataSet$ID == QuestionID)]),
                              choices = samp, 
                              width=800, 
                              selected = character(0)
                              ),
                 ## 'placeholder' allows for ease of inserting UI elements later in code
                 tags$div(id = 'placeholder'), 
                 actionButton("submit", "Submit"),
                 downloadButton("save_review", label = "Download Selections"), width=500
               ),
               DT::dataTableOutput("responses", width = 300)
             ),
             ## sidebar panel displaying points
             sidebarPanel(
               fluidRow(
                 fluidRow(id = 'points', 
                          HTML("&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;Points: 0")),
                 tags$div(id = 'placeholder2')
               )
             )
        )
  )
  
  server=shinyServer(function(input, output) {
    ## creates points object 'pts' for keeping score
    pts <<- 0
    
    ## reactive function that creates object 'data' based on radio button input
    ## loop used to create vector with all available choices
    formData <- reactive({
      choice <<- NULL
      data <- c()
      ##runs loop of documents and identifies which was chosen
      for (i in 3:ncol(dataSet2)){
        if(substr(input$review_choice, 1, 20) == substr(samp[i-2], 1, 20)) choice <<- dataSet2[index, i]
        data[i-2] <- dataSet2[index, i]
      }
      ##incorportates 'choice' id into 'data' object
      data <- c(data, choice, QuestionID, as.character(Sys.time()))
      data
    })
    
    ## function that creates 'responses' data.frame with 'data' object
    saveData <- function(data) {
      data <- as.data.frame(t(data))
      if (exists("responses")) {
        responses <<- rbind(responses, data)
      } else {
        responses <<- data
      }
    }
    ## function used to determine presence of 'responses' for data table or download
    loadData <- function() {
      if (exists("responses")) {
        responses
      }
    }
    
    ## create index objects for moving through pairs from second input dataset
    if (!exists("index")) index <<- 1
    
    ## when 'submit' is clicked, save the data
    ## removes current radio buttons, replaces with new radio buttons with updated samp texts
    ## index object created here to move through consecutive rows of the input dataset2
    observeEvent(input$submit, {
      ## toggle boolean created to ensure that an option is selected before 'submit' carries
      ## out any other actions
      toggle <- F
      for (i in 3:ncol(dataSet2)){
        if (substr(input$review_choice, 1, 20) == substr(samp[i-2], 1, 20)) toggle <- T
      }
      if (toggle == T){
        ## either creates object 'responses' or updates it to include next response
        saveData(formData())
        
        ## create rate object of ratings of each document
        rate <<- c()
        for (i in 3:ncol(dataSet2)){
          rate[i-2] <<- as.numeric(dataSet[dataSet2[eval(index),i], 1])
        }
        ## then check to see if submitted selection had highest or lowest rating
        ## award points based on this criteria
        if (min(rate) == dataSet[as.numeric(choice), 1]) pts <<- pts - 10
        if (max(rate) == dataSet[as.numeric(choice), 1]) pts <<- pts + 10
        
        ## update index to next row in 'Input' dataset
        index <<- index + 1
        ## if index passes last row of 'Input', remove radio buttons
        if(index > nrow(dataSet2)){
          removeUI(
            selector = '#review_choice'
          )
          ## remove submit button
          removeUI(
            selector = '#submit'
          )
          ## remove old UI element with points
          removeUI(
            selector = '#points'
          )
          ## update points with new UI element
          insertUI(
            ui = fluidRow(id = 'points', 
                          HTML("&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;Points: ", 
                               eval(pts))),
            selector = '#placeholder2',
            where = 'beforeBegin'
          )
          ## prompts user to download 'responses' if at the end of dataset2
          insertUI(
            selector = '#placeholder',
            ui = titlePanel(h5("The survey is now complete. Please select download, save, and exit."))
          )
        }
        ## if not at the end of 'Input', runs a loop to update next choices' text, and inserts new
        ## radio buttons that contain the updated choices after removing the old ones
        ## '#placeholder' tag is used to specify where the new buttons go
        else{ 
          for(i in 3:ncol(dataSet2)){
            samp[i-2] <<- dataSet[dataSet2[eval(index),i], 2]
          }
          ## remove old radio buttons
          removeUI(
            selector = '#review_choice'
          )
          ## insert new radio buttons
          insertUI(
            selector = '#placeholder',
            where = 'beforeBegin',
            ui = radioButtons("review_choice", label = h3(dataSet[,2][which(dataSet$ID == QuestionID)]),
                              choices = samp, width=800, selected = character(0))
          )
          ## remove old UI element with points 
          removeUI(
            selector = '#points'
          )
          ## insert updated points in new UI element
          insertUI(
            ui = fluidRow(id = 'points', 
                          HTML("&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&ensp;Points: ", 
                               eval(pts))),
            selector = '#placeholder2',
            where = 'beforeBegin'
          )
        }
      }
    })
    
    ## code to show in-app data table
    ## updates with current response when "submit" is clicked
    ## output$responses <- DT::renderDataTable({
    ##  input$submit
    ##  loadData()
    ## })
    
    ## creates column names for .csv file based on number of choices
    colnames <- c()
    for (i in 3:ncol(dataSet2)){
      colnames[i-2] <- paste("Document_", (i-2), sep = "")
    }
    ## downloads data.frame "responses" to a .csv file 
    output$save_review <- downloadHandler(filename = function() {paste0('data', Sys.time(), ".csv")}, 
                                          content = function(filename) {
                                                                        responses[,ncol(responses) + 1] <- workerID
                                                                        colnames(responses) <- c(colnames, 
                                                                            'Answer', 'QuestionID',
                                                                            'Completion Time', 
                                                                            'Worker ID')
                                                                        write.csv(responses, filename)
                                                    }
                                          )
  })
  ## essential function for putting the server and ui into an app and running it
  shinyApp(server = server, ui=ui)
}

## -------IMPORTANT-------
## to restart the survey the from the 
## first set of movie reviews the
## 'index' variable must be removed 
## -----------------------
suppressWarnings(rm(index))

## this line ultimately runs the app
runShiny(Movies, Input, 'Movie Reviews', 501, "CB")

## for abbreviated survey experience set: index <- 16

