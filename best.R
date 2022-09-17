# this code reads data set provided by the US department of human
# health and human services. The data contains information regarding quality
# of care and mortality rates relating to certain health events and diseases
# such as heart attacks, heart failure, and pneumonia. Given the 
# state and the health event/condition, bestHospital will return the hospital
# in that state with the lowest mortality rate of the given condition


#read hospital statistics file
outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#variable to store lowest rate
theMin <- .Machine$double.xmax
#to store the best hospital
lowHospital <- character()

bestHospital <- function(state, wantedRate = "heart attack"){
  #forms the name of the column
  causeName <- causeString(wantedRate)
  #gets valid sates from original file
  validStates <- outcomeFile[['State']]
  #checks state against valid states
  checkState(state, validStates)
  #checks if the passed condition is valid 
  checkRate(wantedRate)
  #gets the death rates for the given condition
  rateColumnNum <- match(causeName,names(outcomeFile))
  #stores all rates, states, and hopsital info in a seperate data frame
  #for iteration 
  outcomeData <- data.frame(as.numeric(outcomeFile[,rateColumnNum]),
                            outcomeFile[['Hospital.Name']], outcomeFile[['State']])
  #renames cloumns for ease of use
  colnames(outcomeData) <- c("Outcome","Hospital", "State" )
  #removes incomplete rows
  outcomeData <- outcomeData[complete.cases(outcomeData),]
  #iterates over rows to find the lowest rate
  #and store it in theMin
  apply(outcomeData, 1, findlowestRate,
                      cause = 'Outcome', wantedState = state)
  #return the results
  return(c(theMin,lowHospital))
}

#logic for comparing the current minmum and current hospital with 
#the hospital currently being read
findlowestRate <- function(x, cause, wantedState){
  if (is.na(x[[cause]])) {
    return()
  }
  if ((as.numeric(x[[cause]])) < theMin && x[["State"]] == wantedState){
    theMin <<- as.numeric(x[[cause]])
    lowHospital <<- x[['Hospital']]
  }
  else if (as.numeric(x[[cause]]) == theMin && x[["State"]] == wantedState) {

    if (x[['Hospital']] < lowHospital ) {
      lowHospital <<- x[['Hospital']]
    }
  }
}
#forms the name of the column condition to use for getting the 
#condition rate column from the original file
causeString <- function(cause){
  columnName <- 'Hospital.30.Day.Death..Mortality..Rates.from.'
  if (cause == 'heart attack'){
    columnName <- paste0(columnName, 'Heart.Attack')
  } else if (cause == 'heart failure'){
    columnName <- paste0(columnName, 'Heart.Failure')
  } else if (cause == 'pneumonia') {
    columnName <- paste0(columnName, 'Pneumonia')
  }
  return(columnName)
}
#checks if the provided state is a valid state
checkState <- function(stateName, statesList){
  if(!(stateName %in% statesList))
  {
    stop('Invalid State')
  }
  
}
#checks if the provided condition is a valiud condition
checkRate <- function (theRate){
  if (!(theRate == 'heart attack') && !(theRate == 'heart failure') 
      && !(theRate == 'pneumonia'))  {
    stop('Invalid outcome')
  }
}
