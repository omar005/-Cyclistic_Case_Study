csv_common_columns <- function(csv_data_frames) {
  
  # If only 1 csv passed, return FALSE
  if (length(csv_data_frames) == 1) {
    return(FALSE)
  }
  
  # Give each CSV an ID
  csv_id <- seq_along(csv_data_frames)
  
  # Create an empty list of lists
  list_of_lists <- list()
  
  # Iterate all the columns names of all the given CSVs
  for (i in seq_along(csv_data_frames)) {
    col_names <- colnames(csv_data_frames[[i]])
    
    # If the column name already exists, add its corresponding CSV ID
    for (j in seq_along(col_names)) {
      #If there is at least one match (i.e., if the column name is already present in the list), Return TRUE
      if (any(sapply(list_of_lists, function(x) col_names[j] %in% x[[1]]))) {
        list_index <- which(sapply(list_of_lists, function(x) col_names[j] %in% x[[1]]))
        list_of_lists[[list_index]]$ids <- c(list_of_lists[[list_index]]$ids, csv_id[i])
      } else {
        # Else If the column name doesn't exist, add it to the list of lists
        list_of_lists[[length(list_of_lists) + 1]] <- list(names = col_names[j], ids = list(csv_id[i]))
      }
    }
  }
  
  common_cols <- NULL
  more_than_one_csv_cols <- NULL
  one_csv_cols <- NULL
  
  for (i in seq_along(list_of_lists)) {
    if (length(list_of_lists[[i]]$ids) == length(csv_data_frames)) {
      common_cols <- c(common_cols, list_of_lists[[i]]$names)
    } else if (length(list_of_lists[[i]]$ids) > 1) {
      more_than_one_csv_cols <- c(more_than_one_csv_cols, list_of_lists[[i]]$names)
    } else {
      one_csv_cols <- c(one_csv_cols, list_of_lists[[i]]$names)
    }
  }
  if (is.null(more_than_one_csv_cols) & is.null(one_csv_cols)){return(TRUE)}
  
  report <- paste(
    "Columns common in all CSVs =" , common_cols,
    "Columns that exist in more than 1 CSV =" ,more_than_one_csv_cols,
    "Columns that exist only in 1 CSV =", one_csv_cols
  )
  
  return(report)
  
}


# #csv_files <- c("202212-divvy-tripdata.csv","202301-divvy-tripdata.csv","202302-divvy-tripdata.csv")
# csv_files <-c(
#   # Read the CSV files
#   "202212-divvy-tripdata.csv",
#   "202301-divvy-tripdata.csv",
#   "202302-divvy-tripdata.csv",
#   "202303-divvy-tripdata.csv",
#   "202304-divvy-tripdata.csv"
# )
# result <- csv_common_columns(csv_files)
# print(result)