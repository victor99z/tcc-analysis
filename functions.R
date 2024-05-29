getTps <- function(data) {
  # Convert the timestamp to a POSIXct date-time format
  data$timeStamp <- as.numeric(data$timeStamp)
  data$timeStamp <- as.POSIXct(data$timeStamp / 1000, origin="1970-01-01")
  
  # Create a new column with rounded seconds for grouping
  data$second <- floor_date(data$timeStamp, unit = "second")
  
  # Calculate the number of transactions per second
  tps <- data[] %>%
    group_by(second) %>%
    summarise(transactions = n())
  
  return(tps)
}

getAvgOnchain <- function(data) {
  data <- data %>%
    mutate(avg_value = ifelse(name == "system_memory_used" , avg_value / (1024 * 1024), avg_value)) %>%
    mutate(avg_value = ifelse(name == "system_disk_readbytes" , avg_value / (1024 * 1024), avg_value)) %>%
    mutate(avg_value = ifelse(name == "system_disk_writebytes" , avg_value / (1024 * 1024), avg_value)) %>%
    mutate(avg_value = ifelse(name == "system_cpu_sysload" , avg_value / 100, avg_value))
  return(data)
}