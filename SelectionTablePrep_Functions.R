
##############################################################
# CURRENT FUNCTIONS:

# unzipfiles() -> this unzips any downloaded folders and puts all the files into one unzipped folder
# create_monthfolders() -> this separates all the files (sound files) into monthly folders with the correct naming system
# zipfiles -> this zips your sound file folders quickly for uploading to drive/sharing
# wav_info_file() -> this creates a wav info csv, creates a new folder for the file, and puts the file into the folder
# create_tables() -> this creates selection tables based on species and table type.
#############################################################


#----------------------------------
# Packages to read in:
#----------------------------------
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)
library(stringr)
library(warbleR) # only used in wav_info_file()
library(zip)     # only used to zipfiles()


#----------------------------------
# Input variables
#----------------------------------

#For all species:
TodaysDate <- "12NOV2026"    # Only used for creating the wav info file
RecorderID <- "NY3"          # Examples: NY3 for NYSERDA Attentive, HCA for Hudson Canyon buoy A, RMIA for RMI buoy A
Deployment <- "D5"           # Example: D5
Species <- "Baleen Whale"    # Options: Baleen Whale, Delphinid, Sperm Whale
TableType <- "full"          # Options: filtered or full
WD <- "C:/Users/mpapadopoulos/Downloads/D5 NY3 WAVs"           # this is the folder you download your sound files to


#Only for Baleen Whale and Delphinid:
WDMattTables <- "C:/Users/mpapadopoulos/Downloads/D5 NY3 WAVs/MattTables"      # this is the folder you download Matt's detector outputs to

#Only for Sperm Whale:
WDcsv <- "C:/Users/mpapadopoulos/Downloads/D5 NY3 WAVs/CSV"    # this is where you download the csv created from the pgdf detector outputs
DeploymentDate <- as_datetime("2024-07-08 08:00:00")           # This is the time of the deployment (specifically when it was deployed, not when it started recording -- reduces the FPs we review)
RecoveryDate <- as_datetime("2024-12-16 16:00:00")             # This is the time of recover (specifically when it was recovery, not when it stopped recording -- reduces the FPs we review)
SoundTrapNumber <- "8587"                                      # this is the soundtrap number, used because the pgdf file names don't conserve the soundtrap number and therefore won't match the wav file names


########################################################################
#                                                                      #
#                                 README                               #
#                                                                      #
# Each function is written out after its header                        #
# The next line of code will be the function ( e.g. zipfiles() ) with  #
# the correct variables passed into it                                 #
#                                                                      #
########################################################################

# ---------------------------------------------------------------------------------------------------------------------------------
# Function unzipfiles()
#
# UNZIP DOWNLOADED SOUND FILE FOLDERS AND PUT THEM IN A SINGLE FOLDER
#----------------------------------------------------------------------------------------------------------------------------------

unzipfiles <- function(WD) {

  
  # 1. Set wd() and get a list of all zip files in your folder
  
setwd(WD)
  
WDSoundFiles <- paste0(WD, "/SoundFiles")


zip_files <- list.files(path = WDSoundFiles, pattern = "\\.zip$", full.names = TRUE)

# 2. Loop through and unzip each one
for (file in zip_files) {
  unzip(file, exdir = WDSoundFiles)
}
# STEP 1: Set working directory to location containing the soundfile folders



setwd(WDSoundFiles)

# STEP 2: Find all .wav files in all subfolders - as long as folders from Matt are unzipped this should work to read all files

wav_files <- list.files(
  path = getwd(),
  pattern = "\\.wav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)


# STEP 3: Check how many WAV files were found

cat("Number of WAV files found:", length(wav_files), "\n")


# STEP 4:

for(i in seq_along(wav_files)){
  
  # Get the filename without the folder path
  filename <- basename(wav_files[i])
  
  # Create folder name
  #allsoundfiles <- "All Sound Files"
  
  # Create all sound files folder if it doesn't already exist
  #if(!dir.exists(allsoundfiles)){
  #  dir.create(allsoundfiles)
  

  # Copy the WAV file into the appropriate month folder
  file.copy(
    wav_files[i],
    file.path(WDSoundFiles, filename)
  )

}  
  
  # Print progress
  cat("Copied:", filename, "->", WDSoundFiles)
  
  
  # Find all immediate subfolders (excludes the parent directory itself)
  subfolders <- list.dirs(WDSoundFiles, full.names = TRUE, recursive = FALSE)
  
  # Delete those subfolders and everything inside them
  unlink(subfolders, recursive = TRUE)
}



############PASS IN THE INPUT VARIABLE##########################
unzipfiles(WD)

# ---------------------------------------------------------------------------------------------------------------------------------
# Function create_monthlyfolders()

# CREATE MONTHLY SOUND FILE FOLDERS
#----------------------------------------------------------------------------------------------------------------------------------

create_monthlyfolders <- function(WD) {
  
  
# Step 1: Set working directory to folder with all unpacked files  
  setwd(WD)
  
  setwd(paste0(WD, "/SoundFiles"))
  
  
# STEP 2: Find all .wav files in all subfolders - as long as folders from Matt are unzipped this should work to read all files

wav_files <- list.files(
  path = getwd(),
  pattern = "\\.wav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

# STEP 3: Check how many WAV files were found

cat("Number of WAV files found:", length(wav_files), "\n")


# STEP 4: Create folders based on characters 12 and 13
#         and COPY each .wav file into the appropriate month folder

for(i in seq_along(wav_files)){
  
  # Get the filename without the folder path
  filename <- basename(wav_files[i])
  
  # Extract characters 8 and 9 = month
  month <- substr(filename, 8, 9) #this is where you might need to change the character position for month based on the file name
  
  # Extract characters 6 and 7 = year
  year <- paste0("20", substr(filename, 6, 7))
  
  # Create month-group name
  monthgroup <- paste0(Deployment, "_", RecorderID, "_", year, "_", month)
  
  setwd(WDSoundFiles)
  
  # Create month folder if it doesn't already exist
  if(!dir.exists(monthgroup)){
    dir.create(monthgroup)
  }
  
  # Copy the WAV file into the appropriate month folder
  file.copy(
    wav_files[i],
    file.path(monthgroup, filename)
  )
  
  # Print progress
  cat("Copied:", filename, "->", monthgroup, "\n")
}

folders <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)

}



############PASS IN THE INPUT VARIABLE##########################
create_monthlyfolders(WD)


# -----------------------------------------------------
# Function zipfiles()

# ZIP MONTHLY SOUND FILE FOLDERS YOU CREATED (the compression level = 1 makes it go faster than zipping through the Windows file folder)
# -----------------------------------------------------

# 2. Loop through each folder and zip it using base R

zipfiles <- function(WD, delete_original = FALSE) {
  
  WDSoundFiles <- file.path(WD, "SoundFiles")
  
  subfolders <- list.dirs(WDSoundFiles, full.names = TRUE, recursive = FALSE)
  
  # 3. Loop through each individual subfolder
  for (folder in subfolders) {
    zip_name <- paste0(folder, ".zip")
    
    # Create clean zip without parent directory trees
    zip::zipr(
      zipfile = zip_name,
      files = folder,
      compression_level = 1
    )
  }
}


############PASS IN THE INPUT VARIABLE##########################
zipfiles(WD)


# ---------------------------------------------------------------------------------------------------------------------------------
# Function wav_info_file()

# CREATE WAV INFO FILE
#----------------------------------------------------------------------------------------------------------------------------------


wav_info_file <- function(WD) {
  
  setwd(WD)
  
  WDSoundFiles <- file.path(WD, "SoundFiles")
  
  setwd(WDSoundFiles)
  
  WAVs_GroupStart <- warbleR::duration_sound_files(path = ".", file.format = "\\.wav$") %>% # read in and find the duration of each wav file in the working directory (uses warbleR package)
    dplyr::mutate(Begin.File = as.character(sound.files), # rename the name of the sound file name column
                  Month = substr(Begin.File, start = 8, stop = 9)) %>%
    dplyr::arrange(Month) %>%
    dplyr::group_by(Month) %>%
    dplyr::mutate(duration_total = cumsum(duration),
                  WAV.BeginTime = duration_total - duration) %>%
    dplyr::select(Begin.File, WAV.BeginTime) # select specific columns
  
  wavinfo <- file.path(WD, "WavInfo")
  
  if(!dir.exists(wavinfo)){
    dir.create(wavinfo)
  }
  
  setwd(wavinfo)
  
  filename <- paste0(wavinfo, "/", Deployment, "_", RecorderID, "_", TodaysDate, "_WavInfo.csv")
  
  write.csv(WAVs_GroupStart, filename)
  
}


############PASS IN THE INPUT VARIABLE##########################
wav_info_file(WD)


# ---------------------------------------------------------------------------------------------------------------------------------
# Function create_tables

# CREATE MONTHLY SELECTION TABLES BASED ON SPECIES AND TABLE TYPE (filtered or full)
#----------------------------------------------------------------------------------------------------------------------------------

  
create_tables <- function(Species, TableType) {  
  
  
  # Function to create column with MonthGrp
  CreateMonthGrp <- function(x){
    dplyr::mutate(x, Begin.File.copy = Begin.File) %>%  #make a copy of source table column that will then be split apart. Original SourceTbl column will be for QC
      separate(Begin.File.copy, c("Extra", "DateTime"), sep = ".", remove = FALSE) %>% #split text string at each underscore
      mutate_at('Begin.File', str_replace, "_", "") %>%
      dplyr::mutate(Deploy = Deployment,
                    Location = RecorderID,
                    Year = substr(Begin.File, start = 6, stop = 7),
                    Year = as.double(Year),
                    Year = Year + 2000,
                    Month = substr(Begin.File, start = 8, stop = 9)) %>%
      unite(MonthGrp, c("Deploy", "Location", "Year", "Month"), sep = "_", remove = FALSE) %>%  #recombine the DeployID and MonthGrp columns 
      dplyr::select(-Deploy, -Location, -Year, -Month)
  }
  
  
  
  # Function to add annotation columns needed during review process
  AddCols <- function(x){
    mutate(x,
           DetectorCode = " ",
           SpeciesCode = " ",
           VocType = " ",
           Comment = " ")
  }
  
  # Function for renaming columns to match formatting in Raven Selection Tables (for Method 1)
  RenameCols <- function(x){
    dplyr::rename(x, 
                  'Begin Time (s)' = New.BeginTime,
                  'End Time (s)' = New.EndTime,
                  'Low Freq (Hz)' = Low.Freq..Hz.,
                  'High Freq (Hz)' = High.Freq..Hz.,
                  'CNN Classification' = End.Classification,
                  'Classification Score' = Classification.Probability.Score....,
                  'Trigger Score' = Trigger.Probability.Score....,
                  'Begin File' = Begin.File,
                  'File Offset (s)' = File.Offset..s.)
  }
  
  # Function for removing any duplicate selections created by the detector
  RemoveDuplicateSelections <- function(x){
    group_by(x, Begin.File) %>%
      dplyr::distinct(File.Offset..s., .keep_all = TRUE)
  }
  
  # Function for selecting and arranging the Desired columns. Note: Duty cycled recordings will not have Begin.Date.Time info
  SelectCols <- function(x){
    dplyr::select(x,
                  'View',
                  'Channel',
                  'Begin File',
                  'File Offset (s)',
                  'Begin Time (s)',
                  'End Time (s)',
                  'Low Freq (Hz)',
                  'High Freq (Hz)',
                  'Trigger Score',
                  'CNN Classification',
                  'Classification Score',
                  Spectrogram.File,
                  DetectorCode,
                  SpeciesCode,
                  VocType,
                  Comment)
  }
  
  
  
  #-----------------------------------
  # For Baleen Whales - full
  #-----------------------------------
  
  
  if(Species == "Baleen Whale" & TableType == "full") {
              
              
              
# STEP 1: Set working directory to folder with baleen whale selection tables from Matt

setwd(WDMattTables)


# STEP 2: Read in all baleen whale selection tables from group (grouped by month) and format the combined table

    CNNOutputs.FULL <-   list.files(path = WDMattTables,
                                    pattern = "*.txt") %>%
      set_names() %>% #use file name as source name
      map_dfr(~read.delim(.), .id = "source") %>% #create dataframe with all txt files, source = file name
      dplyr::select(-Begin.File) %>% #remove the old begin.file column with matching errors
      dplyr::rename('Begin.File' = source) %>%  #rename source column as Begin.File - need for joining wav file info
      dplyr::mutate_at("Begin.File", str_replace, ".txt", ".wav") %>% #replace ".txt" string in Begin File with ".wav"
      RemoveDuplicateSelections() %>% #remove duplicate selections based on File Offset value (grouped by BeginFile)
      CreateMonthGrp() %>% #create new column with the MonthGrp Deployment ID and Month number
      #dplyr::mutate(MonthGrp = case_match(MonthGrp, c("D4_HRFNJ1_2019_07") ~ "D4_HRFNJ1_2019_08", .default = MonthGrp)) %>% #For partial months that are reviewed with the preceeding/next full month (the partial month goes in the parentheses)
      left_join(WAVs_GroupStart, by = c('Begin.File')) %>% #join in the needed WAV file info for this Month Group
      dplyr::mutate(New.BeginTime = (Begin.Time..s. + WAV.BeginTime), New.EndTime = (End.Time..s. + WAV.BeginTime)) %>% #calculate new begin/end time for selections
      split(.,.$MonthGrp) %>%  #split tables by the Month-Group column
      lapply(., RenameCols) %>% #read in functions below
      lapply(., AddCols) %>%
      lapply(., SelectCols)
    
    #Check that you have the correct months
    
    print(names(CNNOutputs.FULL))
    
    # If Step 1 returns the wrong deployment/location/year/months, check:
    # 1. That you have entered the deployment/location information correctly in the CreateMonthGrp function
    # 2. That you have entered the deployment/location/year/month information correctly in the partial month line of the Table Prep code chunk, if you have partial months (code line starts with dplyr::mutate(MonthGrp = case_match........))
    # 3. That your Wav Info dataframe was created correctly.
    
    
    
    # STEP 2: Confirm that there are no NAs in the Begin Time (s) column in the dataframe - View(FULL_Check) should return a list of dataframes, each with 0 rows. If there are any rows in any of the dataframes, that means that one of the dataframes has NAs in the Begin Time (s) column.
    
    
    FULL_Check <- map(CNNOutputs.FULL, ~dplyr::filter(., is.na(`Begin Time (s)`)))
    
    na_counts <- sapply(FULL_Check, nrow)
    
    print(na_counts)
    
    
    # STEP 1: Set working directory to location where selection tables should be saved - GROUPS SEPARATED AS A LIST
    
    preppedtables <- file.path(WD, "PreppedTables")
    
    if(!dir.exists(preppedtables)){
      dir.create(preppedtables)
    }
    
    setwd(preppedtables)
    
    fulltables <- file.path(preppedtables, "FullTables")
    
    if(!dir.exists(fulltables)){
      dir.create(fulltables)
    }
    
    setwd(fulltables)
    
    # STEP 2: write each list element into own text file
    
    for(i in 1:length(CNNOutputs.FULL)){
      write.table(CNNOutputs.FULL[[i]], paste0((names(CNNOutputs.FULL[i])), "_CNN", ".txt"), 
                  row.names = FALSE, quote = FALSE, sep = "\t")
      
     
    }
  } 
  
  
  #-----------------------------------
  # For Baleen Whales - filtered
  #-----------------------------------
  
  
    
    else if(Species == "Baleen Whale" & TableType == "filtered") {
      
      
      
      # Function for selecting only Fin_pc selections
      OnlyPC <- function(x){
        filter(x, End.Classification == 'Fin_pc') %>%
          group_by(Begin.File) %>%
          dplyr::slice_sample(n = 20)
      }
      
      
      # Function for removing Fin_pc selections
      RemovePC <- function(x){
        filter(x, End.Classification != 'Fin_pc', End.Classification != 'Possible MixedSpecies')
      }
      
      
      # Function for joining tables back together
      JoinTables <- function(x){
        full_join(x, CNNOutputs.FW) %>%
          arrange(Begin.File, Begin.Time..s.)
      }
      
      
      # STEP 1: Set working directory to folder with baleen whale selection tables from Matt
      
      setwd(WDMattTables)
      
      
      # STEP 2: Read in all baleen whale selection tables; create table with only Fin_pc selections and reduce these to <= 20 Fin_pc calls per hour
      
      CNNOutputs.FW <- list.files(path = WDMattTables,
                                  pattern = "*.txt") %>% #creates list with file names
        set_names() %>% #use file name as source name
        map_dfr(~read.delim(.), .id = "source") %>% #create dataframe with all txt files, source = file name
        dplyr::select(-Begin.File) %>% #remove the old begin.file column with matching errors
        dplyr::rename('Begin.File' = source) %>%  #rename source column as Begin.File - need for joining wav file info
        mutate_at("Begin.File", str_replace, ".txt", ".wav") %>% #replace .txt with .wav to allow WAV file info to be joined to dataframe
        RemoveDuplicateSelections() %>% #remove duplicate selections based on File Offset value (grouped by BeginFile)
        OnlyPC() %>% #keep <=20 Fin_pc selections per hour, remove all other selections
        bind_rows() #combine dataframes into one
      
      
      # STEP 3: Read in all baleen whale selection tables; create second table with Fin_pc calls removed, then join first and second table together. Add new columns for review in Raven.
      
      CNNOutputs.FLTR <- list.files(path = WDMattTables,
                                    pattern = "*.txt") %>% #creates list with file names
        set_names() %>% #use file name as source name
        map_dfr(~read.delim(.), .id = "source") %>% #create dataframe with all txt files, source = file name
        dplyr::select(-Begin.File) %>% #remove the old begin.file column with matching errors
        dplyr::rename('Begin.File' = source) %>%  #rename source column as Begin.File - need for joining wav file info
        dplyr::mutate_at("Begin.File", str_replace, ".txt", ".wav") %>% #replace .txt with .wav to allow WAV file info to be joined to dataframe
        RemoveDuplicateSelections() %>% #remove duplicate selections based on File Offset value (grouped by BeginFile)
        RemovePC() %>% #remove any Fin_pc selections from this dataframe
        JoinTables() %>% #join this dataframe with the CNNOutputs.FW dataframe (only Fin_pc selections) made in STEP 2
        CreateMonthGrp() %>% #create new column with the MonthGrp Deployment ID and Month number
        dplyr::mutate(MonthGrp = case_match(MonthGrp, c("D2_NY6_2025_05") ~ "D2_NY6_2025_04", .default = MonthGrp)) %>% #For partial months that are reviewed with the preceeding/next full month (the partial month goes in the parentheses)
        left_join(WAVs_GroupStart, by = c('Begin.File')) %>% #join in the needed WAV file info for this Month Group
        dplyr::mutate(New.BeginTime = (Begin.Time..s. + WAV.BeginTime), New.EndTime = (End.Time..s. + WAV.BeginTime)) %>% #calculate new begin/end time for selections
        split(.,.$MonthGrp) %>%  #split tables by the Month-Group column
        lapply(., RenameCols) %>% #read in functions below
        lapply(., AddCols) %>%
        lapply(., SelectCols)
      
      
      
      # check that you have the correct months
      print(names(CNNOutputs.FLTR))
      
      # If Step 1 returns the wrong deployment/location/year/months, check:
      # 1. That you have entered the deployment/location information correctly in the CreateMonthGrp function
      # 2. That you have entered the deployment/location/year/month information correctly in the partial month line of the Table Prep code chunk, if you have partial months (code line starts with dplyr::mutate(MonthGrp = case_match........))
      # 3. That your Wav Info dataframe was created correctly.
      
      
      
      # STEP 2: Confirm that there are no NAs in the Begin Time (s) column in the dataframe - View(FULL_Check) should return a list of dataframes, each with 0 rows. If there are any rows in any of the dataframes, that means that one of the dataframes has NAs in the Begin Time (s) column.
      
      
      FLTR_Check <- map(CNNOutputs.FLTR, ~dplyr::filter(., is.na(`Begin Time (s)`)))
      
      na_counts <- sapply(FLTR_Check, nrow)
      
      print(na_counts)
      
      
      
      # STEP 1: Set working directory to location where selection tables should be saved - GROUPS SEPARATED AS A LIST
      
      preppedtables <- file.path(WD, "PreppedTables")
      
      if(!dir.exists(preppedtables)){
        dir.create(preppedtables)
      }
      
      setwd(preppedtables)
      
      filteredtables <- file.path(preppedtables, "FilteredTables")
      
      if(!dir.exists(filteredtables)){
        dir.create(filteredtables)
      }
      
      setwd(filteredtables)
      
      # STEP 2: write each list element into own text file
      
      for(i in 1:length(CNNOutputs.FLTR)){
        write.table(CNNOutputs.FLTR[[i]], paste0((names(CNNOutputs.FLTR[i])), "_CNN", ".txt"), 
                    row.names = FALSE, quote = FALSE, sep = "\t")
        
        
      }

    }    
      
    #-----------------------------------
    # For Delphinids - full
    #-----------------------------------


   else if (Species == "Delphinid" & TableType == "full") {
    
    
    # STEP 1: Set Working directory to folder with dolphin selection tables from Matt
    
    setwd(WDMattTables)
    
    
    # STEP 2: Read in all dolphin selection tables from group (grouped by month) and format the combined table
    
    CNNOutputs.DEL <- list.files(path = WDMattTables,
                                 pattern = "*.txt") %>% #creates list with file names
      set_names() %>% #use file name as source name
      map_dfr(~read.delim(.), .id = "source") %>% #create dataframe with all txt files, source = file name
      dplyr::select(-Begin.File) %>% #remove the old begin.file column with matching errors
      dplyr::rename('Begin.File' = source) %>%  #rename source column as Begin.File - need for joining wav file info
      dplyr::mutate_at("Begin.File", str_replace, ".txt", ".wav") %>% #replace ".txt" string in Begin File with ".wav"
      RemoveDuplicateSelections() %>% #remove duplicate selections based on File Offset value (grouped by BeginFile)
      CreateMonthGrp() %>% #create new column with the MonthGrp Deployment ID and Month number
      left_join(WAVs_GroupStart, by = c('Begin.File')) %>% #join in the needed WAV file info for this Month Group
      mutate(New.BeginTime = (Begin.Time..s. + WAV.BeginTime), New.EndTime = (End.Time..s. + WAV.BeginTime)) %>% #calculate new begin/end time for selections
      dplyr::mutate(MonthGrp = case_match(MonthGrp, c("D3_NY7_2025_09") ~ "D3_NY7_2025_08", .default = MonthGrp)) %>% #For partial months that are reviewed with the preceeding/next full month (the partial month goes in the parentheses)
      split(.,.$MonthGrp) %>% #turn dataframe into list of dataframes, grouped by month
      lapply(., RenameCols) %>% #read in functions below
      lapply(., AddCols) %>%
      lapply(., SelectCols)
    
    #Check that you have the correct months
    
    print(names(CNNOutputs.DEL))
    
    # If Step 1 returns the wrong deployment/location/year/months, check:
    # 1. That you have entered the deployment/location information correctly in the CreateMonthGrp function
    # 2. That you have entered the deployment/location/year/month information correctly in the partial month line of the Table Prep code chunk, if you have partial months (code line starts with dplyr::mutate(MonthGrp = case_match........))
    # 3. That your Wav Info dataframe was created correctly.
    
    
    
    # STEP 2: Confirm that there are no NAs in the Begin Time (s) column in the dataframe - View(FULL_Check) should return a list of dataframes, each with 0 rows. If there are any rows in any of the dataframes, that means that one of the dataframes has NAs in the Begin Time (s) column.
    
    
    FULL_Check <- map(CNNOutputs.DEL, ~dplyr::filter(., is.na(`Begin Time (s)`)))
    
    na_counts <- sapply(FULL_Check, nrow)
    
    print(na_counts)
    
    
    # STEP 1: Set working directory to location where selection tables should be saved - GROUPS SEPARATED AS A LIST
    
    preppedtables <- file.path(WD, "PreppedTables")
    
    if(!dir.exists(preppedtables)){
      dir.create(preppedtables)
    }
    
    setwd(preppedtables)
    
    fulltables <- file.path(preppedtables, "FullTables")
    
    if(!dir.exists(fulltables)){
      dir.create(fulltables)
    }
    
    setwd(fulltables)
    
    
    
    # STEP 2: write each list element into own text file
    
    for(i in 1:length(CNNOutputs.DEL)){
      write.table(CNNOutputs.DEL[[i]], paste0((names(CNNOutputs.DEL[i])), "_DEL", ".txt"), 
                  row.names = FALSE, quote = FALSE, sep = "\t")
    }
    
    
   } 
  
  
  #-----------------------------------
  # For Delphinids - filtered
  #-----------------------------------
  
  else if (Species == "Delphinid" & TableType == "filtered") {
    

    #Filtering: Remove all hours with > 20 detections
    
    DelThreshold <- function(x){
      group_by(x, Begin.File) %>%
        dplyr::summarize(
          total_detections = n(),
          .groups = "drop") %>%
        filter(total_detections < 20)
    }
    
    #keep only selections in Begin.Files in CNNOutputs.Threshold
    DelFilter <- function (x) {
      filter(x, Begin.File %in% CNNOutputs.Threshold$Begin.File)
    }
    
    
    # STEP 1: Set Working directory to folder with dolphin selection tables from Matt
    
    setwd(WDMattTables)
    
    
    # STEP 2 : Read in all dolphin selection tables from group (grouped by month) and format the combined table
    
    CNNOutputs.Threshold <- list.files(path = WDMattTables,
                                       pattern = "*.txt") %>% #creates list with file names
      set_names() %>% #use file name as source name
      map_dfr(~read.delim(.), .id = "source") %>% #create dataframe with all txt files, source = file name
      dplyr::select(-Begin.File) %>% #remove the old begin.file column with matching errors
      dplyr::rename('Begin.File' = source) %>%  #rename source column as Begin.File - need for joining wav file info
      dplyr::mutate_at("Begin.File", str_replace, ".txt", ".wav") %>% #replace ".txt" string in Begin File with ".wav"
      RemoveDuplicateSelections() %>%
      DelThreshold #filter out Begin.Files with > 20 whistle detections

    
    # STEP 3: Read in all dolphin selection tables from group (grouped by month) and format the combined table
    
    CNNOutputs.DEL.FLTR <- list.files(path = WDMattTables,
                                      pattern = "*.txt") %>% #creates list with file names
      set_names() %>% #use file name as source name
      map_dfr(~read.delim(.), .id = "source") %>% #create dataframe with all txt files, source = file name
      dplyr::select(-Begin.File) %>% #remove the old begin.file column with matching errors
      dplyr::rename('Begin.File' = source) %>%  #rename source column as Begin.File - need for joining wav file info
      dplyr::mutate_at("Begin.File", str_replace, ".txt", ".wav") %>% #replace ".txt" string in Begin File with ".wav"
      RemoveDuplicateSelections() %>% #remove duplicate selections based on File Offset value (grouped by BeginFile)
      DelFilter() %>% #keep only selections in Begin.Files in CNNOutputs.Threshold
      CreateMonthGrp() %>% #create new column with the MonthGrp Deployment ID and Month number
      left_join(WAVs_GroupStart, by = c('Begin.File')) %>% #join in the needed WAV file info for this Month Group
      mutate(New.BeginTime = (Begin.Time..s. + WAV.BeginTime), New.EndTime = (End.Time..s. + WAV.BeginTime)) %>% #calculate new begin/end time for selections
      dplyr::mutate(MonthGrp = case_match(MonthGrp, c("D1_HRFNY1_2018_11") ~ "D1_HRFNY1_2018_10", .default = MonthGrp)) %>% #For partial months that are reviewed with the preceeding/next full month (the partial month goes in the parentheses)
      split(.,.$MonthGrp) %>% #turn dataframe into list of dataframes, grouped by month
      lapply(., RenameCols) %>% #read in functions below
      lapply(., AddCols) %>%
      lapply(., SelectCols)
    
    
    #Check that you have the correct months
    
    print(names(CNNOutputs.DEL.FLTR))
    
    # If Step 1 returns the wrong deployment/location/year/months, check:
    # 1. That you have entered the deployment/location information correctly in the CreateMonthGrp function
    # 2. That you have entered the deployment/location/year/month information correctly in the partial month line of the Table Prep code chunk, if you have partial months (code line starts with dplyr::mutate(MonthGrp = case_match........))
    # 3. That your Wav Info dataframe was created correctly.
    
    
    
    # STEP 2: Confirm that there are no NAs in the Begin Time (s) column in the dataframe - View(FULL_Check) should return a list of dataframes, each with 0 rows. If there are any rows in any of the dataframes, that means that one of the dataframes has NAs in the Begin Time (s) column.
    
    
    FULL_Check <- map(CNNOutputs.DEL.FLTR, ~dplyr::filter(., is.na(`Begin Time (s)`)))
    
    na_counts <- sapply(FULL_Check, nrow)
    
    print(na_counts)
    
    
    # STEP 1: Set working directory to location where selection tables should be saved - GROUPS SEPARATED AS A LIST
    
    preppedtables <- file.path(WD, "PreppedTables")
    
    if(!dir.exists(preppedtables)){
      dir.create(preppedtables)
    }
    
    setwd(preppedtables)
    
    filteredtables <- file.path(preppedtables, "FilteredTables")
    
    if(!dir.exists(filteredtables)){
      dir.create(filteredtables)
    }
    
    setwd(filteredtables)
    
    
    
    # STEP 2: write each list element into own text file
    
    for(i in 1:length(CNNOutputs.DEL.FLTR)){
      write.table(CNNOutputs.DEL.FLTR[[i]], paste0((names(CNNOutputs.DEL.FLTR[i])), "_DEL", ".txt"), 
                  row.names = FALSE, quote = FALSE, sep = "\t")
    }
    
  }
  
  #-----------------------------------
  # For Sperm Whales
  #-----------------------------------
  
  else if (Species == "Sperm Whale") {
    
    
    setwd(WDcsv)
    # STEP 1: Convert pgdf columns
    spermwhale <-  list.files(path = WDcsv,
                 pattern = "*.csv",                                           #read in all files from your selected folder with the CSVs you want
                 full.names = TRUE) %>%                                       #full.names = TRUE allows the computer to locate the entire path name for the file (and therefore the file itself), as opposed to just calling in the file name (e.g. D1_NY1_DEL_MasterDoc)
      map_dfr(~read_csv(.)) %>% #use this if you are reading in a csv
      rename_with(
        .fn = ~ rep("file", length(.x)),
        .cols = where(~ is.character(.x) && 
                        any(!is.na(.x)) && 
                        all(grepl("pgdf", .x, fixed = TRUE)[!is.na(.x)]))) %>%
      dplyr::mutate(MonthGrp = as.integer(substr(file, start = 42, stop = 43))) %>%
      dplyr::mutate(source = "WAV",    #determine the source - SUD for code 1 classifier, WAV for code 2-8 classifiers
                    startTime = startSample/48000,   #convert startSample to startTime by dividing by the sample rate
                    date = lubridate::force_tz(as_datetime(date), tzone = "America/New_York"),
                    simpleEST = force_tz(as_datetime(date), tzone = "America/New_York"),    #make second date column for alteration later
                    endTime = startTime + 0.1,    #add column for selection end times that is 0.1 seconds after selection start time
                    String = substr(file, start = 38, stop = 50)) %>%    #in case the pgdf files round the seconds differently than the wav files, this string will be used to join the wav info to the detection df
      dplyr::select(file, date, startTime, endTime, type, maxAmplitude, simpleEST, MonthGrp, String)    #select certain columns
    
    second(spermwhale$simpleEST) <-  ifelse(second(spermwhale$simpleEST) >= 30, 30, 00)   #convert the seconds in column simpleEST to 00 if the selection occured in seconds 0-29 of the minute, and 30 if the selection occured in seconds 30-59 of the minute (used for grouping into 30 sec windows)
    
    csvname <- paste0("SPW_ClickDetections", "_", Deployment, "_", RecorderID, ".csv")
    write.csv(spermwhale, csvname)
    
    # STEP 2: Remove overlapping detections
    overlap <- c(spermwhale$endTime)   #create vector from endTime column in newspermwhale df
    overlap <- c(0, overlap)   #add a 0 to the beginning of the vector; this shifts all the endTimes down (in the vector/column) by 1 row
    overlap <- head(overlap, -1)   #remove the last entry in the vector/column
    spermwhale$endTime_n_minus1 <- overlap   #add the altered vector back to the df as a new column representing the endTime of row [n-1]
    
    newspermwhale <- spermwhale %>%
      mutate(overlap = startTime - endTime_n_minus1) %>%   #subtract startTime[n] - endTime[n-1] to find instances in which selection n starts before selection n-1 ends
      filter(overlap >= 0)    #remove all selections that overlap with the previous selection
    
    csvname <- paste0("SPW_ClickDetections", "_", Deployment, "_", RecorderID, "_ALL CLICKS.csv")
    write.csv(newspermwhale, csvname)
    
    # Function to create column with MonthGrp
    CreateMonthGrp <- function(x){
      mutate_at(x, 'file', str_replace, "Click_Detector_Click_Detector_Clicks_20", SoundTrapNumber) %>% #these first four lines mutate the strings in the file and String columns
        mutate(file = paste0(substr(file, 1, 4), ".", substr(file, 5, nchar(file)))) %>%
        mutate_at('file', str_replace, ".pgdf", ".wav") %>%
        mutate_at('file', str_replace, "_", "") %>%
        mutate_at('String', str_replace, "_", "") %>%
        mutate(file = substr(file, start = 1, stop = 24),
               DeployID = Deployment,
               Location = RecorderID,
               Month = as.integer(substr(file, start = 8, stop = 9)),
               Year = as.integer(substr(file, start = 6, stop = 7)) + 2000) %>%
        tidyr::unite(MonthGrp, c("DeployID", "Location", "Year", "Month"), sep = "_", remove = FALSE) #combine the DeployID and MonthGrp columns 
    }
    
    
    # Function to set important date/times(deployment date, recovery date, date of each file), filter out any non-recording hours, and combine WAV Info
    DatesInfo <- function(x) {
      mutate(x, DeployDate = force_tz(DeploymentDate, tz = "America/New_York"),
             RecoveryDate = force_tz(RecoveryDate, tz = "America/New_York")) %>%
        mutate(MonthGrp = case_match(MonthGrp, c("D2_NY6_2025_5") ~ "D2_NY6_2025_4", .default = MonthGrp)) %>%
        left_join(WAVs_GroupStart, by = c('String')) %>% #join in the needed WAV file info for this Month Group
        filter(date > DeployDate, #remove all selections from before the deployment of the unit
               date < RecoveryDate) %>% #remove all selections from after the recovery of the unit
        mutate(FileStart = lubridate::force_tz(make_datetime(year = as.integer(substr(file, start = 6, stop = 7)) + 2000, #create a datetime for the start of the wav file (using the pgdf file name)
                                                             month = as.integer(substr(file, start = 8, stop = 9)),
                                                             day = as.integer(substr(file, start = 10, stop = 11)),
                                                             hour = as.integer(substr(file, start = 12, stop = 13)),
                                                             min = as.integer(substr(file, start = 14, stop = 15)),
                                                             sec = as.integer(substr(file, start = 16, stop = 17))), tz = "America/New_York"))
    }
    
    # Function to add annotation columns needed during review process
    AddCols <- function(x){
      mutate(x,
             DetectorCode = "",
             SpeciesCode = "",
             VocType = "",
             Comment = "")
    }
    
    # Function for renaming columns to match formatting in Raven Selection Tables
    RenameCols <- function(x){
      dplyr::rename(x, 
                    'Begin Time (s)' = New.BeginTime,
                    'End Time (s)' = New.EndTime,
                    'Low Freq (Hz)' = minFreq,
                    'High Freq (Hz)' = maxFreq,
                    'Begin File' = Begin.File,
                    'File Offset (s)' = startTime)
    }
    
    #Selecting and Arranging the Desired columns. Note: Duty cycled recordings will not have Begin.Date.Time info
    SelectCols <- function(x){
      select(x,
             'Begin File',
             'File Offset (s)',
             'Begin Time (s)',
             'End Time (s)',
             'Low Freq (Hz)',
             'High Freq (Hz)',
             'DetectorCode',
             'SpeciesCode',
             'VocType',
             'Comment')
    }
    
    
    
    #Function to set Begin and End times and Min and Max Frequencies of each 30 second window with clicks (using applied filters)
    ThirtySecWindows <- function(x) {
      mutate(x, minFreq = 3000,   #set low frequenct as 3000 Hz
             maxFreq = 24000) %>% #set high frequency as 30000 Hz (only for HF recorders; 24000 for standard recorders)
        dplyr::group_by(simpleEST, WAV.BeginTime, Begin.File, FileStart, minFreq, maxFreq, MonthGrp) %>%
        dplyr::summarize(Number_of_Clicks = n())   %>%    #calculate number of clicks per 30 second window
        dplyr::mutate(startTime = as.numeric(difftime(simpleEST, FileStart, units = "secs")), endTime = startTime + 30) %>%    #recreate start and end time for 30 second windows
        mutate(New.BeginTime = (startTime + WAV.BeginTime), New.EndTime = (endTime + WAV.BeginTime)) %>%    #calculate new begin/end time for selections
        ungroup()
    }
    
    
    
    
#------------------------------------------
# For Sperm Whale Tables - full
#------------------------------------------
    if (TableType == "full") {

   

    # STEP 1: Create table
    spermwindows <- newspermwhale %>%
      CreateMonthGrp() %>%
      DatesInfo() %>%
      ThirtySecWindows() %>%
      split(.,. $MonthGrp) %>%
      lapply(., AddCols) %>%
      lapply(., RenameCols) %>%
      lapply(., SelectCols)
    


    print(names(spermwindows))
    
    # If Step 1 returns the wrong deployment/location/year/months, check:
    # 1. That you have entered the deployment/location information correctly in the CreateMonthGrp function
    # 2. That you have entered the deployment/location/year/month information correctly in the partial month line of the Table Prep code chunk, if you have partial months (code line starts with dplyr::mutate(MonthGrp = case_match........))
    # 3. That your Wav Info dataframe was created correctly.
    
    
    
    # STEP 1: Confirm that there are no NAs in the Begin Time (s) column in the dataframe - View(FULL_Check) should return a list of dataframes, each with 0 rows. If there are any rows in any of the dataframes, that means that one of the dataframes has NAs in the Begin Time (s) column.
    
    
    FULL_Check <- map(spermwindows, ~dplyr::filter(., is.na(`Begin Time (s)`)))
    
    na_counts <- sapply(FULL_Check, nrow)
    
    print(na_counts)
    
    # STEP 2: Confirm that there are no NAs in the Begin Time (s) column in the dataframe - View(FULL_Check) should return a list of dataframes, each with 0 rows. If there are any rows in any of the dataframes, that means that one of the dataframes has NAs in the Begin Time (s) column.
    
    FULL_Check <- map(spermwindows, ~dplyr::filter(., is.na(`Begin Time (s)`)))
    
    na_counts <- sapply(FULL_Check, nrow)
    
    
  
    preppedtables <- file.path(WD, "PreppedTables")
    
    if(!dir.exists(preppedtables)){
      dir.create(preppedtables)
    }
    
    setwd(preppedtables)
    
    fulltables <- file.path(preppedtables, "FullTables")
    
    if(!dir.exists(fulltables)){
      dir.create(fulltables)
    }
    
    setwd(fulltables)
    
    for(i in 1:length(spermwindows)){
      write.table(spermwindows[[i]], paste0((names(spermwindows[i])), "_SPW_30sec", ".txt"), 
                  row.names = FALSE, quote = FALSE, sep = "\t")
    } 
    
    
  
    
    
#----------------------------------------
# For Sperm Whale Tables - filtered
#----------------------------------------
    
    }
    
    else if(TableType == "filtered") {
      
      ICI <- c(newspermwhale$startTime)  #create vector from startTime column in newspermwhale df
      ICI <- c(0, ICI)   #add a 0 to the beginning of the vector; this shifts all the startTimes down (in the vector/column) by 1 row
      ICI <- head(ICI, -1)    #remove the last entry in the vector/column
      newspermwhale$startTime_n_minus1 <- ICI    #add the altered vector back to the df as a new column representing the startTime of row [n-1]
      
      spermwhale_filtered <- newspermwhale %>%
        mutate(ICI_s = startTime - startTime_n_minus1,    #calculate ICI by subtracting startTime[n] - startTime[n-1]
               ICIms = ICI_s*1000,
               logICI = log10(ICI_s)) %>%
        group_by(simpleEST) %>%    #group by the 30 second window
        filter(ICI_s >= 0.1 & ICI_s <= 2) %>%     #filter out any selections with an ICI < 5 or > 20
        filter(n() >= 5) %>%      #filter out any 30 second windows that have fewer than 5 clicks
        filter(n() <= 250) %>%    #filter out any 30 second windows that have more than 20 clicks
        ungroup()
      
      csvname <- paste0("SPW_ClickDetections", "_", Deployment, "_", RecorderID, "_CLICKS_fltr.csv")
      write.csv(spermwhale_filtered, csvname)
      
      
      # STEP 1: Create table
      spermwindows <- spermwhale_filtered %>%
        CreateMonthGrp() %>%
        DatesInfo() %>%
        ThirtySecWindows() %>%
        split(.,. $MonthGrp) %>%
        lapply(., AddCols) %>%
        lapply(., RenameCols) %>%
        lapply(., SelectCols)
      
      
      View(newspermwhale)
      
      print(names(spermwindows))
      
      # If Step 1 returns the wrong deployment/location/year/months, check:
      # 1. That you have entered the deployment/location information correctly in the CreateMonthGrp function
      # 2. That you have entered the deployment/location/year/month information correctly in the partial month line of the Table Prep code chunk, if you have partial months (code line starts with dplyr::mutate(MonthGrp = case_match........))
      # 3. That your Wav Info dataframe was created correctly.
      
      
      
      # STEP 1: Confirm that there are no NAs in the Begin Time (s) column in the dataframe - View(FULL_Check) should return a list of dataframes, each with 0 rows. If there are any rows in any of the dataframes, that means that one of the dataframes has NAs in the Begin Time (s) column.
      
      
      FLTR_Check <- map(spermwindows, ~dplyr::filter(., is.na(`Begin Time (s)`)))
      
      na_counts <- sapply(FLTR_Check, nrow)
      
      print(na_counts)
      
      preppedtables <- file.path(WD, "PreppedTables")
      
      if(!dir.exists(preppedtables)){
        dir.create(preppedtables)
      }
      
      setwd(preppedtables)
      
      filteredtables <- file.path(preppedtables, "FilteredTables")
      
      if(!dir.exists(filteredtables)){
        dir.create(filteredtables)
      }
      
      setwd(filteredtables)
      
      
      for(i in 1:length(spermwindows)){
        write.table(spermwindows[[i]], paste0((names(spermwindows[i])), "_SPW_30sec_fltr", ".txt"), 
                    row.names = FALSE, quote = FALSE, sep = "\t")
      } 
      
        
    }
  }
  
  else if (!(Species %in% c("Baleen Whale", "Delphinid", "Sperm Whale"))) {
    print("The species you put in is not valid. Scroll up to the top (Input Variables) to see the options.")
  }
  
  else if (!(TableType %in% c("full", "filtered"))) {
    print("The table type you put in is not valid. Scroll up to the top (Input Variables) to see the options.")
  }
}



############PASS IN THE INPUT VARIABLE##########################

create_tables(Species, "fullll")




