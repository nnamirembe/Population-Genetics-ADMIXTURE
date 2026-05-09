#=========== ADMIXTURE PLOT =================================================
# Loading Libraries
library(tidyverse)

# ========== ADMIXTURE PLOT with barchart ==========

tbl <- read.table("R scripts/prunedData.3.Q")

barplot(t(as.matrix(tbl)),
        col = rainbow(3),
        xlab ="Individuals (ordered by population)",
        ylab ="Ancestry",
        border = NA,
        main = "ADMIXTURE Results (K=3)")


#######ADMIXTURE PLOT WITH GGPLOT2 ==========
# ========== Step 1 ===========
# Read the .Q file using read.table for ancestry proportions
Q_file <- read.table("R scripts/prunedData.3.Q")

# Read the .fam file to get individual IDs and their order

fam_file <- read.table("R scripts/prunedData.fam")

# Read the tsv file for population metadata (population labels)
pop_file <- read.table("R scripts/igsr_samples-2.tsv",
                       header = TRUE,
                       sep = "\t",
                       stringsAsFactors = FALSE)

# view the Q_file
head(Q_file)

# view the fam_file
head(fam_file)

# view the pop_file
head(pop_file)

# =================== MERGE ALL INFORMATION ==================
# Create one dataframe containing info from the Q, fam and tsv files 

# Add sample IDs from fam file to Q file 
Q_file$SampleID <- fam_file$V2

colnames(Q_file)

# Identify the matching column in the TSV file
colnames(pop_file)
head(pop_file)
# sample IDs are in column Sample.name

# Merge the Q_file with pop_file
merged_data <- merge(Q_file,
                     pop_file,
                     by.x = "SampleID",
                     by.y = "Sample.name",
                     all.x = TRUE)

# view column names of the merged dataframe
colnames(merged_data)

# Rename ancestry columns for clarity
colnames(merged_data)[2:4] <- c("K1", "K2", "K3")
colnames(merged_data)

# =================== CONVERT TO LONG FORMAT ===================
# Convert the merged dataframe from wide format to long format

long_data <- merged_data |>
  pivot_longer(cols = starts_with ("K"),
               names_to = "Cluster",
               values_to = "Ancestry") |>
  arrange(Population.name, SampleID)

# Remove samples without population labels
long_data <- long_data |>
  filter(!is.na(Population.name),   
         !is.na(SampleID),
         !is.na(Ancestry))

# =================== CREATE THE PLOT ============================

ggplot(long_data,
       aes(x = SampleID,
           y = Ancestry,
           fill = Cluster)) +
  
  facet_grid(~ Population.name, scales = "free_x", space = "free_x", switch = "x") +
  
  geom_bar(stat = "identity",
           width = 1,
           color = NA,
           position = position_stack(reverse = TRUE)) +
  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        
        panel.border = element_blank(),
        strip.text.x = element_text(angle = 45, size = 10),
        strip.placement = "outside",
        strip.background = element_blank(),
        
        legend.position = "right",
        legend.title = element_blank(),
        panel.spacing = unit(0, "lines")
  ) +
  
  labs(title = "ADMIXTURE Plot (K=3) - HapMap3",
       x = "Individuals (Grouped by Population)",
       y = "Ancestry Proportion")

