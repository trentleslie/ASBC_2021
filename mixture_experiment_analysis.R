#This script was prepared for the 2021 ASBC Virtual Conference presentation
#"But Sometimes You Get What You Need: Using R to Optimize Dry Hop Mixtures 
#for Aroma and Flavor"
#For questions, please feel free to reach out to me at trentleslie@gmail.com.
#And a general disclaimer: https://www.ucunleashed.com/code-disclaimer
#TL;DR - this script isn't perfect, please use at your own risk.

library(readxl)
library(mixexp)
library(corrplot)

#Loading the data - will need to update the file path
#                      v v v v v v v v v v v v v v v v v v v v v v v v v v v v
mix_data <- read_excel("C:/Users/trent/OneDrive/Desktop/Double IPA Data.xlsx")

#experimental parameters - coincides with x1, x2, and x3 below
#in the case of this example, x1 is mosaic, x2 is citra, and x3 is simcoe
#         v v v v v v v v v v v v v v v #
mix1_min <- 0
mix1_max <- 1
mix2_min <- 0
mix2_max <- 0.5
mix3_min <- 0
mix3_max <- 0.5
mix_fraction_increment <- 0.05
mixture_loads <- factor(c(0.5,1.0,1.5))

#getting the column names - this is used for the actual modeling below.
#note that mix_data must have the seven first columns for this to work!
response_names <- colnames(mix_data)[8:length(colnames(mix_data))]

#this is for the labels in the contour plots - you may type them out manually or
#just use the response names from above ( response_labels <- response_names )
#Either way, the labels must be in the same order as response_names!
#                    v v v v v v v v v v v v v v v v v v v v v v v v v v v v v #
response_labels <- c("Preference","OHAI","Dank","Aroma - Citrus", 
                     "Aroma - Tropical","Aroma - Resinous","Aroma - Fusel",
                     "Aroma - Sulfur","Bitterness Level","Bitterness Finish",
                     "Bitterness Character","Taste - Citrus","Taste - Tropical",
                     "Sweetness","Taste - Resinous","Body")

#Setting panelists and hop load to factors (categorical data).
#This allows us to block the panelists, which allows us to account for
#panelists using our scales differently
mix_data$panelist <- factor(mix_data$panelist, levels=unique(mix_data$panelist))
mix_data$hop_load <- factor(mix_data$hop_load, levels=unique(mix_data$hop_load))

##################################
#----------LINEAR MODEL----------#
##################################

counter <- 0
for (response_name in response_names) {
  counter <- counter + 1
  for (hop_load in mixture_loads) {
    results <- c()
    current_response <- response_labels[counter]
    for (x1 in seq(mix1_min,mix1_max,mix_fraction_increment)) {
      for (x2 in seq(mix2_min,mix2_max,mix_fraction_increment)) {
        for (x3 in seq(mix3_min,mix3_max,mix_fraction_increment)) {
          if (x1 + x2 + x3 == 1.0) {
            # May need to update hop names to reflect the mixture columns from the
            # spreadsheet. If they are updated, the predict function will also
            # need to be updated to reflect this.
            #                     v v v v v v v v v v v v v v v v v v v v v v v #
            current_model <- lm(paste(response_name,
                                " ~ panelist + (mosaic * citra * simcoe * hop_load)",
                                sep=""), data = mix_data)
            new_result <- predict(current_model,
                                  data.frame(mosaic=x1,citra=x2,simcoe=x3,
                                             hop_load=hop_load,
                                             panelist=unique(mix_data$panelist)))
            results <- rbind(results,c(x1,x2,x3,mean(new_result)))
          }
        }
      }
    }
    colnames(results) <- c("x1","x2","x3","y")
    # Plot file names - can update to whatever
    #                    v v v v v v v v v v v v v v v v v v v v v v v v #
    png(filename = paste("Double IPA - ",current_response," - Hop Load ",
                         gsub("\\.","",toString(format(hop_load,nsmall=1))),
                         " - Linear Model ",".png",sep=""),width=700,height=700)
    # In the case of this example, mixture fractions are presented as
    # [mosaic fration]/[citra fraction]/[simcoe fraction].
    # The corner labels (last line) will need to be updated to reflect whatever
    # the mixture components are.
    MixturePlot(des = data.frame(results), mod = 1, 
                x1lab=paste("Min=",toString(round(min(results[,4]),digits=3)),
                " @ ",toString(results[results[,4]==min(results[,4]),][1]),"/",
                toString(results[results[,4]==min(results[,4]),][2]),"/",
                toString(results[results[,4]==min(results[,4]),][3])," | Max=",
                toString(round(max(results[,4]),digits=3))," @ ",
                toString(results[results[,4]==max(results[,4]),][1]),"/",
                toString(results[results[,4]==max(results[,4]),][2]),"/",
                toString(results[results[,4]==max(results[,4]),][3]),sep=""),  
                x2lab=current_response, x3lab=paste("Hop Load =",hop_load,
                " | r2=",round(summary(current_model)$r.squared,digits=3),
                " | Adj r2=",round(summary(current_model)$adj.r.squared,digits=3),
                " | Range=",round(max(results[,4])-min(results[,4]),digits=3),sep=""),
                corner.labs = c("Simcoe","Citra","Mosaic"))
    dev.off()
  }
}

#####################################
#----------QUADRATIC MODEL----------#
#####################################

counter <- 0
for (response_name in response_names) {
  counter <- counter + 1
  for (hop_load in mixture_loads) {
    results <- c()
    current_response <- response_labels[counter]
    for (x1 in seq(mix1_min,mix1_max,mix_fraction_increment)) {
      for (x2 in seq(mix2_min,mix2_max,mix_fraction_increment)) {
        for (x3 in seq(mix3_min,mix3_max,mix_fraction_increment)) {
          if (x1 + x2 + x3 == 1) {
            # May need to update hop names to reflect the mixture columns from the
            # spreadsheet. If they are updated, the predict function will also
            # need to be updated to reflect this.
            #                     v v v v v v v v v v v v v v v v v v v v v v v #
            current_model <- lm(paste(response_name,
                                " ~ panelist + (mosaic * citra * simcoe * hop_load) + 
                                (I(mosaic^2) * I(citra^2) * I(simcoe^2))"
                                ,sep=""), data = mix_data)
            new_result <- predict(current_model,
                                data.frame(mosaic=x1,citra=x2,simcoe=x3,
                                           hop_load=hop_load,
                                           panelist=unique(mix_data$panelist)))
            results <- rbind(results,c(x1,x2,x3,mean(new_result)))
          }
        }
      }
    }
    colnames(results) <- c("x1","x2","x3","y")
    # Plot file names - can update to whatever
    #                    v v v v v v v v v v v v v v v v v v v v v v v v #
    png(filename = paste("Double IPA - ",current_response," - Hop Load ",
                         gsub("\\.","",toString(format(hop_load,nsmall=1))),
                         " - Quadratic Model ",".png",sep=""),width=700,height=700)
    # In the case of this example, mixture fractions are presented as
    # [mosaic fration]/[citra fraction]/[simcoe fraction].
    # The corner labels (last line) will need to be updated to reflect whatever
    # the mixture components are.
    MixturePlot(des = data.frame(results), mod = 2,
                x1lab=paste("Min=",toString(round(min(results[,4]),digits=3)),
                " @ ",toString(results[results[,4]==min(results[,4]),][1]),"/"
                ,toString(results[results[,4]==min(results[,4]),][2]),"/",
                toString(results[results[,4]==min(results[,4]),][3])," | Max=",
                toString(round(max(results[,4]),digits=3))," @ ",
                toString(results[results[,4]==max(results[,4]),][1]),"/",
                toString(results[results[,4]==max(results[,4]),][2]),"/",
                toString(results[results[,4]==max(results[,4]),][3]),sep=""), 
                x2lab=current_response, x3lab=paste("Hop Load =",hop_load," | r2=",
                round(summary(current_model)$r.squared,digits=3)," | Adj r2=",
                round(summary(current_model)$adj.r.squared,digits=3)," | Range=",
                round(max(results[,4])-min(results[,4]),digits=3),sep=""), 
                corner.labs = c("Simcoe","Citra","Mosaic"))
    dev.off()
  }
}

######################################
#----------CORRELATION PLOT----------#
######################################

mix_data_no_none <- na.omit(mix_data)

res <- cor(mix_data_no_none[,8:ncol(mix_data_no_none)])
corrplot(res, type="upper",order="hclust", tl.col="black", tl.srt=45)

# Plot file name - can update to whatever
#            v v v v v v v v v v v v v v v v v #
png(filename="Double IPA Correlation Plot.png",width=800,height=800)
corrplot(res, type="upper",order="hclust", tl.col="black", tl.srt=45)
dev.off()


