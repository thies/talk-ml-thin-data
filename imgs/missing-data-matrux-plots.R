try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))


set.seed(123)

library('wesanderson')
library(tidyverse) 

zissou <- wes_palette("Zissou1")

# create the matrix
mat <- matrix(
  rbinom(50 * 50, size = 1, prob = 0.97),
  nrow = 50,
  ncol = 50
)

svg("missing-data-matrix-1.svg", width=7, height=7)
# plot the matrix
par(mar=c(4, 4, 4, 0.1))
image(
  t(mat[nrow(mat):1, ]),
  col = zissou[c(5,1)],
  axes = FALSE,
  xlab='Features',
  ylab='Observations',
  cex.lab=2
  
)
grid(nrow(mat_ordered), ncol(mat_ordered), lwd=2, lty=1)
dev.off()

# order rows and columns by their sums
mat_ordered <- mat[ rev(order(rowSums(mat))),]

first_zero_col <- apply(mat_ordered, 2, function(x) which(x == 0)[1]) %>%
  as.data.frame 
colnames(first_zero_col) <- 'firstzero'
first_zero_col$index <- 1:nrow(first_zero_col)
first_zero_col$firstzero[is.na(first_zero_col$firstzero)] <- nrow(first_zero_col)+1
first_zero_col <- first_zero_col[rev(order(first_zero_col$firstzero)),]

mat_ordered <- mat_ordered[, first_zero_col$index]



# plot the matrix
svg("missing-data-matrix-2.svg", width=600, height=400)
image(
  t(mat_ordered[nrow(mat_ordered):1, ]),
  col = zissou[c(4,1)],
  axes = FALSE,
)
grid(nrow(mat_ordered), ncol(mat_ordered))
dev.off()
