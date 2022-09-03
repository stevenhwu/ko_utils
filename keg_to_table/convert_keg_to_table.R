library(testthat)


REG <- list(
    A="^A<b>(.+)<\\/b>",
    B="^B\\s+(.+)",
    C="^C\\s+(.+)",
    D="^D\\s+(.+)"
)

REG_KO <- list(
    C="C\\s+(K[0-9]{5})\\s+.+",
    D="D\\s+(K[0-9]{5})\\s+.+"
)



filename <- "ko02000.keg"
data <- readLines(filename)


index <- sapply(REG, function(reg){
    grep(reg, data)
})

# Some index_C got KO number as well
index_temp <- grepl(REG_KO[["C"]], data[index[["C"]]])
KO_index_C <- index[["C"]][index_temp]
index[["C"]] <- index[["C"]][!index[["C"]] %in% KO_index_C]


# sanity check
expect_true(all(grepl(REG_KO[["D"]], data[index[["D"]]])))
expect_true(all(grepl(REG_KO[["C"]], data[KO_index_C])))
expect_true(all(!grepl(REG_KO[["C"]], data[index[["C"]]])))



result <- data.frame(matrix(NA, ncol=5, nrow=length(data), 
    dimnames=list(NULL, c(paste0("keg_", LETTERS[1:4]), "KO_num"))))


for (keg in LETTERS[1:3]){
    labels <- c(index[[keg]], length(data))
    for (i in seq_len(length(labels) - 1)) {
        range <- seq(labels[i], labels[i+1]-1)
        result[range, paste0("keg_", keg)] <- data[labels[i]]
    }
}
result[KO_index_C, paste0("keg_", "C")] <- data[KO_index_C]
result[KO_index_C, "KO_num"] <- gsub(REG_KO[["C"]], "\\1", data[KO_index_C])

result[index[["D"]], paste0("keg_", "D")] <- data[index[["D"]]]
result[index[["D"]], "KO_num"] <- gsub(REG_KO[["D"]], "\\1", data[index[["D"]]])

index_na <- is.na(result[,"KO_num"])
result <- result[!index_na,]

expect_equal(length(grep("K[0-9]{5}", data)), NROW(result))


write.csv(result, file=paste0(filename,"_parsed.csv"))
