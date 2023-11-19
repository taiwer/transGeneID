options(
  "repos"=c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

# 文件全路径
file_path = ""  #原始文件
out_file_path = "" #输出文件

# 读取文件
df <- read.csv(file_path,header=TRUE,sep='\t')
sprintf("读取数据:%d 行, %d 列", nrow(df),ncol(df))

df_col_name = colnames(df)

# 开始处理 main_index
# 去掉 NA
col_1 = unlist(df[1])
col_noNA = col_1[which(!is.na(col_1) | col_1 != '')]
col_NA = col_1[which(is.na(col_1))]
sprintf("清理为NA的行:%d 行,清理后数据为 %d 行", length(col_NA), length(col_noNA))

# main_index 排序重复
main_unique <- unique(col_noNA)
main_index <- sort(main_unique)

data_A1 <-  df_col_name[1]
list_data <- list()

# 处理重复
for ( i in seq_along(main_index)) {
  filtered_rows <- df[!is.na(df[1]) & df[1] == main_index[i],]
  if(nrow(filtered_rows) > 1){
    new_main_index = filtered_rows[1,1]
    df_numeric <- as.data.frame(lapply(filtered_rows[,2:length(df_col_name)], as.numeric))
    # 均值填充
    means_num <- colMeans(df_numeric, na.rm = TRUE)
    # 中位数填充
    # means_num <- sapply(df_numeric, median)
    means_str <- as.character(means_num)
    means_str_row = c(new_main_index,means_str)
    list_data <- append(list_data, list(means_str_row))
    print(i)
    print(means_str_row)
  }
  else{
    list_d <- as.vector(unlist(filtered_rows[1,]))
    list_data <- append(list_data, list(list_d))
    print(list_d)
  }
}
my_dataframe <- as.data.frame(do.call(rbind, list_data))
colnames(my_dataframe) <- df_col_name
#
write.table(my_dataframe,
            file = out_file_path, quote = FALSE,
            row.names = FALSE, col.names = TRUE, sep = "\t")



