add_link_icon <- function(url_path,url_text, icon_class) {
  html <- glue::glue('<a href = "{url_path}"> <i class="{icon_class}"> {url_text} </i></a>')
  cat("  ",html, sep="")
}

bib_2_pub_list <- function(bib,yml,pdf_dir,base_url_to_pdfs){
  
  # load bib file to df
  bib_df <- bib2df::bib2df(bib)
  
  # clean {{}} from entries
  # to do: improve this part
  bib_df$TITLE <- stringi::stri_replace_all_regex(bib_df$TITLE, "[\\{\\}]", "")
  bib_df$JOURNAL <- stringi::stri_replace_all_regex(bib_df$JOURNAL, "[\\{\\}]", "")
  bib_df$BOOKTITLE <- stringi::stri_replace_all_regex(bib_df$BOOKTITLE, "[\\{\\}]", "")
  
  # sort bib_df by year
  # to do: add sort options
  bib_df <- bib_df[order(bib_df$YEAR, decreasing=T),]
  
  # read yml with links for bib entries
  yml_links <- yaml::read_yaml(yml)
  
  # print entries
  
  for (i in 1:dim(bib_df)[1] ){
    
    # convert row to .bib entry
    # to do: make row to bib entry a function
    t_bib_entry <- paste0(capture.output(bib2df::df2bib(bib_df[i,])), collapse="")
    # generate markdown text for citation
    t_md_citation<- paste0(stevemisc::print_refs(t_bib_entry,csl = "apa.csl",
                                                 spit_out = FALSE,
                                                 delete_after = FALSE), collapse=" ")
    cat(t_md_citation)
    
    cat("<span class = 'publinks'>")
    
    ### add pdf links
    if( !is.na(bib_df$FILE[i]) ) { #check pdf exists
      
      pdf_name <- basename(bib_df$FILE[i])
      rel_path_to_pdf <- list.files(here::here(pdf_dir),
                                    basename(bib_df$FILE[i]),
                                    recursive=T)
      build_url <- paste0(base_url_to_pdfs,"/",rel_path_to_pdf,collapse="")
      crumplab::add_link_icon(build_url,"pdf","fas fa-file-pdf")
      
    }
    
    ## add all other links
    if( exists(bib_df$BIBTEXKEY[i],yml_links) ) { # check yml bib entry exists
      
      link_list <- yml_links[[bib_df$BIBTEXKEY[i]]]
      
      for(l in link_list){
        crumplab::add_link_icon(l$url,l$name,l$icon)
      }
      
    }
    cat("</span>")
    cat("\n\n")
  }
  
}

