// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_headings.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_headings.html

/++
# ATX-Style Headings

# H1
## H2
### H3
#### H4
##### H5
###### H6

 ### headings
  ## with initial
   # spaces

## heading with *emphasis*

## heading with trailing `#`'s #######
## heading with trailing literal ##'s
## heading with another trailing literal#
## heading with backslash-escaped trailing #\##

## Some empty headers:
##
#
### ###

# Not Headings

#hashtag not a heading because there's no space after the `#`

####### Not a heading because it has more than 6 `#`'s

\## Not a heading because of the preceeding backslash
+/
module ddoc_markdown_headings;
