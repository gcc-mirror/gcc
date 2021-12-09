// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -wi -o- -transition=vmarkdown
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_emphasis_verbose.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_emphasis_verbose.html

/++
Markdown Emphasis:

*emphasized text*

**strongly emphasized text**
+/
module ddoc_markdown_emphasis;
