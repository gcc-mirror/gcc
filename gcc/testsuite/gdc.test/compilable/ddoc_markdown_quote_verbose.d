// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o- -transition=vmarkdown
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_quote_verbose.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_quote_verbose.html

/++
Quote Block:

> Great, just what I need.. another D in programming. -- Segfault
+/
module test.compilable.ddoc_markdown_code_verbose;
