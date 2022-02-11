// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o- -transition=vmarkdown
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_tables_verbose.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_tables_verbose.html

/++
Table:

| this | that |
| ---- | ---- |
| cell | cell |
+/
module test.compilable.ddoc_markdown_tables_verbose;
