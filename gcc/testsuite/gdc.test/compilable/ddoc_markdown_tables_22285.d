// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_tables_22285.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_tables_22285.html

module test.compilable.ddoc_markdown_tables_22285;

/**
| A | B | C |
|---|---|---|
| a | 0 |   |
| b | 1 1 1   |              |
| c | 2 |   |
*/
enum _ = 0;
