// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

/*
TEST_OUTPUT:
----
----
*/

/++
 + Nested Code
 +
 + ------------------------------------
 + /**
 +  * Examples:
 +  * --------------------
 +  * writeln("3"); // writes '3' to stdout
 +  * --------------------
 +  */
 + ------------------------------------
 +/
module test.compilable.ddoc_markdown_nested_code;
