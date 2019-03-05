// PERMUTE_ARGS:
// EXTRA_SOURCES: extra-files/ddoc9676a.ddoc
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 9676a

module ddoc9676a;

///
deprecated void foo() {}
