// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -w -o- -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc9789;

///
struct S {}

///
alias A = S;
