// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc8739;

///
void delegate(int a) dg;

///
void delegate(int b) dg2;

///
void delegate(int c)[] dg3;

///
void delegate(int d)* dg4;

void main() {}
