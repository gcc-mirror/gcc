// PERMUTE_ARGS:
// REQUIRED_ARGS: -w -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh
module ddoc11511;

/**
Params:
abcd = none1
bcdef = none23
... = doo
*/
void foo(int abcd, int bcdef, ...);

/**
Params:
abcd = none1
bcdef = none23
arr = doo
*/
void foo(int abcd, int bcdef, int[] arr...);
