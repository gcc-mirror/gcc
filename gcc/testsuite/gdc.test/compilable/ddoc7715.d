// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc7656;

/**
&#36;1 &#36;2
---
string s = "$1$2 $ &#36;4";
---
*/
void foo(){}

///
void test(string a = ")") {}
