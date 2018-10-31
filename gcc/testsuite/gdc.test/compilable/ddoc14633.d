// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -w -o-

/*
TEST_OUTPUT:
---
---
*/

/** Blah
 Params:
    T = some type
    test = something
    overnext = for testing overloaded functions
*/
template case1(T)
{
  void case1(R)(R test) { }
  void case1(R)(R test, string overnext) { }
}

///ditto
alias case2 = case1!int;
