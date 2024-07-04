// REQUIRED_ARGS: -c -o-
/*
TEST_OUTPUT:
---
fail_compilation/gag4269d.d(10): Error: undefined identifier `Y4`, did you mean function `X4`?
---
*/

static if(is(typeof(X4.init))) {}
Y4 X4() { return typeof(return).init; }
