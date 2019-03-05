// REQUIRED_ARGS: -c -o-
/*
TEST_OUTPUT:
---
fail_compilation/gag4269g.d(10): Error: undefined identifier `Y13`, did you mean template `X13(Y13 y)`?
---
*/

static if(is(typeof(X13!(0).init))) {}
template X13(Y13 y) {}
