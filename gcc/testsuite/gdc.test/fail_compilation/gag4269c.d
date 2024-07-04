// REQUIRED_ARGS: -c -o-
/*
TEST_OUTPUT:
---
fail_compilation/gag4269c.d(10): Error: undefined identifier `T3`, did you mean function `X3`?
---
*/

static if(is(typeof(X3.init))) {}
void X3(T3) { }
