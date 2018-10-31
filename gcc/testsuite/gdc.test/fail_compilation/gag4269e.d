// REQUIRED_ARGS: -c -o-
/*
TEST_OUTPUT:
---
fail_compilation/gag4269e.d(10): Error: undefined identifier `Y8`, did you mean class `X8`?
---
*/

static if(is(typeof(X8.init))) {}
class X8 : Y8 {}
