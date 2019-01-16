// REQUIRED_ARGS: -c -o-
/*
TEST_OUTPUT:
---
fail_compilation/gag4269f.d(11): Error: undefined identifier `Y9`, did you mean interface `X9`?
fail_compilation/gag4269f.d(11): Error: variable gag4269f.X9.y field not allowed in interface
---
*/

static if(is(typeof(X9.init))) {}
interface X9 { Y9 y; }
