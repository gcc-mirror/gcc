/*
TEST_OUTPUT:
---
fail_compilation/failinout2.d(7): Error: variable `failinout2.x` only parameters or stack based variables can be `inout`
---
*/
inout int x;
