/*
TEST_OUTPUT:
---
fail_compilation/ice15002.d(10): Error: array index 5 is out of bounds `x[0 .. 3]`
fail_compilation/ice15002.d(10): Error: array index 5 is out of bounds `x[0 .. 3]`
---
*/

int[][3] x = [];
int* p = &x[5][0];
