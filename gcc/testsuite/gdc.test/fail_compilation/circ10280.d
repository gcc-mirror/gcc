/*
TEST_OUTPUT:
---
fail_compilation/circ10280.d(11): Error: circular initialization of variable 'circ10280.q10280'
fail_compilation/circ10280.d(10):        called from here: foo10280()
---
*/
// 10280

const int q10280 = foo10280();
int foo10280() { return q10280; }

