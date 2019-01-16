/*
TEST_OUTPUT:
---
fail_compilation/fail113.d(10): Error: forward reference to 'test'
---
*/

// Issue 370 - Compiler stack overflow on recursive typeof in function declaration.

void test(typeof(test) p) {}
