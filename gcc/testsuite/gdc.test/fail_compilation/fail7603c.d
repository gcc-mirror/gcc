/*
TEST_OUTPUT:
---
fail_compilation/fail7603c.d(8): Error: cannot modify constant `3`
---
*/
enum x = 3;
void test(ref int val = x) { }
