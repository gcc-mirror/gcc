/*
TEST_OUTPUT:
---
fail_compilation/fail7603a.d(7): Error: cannot modify constant `true`
---
*/
void test(ref bool val = true) { }
