/*
TEST_OUTPUT:
---
fail_compilation/fail7603a.d(7): Error: cannot create default argument for `ref` / `out` parameter from constant `true`
---
*/
void test(ref bool val = true) { }
