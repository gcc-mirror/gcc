/*
TEST_OUTPUT:
---
fail_compilation/fail7603b.d(7): Error: cannot create default argument for `ref` / `out` parameter from constant `true`
---
*/
void test(out bool val = true) { }
