/*
TEST_OUTPUT:
---
fail_compilation/fail7603c.d(8): Error: cannot create default argument for `ref` / `out` parameter from constant `3`
---
*/
enum x = 3;
void test(ref int val = x) { }
