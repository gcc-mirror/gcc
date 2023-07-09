/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/deprecatedinref.d(9): Deprecation: using `in ref` is deprecated, use `-preview=in` and `in` instead
fail_compilation/deprecatedinref.d(10): Deprecation: using `ref in` is deprecated, use `-preview=in` and `in` instead
---
*/
void foo(in ref int);
void foor(ref in int);
