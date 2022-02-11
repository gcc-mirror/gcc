/* REQUIRED_ARGS: -main -de
 * TEST_OUTPUT:
---
fail_compilation/test11006.d(10): Deprecation: cannot subtract pointers to different types: `void*` and `int*`.
fail_compilation/test11006.d(10):        while evaluating: `static assert(2L == 2L)`
fail_compilation/test11006.d(11): Deprecation: cannot subtract pointers to different types: `int*` and `void*`.
fail_compilation/test11006.d(11):        while evaluating: `static assert(8L == 8L)`
---
 */
static assert(cast(void*)8 - cast(int*) 0 == 2L);
static assert(cast(int*) 8 - cast(void*)0 == 8L);
