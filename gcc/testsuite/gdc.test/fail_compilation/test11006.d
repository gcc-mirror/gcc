/* TEST_OUTPUT:
---
fail_compilation/test11006.d(11): Error: cannot subtract pointers to different types: `void*` and `int*`.
fail_compilation/test11006.d(11):        while evaluating: `static assert(cast(void*)8 - cast(int*)0 == 2L)`
fail_compilation/test11006.d(12): Error: cannot subtract pointers to different types: `int*` and `void*`.
fail_compilation/test11006.d(12):        while evaluating: `static assert(cast(int*)8 - cast(void*)0 == 8L)`
fail_compilation/test11006.d(13): Error: cannot subtract pointers to different types: `ushort*` and `ubyte*`.
fail_compilation/test11006.d(13):        while evaluating: `static assert(null - null == 0)`
---
 */
static assert(cast(void*)8 - cast(int*) 0 == 2L);
static assert(cast(int*) 8 - cast(void*)0 == 8L);
static assert((ushort*).init - (ubyte*).init == 0);
