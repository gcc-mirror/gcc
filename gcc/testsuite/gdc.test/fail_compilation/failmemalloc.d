/*
TEST_OUTPUT:
---
fail_compilation/failmemalloc.d(13): Error: member allocators not supported by CTFE
---
*/

struct S
{
    new(size_t sz) { return null; }
}

S* s = new S();
