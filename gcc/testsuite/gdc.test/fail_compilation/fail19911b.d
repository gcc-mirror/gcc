/*
DFLAGS:
EXTRA_SOURCES: extra-files/minimal/object.d
TEST_OUTPUT:
---
fail_compilation/fail19911b.d(10): Error: function `fail19911b.fun` `object.TypeInfo_Tuple` could not be found, but is implicitly used in D-style variadic functions
---
*/

void fun(...)
{
}
