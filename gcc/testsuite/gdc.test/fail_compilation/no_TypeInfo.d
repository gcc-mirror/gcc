/*
DFLAGS:
REQUIRED_ARGS: -c
EXTRA_SOURCES: extra-files/minimal/object.d
TEST_OUTPUT:
---
fail_compilation/no_TypeInfo.d(14): Error: `object.TypeInfo` could not be found, but is implicitly used
---
*/

void test()
{
    int i;
    auto ti = typeid(i);
}
