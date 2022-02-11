// REQUIRED_ARGS: -o- -unittest
// EXTRA_FILES: imports/a14424.d
/*
TEST_OUTPUT:
---
fail_compilation/ice14424.d(13): Error: `tuple(__unittest_L3_C1)` has no effect
---
*/

void main()
{
    import imports.a14424;
    __traits(getUnitTests, imports.a14424);
}
