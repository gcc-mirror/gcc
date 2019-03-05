// REQUIRED_ARGS: -o- -unittest
/*
TEST_OUTPUT:
---
fail_compilation/ice14424.d(12): Error: `tuple` has no effect in expression `tuple(__unittestL3_$n$)`
---
*/

void main()
{
    import imports.a14424;
    __traits(getUnitTests, imports.a14424);
}
