/*
REQUIRED_ARGS: -de

TEST_OUTPUT:
---
fail_compilation/test20763.d(25): Deprecation: type `ulong` has no value
fail_compilation/test20763.d(25):        perhaps use `ulong.init`
fail_compilation/test20763.d(26): Deprecation: type `ulong` has no value
fail_compilation/test20763.d(26):        perhaps use `ulong.init`
fail_compilation/test20763.d(27): Error: type `ulong` has no value
fail_compilation/test20763.d(27):        perhaps use `ulong.init`
fail_compilation/test20763.d(28): Error: type `ulong` has no value
fail_compilation/test20763.d(28):        perhaps use `ulong.init`
fail_compilation/test20763.d(29): Error: type `ulong` has no value
fail_compilation/test20763.d(29):        perhaps use `ulong.init`
fail_compilation/test20763.d(30): Error: type `ulong` has no value
fail_compilation/test20763.d(30):        perhaps use `ulong.init`
---
*/

// https://github.com/dlang/dmd/issues/20763
void test()
{
    alias I = ulong;
    alias U0 = typeof(I + 1u);
    alias U1 = typeof(1 - I);
    alias U2 = typeof(+I);
    alias U3 = typeof(I * 1);
    alias U4 = typeof(I << 1);
    alias U5 = typeof(I | 1);
}
