/*
TEST_OUTPUT:
---
fail_compilation/failinout1.d(9): Error: cannot modify `inout` expression `x`
---
*/
inout(int) foo(inout(int) x)
{
    x = 5;  // cannot modify inout
    return 0;
}
