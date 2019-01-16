/*
TEST_OUTPUT:
---
fail_compilation/diag15340.d(11): Error: undefined identifier `undef1`
fail_compilation/diag15340.d(12): Error: undefined identifier `undef2`
---
*/

class C
{
    auto a = undef1;
    auto b = undef2;
}
