/*
TEST_OUTPUT:
---
fail_compilation/diag9574.d(12): Error: cannot use syntax `alias this = x`, use `alias x this` instead
fail_compilation/diag9574.d(18): Error: cannot use syntax `alias this = x`, use `alias x this` instead
---
*/

struct S
{
    int x;
    alias this = x;
}

class C
{
    int x;
    alias this = x;
}
