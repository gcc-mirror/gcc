/*
TEST_OUTPUT:
---
fail_compilation/fail18236.d(20): Error: cannot cast expression `V(12)` of type `V` to `int`
---
*/

struct V
{
    int a;
}

struct S
{
    enum A = V(12);
}

void main()
{
    int b = cast(int)S.A;
}
