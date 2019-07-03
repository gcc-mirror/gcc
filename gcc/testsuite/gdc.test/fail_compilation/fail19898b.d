/*
PERMUTE_ARGS:
REQUIRED_ARGS: -m64
TEST_OUTPUT:
---
fail_compilation/fail19898b.d(18): Error: cannot implicitly convert expression `m` of type `S` to `__vector(int[4])`
fail_compilation/fail19898b.d(18): Error: incompatible types for `(__key2) != (__limit3)`: both operands are of type `__vector(int[4])`
fail_compilation/fail19898b.d(18): Error: cannot cast expression `__key2` of type `__vector(int[4])` to `S`
---
*/
struct S
{
    int a;
}

void f (__vector(int[4]) n, S m)
{
    foreach (i; m .. n)
        cast(void)n;
}

