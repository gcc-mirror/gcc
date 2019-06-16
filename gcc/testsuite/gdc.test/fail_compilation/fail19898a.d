/*
PERMUTE_ARGS:
REQUIRED_ARGS: -m64
TEST_OUTPUT:
---
fail_compilation/fail19898a.d(11): Error: incompatible types for `(__key2) < (__limit3)`: both operands are of type `__vector(int[4])`
---
*/
void f (__vector(int[4]) n)
{
    foreach (i; 0 .. n)
        cast(void)n;
}

