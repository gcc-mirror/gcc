/*
REQUIRED_ARGS: -m64
TEST_OUTPUT:
---
fail_compilation/fail19898a.d(10): Error: expression `__key2 < __limit3` of type `__vector(int[4])` does not have a boolean value
---
*/
void f (__vector(int[4]) n)
{
    foreach (i; 0 .. n)
        cast(void)n;
}
