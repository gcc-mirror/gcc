/*
TEST_OUTPUT:
---
fail_compilation/fail12932.d(11): Error: array literal in `@nogc` function `fail12932.foo` may cause a GC allocation
fail_compilation/fail12932.d(15): Error: array literal in `@nogc` function `fail12932.foo` may cause a GC allocation
---
*/

int* foo() @nogc
{
    foreach (ref e; [1,2,3])
    {
    }

    foreach (ref e; [1,2,3])
    {
        return &e;
    }
}
