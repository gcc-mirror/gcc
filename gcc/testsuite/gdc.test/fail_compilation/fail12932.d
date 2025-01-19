/*
TEST_OUTPUT:
---
fail_compilation/fail12932.d(11): Error: this array literal causes a GC allocation in `@nogc` function `foo`
fail_compilation/fail12932.d(15): Error: this array literal causes a GC allocation in `@nogc` function `foo`
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
