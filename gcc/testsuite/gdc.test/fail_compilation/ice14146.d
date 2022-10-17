/*
TEST_OUTPUT:
---
fail_compilation/ice14146.d(15): Error: constructor `ice14146.Array.this` default constructor for structs only allowed with `@disable`, no body, and no parameters
---
*/

struct RangeT(A)
{
    A[1] XXXouter;
}

struct Array
{
    this()
    {
    }

    alias Range = RangeT!Array;

    bool opEquals(Array)
    {
    }
}
