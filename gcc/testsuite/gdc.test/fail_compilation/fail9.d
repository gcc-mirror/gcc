/*
TEST_OUTPUT:
---
fail_compilation/fail9.d(24): Error: no property `Vector` for type `fail9.Vector!int`
fail_compilation/fail9.d(13):        class `Vector` defined here
---
*/

template Vector(T)
{
    int x;

    class Vector
    {
    }
}

struct Sorter
{
}

void Vector_test_int()
{
    alias Vector!(int).Vector vector_t;
    vector_t v;
    Sorter sorter;
    v.sort_with!(int)(sorter);
}
