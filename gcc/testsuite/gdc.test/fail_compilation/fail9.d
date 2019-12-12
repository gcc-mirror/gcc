/*
TEST_OUTPUT:
---
fail_compilation/fail9.d(23): Error: no property 'Vector' for type 'fail9.Vector!int'
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
