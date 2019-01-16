/*
TEST_OUTPUT:
---
fail_compilation/ice12838.d(27): Error: cannot implicitly convert expression `1` of type `int` to `string`
---
*/

struct Tuple(T...)
{
    T field;
    alias field this;
}

struct Data
{
    string a;
}

template toTuple(T)
{
    mixin(`alias toTuple = Tuple!(string);`);
}

void main()
{
    toTuple!Data a;
    a[0] = 1;   // ICE!
}
