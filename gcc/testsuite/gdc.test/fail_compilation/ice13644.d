/*
TEST_OUTPUT:
---
fail_compilation/ice13644.d(22): Error: foreach: key cannot be of non-integral type `string`
---
*/

struct Tuple(T...)
{
    T field;
    alias field this;
}

Tuple!(string, string)[] foo()
{
    Tuple!(string, string)[] res;
    return res;
}

void main()
{
    foreach (string k2, string v2; foo())
    {
    }
}
