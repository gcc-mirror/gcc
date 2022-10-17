/*
TEST_OUTPUT:
---
fail_compilation/ice12350.d(15): Error: type `MyUDC` has no value
fail_compilation/ice12350.d(30): Error: template instance `ice12350.testAttrs!(MyStruct)` error instantiating
---
*/


enum MyUDC;

struct MyStruct
{
    int a;
    @MyUDC int b;
}

void testAttrs(T)(const ref T t)
if (is(T == struct))
{
    foreach (name; __traits(allMembers, T))
    {
        auto tr = __traits(getAttributes, __traits(getMember, t, name));
    }
}

void main()
{
    MyStruct s;
    testAttrs(s);
}
