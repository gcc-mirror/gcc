// PERMUTE_ARGS: -d -dw
// segfault on DMD0.150, never failed if use typeid() instead.
/*
TEST_OUTPUT:
---
fail_compilation/fail121.d(23): Error: no property `typeinfo` for type `fail121.myobject`
fail_compilation/fail121.d(23): Error: no property `typeinfo` for type `int`
---
*/

struct myobject
{
    TypeInfo objecttype;
    void* offset;
}

myobject[] list;

void foo()
{
    int i;

    list[1].typeinfo = i.typeinfo;
}
