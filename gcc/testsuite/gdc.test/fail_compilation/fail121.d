// PERMUTE_ARGS: -d -dw
// segfault on DMD0.150, never failed if use typeid() instead.
/*
TEST_OUTPUT:
---
fail_compilation/fail121.d(24): Error: no property `typeinfo` for `list[1]` of type `fail121.myobject`
fail_compilation/fail121.d(12):        struct `myobject` defined here
fail_compilation/fail121.d(24): Error: no property `typeinfo` for `i` of type `int`
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
