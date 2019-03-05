// PERMUTE_ARGS: -d -dw
// segfault on DMD0.150, never failed if use typeid() instead.

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
