// { dg-do compile }
// { dg-additional-options "-Warray-bounds" }
extern(C++) class C117002
{
    ubyte[4] not_multiple_of_8;
}

int pr117002a(void *p)
{
    auto init = __traits(initSymbol, C117002);
    if (init.ptr + init.length <= p)
        return 1;
    return 0;
}

void pr117002b(void *p)
{
    auto init = __traits(initSymbol, C117002);
    p[0 .. init.length] = init[];
}

void pr117002c()
{
    scope var = new C117002;
    void *p = cast(void*)var;
    auto init = __traits(initSymbol, C117002);
    p[0 .. __traits(classInstanceSize, C117002)] = init[];
}
