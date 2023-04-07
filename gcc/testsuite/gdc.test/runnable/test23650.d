// https://issues.dlang.org/show_bug.cgi?id=23650

__gshared int x;

void main()
{

    static assert(__traits(compiles,
    {
        struct S { int *p = &x; }
        auto t = typeid(S);
    }));
}
