void test()
{
    int a;

    struct Field
    {
        this(this) { ++a; }
        ~this() { --a; }
    }

    struct S
    {
        Field field; // generates __fieldPostblit, __fieldDtor, and opAssign
    }

    static assert(__traits(isNested, Field));
    static assert(!__traits(isNested, S));
}
