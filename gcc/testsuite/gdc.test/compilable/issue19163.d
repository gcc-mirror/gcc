struct NameAttribute
{
    int delegate() foo;
}

static NameAttribute getNamedAttribute(alias S)()
{
    return __traits(getAttributes, S)[0];
}

struct TestStruct
{
    @NameAttribute({ return 42; }) int a;
}

void test()
{
    TestStruct m;
    enum nameAttr = getNamedAttribute!(m.a);
}
