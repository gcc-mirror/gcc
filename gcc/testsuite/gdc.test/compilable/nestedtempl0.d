class C2
{
    class N(alias a) {}
}

void test2()
{
    int a;
    static assert(__traits(isSame, __traits(parent, C2.N!0), C2));
    static assert(__traits(isSame, __traits(parent, C2.N!a), C2));
    static assert(__traits(classInstanceSize, C2.N!0) == size_t.sizeof * 3);
    static assert(__traits(classInstanceSize, C2.N!a) == size_t.sizeof * 4);
}
