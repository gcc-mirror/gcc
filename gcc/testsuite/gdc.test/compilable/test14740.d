// https://issues.dlang.org/show_bug.cgi?id=14740

void test()
{
    struct S
    {
        void fun() {}
    }
    static assert([__traits(allMembers, S)] == ["fun"]);
}
