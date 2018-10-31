void test16563()
{
    align(1)
    struct S
    {
        uint i;
        ubyte b;
        static assert(S.sizeof == 5);
    }
}
