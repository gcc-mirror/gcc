void test12523(inout(int))
{
    void check(T)()
    {
        T[] a;
        foreach (ref e; a)
            static assert(is(typeof(e) == T));
    }

    check!(int)();
    check!(inout(int))();
    check!(inout(const(int)))();
    check!(const(int))();
    check!(immutable(int))();
}
