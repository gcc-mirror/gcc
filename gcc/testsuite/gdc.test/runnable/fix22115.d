/* PERMUTE_ARGS: -O -inline
 */
// https://issues.dlang.org/show_bug.cgi?id=22115


int sx;
void sss() {  ++sx; }

static if (1)
{
    struct S { int a; }

    void test1(S* s)
    {
        if (s.a == 3 ? s : null)
            sss();
    }
}

static if (1)
{
    extern (C++) class Exp
    {
        int a;

        void func() { }
        final inout(AddExp) isAddExp() inout { return a == 3 ? cast(typeof(return))this : null; }
    }

    extern (C++) class AddExp : Exp
    {
    }

    void test2(Exp e)
    {
        if (e.isAddExp())
            sss();
    }
}


int main()
{
    static if (1)
    {
        S s;
        s.a = 3;
        test1(&s);
        assert(sx == 1);
        s.a = 2;
        test1(&s);
        assert(sx == 1);
    }
    sx = 1;

    static if (1)
    {
        auto c = new AddExp();
        c.a = 3;
        test2(c);
        assert(sx == 2);
        auto ae = c.isAddExp();
        assert(ae && ae.a == 3);
        c.a = 2;
        test2(c);
        assert(sx == 2);
    }

    return 0;
}
