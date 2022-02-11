// https://issues.dlang.org/show_bug.cgi?id=20470

alias AliasSeq(Args...) = Args;

int g, h;

void test0()
{
    static struct S
    {
        int a, b;
        float c = 0, d = 0;
        alias fields = AliasSeq!(a, b, c, d);
        alias ints = AliasSeq!(a, b);
        alias floats = AliasSeq!(c, d);
        alias reversed = AliasSeq!(d, c, b, a);
        alias globals = AliasSeq!(g, h);

        alias properties = AliasSeq!(e, f);
        @property int e() { return a; }
        @property void e(int i) { a = i; }
        @property float f() { return c; }
        @property void f(float j) { c = j; }
    }

    S s;
    assert(s.fields == AliasSeq!(0, 0, 0, 0));
    s.ints = AliasSeq!(1, 2);
    assert(s.fields == AliasSeq!(1, 2, 0, 0));
    s.floats = AliasSeq!(3, 4);
    assert(s.fields == AliasSeq!(1, 2, 3, 4));

    int a, b;
    float c, d;
    AliasSeq!(d, c, b, a) = s.reversed;
    assert(AliasSeq!(a, b, c, d) == AliasSeq!(1, 2, 3, 4));

    s.globals = AliasSeq!(30, 40);
    assert(g == 30 && h == 40);

    // Propagating `this` to functions and properties within tuples will be a breaking change.
    // See `test2()` below for an example of existing code that would need to be fixed.

    //s.properties = AliasSeq!(11, 12);
    //assert(s.e == 11 && s.f == 12);
}

class Nested(Vars...)
{
    int a, b;
    alias outervars = Vars;
    alias fields = AliasSeq!(a, b);
    alias all = AliasSeq!(a, b, Vars);
}

auto makeNested()
{
    static class C
    {
        bool b;
        double d;
        auto nested() { return new Nested!(b, d)(); }
    }
    return new C().nested();
}

void test1()
{
    auto n = makeNested();
    n.fields = AliasSeq!(1, 2);
    n.outervars = AliasSeq!(true, 1.3);
    assert(n.all == AliasSeq!(1, 2, true, 1.3));
}

void test2()
{
    // backwards compatibility test for functions within tuples

    static struct S
    {
        void f();
        void g();
        alias funcs = AliasSeq!(f, g);
    }

    S s;
    alias voidTf = void();
    foreach (f; s.funcs)
        static assert(is(typeof(f) == voidTf));
}

void main()
{
    test0();
    test1();
    test2();
}
