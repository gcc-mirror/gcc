extern(C) int printf(const char*, ...);

template Seq(T...) { alias T Seq; }

/***************************************************/
// 3133

void test3133()
{
    short[2] x = [1, 2];
    auto y = cast(int[1])x;     // no error
}

/***************************************************/
// 7504

void test7504() pure nothrow @safe
{
    auto n = null;
    char[] k = n;
    assert(k.ptr == null);
    assert(k.length == 0);

    double[] l;
    l = n;
    assert(l.ptr == null);
    assert(l.length == 0);

    immutable(int[]) m = n;
    assert(m.ptr == null);
    assert(m.length == 0);

    const(float)[] o;
    o = n;
    assert(o.ptr == null);
    assert(o.length == 0);

    auto c = create7504(null, null);
    assert(c.k.ptr == null);
    assert(c.k.length == 0);
    assert(c.l.ptr == null);
    assert(c.l.length == 0);
}

class C7504
{
    int[] k;
    string l;
}

C7504 create7504(T...)(T input)
{
    auto obj = new C7504;
    obj.tupleof = input;
    return obj;
}

/***************************************************/
// 8119

struct S8119;

void test8119()
{
    void* v;
    auto sp1 = cast(S8119*)v;

    int* i;
    auto sp2 = cast(S8119*)i;

    S8119* s;
    auto ip = cast(int*)s;
}

/***************************************************/
// 8645

template TypeTuple8645(TL...)
{
    alias TL TypeTuple8645;
}

void test8645()
{
    alias TypeTuple8645!(int) Foo;
    int bar;
    static assert(!is(typeof( cast(Foo)bar )));
}

/***************************************************/
// 10497

struct S10497;

void test10497(S10497** s)
{
    void* ptr;
    *s = cast(S10497*)ptr;
}

/***************************************************/
// 10793

struct RealFoo10793
{
    int i;
}

struct Foo10793;

void test10793()
{
    auto rf = RealFoo10793(10);
    void* prf = cast(void*)&rf;
    Foo10793* f = cast(Foo10793*)prf;
}

/***************************************************/
// 10834

void test10834()
{
    struct S { int i; }
    S s;
    cast(void)s;

    class C { int i; }
    C c;
    cast(void)c;

    enum E { a, b }
    E e;
    cast(void)e;

    int[] ia;
    cast(void)ia;
}

/***************************************************/
// 10842

template Test10842(F, T)
{
    bool res;
    F from()
    {
        res = true;
        return F.init;
    }
    T to()
    {
        // The cast operand had incorrectly been eliminated
        return cast(T)from();
    }
    bool test()
    {
        res = false;
        to();
        return res;
    }
}

void test10842()
{
    foreach (From; Seq!(bool, byte, ubyte, short, ushort, int, uint, long, ulong, float, double, real))
    {
        foreach (To; Seq!(ifloat, idouble, ireal))
        {
            if (!Test10842!(From, To).test())
                assert(0);
        }
    }

    foreach (From; Seq!(ifloat, idouble, ireal))
    {
        foreach (To; Seq!(/*bool*, */byte, ubyte, short, ushort, int, uint, long, ulong, float, double, real))
        {
            if (!Test10842!(From, To).test())
                assert(0);
        }
    }

    if (!Test10842!(typeof(null), string).test())   // 10842
        assert(0);
}

/***************************************************/
// 11722

class C11722
{
    T opCast(T)() { assert(0); }
}

void test11722()
{
    C11722 c = new C11722();
    shared C11722 sc = cast(shared)c;
}

/***************************************************/
// 14218

void test14218()
{
    foreach (To; Seq!( byte,  short,  int,  long,
                      ubyte, ushort, uint, ulong,
                       char,  wchar, dchar, bool))
    {
        auto x = cast(To)null;
        assert(x == 0);     // false, '0x00'
    }

    version (DigitalMars)
    {
        // Questionable but currently accepted by DMD (but not GDC).
        foreach (To; Seq!( float,  double,  real,
                           ifloat, idouble, ireal))
        {
            auto x = cast(To)null;
            assert(x == 0);     // 0i
        }

        // Internal error: backend/el.c in el_long()
        //foreach (To; Seq!(cfloat, cdouble, creal))
        //{
        //    static assert(!__traits(compiles, { auto x = cast(To)null; }));
        //}
    }
}

/***************************************************/

int main()
{
    test3133();
    test7504();
    test8119();
    test8645();
    test10793();
    test10834();
    test10842();
    test11722();
    test14218();

    printf("Success\n");
    return 0;
}
