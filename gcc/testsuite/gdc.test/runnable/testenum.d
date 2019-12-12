// PERMUTE_ARGS:

extern(C) int printf(const char*, ...);

/**********************************************/

enum Bar
{
    bar2 = 2,
    bar3,
    bar4 = 0
}

void test1()
{
    Bar b;

    assert(b == 2);
}

/**********************************************/

void test2()
{
    enum E
    {
        a=-1
    }

    assert(E.min == -1);
    assert(E.max == -1);
}


/**********************************************/

void test3()
{
    enum E
    {
        a = 1,
        b = -1,
        c = 3,
        d = 2
    }

    assert(E.min == -1);
    assert(E.max == 3);
}

/**********************************************/

void test4()
{
    enum E
    {
        a = -1,
        b = -1,
        c = -3,
        d = -3
    }

    assert(E.min==-3);
    assert(E.max==-1);
}

/**********************************************/

enum Enum5
{
    A = 3,
    B = 10,
    E = -5,
}

void test5()
{
    assert(Enum5.init == Enum5.A);
    assert(Enum5.init == 3);
    Enum5 e;
    assert(e == Enum5.A);
    assert(e == 3);
}

/***********************************/

enum E6 : byte
{
    NORMAL_VALUE = 0,
    REFERRING_VALUE = NORMAL_VALUE + 1,
    OTHER_NORMAL_VALUE = 2
}

void foo6(E6 e)
{
}

void test6()
{
     foo6(E6.NORMAL_VALUE);
     foo6(E6.REFERRING_VALUE);
     foo6(E6.OTHER_NORMAL_VALUE);
}

/**********************************************/
// 2407

int i2407;

void add2407() { ++i2407; }
void sub2407() { --i2407; }

enum EF2407f : void function()
{
    a = &add2407,
    s = &sub2407,
}

enum EF2407s
{
    a = &add2407,
    s = &sub2407,
}

enum
{
    a2407 = &add2407,
    s2407 = &sub2407,
}

enum : void function()
{
    at2407 = &add2407,
    st2407 = &sub2407,
}

enum EEF2407 : EF2407s
{
    a = EF2407s.a,
    s = EF2407s.s,
}

void test2407()
{
    alias i2407 i;

    EF2407f.a();
    assert(i == 1);
    EF2407f.s();
    assert(i == 0);

    EF2407s.a();
    assert(i == 1);
    EF2407s.s();
    assert(i == 0);

    a2407();
    assert(i == 1);
    s2407();
    assert(i == 0);

    at2407();
    assert(i == 1);
    st2407();
    assert(i == 0);

    EEF2407.a();
    assert(i == 1);
    EEF2407.s();
    assert(i == 0);

    EEF2407.init();
    assert(i == 1);

    struct S { int i; }
    enum ES : S
    {
        a = S(1),
        b = S(3),
        c = S(2),
    }
    static assert(ES.init == S(1));
    static assert(!__traits(compiles, ES.min));
    static assert(!__traits(compiles, ES.max));

    enum EES : ES
    {
        a = ES.a,
        b = ES.b,
        c = ES.c,
    }
    static assert(EES.init == ES.init);
    static assert(EES.init == S(1));
    static assert(!__traits(compiles, EES.min));
    static assert(!__traits(compiles, EES.max));

    ES es = ES.c;
    assert(es.i == 2);
    es = ES.b;
    assert(es.i == 3);

    class C { this(int i) { this.i = i; } int i; }
    enum EC : C
    {
        a = new C(42),
        b = null,
        c = new C(1),
        d = new C(33),
    }
    static assert(EC.init.i == (new C(42)).i);
    static assert(!__traits(compiles, EC.min));
    static assert(!__traits(compiles, EC.max));

    EC ec = EC.d;
    assert(ec.i == 33);
    ec = EC.b;
    assert(ec is null);
}

/**********************************************/
// 3096

void test3096()
{
    template Tuple(T...) { alias Tuple = T; }

    template Base(E)
    {
        static if(is(E B == enum))
            alias Base = B;
    }

    template GetEnum(T)
    {
        enum GetEnum { v = T.init }
    }

    struct S { }
    class C { }

    foreach (Type; Tuple!(char, wchar, dchar, byte, ubyte,
                          short, ushort, int, uint, long,
                          ulong, float, double, real, S, C))
    {
        static assert(is(Base!(GetEnum!Type) == Type));
    }
}

/**********************************************/
// 7719

enum foo7719 = bar7719;
enum { bar7719 = 1 }

/**********************************************/
// 9845

enum { A9845 = B9845 }
enum { B9845 = 1 }

/**********************************************/
// 9846

const int A9846 = B9846;
enum { B9846 = 1 }

/**********************************************/
// 10105

enum E10105 : char[1] { a = "a" }

/**********************************************/
// 10113

enum E10113 : string
{
    a = "a",
    b = "b",
    abc = "abc"
}

void test10113()
{
    E10113 v = E10113.b;
    bool check = false;

    final switch (v) {
    case E10113.a: assert(false);
    case E10113.b: check = true; break;
    case E10113.abc: assert(false);
    }

    assert(check);
}

/**********************************************/
// 10503

@property int octal10503(string num)()
{
    return num.length;
}

enum
{
    A10503 = octal10503!"2000000",
    B10503 = octal10503!"4000",
}

/**********************************************/
// 10505

enum
{
    a10505 = true,
    b10505 = 10.0f,
    c10505 = false,
    d10505 = 10,
    e10505 = null
}

static assert(is(typeof(a10505) == bool));
static assert(is(typeof(b10505) == float));
static assert(is(typeof(c10505) == bool));
static assert(is(typeof(d10505) == int));
static assert(is(typeof(e10505) == typeof(null)));

/**********************************************/
// 10561

void test10561()
{
    template Tuple(T...) { alias Tuple = T; }

    foreach (Type; Tuple!(char, wchar, dchar, byte, ubyte,
                          short, ushort, int, uint, long,
                          ulong, float, double, real))
    {
        enum : Type { v = 0, w = 0, x, y = x }
        static assert(is(typeof(v) == Type));
        static assert(is(typeof(w) == Type));
        static assert(is(typeof(x) == Type));
        static assert(is(typeof(y) == Type));
    }

    class B { }
    class D : B { }
    enum : B { a = new D, b = new B, c = null }
    static assert(is(typeof(a) == B));
    static assert(is(typeof(b) == B));
    static assert(is(typeof(c) == B));

    struct S { this(int) { } }
    enum : S { d = S(1), e = S(2) }
    static assert(is(typeof(d) == S));
    static assert(is(typeof(e) == S));

    enum : float[] { f = [], g = [1.0, 2.0], h = [1.0f] }
    static assert(is(typeof(f) == float[]));
    static assert(is(typeof(g) == float[]));
    static assert(is(typeof(h) == float[]));
}

/**********************************************/
// 10612

int[E10612] ie10612;
E10612[int] ei10612;
E10612[E10612] ee10612;

enum E10612 { a }

/**********************************************/
// 10788

enum v10788 = e10788;
enum : int { e10788 }

/**********************************************/

class C7
{
    enum Policy
    {
        PREFER_READERS,
        PREFER_WRITERS
    }

    void foo1( Policy policy = Policy.PREFER_READERS ) { }
    void foo2( Policy policy = Policy.PREFER_WRITERS ) { }
}

/**********************************************/

void test8()
{
    enum E
    {
        A = B,
        E = D + 7,
        B = 3,
        C,
        D,
    }

    assert(E.A == 3);
    assert(E.B == 3);
    assert(E.C == 4);
    assert(E.D == 5);
    assert(E.E == 12);
    assert(E.max == 12);
}

/**********************************************/
// 13220

enum E13220a;
@(1) enum E13220b;

void test13220()
{
    auto prot = __traits(getProtection, E13220a);
    assert(prot == "public");

    auto udas = __traits(getAttributes, E13220b);
    assert(udas[0] == 1);
}

/**********************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test2407();
    test10113();
    test10561();
    test8();
    test13220();

    printf("Success\n");
    return 0;
}
