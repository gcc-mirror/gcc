import std.stdio;

struct S
{
    int x;
    int y;
}

/********************************************/

void test1()
{
    S s = S(1,2);
    assert(s.x == 1);
    assert(s.y == 2);
}

/********************************************/

void foo2(S s)
{
    assert(s.x == 1);
    assert(s.y == 2);
}

void test2()
{
    foo2( S(1,2) );
}

/********************************************/

S foo3()
{
    return S(1, 2);
}

void test3()
{
    S s = foo3();
    assert(s.x == 1);
    assert(s.y == 2);
}

/********************************************/

struct S4
{
    long x;
    long y;
    long z;
}

S4 foo4()
{
    return S4(1, 2, 3);
}

void test4()
{
    S4 s = foo4();
    assert(s.x == 1);
    assert(s.y == 2);
    assert(s.z == 3);
}

/********************************************/

struct S5
{
    long x;
    char y;
    long z;
}

S5 foo5()
{
    return S5(1, 2, 3);
}

void test5()
{
    S5 s = foo5();
    assert(s.x == 1);
    assert(s.y == 2);
    assert(s.z == 3);
}

/********************************************/

struct S6
{
    long x;
    char y;
    long z;
}

void test6()
{
    S6 s1 = S6(1,2,3);
    S6 s2 = S6(1,2,3);
    assert(s1 == s2);

    s1 = S6(4,5,6);
    s2 = S6(4,5,6);
    assert(s1 == s2);

    S6* p1 = &s1;
    S6* p2 = &s2;
    *p1 = S6(7,8,9);
    *p2 = S6(7,8,9);
    assert(*p1 == *p2);
}

/********************************************/

struct S7
{
    long x;
    char y;
    long z;
}

void test7()
{
    static S7 s1 = S7(1,2,3);
    static S7 s2 = S7(1,2,3);
    assert(s1 == s2);
}

/********************************************/

struct S8
{
    int i;
    string s;
}

void test8()
{
    S8 s = S8(3, "hello");
    assert(s.i == 3);
    assert(s.s == "hello");

    static S8 t = S8(4, "betty");
    assert(t.i == 4);
    assert(t.s == "betty");

    S8 u = S8(3, ['h','e','l','l','o']);
    assert(u.i == 3);
    assert(u.s == "hello");

    static S8 v = S8(4, ['b','e','t','t','y']);
    assert(v.i == 4);
    assert(v.s == "betty");
}

/********************************************/

struct S9
{
    int i;
    char[5] s;
}

void test9()
{
    S9 s = S9(3, "hello");
    assert(s.i == 3);
    assert(s.s == "hello");

    static S9 t = S9(4, "betty");
    assert(t.i == 4);
    assert(t.s == "betty");

    S9 u = S9(3, ['h','e','l','l','o']);
    assert(u.i == 3);
    assert(u.s == "hello");

    static S9 v = S9(4, ['b','e','t','t','y']);
    assert(v.i == 4);
    assert(v.s == "betty");
}

/********************************************/

alias int myint10;

struct S10
{
    int i;
    union
    {
        int x = 2;
        int y;
    }
    int j = 3;
    myint10 k = 4;
}

void test10()
{
    S10 s = S10( 1 );
    assert(s.i == 1);
    assert(s.x == 2);
    assert(s.y == 2);
    assert(s.j == 3);
    assert(s.k == 4);

    static S10 t = S10( 1 );
    assert(t.i == 1);
    assert(t.x == 2);
    assert(t.y == 2);
    assert(t.j == 3);
    assert(t.k == 4);

    S10 u = S10( 1, 5 );
    assert(u.i == 1);
    assert(u.x == 5);
    assert(u.y == 5);
    assert(u.j == 3);
    assert(u.k == 4);

    static S10 v = S10( 1, 6 );
    assert(v.i == 1);
    assert(v.x == 6);
    assert(v.y == 6);
    assert(v.j == 3);
    assert(v.k == 4);
}

/********************************************/

struct S11
{
    int i;
    int j = 3;
}


void test11()
{
    static const s = S11( 1, 5 );
    static const i = s.i;
    assert(i == 1);
    static assert(s.j == 5);
}

/********************************************/

struct S12
{
    int[5] x;
    int[5] y = 3;
}

void test12()
{
    S12 s = S12();
    foreach (v; s.x)
        assert(v == 0);
    foreach (v; s.y)
        assert(v == 3);
}

/********************************************/

struct S13
{
    int[5] x;
    int[5] y;
    int[6][3] z;
}

void test13()
{
    S13 s = S13(0,3,4);
    foreach (v; s.x)
        assert(v == 0);
    foreach (v; s.y)
        assert(v == 3);
    for (int i = 0; i < 6; i++)
    {
        for (int j = 0; j < 3; j++)
        {
            assert(s.z[j][i] == 4);
        }
    }
}

/********************************************/

struct S14a { int n; }
struct S14b { this(int n){} }

void foo14(ref S14a s) {}
void foo14(ref S14b s) {}
void hoo14()(ref S14a s) {}
void hoo14()(ref S14b s) {}
void poo14(S)(ref S s) {}

void bar14(S14a s) {}
void bar14(S14b s) {}
void var14()(S14a s) {}
void var14()(S14b s) {}
void war14(S)(S s) {}

int baz14(    S14a s) { return 1; }
int baz14(ref S14a s) { return 2; }
int baz14(    S14b s) { return 1; }
int baz14(ref S14b s) { return 2; }
int vaz14()(    S14a s) { return 1; }
int vaz14()(ref S14a s) { return 2; }
int vaz14()(    S14b s) { return 1; }
int vaz14()(ref S14b s) { return 2; }
int waz14(S)(    S s) { return 1; }
int waz14(S)(ref S s) { return 2; }

void test14()
{
    // can not bind rvalue-sl with ref
    static assert(!__traits(compiles, foo14(S14a(0))));
    static assert(!__traits(compiles, foo14(S14b(0))));
    static assert(!__traits(compiles, hoo14(S14a(0))));
    static assert(!__traits(compiles, hoo14(S14b(0))));
    static assert(!__traits(compiles, poo14(S14a(0))));
    static assert(!__traits(compiles, poo14(S14b(0))));

    // still can bind rvalue-sl with non-ref
    bar14(S14a(0));
    bar14(S14b(0));
    var14(S14a(0));
    var14(S14b(0));
    war14(S14a(0));
    war14(S14b(0));

    // preferred binding of rvalue-sl in overload resolution
    assert(baz14(S14a(0)) == 1);
    assert(baz14(S14b(0)) == 1);
    assert(vaz14(S14a(0)) == 1);
    assert(vaz14(S14b(0)) == 1);
    assert(waz14(S14a(0)) == 1);
    assert(waz14(S14b(0)) == 1);
}

/********************************************/

void check15(T, ubyte results, A...)(A args)
{
                         // m c i s sc
    enum m  = (results & 0b_1_0_0_0_0) != 0;
    enum c  = (results & 0b_0_1_0_0_0) != 0;
    enum i  = (results & 0b_0_0_1_0_0) != 0;
    enum s  = (results & 0b_0_0_0_1_0) != 0;
    enum sc = (results & 0b_0_0_0_0_1) != 0;

    // allocation on stack
    static assert((is(typeof(                  T(args) ) U) && is(U ==              T  )) == m);
    static assert((is(typeof(            const T(args) ) U) && is(U ==        const(T) )) == c);
    static assert((is(typeof(        immutable T(args) ) U) && is(U ==    immutable(T) )) == i);
    static assert((is(typeof(           shared T(args) ) U) && is(U ==       shared(T) )) == s);
    static assert((is(typeof(     shared const T(args) ) U) && is(U == shared(const T) )) == sc);

    // allocation on heap
    static assert((is(typeof( new              T(args) ) U) && is(U ==              T *)) == m);
    static assert((is(typeof( new        const T(args) ) U) && is(U ==        const(T)*)) == c);
    static assert((is(typeof( new    immutable T(args) ) U) && is(U ==    immutable(T)*)) == i);
    static assert((is(typeof( new       shared T(args) ) U) && is(U ==       shared(T)*)) == s);
    static assert((is(typeof( new shared const T(args) ) U) && is(U == shared(const T)*)) == sc);
}
void test15a()
{
    static struct Foo1 { this(int v) {}                int value; }
    static struct Boo1 { this(int v) const {}          int[] value; }
    static struct Bar1 { this(int[] v) {}              int[] value; }
    static struct Baz1 { this(const int[] v) pure {}   int[] value; }  // unique ctor
    static struct Coo1 { this(int[] v) immutable {}    int[] value; }
    static struct Car1 { this(int[] v) immutable {}    immutable(int)[] value; }
    check15!(Foo1, 0b_1_1_0_0_0)(1);
    check15!(Boo1, 0b_0_1_0_0_0)(1);
    check15!(Bar1, 0b_1_1_0_0_0)(null);
    check15!(Baz1, 0b_1_1_1_1_1)(null);
    check15!(Coo1, 0b_0_1_1_0_1)(null);
    check15!(Car1, 0b_0_1_1_0_1)(null);
                   // m c i s sc

    // Template constructor should work as same as non-template ones
    static struct Foo2 { this()(int v) {}              int value; }
    static struct Boo2 { this()(int v) const {}        int[] value; }
    static struct Bar2 { this()(int[] v) {}            int[] value; }  // has mutable indieection
    static struct Baz2 { this()(const int[] v) pure {} int[] value; }  // unique ctor
    static struct Coo2 { this()(int[] v) immutable {}  int[] value; }
    static struct Car2 { this()(int[] v) immutable {}  immutable(int)[] value; }
    check15!(Foo2, 0b_1_1_0_0_0)(1);
    check15!(Boo2, 0b_0_1_0_0_0)(1);
    check15!(Bar2, 0b_1_1_0_0_0)(null);
    check15!(Baz2, 0b_1_1_1_1_1)(null);
    check15!(Coo2, 0b_0_1_1_0_1)(null);
    check15!(Car2, 0b_0_1_1_0_1)(null);
                   // m c i s sc

    // Except Bar!().__ctor, their constructors are inferred to pure, then they become unique ctors.
    static struct Foo3() { this(int v) {}              int value; }
    static struct Boo3() { this(int v) const {}        int[] value; }
    static struct Bar3() { this(int[] v) {}            int[] value; }  // has mutable indieection
    static struct Baz3() { this(const int[] v) pure {} int[] value; }  // unique ctor
    static struct Coo3() { this(int[] v) immutable {}  int[] value; }
    static struct Car3() { this(int[] v) immutable {}  immutable(int)[] value; }
    check15!(Foo3!(), 0b_1_1_1_1_1)(1);
    check15!(Boo3!(), 0b_1_1_1_1_1)(1);
    check15!(Bar3!(), 0b_1_1_0_0_0)(null);
    check15!(Baz3!(), 0b_1_1_1_1_1)(null);
    check15!(Coo3!(), 0b_1_1_1_1_1)(null);
    check15!(Car3!(), 0b_1_1_1_1_1)(null);
                      // m c i s sc
}

// inout constructor works as like unique constructor in many cases
void test15b()
{
    static struct Nullable1
    {
        private int[] _value;
        private bool _isNull = true;
        this(inout int[] v) inout //pure
        {
            _value = v;
            //static int g; auto x = g; // impure access
            _isNull = false;
        }
    }
    static assert( __traits(compiles,           Nullable1([1,2,3])));
    static assert(!__traits(compiles,           Nullable1([1,2,3].idup)));
    static assert(!__traits(compiles, immutable Nullable1([1,2,3])));
    static assert( __traits(compiles, immutable Nullable1([1,2,3].idup)));
    static assert(!__traits(compiles,    shared Nullable1([1,2,3])));
    static assert(!__traits(compiles,    shared Nullable1([1,2,3].idup)));

    static struct Nullable2(T)
    {
        private T _value;
        private bool _isNull = true;
        this(inout T v) inout //pure
        {
            _value = v;
            //static int g; auto x = g; // impure access
            _isNull = false;
        }
    }
    static assert( __traits(compiles,           Nullable2!(int[])([1,2,3])));
    static assert(!__traits(compiles,           Nullable2!(int[])([1,2,3].idup)));
    static assert(!__traits(compiles, immutable Nullable2!(int[])([1,2,3])));
    static assert( __traits(compiles, immutable Nullable2!(int[])([1,2,3].idup)));
    static assert(!__traits(compiles,    shared Nullable2!(int[])([1,2,3])));
    static assert(!__traits(compiles,    shared Nullable2!(int[])([1,2,3].idup)));

    // ctor is inout pure, but cannot create unique object.
    struct S
    {
        int[] marr;
        const int[] carr;
        immutable int[] iarr;
        this(int[] m, const int[] c, immutable int[] i) inout pure
        {
            static assert(!__traits(compiles, marr = m));
            static assert(!__traits(compiles, carr = c));  // cannot implicitly convertible const(int[]) to inout(const(int[]))
            iarr = i;
        }
    }
    static assert(!__traits(compiles, { int[] ma; immutable int[] ia; auto m =           S(ma, ma, ia); }));
    static assert( __traits(compiles, { int[] ma; immutable int[] ia; auto c =     const S(ma, ma, ia); }));
    static assert(!__traits(compiles, { int[] ma; immutable int[] ia; auto i = immutable S(ma, ma, ia); }));
}

// TemplateThisParameter with constructor should work
void test15c()
{
    static class C
    {
        this(this This)()
        {
            static assert(is(This == immutable C));
        }

        this(T = void, this This)(int)
        {
            static assert(is(This == immutable C));
        }
    }
    auto c1 = new immutable C;
    auto c2 = new immutable C(1);
}

void test15d()  // Bugzilla 9974
{
    class CM { this() {} }
    auto cm = new CM();

    const class CC { this() {} }
    const cc = new const CC();

    immutable class CI { this() {} }
    immutable ci = new immutable CI();

    shared class CS { this() {} }
    shared cs = new shared CS();

    shared const class CSC { this() {} }
    shared const csc = new shared const CSC();


    struct SM { this(int) {} }
    auto sm = new SM(1);

    const struct SC { this(int) {} }
    const sc = new const SC(1);

    immutable struct SI { this(int) {} }
    immutable si = new immutable SI(1);

    shared struct SS { this(int) {} }
    shared ss = new shared SS(1);

    shared const struct SSC { this(int) {} }
    shared const ssc = new shared const SSC(1);
}

void test15e()  // Bugzilla 10005
{
    // struct literal
    static struct S
    {
        int[] a;
    }
    int[] marr = [1,2,3];
    static assert( __traits(compiles, {           S m =           S(marr); }));
    static assert( __traits(compiles, {     const S c =           S(marr); }));
    static assert(!__traits(compiles, { immutable S i =           S(marr); }));
    immutable int[] iarr = [1,2,3];
    static assert(!__traits(compiles, {           S m = immutable S(iarr); }));
    static assert( __traits(compiles, {     const S c = immutable S(iarr); }));
    static assert( __traits(compiles, { immutable S i = immutable S(iarr); }));

    // mutable constructor
    static struct MS
    {
        int[] a;
        this(int n) { a = new int[](n); }
    }
    static assert( __traits(compiles, {           MS m =           MS(3); }));
    static assert( __traits(compiles, {     const MS c =           MS(3); }));
    static assert(!__traits(compiles, { immutable MS i =           MS(3); }));
    static assert(!__traits(compiles, {           MS m = immutable MS(3); }));
    static assert(!__traits(compiles, {     const MS c = immutable MS(3); }));
    static assert(!__traits(compiles, { immutable MS i = immutable MS(3); }));

    // immutable constructor
    static struct IS
    {
        int[] a;
        this(int n) immutable { a = new int[](n); }
    }
    static assert(!__traits(compiles, {           IS m =           IS(3); }));
    static assert(!__traits(compiles, {     const IS c =           IS(3); }));
    static assert(!__traits(compiles, { immutable IS i =           IS(3); }));
    static assert(!__traits(compiles, {           IS m = immutable IS(3); }));
    static assert( __traits(compiles, {     const IS c = immutable IS(3); }));
    static assert( __traits(compiles, { immutable IS i = immutable IS(3); }));
}

struct Foo9984
{
    int[] p;
    // Prefix storage class and tempalte constructor
    inout this()(inout int[] a) { p = a; }
    auto foo() inout { return inout(Foo9984)(p); }
}

void test9993a()
{
    static class A
    {
        int x;
        this()           { x = 13; }
        this() immutable { x = 42; }
    }
              A ma = new           A;   assert(ma.x == 13);
    immutable A ia = new immutable A;   assert(ia.x == 42);
    static assert(!__traits(compiles, { immutable A ia = new A; }));

    static class B
    {
        int x;
        this()       { x = 13; }
        this() const { x = 42; }
    }
    const B mb = new       B;           assert(mb.x == 13);
    const B cb = new const B;           assert(cb.x == 42);
    static assert(!__traits(compiles, { immutable B ib = new B; }));

    static class C
    {
        int x;
        this() const     { x = 13; }
        this() immutable { x = 42; }
    }
        const C cc = new     const C;   assert(cc.x == 13);
    immutable C ic = new immutable C;   assert(ic.x == 42);
    static assert(!__traits(compiles, { C mc = new C; }));
}
void test9993b()
{
    static class A
    {
        int x;
        this()()           { x = 13; }
        this()() immutable { x = 42; }
    }
              A ma = new           A;   assert(ma.x == 13);
    immutable A ia = new immutable A;   assert(ia.x == 42);
    static assert(__traits(compiles, { immutable A ia = new A; }));

    static class B
    {
        int x;
        this()()       { x = 13; }
        this()() const { x = 42; }
    }
    const B mb = new       B;           assert(mb.x == 13);
    const B cb = new const B;           assert(cb.x == 42);
    static assert(__traits(compiles, { immutable B ib = new B; }));

    static class C
    {
        int x;
        this()() const     { x = 13; }
        this()() immutable { x = 42; }
    }
        const C cc = new     const C;   assert(cc.x == 13);
    immutable C ic = new immutable C;   assert(ic.x == 42);
    static assert(!__traits(compiles, { C mc = new C; }));
}

/********************************************/
// 1914

struct Bug1914a
{
    const char[10] i = [1,0,0,0,0,0,0,0,0,0];
    char[10] x = i;
    int y = 5;
}

struct Bug1914b
{
    const char[10] i = [0,0,0,0,0,0,0,0,0,0];
    char[10] x = i;
    int y = 5;
}

struct Bug1914c
{
    const char[2] i = ['a', 'b'];
    const char[2][3] j = [['x', 'y'], ['p', 'q'], ['r', 's']];
    const char[2][3] k = ["cd", "ef", "gh"];
    const char[2][3] l = [['x', 'y'], ['p'], ['h', 'k']];
    char[2][3] x = i;
    int y = 5;
    char[2][3] z = j;
    char[2][3] w = k;
    int v = 27;
    char[2][3] u = l;
    int t = 718;
}

struct T3198
{
    int g = 1;
}

class Foo3198
{
    int[5] x = 6;
    T3198[5] y = T3198(4);
}

void test3198and1914()
{
    Bug1914a a;
    assert(a.y == 5, "bug 1914, non-zero init");
    Bug1914b b;
    assert(b.y == 5, "bug 1914, zero init");
    Bug1914c c;
    assert(c.y == 5, "bug 1914, multilevel init");
    assert(c.v == 27, "bug 1914, multilevel init2");
    assert(c.x[2][1] == 'b');
    assert(c.t == 718, "bug 1914, multi3");
    assert(c.u[1][0] == 'p');
    assert(c.u[1][1] == char.init);
    auto f = new Foo3198();
    assert(f.x[0] == 6);
    assert(f.y[0].g == 4, "bug 3198");
}

/********************************************/
// 14996

enum E14996a : string { confirm = "confirm" }
enum E14996b : long[] { confirm = [1,2,3,4] }

struct S14996
{
    E14996a[1] data1;
    E14996b[1] data2;
}

/********************************************/
// 2427

void test2427()
{
    struct S
    {
        int x;
    }

    int foo(int i)
    {
        return i;
    }

    int i;
    S s = { foo(i) };
}

/********************************************/

struct T5885 {
    uint a, b;
}

double mulUintToDouble(T5885 t, double m) {
    return t.a * m;
}

void test5885()
{
    enum ae = mulUintToDouble(T5885(10, 0), 10.0);
    enum be = mulUintToDouble(T5885(10, 20), 10.0);
    static assert(ae == be);

    auto a = mulUintToDouble(T5885(10, 0), 10.0);
    auto b = mulUintToDouble(T5885(10, 20), 10.0);
    assert(a == b);
}

/********************************************/
// 5889

struct S5889a { int n; }
struct S5889b { this(int n){} }

bool isLvalue(S)(auto ref S s){ return __traits(isRef, s); }

int foo(ref S5889a s) { return 1; }
int foo(    S5889a s) { return 2; }
int foo(ref S5889b s) { return 1; }
int foo(    S5889b s) { return 2; }

int goo(ref const(S5889a) s) { return 1; }
int goo(    const(S5889a) s) { return 2; }
int goo(ref const(S5889b) s) { return 1; }
int goo(    const(S5889b) s) { return 2; }

int too(S)(ref S s) { return 1; }
int too(S)(    S s) { return 2; }

S makeRvalue(S)(){ S s; return s; }

void test5889()
{
    S5889a sa;
    S5889b sb;

    assert( isLvalue(sa));
    assert( isLvalue(sb));
    assert(!isLvalue(S5889a(0)));
    assert(!isLvalue(S5889b(0)));
    assert(!isLvalue(makeRvalue!S5889a()));
    assert(!isLvalue(makeRvalue!S5889b()));

    assert(foo(sa) == 1);
    assert(foo(sb) == 1);
    assert(foo(S5889a(0)) == 2);
    assert(foo(S5889b(0)) == 2);
    assert(foo(makeRvalue!S5889a()) == 2);
    assert(foo(makeRvalue!S5889b()) == 2);

    assert(goo(sa) == 1);
    assert(goo(sb) == 1);
    assert(goo(S5889a(0)) == 2);
    assert(goo(S5889b(0)) == 2);
    assert(goo(makeRvalue!S5889a()) == 2);
    assert(goo(makeRvalue!S5889b()) == 2);

    assert(too(sa) == 1);
    assert(too(sb) == 1);
    assert(too(S5889a(0)) == 2);
    assert(too(S5889b(0)) == 2);
    assert(too(makeRvalue!S5889a()) == 2);
    assert(too(makeRvalue!S5889b()) == 2);
}

/********************************************/
// 4147

struct S4247
{
    int n = 1024;
    this(int x) { n = x; }
}
void test4247()
{
    auto p1 = S4247();
    assert(p1.n == 1024);

    auto p2 = S4247(1);
    assert(p2.n == 1);
}

/********************************************/
// 6937

void test6937()
{
    static struct S
    {
        int x, y;
    }

    auto s1 = S(1, 2);
    auto ps1 = new S(1, 2);
    assert(ps1.x == 1);
    assert(ps1.y == 2);
    assert(*ps1 == s1);

    auto ps2 = new S(1);
    assert(ps2.x == 1);
    assert(ps2.y == 0);
    assert(*ps2 == S(1, 0));

    static assert(!__traits(compiles, new S(1,2,3)));

    int v = 0;
    struct NS
    {
        int x;
        void foo() { v = x; }
    }
    auto ns = NS(1);
    ns.foo();
    assert(ns.x == 1);
    assert(v == 1);
    auto pns = new NS(2);
    assert(pns.x == 2);
    pns.foo();
    assert(v == 2);
    pns.x = 1;
    assert(*pns == ns);

    static struct X {
        int v;
        this(this) { ++v; }
    }
    static struct Y {
        X x;
    }
    Y y = Y(X(1));
    assert(y.x.v == 1);
    auto py1 = new Y(X(1));
    assert(py1.x.v == 1);
    assert(*py1 == y);
    auto py2 = new Y(y.x);
    assert(py2.x.v == 2);
}

/********************************************/
// 12681

struct HasUnion12774
{
    union
    {
        int a, b;
    }
}

bool test12681()
{
    immutable int x = 42;

    static struct S1
    {
        immutable int *p;
    }
    immutable s1 = new S1(&x);
    assert(s1.p == &x);

    struct S2
    {
        immutable int *p;
        void foo() {}
    }
    auto s2 = new S2(&x);
    assert(s2.p == &x);

    struct S3
    {
        immutable int *p;
        int foo() { return x; }
    }
    auto s3 = new S3(&x);
    assert(s3.p == &x);
    assert(s3.foo() == 42);

    auto x12774 = new HasUnion12774();

    return true;
}
static assert(test12681());

/********************************************/
// 3991

union X3991
{
    int   a = void;
    dchar b = void;
}

union Y3991
{
    int   a = void;
    dchar b = 'a';
}

union Z3991
{
    int   a = 123;
    dchar b = void;
}

void test3991()
{
    X3991 x;

    Y3991 y;
    assert(y.b == 'a');

    Z3991 z;
    assert(z.a == 123);
}

/********************************************/
// 7727

union U7727A1 { int i;       double d;       }
union U7727A2 { int i = 123; double d;       }
//union U7727A3 { int i;       double d = 2.5; }

union U7727B1 { double d;       int i;       }
union U7727B2 { double d = 2.5; int i;       }
//union U7727B3 { double d;       int i = 123; }

void test7727()
{
    import core.stdc.math : isnan;

    { U7727A1 u;                assert(u.i == 0); }
    { U7727A1 u = { i: 1024 };  assert(u.i == 1024); }
    { U7727A1 u = { d: 1.225 }; assert(u.d == 1.225); }
  static assert(!__traits(compiles,
    { U7727A1 u = { i: 1024, d: 1.225 }; }
  ));

    { U7727A2 u;                assert(u.i == 123); }
    { U7727A2 u = { i: 1024 };  assert(u.i == 1024); }
    { U7727A2 u = { d: 1.225 }; assert(u.d == 1.225); }
  static assert(!__traits(compiles,
    { U7727A2 u = { i: 1024, d: 1.225 }; }
  ));

// Blocked by issue 1432
//    { U7727A3 u;                assert(u.d == 2.5); }
//    { U7727A3 u = { i: 1024 };  assert(u.i == 1024); }
//    { U7727A3 u = { d: 1.225 }; assert(u.d == 1.225); }
//  static assert(!__traits(compiles,
//    { U7727A3 u = { i: 1024, d: 1.225 }; }
//  ));

    { U7727B1 u;                assert(isnan(u.d)); }
    { U7727B1 u = { i: 1024 };  assert(u.i == 1024); }
    { U7727B1 u = { d: 1.225 }; assert(u.d == 1.225); }
  static assert(!__traits(compiles,
    { U7727B1 u = { i: 1024, d: 1.225 }; }
  ));

    { U7727B2 u;                assert(u.d == 2.5); }
    { U7727B2 u = { i: 1024 };  assert(u.i == 1024); }
    { U7727B2 u = { d: 1.225 }; assert(u.d == 1.225); }
  static assert(!__traits(compiles,
    { U7727B2 u = { i: 1024, d: 1.225 }; }
  ));

// Blocked by issue 1432
//    { U7727B3 u;                assert(u.i == 123); }
//    { U7727B3 u = { i: 1024 };  assert(u.i == 1024); }
//    { U7727B3 u = { d: 1.225 }; assert(u.d == 1.225); }
//  static assert(!__traits(compiles,
//    { U7727B3 u = { i: 1024, d: 1.225 }; }
//  ));


    test7727a();
    test7727b();
}

// --------

struct Foo7727a
{
    ushort bar2;
}
struct Foo7727b
{
    union
    {
        ubyte[2] bar1;
        ushort bar2;
    }
}

void test7727a()
{
    immutable Foo7727a foo1 = { bar2: 100 }; // OK
    immutable Foo7727b foo2 = { bar2: 100 }; // OK <-- error
}

// --------

struct S7727 { int i; double d; }
union U7727 { int i; double d; }

void test7727b()
{
    S7727 s = { d: 5 }; // OK
    U7727 u = { d: 5 }; // OK <-- Error: is not a static and cannot have static initializer
}

/********************************************/
// 7929

void test7929()
{
    static struct S
    {
        int [] numbers;
    }

    const int [] numbers = new int[2];
    const S si = {numbers};
    // Error: cannot implicitly convert expression (numbers) of type const(int[]) to int[]

    const S se = const(S)(numbers);
    // OK
}

/********************************************/
// 7021

struct S7021
{
    @disable this();
}

void test7021()
{
  static assert(!is(typeof({
    auto s = S7021();
  })));
}

/********************************************/
// 8738

void test8738()
{
    int[3] a = [1, 2, 3];

    struct S { int a, b, c; }
    S s = S(1, 2, 3);

    a = [4, a[0], 6];
    s = S(4, s.a, 6);

    assert(a == [4, 1, 6]);
    assert(s == S(4, 1, 6));
}

/********************************************/
// 8763

void test8763()
{
    struct S
    {
        this(int) {}
    }

    void foo(T, Args...)(Args args)
    {
        T t = T(args);
        // Error: constructor main.S.this (int) is not callable using argument types ()
    }

    S t = S(); // OK, initialize to S.init
    foo!S();
}

/********************************************/
// 8902

union U8902 { int a, b; }

enum U8902 u8902a = U8902.init; // No errors
U8902 u8902b;                   // No errors
U8902 u8902c = U8902.init;      // Error: duplicate union initialization for b

void test8902()
{
    U8902 u8902d = U8902.init;                  // No errors
    immutable U8902 u8902e = U8902.init;        // No errors
    immutable static U8902 u8902f = U8902.init; // Error: duplicate union...
    static U8902 u8902g = u8902e;               // Error: duplicate union...
    static U8902 u8902h = U8902.init;           // Error: duplicate union...
}

/********************************************/
// 9116

void test9116()
{
    static struct X
    {
        int v;
        this(this) { ++v; }
    }
    static struct Y
    {
        X x;
    }
    X x = X(1);
    assert(x.v == 1);
    Y y = Y(X(1));
    //printf("y.x.v = %d\n", y.x.v);  // print 2, but should 1
    assert(y.x.v == 1); // fails
}

/********************************************/
// 9293

void test9293()
{
    static struct A
    {
    //  enum A zero = A(); // This works as expected
        enum A zero = {};  // Note the difference here

        int opCmp(const ref A a) const
        {
            assert(0);
        }

        int opCmp(const A a) const
        {
            return 0;
        }
    }

    A a;
    auto b = a >= A.zero;  // Error: A() is not an lvalue
}

/********************************************/
// 9566

void test9566()
{
    static struct ExpandData
    {
        ubyte[4096] window = 0;
    }
    ExpandData a;
    auto b = ExpandData.init;   // bug
}

/********************************************/
// 9775

enum Month9775 : ubyte { jan = 1, }
struct Date9775
{
    this(int year, int month, int day) pure
    {
        _year  = cast(short)year;
        _month = cast(Month9775)month;
        _day   = cast(ubyte)day;
    }
    short     _year  = 1;
    Month9775 _month = Month9775.jan;
    ubyte     _day   = 1;
}

const Date9775 date9775c1 = Date9775(2012, 12, 21);
const          date9775c2 = Date9775(2012, 12, 21);
enum  Date9775 date9775e1 = Date9775(2012, 12, 21);
enum           date9775e2 = Date9775(2012, 12, 21);

/********************************************/
// 11105

struct S11105
{
    int[2][1] a21;
}

void test11105()
{
    S11105 s = S11105([1, 2]);
}

/********************************************/
// 11147

struct V11147
{
    union
    {
        struct
        {
            float x = 0;
            float y = 0;
            float z = 0;
        }
        struct
        {
            float r;
            float g;
            float b;
        }
    }
}

void test11147()
{
    auto v = V11147.init;
    assert(v.x == 0f);
    assert(v.y == 0f);
    assert(v.z == 0f);
    assert(v.r == 0f);
    assert(v.g == 0f);
    assert(v.b == 0f);
}

/********************************************/
// 11256

struct S11256 { @disable this(); }

struct Z11256a(Ranges...)
{
    Ranges ranges;
    this(Ranges rs) { ranges = rs; }
}
struct Z11256b(Ranges...)
{
    Ranges ranges = Ranges.init;    // Internal error: e2ir.c 5321
    this(Ranges rs) { ranges = rs; }
}
struct Z11256c(Ranges...)
{
    Ranges ranges = void;           // todt.c(475) v->type->ty == Tsarray && vsz == 0
    this(Ranges rs) { ranges = rs; }
}

struct F11256(alias pred)
{
    this(int[] = null) { }
}

Z!Ranges z11256(alias Z, Ranges...)(Ranges ranges)
{
    return Z!Ranges(ranges);
}

void test11256()
{
    z11256!Z11256a(S11256.init, F11256!(gv => true)());
    z11256!Z11256b(S11256.init, F11256!(gv => true)());
    z11256!Z11256c(S11256.init, F11256!(gv => true)());
}

/********************************************/
// 11269

struct Atom
{
    union
    {
        int i;
        struct
        {
            ulong first, rest;
        }
        struct
        {
            uint a, b;
        }
    }
}

void test11269()
{
    Atom a1;
    Atom a2 = {i:1, rest:10, b:2};
}

/********************************************/
// 11427

struct S11427
{
    union
    {
        ubyte a;
        int x;
    }
    void[] arr;
}

int foo11427() @safe
{
    S11427 s1 = S11427();
    S11427 s2;
    return 0;
}

/********************************************/
// 12011

struct S12011a
{
    int f() { return i; }
    enum e = this.init.f();
    int i = 1, j = 2;
}

struct S12011b
{
    int f() { return i; }
    enum e = S12011b().f();
    int i = 1, j = 2;
}

void test12011()
{
    static assert(S12011a.e == 1);
    static assert(S12011b.e == 1);
}

/********************************************/
// 13021

void test13021()
{
    static union U1
    {
        float a;
        int b;
    }

    static union U2
    {
        double a;
        long b;
    }

    static union U3
    {
        real a;
        struct B { long b1, b2; } // ICE happens only if B.sizeof == real.sizeof
        B b;
    }

    static union U4
    {
        real a;
        long[2] b; // ditto
    }

    auto f = U1(1.0);  auto ok = f.b;

    auto fail1 = U1(1.0).b; // OK <- Internal error: e2ir.c 1162
    auto fail2 = U2(1.0).b; // OK <- Internal error: e2ir.c 1162
    auto fail3 = U3(1.0).b; // OK <- Internal error: e2ir.c 1162
    auto fail4 = U4(1.0).b; // OK <- Internal error: backend/el.c 2904
}

/********************************************/
// 14556

enum E14556 { a = 1 }

struct S14556a
{
    this(int) {}
    E14556[1] data;
}

struct S14556b
{
    this(int) {}
    void[1] data;
}

void test14556()
{
    auto sa = S14556a(0);
    assert(sa.data == [E14556.a]);

    auto sb = S14556b(0);
    assert(sb.data[] == cast(ubyte[1])[0]);
}

/********************************************/
// https://issues.dlang.org/show_bug.cgi?id=17622

struct S17622
{
    int i;

    this(ubyte)
    {
        return;
    }

    void fun()
    {
        assert(i == 0);
    }
}

S17622 make()
{
    return S17622(0);
}

void test17622()
{
    S17622 s = make();

    auto rdg = (){ s.fun(); };

    s.fun();
}

/********************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();
    test10();
    test11();
    test12();
    test13();
    test14();
    test15a();
    test15b();
    test15c();
    test15d();
    test15e();
    test9993a();
    test9993b();
    test3198and1914();
    test2427();
    test5885();
    test5889();
    test4247();
    test6937();
    test12681();
    test3991();
    test7727();
    test7929();
    test7021();
    test8738();
    test8763();
    test8902();
    test9116();
    test9293();
    test9566();
    test11105();
    test11147();
    test11256();
    test13021();
    test14556();
    test17622();

    printf("Success\n");
    return 0;
}
