// EXTRA_SOURCES: imports/ufcs5a.d imports/ufcs5b.d imports/ufcs5c.d imports/ufcs5d.d imports/ufcs5e.d
// UNICODE_NAMES:

module ufcs;

extern (C) int printf(const char*, ...);

/*******************************************/

struct S {}

int foo(int n)          { return 1; }
int foo(int n, int m)   { return 2; }
int goo(int[] a)        { return 1; }
int goo(int[] a, int m) { return 2; }
int bar(S s)            { return 1; }
int bar(S s, int n)     { return 2; }

int baz(X)(X x)         { return 1; }
int baz(X)(X x, int n)  { return 2; }

int temp;
ref int boo(int n)      { return temp; }
ref int coo(int[] a)    { return temp; }
ref int mar(S s)        { return temp; }

ref int maz(X)(X x)     { return temp; }

void test1()
{
    int n;
    int[] a;
    S s;

    assert(   foo(4)    == 1);      assert(   baz(4)    == 1);
    assert( 4.foo()     == 1);      assert( 4.baz()     == 1);
    assert( 4.foo       == 1);      assert( 4.baz       == 1);
    assert(   foo(4, 2) == 2);      assert(   baz(4, 2) == 2);
    assert( 4.foo(2)    == 2);      assert( 4.baz(2)    == 2);
    assert((4.foo = 2)  == 2);      assert((4.baz = 2)  == 2);

    assert(   goo(a)    == 1);      assert(   baz(a)    == 1);
    assert( a.goo()     == 1);      assert( a.baz()     == 1);
    assert( a.goo       == 1);      assert( a.baz       == 1);
    assert(   goo(a, 2) == 2);      assert(   baz(a, 2) == 2);
    assert( a.goo(2)    == 2);      assert( a.baz(2)    == 2);
    assert((a.goo = 2)  == 2);      assert((a.baz = 2)  == 2);

    assert(   bar(s)    == 1);      assert(   baz(s)    == 1);
    assert( s.bar()     == 1);      assert( s.baz()     == 1);
    assert( s.bar       == 1);      assert( s.baz       == 1);
    assert(   bar(s, 2) == 2);      assert(   baz(s, 2) == 2);
    assert( s.bar(2)    == 2);      assert( s.baz(2)    == 2);
    assert((s.bar = 2)  == 2);      assert((s.baz = 2)  == 2);

    assert((  boo(4) = 2) == 2);    assert((  maz(4) = 2) == 2);
    assert((4.boo    = 2) == 2);    assert((4.maz    = 2) == 2);
    assert((  coo(a) = 2) == 2);    assert((  maz(a) = 2) == 2);
    assert((a.coo    = 2) == 2);    assert((a.maz    = 2) == 2);
    assert((  mar(s) = 2) == 2);    assert((  maz(s) = 2) == 2);
    assert((s.mar    = 2) == 2);    assert((s.maz    = 2) == 2);
}

int hoo(T)(int n)          { return 1; }
int hoo(T)(int n, int m)   { return 2; }
int koo(T)(int[] a)        { return 1; }
int koo(T)(int[] a, int m) { return 2; }
int var(T)(S s)            { return 1; }
int var(T)(S s, int n)     { return 2; }

int vaz(T, X)(X x)         { return 1; }
int vaz(T, X)(X x, int n)  { return 2; }

//int temp;
ref int voo(T)(int n)      { return temp; }
ref int woo(T)(int[] a)    { return temp; }
ref int nar(T)(S s)        { return temp; }

ref int naz(T, X)(X x)     { return temp; }

void test2()
{
    int n;
    int[] a;
    S s;

    assert(   hoo!int(4)    == 1);  assert(   vaz!int(4)    == 1);
    assert( 4.hoo!int()     == 1);  assert( 4.vaz!int()     == 1);
    assert( 4.hoo!int       == 1);  assert( 4.vaz!int       == 1);
    assert(   hoo!int(4, 2) == 2);  assert(   vaz!int(4, 2) == 2);
    assert( 4.hoo!int(2)    == 2);  assert( 4.vaz!int(2)    == 2);
    assert((4.hoo!int = 2)  == 2);  assert((4.vaz!int = 2)  == 2);

    assert(   koo!int(a)    == 1);  assert(   vaz!int(a)    == 1);
    assert( a.koo!int()     == 1);  assert( a.vaz!int()     == 1);
    assert( a.koo!int       == 1);  assert( a.vaz!int       == 1);
    assert(   koo!int(a, 2) == 2);  assert(   vaz!int(a, 2) == 2);
    assert( a.koo!int(2)    == 2);  assert( a.vaz!int(2)    == 2);
    assert((a.koo!int = 2)  == 2);  assert((a.vaz!int = 2)  == 2);

    assert(   var!int(s)    == 1);  assert(   vaz!int(s)    == 1);
    assert( s.var!int()     == 1);  assert( s.vaz!int()     == 1);
    assert( s.var!int       == 1);  assert( s.vaz!int       == 1);
    assert(   var!int(s, 2) == 2);  assert(   vaz!int(s, 2) == 2);
    assert( s.var!int(2)    == 2);  assert( s.vaz!int(2)    == 2);
    assert((s.var!int = 2)  == 2);  assert((s.vaz!int = 2)  == 2);

    assert((  voo!int(4) = 2) == 2);    assert((  naz!int(4) = 2) == 2);
    assert((4.voo!int    = 2) == 2);    assert((4.naz!int    = 2) == 2);
    assert((  woo!int(a) = 2) == 2);    assert((  naz!int(a) = 2) == 2);
    assert((a.woo!int    = 2) == 2);    assert((a.naz!int    = 2) == 2);
    assert((  nar!int(s) = 2) == 2);    assert((  naz!int(s) = 2) == 2);
    assert((s.nar!int    = 2) == 2);    assert((s.naz!int    = 2) == 2);
}

/*******************************************/

auto init(T)(T val) { return 1; }

auto sort(alias fun, T)(T val) { return 1; }

@property auto max(alias fun, T)(T val) { return 1; }

@property auto infinity(alias opt, T)(T val) { return 1; }

void test3()
{
    // See built-in 'init' property
    assert(1    .init == 0);
    assert([1]  .init == null);
    assert([1:1].init == null);
    assert(1.0  .init is double.nan);
    assert('c'  .init == 0xFF);
    assert("s"  .init == null);

    // x.init() has parens, so it runs UFCS call
    assert( 1   .init() == 1);
    assert([1]  .init() == 1);
    assert([1:1].init() == 1);
    assert(1.0  .init() == 1);
    assert('c'  .init() == 1);
    assert("s"  .init() == 1);

    // x.init!YYY matches templatized UFCS call.
    assert( 1   .init!int()        == 1);
    assert([1]  .init!(int[])()    == 1);
    assert([1:1].init!(int[int])() == 1);
    assert(1.0  .init!double()     == 1);
    assert('c'  .init!char()       == 1);
    assert("s"  .init!string()     == 1);

    assert([1].sort!"a<b"() == 1);

    // templatized properties runs UFCS call.
    assert(1024.max!"a<b" == 1);
    assert(1024.max  == int.max);

    assert(3.14.infinity!"+" == 1);
    assert(3.14.infinity == (double).infinity);
}

/*******************************************/

template Signal4()
{
    void connect(){}
}
struct S4
{
    mixin Signal4!() s;
}
void test4()
{
    S4 s;
    s.s.connect();  // s.s is TOKdotexp, so never match UFCS
}

/*******************************************/

auto f5_1(int)    { return 1; }
auto f5_2(string) { return 2; }
auto f5_3(double) { return 3; }
alias f5_4 = f5_1, f5_4 = f5_2;
alias f5_5 = f5_3, f5_5 = f5_4;

@property p5_1(int)    { return 1; }    @property p5_1(int,    int) { return 1; }
@property p5_2(string) { return 2; }    @property p5_2(string, int) { return 2; }
@property p5_3(double) { return 3; }    @property p5_3(double, int) { return 3; }
alias p5_4 = p5_1, p5_4 = p5_2;         alias p5_4 = p5_1, p5_4 = p5_2;
alias p5_5 = p5_3, p5_5 = p5_4;         alias p5_5 = p5_3, p5_5 = p5_4;

// import overload set 'f5ov' and 'p5ov'
import imports.ufcs5b, imports.ufcs5c;

void test5()
{
    {
        // f5_1 .. f5_5 are symbols which declared in module scope
        assert(100.f5_1() == 1);
        assert(001.f5_1() == 1); // https://issues.dlang.org/show_bug.cgi?id=8346
        assert("s".f5_2() == 2);
        assert(1.4.f5_3() == 3);
        assert(100.f5_4() == 1);
        assert("s".f5_4() == 2);
        assert(100.f5_5() == 1);
        assert("s".f5_5() == 2);
        assert(1.4.f5_5() == 3);
        // overload set
        assert(100.f5ov() == 1);
        assert("s".f5ov() == 2);
        // UFCS does not see function local alias
        alias func5 = f5_1;
        static assert(!__traits(compiles, { 1.func5(); }));

        // property getter/setter
        assert(100.p5_1 == 1);      assert((100.p5_1 = 1) == 1);
        assert("s".p5_2 == 2);      assert(("s".p5_2 = 1) == 2);
        assert(1.4.p5_3 == 3);      assert((1.4.p5_3 = 1) == 3);
        assert(100.p5_4 == 1);      assert((100.p5_4 = 1) == 1);
        assert("s".p5_4 == 2);      assert(("s".p5_4 = 1) == 2);
        assert(100.p5_5 == 1);      assert((100.p5_5 = 1) == 1);
        assert("s".p5_5 == 2);      assert(("s".p5_5 = 1) == 2);
        assert(1.4.p5_5 == 3);      assert((1.4.p5_5 = 1) == 3);
        // overload set     );      assert(
        assert(100.p5ov == 1);      assert((100.p5ov = 1) == 1);
        assert("s".p5ov == 2);      assert(("s".p5ov = 1) == 2);
        // local alias
        alias prop5 = p5_1;
        static assert(!__traits(compiles, { 1.prop5; }));
        static assert(!__traits(compiles, { 1.prop5 = 1; }));
    }

    {
        // f5a1 .. f5a5 are symbols which declared in module scope
        import imports.ufcs5a;
        // overload set 'f5ov' and 'p5ov'
        import imports.ufcs5b, imports.ufcs5c;

        assert(100.f5a1() == 1);
        assert("s".f5a2() == 2);
        assert(1.4.f5a3() == 3);
        assert(100.f5a4() == 1);
        assert("s".f5a4() == 2);
        assert(100.f5a5() == 1);
        assert("s".f5a5() == 2);
        assert(1.4.f5a5() == 3);
        assert(100.f5ov() == 1);
        assert("s".f5ov() == 2);

        assert(100.p5a1 == 1);      assert((100.p5a1 = 1) == 1);
        assert("s".p5a2 == 2);      assert(("s".p5a2 = 1) == 2);
        assert(1.4.p5a3 == 3);      assert((1.4.p5a3 = 1) == 3);
        assert(100.p5a4 == 1);      assert((100.p5a4 = 1) == 1);
        assert("s".p5a4 == 2);      assert(("s".p5a4 = 1) == 2);
        assert(100.p5a5 == 1);      assert((100.p5a5 = 1) == 1);
        assert("s".p5a5 == 2);      assert(("s".p5a5 = 1) == 2);
        assert(1.4.p5a5 == 3);      assert((1.4.p5a5 = 1) == 3);
        assert(100.p5ov == 1);      assert((100.p5ov = 1) == 1);
        assert("s".p5ov == 2);      assert(("s".p5ov = 1) == 2);
    }

    {
        // selective imports also work as expected
        import imports.ufcs5a : f5a1, f5a2;
        import imports.ufcs5a : p5a1, p5a2;

        assert(100.f5a1() == 1);
        assert("s".f5a2() == 2);
        static assert(!__traits(compiles, { 1.4.f5a3(); }));
        static assert(!__traits(compiles, { 100.f5a4(); }));
        static assert(!__traits(compiles, { "s".f5a4(); }));
        static assert(!__traits(compiles, { 100.f5a5(); }));
        static assert(!__traits(compiles, { "s".f5a5(); }));
        static assert(!__traits(compiles, { 1.4.f5a5(); }));

        assert(100.p5a1 == 1);      assert((100.p5a1 = 1) == 1);
        assert("s".p5a2 == 2);      assert(("s".p5a2 = 1) == 2);
        static assert(!__traits(compiles, { 1.4.p5a3; }) && !__traits(compiles, { 1.4.p5a3 = 1; }));
        static assert(!__traits(compiles, { 100.p5a4; }) && !__traits(compiles, { 100.p5a4 = 1; }));
        static assert(!__traits(compiles, { "s".p5a4; }) && !__traits(compiles, { "s".p5a4 = 1; }));
        static assert(!__traits(compiles, { 100.p5a5; }) && !__traits(compiles, { 100.p5a5 = 1; }));
        static assert(!__traits(compiles, { "s".p5a5; }) && !__traits(compiles, { "s".p5a5 = 1; }));
        static assert(!__traits(compiles, { 1.4.p5a5; }) && !__traits(compiles, { 1.4.p5a5 = 1; }));
    }

    {
        // renamed imports also work as expected
        import imports.ufcs5a : f5x1 = f5a1, f5x2 = f5a2;
        import imports.ufcs5a : p5x1 = p5a1, p5x2 = p5a2;

        assert(100.f5x1() == 1);
        assert("s".f5x2() == 2);
        static assert(!__traits(compiles, { 100.f5a1(); }));
        static assert(!__traits(compiles, { "s".f5a2(); }));
        static assert(!__traits(compiles, { 1.4.f5a3(); }));
        static assert(!__traits(compiles, { 100.f5a4(); }));
        static assert(!__traits(compiles, { "s".f5a4(); }));
        static assert(!__traits(compiles, { 100.f5a5(); }));
        static assert(!__traits(compiles, { "s".f5a5(); }));
        static assert(!__traits(compiles, { 1.4.f5a5(); }));

        assert(100.p5x1 == 1);      assert((100.p5x1 = 1) == 1);
        assert("s".p5x2 == 2);      assert(("s".p5x2 = 1) == 2);
        static assert(!__traits(compiles, { 100.p5a1; }) && !__traits(compiles, { 100.p5a1 = 1; }));
        static assert(!__traits(compiles, { "s".p5a2; }) && !__traits(compiles, { "s".p5a2 = 1; }));
        static assert(!__traits(compiles, { 1.4.p5a3; }) && !__traits(compiles, { 1.4.p5a3 = 1; }));
        static assert(!__traits(compiles, { 100.p5a4; }) && !__traits(compiles, { 100.p5a4 = 1; }));
        static assert(!__traits(compiles, { "s".p5a4; }) && !__traits(compiles, { "s".p5a4 = 1; }));
        static assert(!__traits(compiles, { 100.p5a5; }) && !__traits(compiles, { 100.p5a5 = 1; }));
        static assert(!__traits(compiles, { "s".p5a5; }) && !__traits(compiles, { "s".p5a5 = 1; }));
        static assert(!__traits(compiles, { 1.4.p5a5; }) && !__traits(compiles, { 1.4.p5a5 = 1; }));
    }

    {
        auto c5 = new C5();
        foreach (name; __traits(allMembers, C5))
        {
            static if (name.length >= 4 && name[0..4] == "test")
            {
                mixin("c5."~name~"();");    // call test function
            }
        }
    }
}

class B5
{
    int g5bm(int) { return 0; }
    static int g5bs(int) { return 0; }

}
class C5 : B5
{
    // normal import works.
    import imports.ufcs5a;
    void test1()
    {
        assert(100.f5a1() == 1);
        assert("s".f5a2() == 2);
        assert(1.4.f5a3() == 3);
        assert(100.f5a4() == 1);
        assert("s".f5a4() == 2);
        assert(100.f5a5() == 1);
        assert("s".f5a5() == 2);
        assert(1.4.f5a5() == 3);

        assert(100.p5a1 == 1);      assert((100.p5a1 = 1) == 1);
        assert("s".p5a2 == 2);      assert(("s".p5a2 = 1) == 2);
        assert(1.4.p5a3 == 3);      assert((1.4.p5a3 = 1) == 3);
        assert(100.p5a4 == 1);      assert((100.p5a4 = 1) == 1);
        assert("s".p5a4 == 2);      assert(("s".p5a4 = 1) == 2);
        assert(100.p5a5 == 1);      assert((100.p5a5 = 1) == 1);
        assert("s".p5a5 == 2);      assert(("s".p5a5 = 1) == 2);
        assert(1.4.p5a5 == 3);      assert((1.4.p5a5 = 1) == 3);
    }

    // selective imports also work as expected
    import imports.ufcs5d : f5d1, f5d2;
    import imports.ufcs5d : p5d1, p5d2;
    void test2()
    {
        assert(100.f5d1() == 1);
        assert("s".f5d2() == 2);
        static assert(!__traits(compiles, { 1.4.f5d3(); }));
        static assert(!__traits(compiles, { 100.f5d4(); }));
        static assert(!__traits(compiles, { "s".f5d4(); }));
        static assert(!__traits(compiles, { 100.f5d5(); }));
        static assert(!__traits(compiles, { "s".f5d5(); }));
        static assert(!__traits(compiles, { 1.4.f5d5(); }));

        assert(100.p5d1 == 1);  assert((100.p5d1 = 1) == 1);
        assert("s".p5d2 == 2);  assert(("s".p5d2 = 1) == 2);
        static assert(!__traits(compiles, { 1.4.p5d3; }) && !__traits(compiles, { 1.4.p5d3 = 1; }));
        static assert(!__traits(compiles, { 100.p5d4; }) && !__traits(compiles, { 100.p5d4 = 1; }));
        static assert(!__traits(compiles, { "s".p5d4; }) && !__traits(compiles, { "s".p5d4 = 1; }));
        static assert(!__traits(compiles, { 100.p5d5; }) && !__traits(compiles, { 100.p5d5 = 1; }));
        static assert(!__traits(compiles, { "s".p5d5; }) && !__traits(compiles, { "s".p5d5 = 1; }));
        static assert(!__traits(compiles, { 1.4.p5d5; }) && !__traits(compiles, { 1.4.p5d5 = 1; }));
    }

    // renamed imports also work as expected
    import imports.ufcs5e : f5y1 = f5e1, f5y2 = f5e2;
    import imports.ufcs5e : p5y1 = p5e1, p5y2 = p5e2;
    void test3()
    {
        assert(100.f5y1() == 1);
        assert("s".f5y2() == 2);
        static assert(!__traits(compiles, { 100.f5e1(); }));
        static assert(!__traits(compiles, { "s".f5e2(); }));
        static assert(!__traits(compiles, { 1.4.f5e3(); }));
        static assert(!__traits(compiles, { 100.f5e4(); }));
        static assert(!__traits(compiles, { "s".f5e4(); }));
        static assert(!__traits(compiles, { 100.f5e5(); }));
        static assert(!__traits(compiles, { "s".f5e5(); }));
        static assert(!__traits(compiles, { 1.4.f5e5(); }));

        assert(100.p5y1 == 1);  assert((100.p5y1 = 1) == 1);
        assert("s".p5y2 == 2);  assert(("s".p5y2 = 1) == 2);
        static assert(!__traits(compiles, { 100.p5e1; }) && !__traits(compiles, { (100.p5e1 = 1); }));
        static assert(!__traits(compiles, { "s".p5e2; }) && !__traits(compiles, { ("s".p5e2 = 1); }));
        static assert(!__traits(compiles, { 1.4.p5e3; }) && !__traits(compiles, { (1.4.p5e3 = 1); }));
        static assert(!__traits(compiles, { 100.p5e4; }) && !__traits(compiles, { (100.p5e4 = 1); }));
        static assert(!__traits(compiles, { "s".p5e4; }) && !__traits(compiles, { ("s".p5e4 = 1); }));
        static assert(!__traits(compiles, { 100.p5e5; }) && !__traits(compiles, { (100.p5e5 = 1); }));
        static assert(!__traits(compiles, { "s".p5e5; }) && !__traits(compiles, { ("s".p5e5 = 1); }));
        static assert(!__traits(compiles, { 1.4.p5e5; }) && !__traits(compiles, { (1.4.p5e5 = 1); }));
    }

    int g5cm(int) { return 0; }
    static int g5cs(int) { return 0; }
    void test4()
    {
        // UFCS does not see aggregate members
        static assert(!__traits(compiles, { 1.g5cm(); }));
        static assert(!__traits(compiles, { 1.g5cs(); }));

        // Even if it is in base class
        static assert(!__traits(compiles, { 1.g5bm(); }));
        static assert(!__traits(compiles, { 1.g5bs(); }));
    }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=662

enum Etest
{
    a,b,c,d
}

//typedef int testi = 10;
//typedef Test Test2;

int test() { return 33; }

class Test
{
    static int test(int i) { return i; }
}

int test(Etest test)
{
    return cast(int)test;
}

//int test(testi i)
//{
//  return cast(int)i;
//}
string to(T)(int i) {
    assert(i == 22);
    return "22";
}

void test682()
{
    assert(22.to!string() == "22");
    assert((new Test).test(11) == 11);
    assert(Test.test(11) == 11);
    //assert(Test2.test(11) == 11);
    assert(test() == 33);
    assert(ufcs.test() == 33);
    assert(Etest.d.test() == Etest.d);
    //testi i;
    //assert(i.test() == i.init);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=3382

@property T twice(T)(T x){ return x * x; }
char toupper(char c){ return ('a'<=c && c<='z') ? cast(char)(c - 'a' + 'A') : c; }

@property ref T setter(T)(ref T x, T v){ x = v; return x; }

auto iota(T)(T min, T max)
{
    static struct Result
    {
        T cur, end;

        T front() { return cur; }
        bool empty() { return front == end; }
        void popFront() { cur++; }
    }
    return Result(min, max);
}

auto map(string s, R)(R range)
{
    static struct Result
    {
        R source;
        auto front() { auto a = source.front; return mixin(s); }
        alias source this;
    }
    return Result(range);
}

auto filter(string s, R)(R range)
{
    static struct Result
    {
        R source;
        alias source this;
        void popFront()
        {
            while (true)
            {
                auto a = source.front;
                if (mixin(s)) break;
                source.popFront();
            }
        }
    }
    return Result(range);
}

void test3382()
{
    auto r = iota(0, 10).map!"a*3"().filter!"a%2 != 0"();
    foreach (e; r)
        printf("e = %d\n", e);

    assert(10.twice == 100);
    assert(0.5.twice == 0.25);
    assert('c'.toupper() == 'C');
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=6185

ref T front(T)(T[] array) { return array[0]; }
bool empty(T)(T[] array) { return array.length == 0; }
void popFront(T)(ref T[] array) { array = array[1..$]; }

bool equal(T, U)(T t, U u)
{
    while (true)
    {
        if (t.empty) return u.empty;
        if (u.empty || t.front != u.front) return false;
        t.popFront();
        u.popFront();
    }
}

void test6185()
{
    auto r1 = [1,2,3].map!"a*2";
    assert(equal(r1, [2,4,6]));

    auto r2 = r1.map!"a+2"();
    assert(equal(r2, [4,6,8]));
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=6070

enum test6070a = ["test"].foo6070();
enum test6070b = foo6070(["test"]);

string foo6070(string[] s) { return ""; }

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7670

struct A7670
{
    double x;
}
@property ref double y7670(return ref A7670 a)
{
    return a.x;
}
void test7670()
{
    A7670 a1;
    a1.y7670() = 2.0; // OK
    a1.y7670 = 2.0; // Error
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7703
void f7703(T)(T a) { }

void test7703()
{
    int x;
    x.f7703;        // accepted
    x.f7703();      // accepted
    x.f7703!int;    // rejected -- "f(x) isn't a template"
    x.f7703!int();  // accepted
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7773

//import core.stdc.stdio;
void writeln7773(int n){}
void test7773()
{
    (int.max).writeln7773(); // OK
    int.max.writeln7773();   // error
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7943

struct Foo7943
{
    int _member;
    alias _member this;
}

int foo7943(Foo7943 f) { return 1; }
int foo7943(int i) { return 2; }

void test7943()
{
    Foo7943 f;
    assert(f.foo7943() == 1);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8180

int writeln8180(T...)(T args) { return 1; }

struct Tuple8180(T...)
{
    T field;
    alias field this;
}

void test8180()
{
    auto t = Tuple8180!(int)(10);
    assert(t.writeln8180() == 1);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8245

          string toStr8245(immutable(char)* p) { return null; }
@property string asStr8245(immutable(char)* p) { return null; }

void test8245()
{
    immutable(char)* p = "foobar".ptr;
    p.toStr8245();
    p.asStr8245;    // Error: no property 'asStr' for type 'immutable(char)'
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8252

bool f(int x) { return !x; }

void test8252()
{
    static assert(!1.f); // ok
    static assert( 0.f); // fail
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8453

T[] sort8453(T)(T[] a) { return a; }

void test8453()
{
    int[int] foo;
    auto bar1 = foo.keys().sort8453(); // OK
    auto bar2 = foo.keys.sort8453();   // Error
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8503

//UTF-8 chars
void α8503(int i) {}

void test8503()
{
    0.α8503();  // Error
    1.α8503();  // Error
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9014

@property ref int foo9014(int[] a)
{
    return a[0];
}
void test9014()
{
    int[] bar;
  static assert(!__traits(compiles, {
    bar.foo9014 = missing.foo9014;
  }));
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9590

auto func9590(E)(lazy E expr) { }

int f9590a()  { assert(0); }
void f9590b() { assert(0); }

void test9590()
{
    func9590(f9590a());  // ok, no exceptions (lazy)
    f9590a().func9590;   // ok, no exceptions (lazy)

    func9590(f9590b());  // ok, no exceptions (lazy)
    f9590b().func9590;   // L12: NG
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9946

size_t count9946(alias x)(int[] haystack)
{
    return 0;
}
void test9946()
{
    int[] data;
    auto n1 = count9946!5(data);          // OK
    auto n2 = data.count9946!5;           // OK
    auto a1 = new int[count9946!5(data)]; // OK
    auto a2 = new int[data.count9946!5];  // Error
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10618

template Temp10618(T)
{
    size_t len = 1;
}
void test10618()
{
    auto arr = new int[Temp10618!int.len];
    assert(arr.length == 1);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10003

void foo10003(void *p) {}
void test10003()
{
    void* p;
    p.foo10003();
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10041

auto writeln10041(T...)(T args) { return typeof(args[0]).stringof; }

void test10041()
{
    auto aa = [1: 2];
    assert(aa.writeln10041 == "int[int]");
    assert(writeln10041(aa) == "int[int]");
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10047

struct Typedef10047(T)
{
    template opDispatch(string name)
    {
        static assert(0);
    }
}

struct A10047 {}
int foo10047(Typedef10047!A10047 a) { return 10; }

void test10047()
{
    Typedef10047!A10047 a;
    assert(a.foo10047() == 10);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10166

auto foo10166()
{
    0.bar10166!({})(0);
}

void bar10166(alias handler, T)(T t, int i)
{
    t.bar10166!buzz10166(i);
}

void buzz10166() {}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10526

struct S10526
{
    int opDispatch(string s, A...)(A args)
    if (s[0..3] == "foo")
    {
        return 1;
    }
}
int bar10526(X)(X) { return 2; }
int baz10526(T, X)(X) { return 3; }

void test10526()
{
    S10526 s;

    // with parenthesis
    assert(s.foo10526() == 1);
    assert(s.bar10526() == 2);
    assert(s.baz10526!string() == 3);

    // without parenthesis
    assert(s.foo10526 == 1);
    assert(s.bar10526 == 2);
    assert(s.baz10526!string == 3);
}

/********************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10609

int foo10609(int x) { return x; }

void test10609()
{
    int x = 1;
    static assert(__traits(compiles, foo10609(x)));
    static assert(__traits(compiles, 1.foo10609 ));
    static assert(__traits(compiles, x.foo10609 ));
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11312

struct S11312;

S11312* getS11312() { return null; }
int getValue(S11312*) { return 10; }

void test11312()
{
    S11312* op = getS11312();
    int x = op.getValue();
    assert(x == 10);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15123

auto keys15123(K, V)(V[K] aa) { return [1]; }
auto values15123(K, V)(V[K] aa) { return [2]; }

alias id15123(alias arg) = arg;

enum int[int] aa15123 = [1:2];
static assert(id15123!(aa15123.keys15123) == [1]);  // TypeIdentifier + UFCS

T[T] f15123(T)() { return [1:2]; }
static assert(id15123!(f15123!int.values15123) == [2]); // TypeInstance + UFCS

/*******************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test682();
    test3382();
    test6185();
    test7670();
    test7703();
    test7773();
    test7943();
    test8180();
    test8245();
    test8252();
    test8453();
    test8503();
    test9014();
    test9590();
    test9946();
    test10618();
    test10003();
    test10041();
    test10047();
    test10526();
    test11312();

    printf("Success\n");
    return 0;
}
