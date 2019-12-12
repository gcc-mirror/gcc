// EXTRA_SOURCES: imports/ovs1528a.d imports/ovs1528b.d
// EXTRA_SOURCES: imports/template_ovs1.d imports/template_ovs2.d imports/template_ovs3.d

import imports.template_ovs1;
import imports.template_ovs2;
import imports.template_ovs3;

extern(C) int printf(const char* fmt, ...);

template TypeTuple(T...){ alias T TypeTuple; }
template Id(      T){ alias T Id; }
template Id(alias A){ alias A Id; }

/***************************************************/
// 1528

int foo1528(long){ return 1; }
int foo1528(int[]){ return 2; }
int foo1528(T)(T) if ( is(T:real)) { return 3; }
int foo1528(T)(T) if (!is(T:real)) { return 4; }
int bar1528(T)(T) if (!is(T:real)) { return 4; }
int bar1528(T)(T) if ( is(T:real)) { return 3; }
int bar1528(int[]){ return 2; }
int bar1528(long){ return 1; }

@property auto getfoo1528   () { return 1; }
@property auto getfoo1528(T)() { return 2; }
@property auto getbar1528(T)() { return 2; }
@property auto getbar1528   () { return 1; }

@property auto setfoo1528   (int) { return 1; }
@property auto setfoo1528(T)(int) { return 2; }
@property auto setbar1528(T)(int) { return 2; }
@property auto setbar1528   (int) { return 1; }

struct S1528
{
    int foo(long){ return 1; }
    int foo(int[]){ return 2; }
    int foo(T)(T) if ( is(T:real)) { return 3; }
    int foo(T)(T) if (!is(T:real)) { return 4; }
    int bar(T)(T) if (!is(T:real)) { return 4; }
    int bar(T)(T) if ( is(T:real)) { return 3; }
    int bar(int[]){ return 2; }
    int bar(long){ return 1; }

    @property auto getfoo   () { return 1; }
    @property auto getfoo(T)() { return 2; }
    @property auto getbar(T)() { return 2; }
    @property auto getbar   () { return 1; }

    @property auto setfoo   (int) { return 1; }
    @property auto setfoo(T)(int) { return 2; }
    @property auto setbar(T)(int) { return 2; }
    @property auto setbar   (int) { return 1; }

    @property auto propboo   ()    { return 1; }
    @property auto propboo(T)(T)   { return 2; }
    @property auto propbaz(T)(T)   { return 2; }
    @property auto propbaz   ()    { return 1; }
}

auto ufoo1528   (S1528) { return 1; }
auto ufoo1528(T)(S1528) { return 2; }
auto ubar1528(T)(S1528) { return 2; }
auto ubar1528   (S1528) { return 1; }

@property auto ugetfoo1528   (S1528) { return 1; }
@property auto ugetfoo1528(T)(S1528) { return 2; }
@property auto ugetbar1528(T)(S1528) { return 2; }
@property auto ugetbar1528   (S1528) { return 1; }

@property auto usetfoo1528   (S1528, int) { return 1; }
@property auto usetfoo1528(T)(S1528, int) { return 2; }
@property auto usetbar1528(T)(S1528, int) { return 2; }
@property auto usetbar1528   (S1528, int) { return 1; }

@property auto upropboo1528   (S1528)      { return 1; }
@property auto upropboo1528(T)(S1528, T)   { return 2; }
@property auto upropbaz1528(T)(S1528, T)   { return 2; }
@property auto upropbaz1528   (S1528)      { return 1; }

void test1528a()
{
    // global
    assert(foo1528(100) == 1);
    assert(foo1528(10L) == 1);
    assert(foo1528([1]) == 2);
    assert(foo1528(1.0) == 3);
    assert(foo1528("a") == 4);
    assert(bar1528(100) == 1);
    assert(bar1528(10L) == 1);
    assert(bar1528([1]) == 2);
    assert(bar1528(1.0) == 3);
    assert(bar1528("a") == 4);

    assert(getfoo1528        == 1);
    assert(getfoo1528!string == 2);
    assert(getbar1528        == 1);
    assert(getbar1528!string == 2);

    assert((setfoo1528        = 1) == 1);
    assert((setfoo1528!string = 1) == 2);
    assert((setbar1528        = 1) == 1);
    assert((setbar1528!string = 1) == 2);

    S1528 s;

    // member
    assert(s.foo(100) == 1);
    assert(s.foo(10L) == 1);
    assert(s.foo([1]) == 2);
    assert(s.foo(1.0) == 3);
    assert(s.foo("a") == 4);
    assert(s.bar(100) == 1);
    assert(s.bar(10L) == 1);
    assert(s.bar([1]) == 2);
    assert(s.bar(1.0) == 3);
    assert(s.bar("a") == 4);

    assert(s.getfoo        == 1);
    assert(s.getfoo!string == 2);
    assert(s.getbar        == 1);
    assert(s.getbar!string == 2);

    assert((s.setfoo        = 1) == 1);
    assert((s.setfoo!string = 1) == 2);
    assert((s.setbar        = 1) == 1);
    assert((s.setbar!string = 1) == 2);

    assert((s.propboo = 1) == 2);
    assert( s.propboo      == 1);
    assert((s.propbaz = 1) == 2);
    assert( s.propbaz      == 1);

    // UFCS
    assert(s.ufoo1528       () == 1);
    assert(s.ufoo1528!string() == 2);
    assert(s.ubar1528       () == 1);
    assert(s.ubar1528!string() == 2);

    assert(s.ugetfoo1528        == 1);
    assert(s.ugetfoo1528!string == 2);
    assert(s.ugetbar1528        == 1);
    assert(s.ugetbar1528!string == 2);

    assert((s.usetfoo1528        = 1) == 1);
    assert((s.usetfoo1528!string = 1) == 2);
    assert((s.usetbar1528        = 1) == 1);
    assert((s.usetbar1528!string = 1) == 2);

    assert((s.upropboo1528 = 1) == 2);
    assert( s.upropboo1528      == 1);
    assert((s.upropbaz1528 = 1) == 2);
    assert( s.upropbaz1528      == 1);

    // overload set
    import imports.ovs1528a, imports.ovs1528b;
    assert(func1528()    == 1);
    assert(func1528(1.0) == 2);
    assert(func1528("a") == 3);
    assert(func1528([1.0]) == 4);
    assert(bunc1528()    == 1);
    assert(bunc1528(1.0) == 2);
    assert(bunc1528("a") == 3);
    assert(bunc1528([1.0]) == 4);

    assert(vunc1528(100) == 1);
    assert(vunc1528("a") == 2);
    assert(wunc1528(100) == 1);
    assert(wunc1528("a") == 2);

    //assert(opUnary1528!"+"(10) == 1);
    //assert(opUnary1528!"-"(10) == 2);
}

// ----

int doo1528a(int a, double=10) { return 1; }
int doo1528a(int a, string="") { return 2; }

int doo1528b(int a) { return 1; }
int doo1528b(T:int)(T b) { return 2; }

int doo1528c(T:int)(T b, double=10) { return 2; }
int doo1528c(T:int)(T b, string="") { return 2; }

int doo1528d(int a) { return 1; }
int doo1528d(T)(T b) { return 2; }

void test1528b()
{
    // MatchLevel by tiargs     / by fargs
    static assert(!__traits(compiles, doo1528a(1)));
            // 1: MATCHexact    / MATCHexact
            // 2: MATCHexact    / MATCHexact
    static assert(!__traits(compiles, doo1528a(1L)));
            // 1: MATCHexact    / MATCHconvert
            // 2: MATCHexact    / MATCHconvert

    static assert(!__traits(compiles, doo1528b(1)));
            // 1: MATCHexact    / MATCHexact
            // 2: MATCHexact    / MATCHexact
    assert(doo1528b(1L) == 1);
            // 1: MATCHexact    / MATCHconvert
            // 2: MATCHnomatch  / -

    static assert(!__traits(compiles, doo1528c(1)));
            // 1: MATCHexact    / MATCHexact
            // 2: MATCHexact    / MATCHexact
    static assert(!__traits(compiles, doo1528c(1L)));
            // 1: MATCHnomatch  / -
            // 2: MATCHnomatch  / -

    assert(doo1528d(1) == 1);
            // 1: MATCHexact    / MATCHexact
            // 2: MATCHconvert  / MATCHexact
    assert(doo1528d(1L) == 1);
            // 1: MATCHexact    / MATCHconvert
            // 2: MATCHconvert  / MATCHexact
            // -> not sure, may be ambiguous...?
}

// ----

char[num*2] toHexString1528(int order, size_t num)(in ubyte[num] digest) { return typeof(return).init; }
     string toHexString1528(int order)(in ubyte[] digest) { assert(0); }

char[8] test1528c()
{
    ubyte[4] foo() { return typeof(return).init; }
    return toHexString1528!10(foo);
}

// ----

int f1528d1(int a, double=10) { return 1; }
int f1528d1(int a, string="") { return 2; }

int f1528d2(T:int)(T b, double=10) { return 1; }
int f1528d2(T:int)(T b, string="") { return 2; }

// vs deduced parameter
int f1528d3(int a) { return 1; }
int f1528d3(T)(T b) { return 2; }

// vs specialized parameter
int f1528d4(int a) { return 1; }
int f1528d4(T:int)(T b) { return 2; }

// vs deduced parameter + template constraint (1)
int f1528d5(int a) { return 1; }
int f1528d5(T)(T b) if (is(T == int)) { return 2; }

// vs deduced parameter + template constraint (2)
int f1528d6(int a) { return 1; }
int f1528d6(T)(T b) if (is(T : int)) { return 2; }

// vs nallowing conversion
int f1528d7(ubyte a) { return 1; }
int f1528d7(T)(T b) if (is(T : int)) { return 2; }

int f1528d10(int, int) { return 1; }
int f1528d10(T)(T, int) { return 2; }

void test1528d()
{
    static assert(!__traits(compiles, f1528d1(1)));  // ambiguous
    static assert(!__traits(compiles, f1528d1(1L))); // ambiguous

    static assert(!__traits(compiles, f1528d2(1)));  // ambiguous
    static assert(!__traits(compiles, f1528d2(1L))); // no match

    assert(f1528d3(1) == 1);
    assert(f1528d3(1L) == 1);    // '1L' matches int
    short short_val = 42;
    assert(f1528d3(cast(short) 42) == 1);
    assert(f1528d3(short_val) == 1);

    static assert(!__traits(compiles, f1528d4(1)));
    assert(f1528d4(1L) == 1);

    assert(f1528d5(1) == 1);
    assert(f1528d5(1L) == 1);

    assert(f1528d6(1) == 1);
    assert(f1528d6(1L) == 1);
    static assert(!__traits(compiles, f1528d6(ulong.max))); // no match
                                          // needs to fix bug 9617
    ulong ulval = 1;
    static assert(!__traits(compiles, f1528d6(ulval)));     // no match

    assert(f1528d7(200u) == 1);  // '200u' matches ubyte
    assert(f1528d7(400u) == 2);
    uint uival = 400;       // TDPL-like range knowledge lost here.
    assert(f1528d7(uival) == 2);
    uival = 200;            // Ditto.
    assert(f1528d7(uival) == 2);


    assert(f1528d10(        1, 9) == 1);
    assert(f1528d10(       1U, 9) == 1);
    assert(f1528d10(       1L, 9) == 1);
    assert(f1528d10(      1LU, 9) == 1);
    assert(f1528d10( long.max, 9) == 2);
    assert(f1528d10(ulong.max, 9) == 2);
    assert(f1528d10(        1, 9L) == 1);
    assert(f1528d10(       1U, 9L) == 1);
    assert(f1528d10(       1L, 9L) == 1);
    assert(f1528d10(      1LU, 9L) == 1);
    assert(f1528d10( long.max, 9L) == 2);
    assert(f1528d10(ulong.max, 9L) == 2);
}

/***************************************************/
// 1680

struct S1680
{
    ulong _y;

           ulong blah1()        { return _y; }
    static S1680 blah1(ulong n) { return S1680(n); }

    static S1680 blah2(ulong n)  { return S1680(n); }
    static S1680 blah2(char[] n) { return S1680(n.length); }
}

class C1680
{
    ulong _y;
    this(ulong n){}

           ulong blah1()        { return _y; }
    static C1680 blah1(ulong n) { return new C1680(n); }

    static C1680 blah2(ulong n)  { return new C1680(n); }
    static C1680 blah2(char[] n) { return new C1680(n.length); }
}

void test1680()
{
    // OK
    S1680 s = S1680.blah1(5);
    void fs()
    {
        S1680 s1 = S1680.blah2(5);              // OK
        S1680 s2 = S1680.blah2("hello".dup);    // OK
        S1680 s3 = S1680.blah1(5);
        // Error: 'this' is only allowed in non-static member functions, not f
    }

    C1680 c = C1680.blah1(5);
    void fc()
    {
        C1680 c1 = C1680.blah2(5);
        C1680 c2 = C1680.blah2("hello".dup);
        C1680 c3 = C1680.blah1(5);
    }
}

/***************************************************/
// 7418

int foo7418(uint a)   { return 1; }
int foo7418(char[] a) { return 2; }

alias foo7418 foo7418a;
template foo7418b(T = void) { alias foo7418 foo7418b; }

void test7418()
{
    assert(foo7418a(1U) == 1);
    assert(foo7418a("a".dup) == 2);

    assert(foo7418b!()(1U) == 1);
    assert(foo7418b!()("a".dup) == 2);
}

/***************************************************/
// 7552

struct S7552
{
    static void foo(){}
    static void foo(int){}
}

struct T7552
{
    alias TypeTuple!(__traits(getOverloads, S7552, "foo")) FooInS;
    alias FooInS[0] foo;    // should be S7552.foo()
    static void foo(string){}
}

struct U7552
{
    alias TypeTuple!(__traits(getOverloads, S7552, "foo")) FooInS;
    alias FooInS[1] foo;    // should be S7552.foo(int)
    static void foo(string){}
}

void test7552()
{
    alias TypeTuple!(__traits(getOverloads, S7552, "foo")) FooInS;
    static assert(FooInS.length == 2);
                                      FooInS[0]();
    static assert(!__traits(compiles, FooInS[0](0)));
    static assert(!__traits(compiles, FooInS[1]()));
                                      FooInS[1](0);

                                      Id!(FooInS[0])();
    static assert(!__traits(compiles, Id!(FooInS[0])(0)));
    static assert(!__traits(compiles, Id!(FooInS[1])()));
                                      Id!(FooInS[1])(0);

    alias TypeTuple!(__traits(getOverloads, T7552, "foo")) FooInT;
    static assert(FooInT.length == 2);                  // fail
                                      FooInT[0]();
    static assert(!__traits(compiles, FooInT[0](0)));
    static assert(!__traits(compiles, FooInT[0]("")));
    static assert(!__traits(compiles, FooInT[1]()));
    static assert(!__traits(compiles, FooInT[1](0)));   // fail
                                      FooInT[1]("");    // fail

    alias TypeTuple!(__traits(getOverloads, U7552, "foo")) FooInU;
    static assert(FooInU.length == 2);
    static assert(!__traits(compiles, FooInU[0]()));
                                      FooInU[0](0);
    static assert(!__traits(compiles, FooInU[0]("")));
    static assert(!__traits(compiles, FooInU[1]()));
    static assert(!__traits(compiles, FooInU[1](0)));
                                      FooInU[1]("");
}

/***************************************************/
// 8668

import imports.m8668a;
import imports.m8668c; //replace with m8668b to make it work

void test8668()
{
    split8668("abc");
    split8668(123);
}

/***************************************************/
// 8943

void test8943()
{
    struct S
    {
        void foo();
    }

    alias TypeTuple!(__traits(getOverloads, S, "foo")) Overloads;
    alias TypeTuple!(__traits(parent, Overloads[0])) P; // fail
    static assert(is(P[0] == S));
}

/***************************************************/
// 9410

struct S {}
int foo(float f, ref S s) { return 1; }
int foo(float f,     S s) { return 2; }
void test9410()
{
    S s;
    assert(foo(1, s  ) == 1); // works fine. Print: ref
    assert(foo(1, S()) == 2); // Fails with: Error: S() is not an lvalue
}

/***************************************************/
// 10171

struct B10171(T) { static int x; }

void test10171()
{
    auto mp = &B10171!(B10171!int).x;
}

/***************************************************/
// 1900 - template overload set

void test1900a()
{
    // function vs function template with IFTI call
    assert(foo1900a(100) == 1);
    assert(foo1900a("s") == 2);
    assert(foo1900b(100) == 1);
    assert(foo1900b("s") == 2);
    // function template call with explicit template arguments
    assert(foo1900a!string("s") == 2);
    assert(foo1900b!string("s") == 2);

    // function template overloaded set call with IFTI
    assert(bar1900a(100) == 1);
    assert(bar1900a("s") == 2);
    assert(bar1900b(100) == 1);
    assert(bar1900b("s") == 2);
    // function template overloaded set call with explicit template arguments
    assert(bar1900a!double(100) == 1);
    assert(bar1900a!string("s") == 2);
    assert(bar1900b!double(100) == 1);
    assert(bar1900b!string("s") == 2);

    // function template overloaded set call with IFTI
    assert(baz1900(1234567890) == 1);
    assert(baz1900([1:1, 2:2]) == 2);
    assert(baz1900(new Object) == 3);
    assert(baz1900("deadbeaf") == 4);
    // function template overloaded set call with explicit template arguments
    assert(baz1900!(double)(14142135) == 1);
    assert(baz1900!(int[int])([12:34]) == 2);
    assert(baz1900!(Object)(new Object) == 3);
    assert(baz1900!(string)("cafe babe") == 4);

    static assert(!__traits(compiles, bad1900!"++"()));
}

void test1900b()
{
    S1900 s;

    // function vs function template with IFTI call
    assert(s.foo1900a(100) == 1);
    assert(s.foo1900a("s") == 2);
    assert(s.foo1900b(100) == 1);
    assert(s.foo1900b("s") == 2);
    // function template call with explicit template arguments
    assert(s.foo1900a!string("s") == 2);
    assert(s.foo1900b!string("s") == 2);

    // function template overloaded set call with IFTI
    assert(s.bar1900a(100) == 1);
    assert(s.bar1900a("s") == 2);
    assert(s.bar1900b(100) == 1);
    assert(s.bar1900b("s") == 2);
    // function template overloaded set call with explicit template arguments
    assert(s.bar1900a!double(100) == 1);
    assert(s.bar1900a!string("s") == 2);
    assert(s.bar1900b!double(100) == 1);
    assert(s.bar1900b!string("s") == 2);

    // function template overloaded set call with IFTI
    assert(s.baz1900(1234567890) == 1);
    assert(s.baz1900([1:1, 2:2]) == 2);
    assert(s.baz1900(new Object) == 3);
    assert(s.baz1900("deadbeaf") == 4);
    // function template overloaded set call with explicit template arguments
    assert(s.baz1900!(double)(14142135) == 1);
    assert(s.baz1900!(int[int])([12:34]) == 2);
    assert(s.baz1900!(Object)(new Object) == 3);
    assert(s.baz1900!(string)("cafe babe") == 4);

    static assert(!__traits(compiles, s.bad1900!"++"()));
}

void test1900c()
{
    S1900 s;

    // This is a kind of Issue 1528 - [tdpl] overloading template and non-template functions
    //s.funca();
    //s.funca(10);
    //s.funcb();
    //s.funcb(10);

    // Call function template overload set through mixin member lookup
    assert(s.mixfooa() == 1);
    assert(s.mixfooa(10) == 2);
    assert(s.mixfoob() == 1);
    assert(s.mixfoob(10) == 2);

    // Call function template overload set through mixin^2 member lookup
    assert(s.mixsubfooa() == 1);
    assert(s.mixsubfooa(10) == 2);
    assert(s.mixsubfoob() == 1);
    assert(s.mixsubfoob(10) == 2);

    // Using mixin identifier can limit overload set
    assert(s.a.mixfooa() == 1);     static assert(!__traits(compiles, s.a.mixfooa(10)));
    assert(s.b.mixfooa(10) == 2);   static assert(!__traits(compiles, s.b.mixfooa()));
    assert(s.b.mixfoob() == 1);     static assert(!__traits(compiles, s.b.mixfoob(10)));
    assert(s.a.mixfoob(10) == 2);   static assert(!__traits(compiles, s.a.mixfoob()));
}

alias merge1900 = imports.template_ovs1.merge1900;
alias merge1900 = imports.template_ovs2.merge1900;

void test1900d()
{
    assert( merge1900!double(100) == 1);
    assert(.merge1900!double(100) == 1);
}

mixin template Foo1900e(T)
{
    void foo(U : T)() { v++;}
}
void test1900e()
{
    struct S
    {
        int v;
        mixin Foo1900e!double;
        mixin Foo1900e!string;
        void test()
        {
            foo!(int);          // ScopeExp(ti->tempovers != NULL)
            foo!(typeof(null)); // ScopeExp(ti->tempovers != NULL)
        }
    }

    S s;
    assert(s.v == 0);
    s.test();
    assert(s.v == 2);
}

/***************************************************/
// 1900

void test1900()
{
    AClass1900 a;
    BClass1900 b;

    static assert(Traits1900!(AClass1900).name == "AClass");
    static assert(Traits1900!(BClass1900).name == "BClass");
    static assert(Traits1900!(int).name == "any");

    Traits1900!(long) obj;

    static assert(Value1900a!double == 1);
    static assert(Value1900b!double == 1);
    static assert(Value1900a!string == 2);
    static assert(Value1900b!string == 2);
}

alias imports.template_ovs1.Traits1900 Traits1900X;
alias imports.template_ovs2.Traits1900 Traits1900X;
alias imports.template_ovs3.Traits1900 Traits1900X;
static assert(Traits1900X!(AClass1900).name == "AClass");
static assert(Traits1900X!(BClass1900).name == "BClass");
static assert(Traits1900X!(int).name == "any");

// Traits1900Y is exact same as imports.template_ovs1.Traits1900.
alias imports.template_ovs1.Traits1900 Traits1900Y1;
alias imports.template_ovs1.Traits1900 Traits1900Y2;
alias Traits1900Y1 Traits1900Y;
alias Traits1900Y2 Traits1900Y;
static assert(Traits1900Y!(AClass1900).name == "AClass");
static assert(!__traits(compiles, Traits1900Y!(BClass1900)));
static assert(!__traits(compiles, Traits1900Y!(int)));

template Foo1900(T)
{
    template Bar1900(U : T)
    {
    }
}
mixin Foo1900!(int) A1900;
mixin Foo1900!(char) B1900;
alias Bar1900!(int) bar1900;    // error

/***************************************************/
// 7780

mixin template A7780()
{
    template C(int n : 0) { int C = 0; }
}
mixin template B7780()
{
    template C(int n : 1) { int C = 1; }
}

class Foo7780
{
    mixin A7780!();
    mixin B7780!();
}

void test7780()
{
    assert(Foo7780.C!0 == 0);
}

/***************************************************/

auto foo7849(string) { return 1; }
auto foo7849(dstring) { return 2; }

enum str7849a = "string";
immutable str7849ai = "string";
immutable str7849bi = str7849ai;
enum str7849b = str7849ai;
enum str7849c = str7849bi;

void test7849()
{
    assert(foo7849(str7849a) == 1);
    assert(foo7849(str7849b) == 1);
    assert(foo7849(str7849c) == 1);
}

/***************************************************/
// 8352

void test8352()
{
    [1, 2].remove8352a!(x => x == 2)();
    [1, 2].remove8352b!(x => x == 2)();
    remove8352a("deleteme");
    remove8352b("deleteme");
}

/***************************************************/
// 8441

mixin template T8441a(string i)
{
    auto j(string s = "a", U)(U u1, U u2)
    {
        return 0;
    }
    auto j(int i,string s = "a", W)(W u1, W u2)
    {
        return i;
    }

    mixin("
        class F" ~ i ~ "
        {
            auto j(string s = \"a\", U)(U u1, U u2)
            {
                return this.outer.t" ~ i ~ ".j!(s, U)(u1, u2);
            }
            auto j(int i, string s = \"a\", W)(W u1, W u2)
            {
                return this.outer.t" ~ i ~ ".j!(i, s, W)(u1, u2);   // <- dmd is giving error for j!(...).j's type
            }
        }
        auto f" ~ i ~ "()
        {
            return new F" ~ i ~ "();
        }
    ");
}
class X8441a
{
    mixin T8441a!("1") t0;
    alias t0 t1;
}
void test8441a()
{
    auto x = new X8441a();
    x.f1().j!(3,"a")(2.2, 3.3);
}

// ----

mixin template T8441b()
{
    void k()() {}

    void j()() {}
    void j(int i)() {}
}
class X8441b
{
    mixin T8441b t0;
}
void test8441b()
{
    auto x = new X8441b();
    x.k!()();    // Fine
    x.j!()();    // Fine
    x.t0.k!()(); // Fine
    x.t0.j!()(); // Derp
}

// ----

mixin template Signal8441c(Args...)
{
    bool call = false;
    final void connect(string method, ClassType)(ClassType obj)
    if (is(ClassType == class) && __traits(compiles, { void delegate(Args) dg = mixin("&obj."~method); }))
    {
        call = true;
    }
}
void test8441c()
{
    class Observer
    {
        void watchInt(string str, int i) {}
    }
    class Bar
    {
        mixin Signal8441c!(string, int)  s1;
        mixin Signal8441c!(string, int)  s2;
        mixin Signal8441c!(string, long) s3;
    }
    auto a = new Bar;
    auto o1 = new Observer;

    a.s1.connect!"watchInt"(o1);

    assert( a.s1.call);
    assert(!a.s2.call);
    assert(!a.s3.call);
}

/***************************************************/
// 9235

template FlowEvaluator9235()
{
    // if control flow
    bool execute(Command cmd)()
        if (cmd == Command.Jump ||
            cmd == Command.Fork)
    {
        return false;
    }
}
template MatchEvaluator9235()
{
    // if operation
    bool execute(Command cmd)()
        if (cmd == Command.Char ||
            cmd == Command.Any ||
            cmd == Command.End)
    {
        return true;
    }
}
void test9235a()
{
    enum Command
    {
        Char, Any, Fork, Jump, End
    }
    struct Machine
    {
        mixin FlowEvaluator9235;
        mixin MatchEvaluator9235;

        bool exec_flow()
        {
            return execute!(Command.Jump)();
        }
        bool exec_match()
        {
            return execute!(Command.Any)();
        }
    }

    Machine m;
    assert(!m.exec_flow());
    assert( m.exec_match());
}

// ----

mixin template mixA9235()
{
    int foo(string s)() if (s == "a") { return 1; }
}
mixin template mixB9235()
{
    int foo(string s)() if (s == "b") { return 2; }
}
struct Foo9235
{
    mixin mixA9235 A;
    mixin mixB9235 B;
    alias A.foo foo;
    alias B.foo foo;
}
void test9235b()
{
    Foo9235 f;
    assert(f.foo!"a"() == 1);
    assert(f.foo!"b"() == 2);
}

/***************************************************/
// 10658

alias Val10658 = imports.template_ovs1.Val10658;
alias Val10658 = imports.template_ovs2.Val10658;
static assert(Val10658!1 == 1);
static assert(Val10658!1L == 2);

// ----

template Foo10658(T) if (is(T == double)) { enum Foo10658 = 1; }
template Bar10658(T) if (is(T == string)) { enum Bar10658 = 2; }
alias Baz10658 = Foo10658;
alias Baz10658 = Bar10658;

template Voo10658(T) if (is(T == cfloat)) { enum Voo10658 = 5; }
template Voo10658(T) if (is(T == Object)) { enum Voo10658 = 6; }

alias Vaz10658 = Baz10658;  // OvarDeclaration
alias Vaz10658 = Voo10658;  // TemplateDeclaration (overnext != NULL)

template Merge10658a(alias A)
{
    enum Merge10658a = A!double + A!string;
}
template Merge10658b(alias A)
{
    enum Merge10658b = A!double + A!string + A!cfloat + A!Object;
}

void test10658a()
{
    static assert(Baz10658!double == 1);
    static assert(Baz10658!string == 2);
    static assert(Voo10658!cfloat == 5);
    static assert(Voo10658!Object == 6);

    // pass OverDeclaration through TemplateAliasParameter
    static assert(Merge10658a!Baz10658 == 1 + 2);
    static assert(Merge10658b!Vaz10658 == 1 + 2 + 5 + 6);
}

// ----

mixin template mix10658A()
{
    int f10658(string s)() if (s == "a") { return 1; }
}
mixin template mix10658B()
{
    int f10658(string s)() if (s == "b") { return 2; }
}
mixin mix10658A A10658;
mixin mix10658B B10658;
alias A10658.f10658 foo10658;
alias B10658.f10658 foo10658;

mixin template mix10658C()
{
    int f10658(string s, T)(T arg) if (s == "c") { return 3; }
}
mixin template mix10658D()
{
    int f10658(string s, T)(T arg) if (s == "d") { return 4; }
}
struct S10658
{
    mixin mix10658C C10658;
    mixin mix10658D D10658;
    alias C10658.f10658 foo10658;
    alias D10658.f10658 foo10658;
}

void test10658b()
{
    assert( foo10658!"a"() == 1);
    assert(.foo10658!"b"() == 2);

    S10658 s;
    assert(s.foo10658!"c"(0) == 3);
    assert(s.foo10658!"d"(0) == 4);
}

/***************************************************/

class InputStream11785
{
    long read(ubyte* bytes, long len)
    {
        return 0;
    }
    void read(T)(ref T val)
    {
        read(cast(ubyte*)&val, cast(long)val.sizeof);
    }
}

long read11785(ubyte* bytes, long len)
{
    return 0;
}
void read11785(T)(ref T val)
{
    read11785(cast(ubyte*)&val, cast(long)val.sizeof);
}

void test11785()
{
    int v;

    read11785(v);

    auto input = new InputStream11785();
    input.read(v);
}

/***************************************************/
// 11915

int f11915(    int) { return 1; }
int f11915(ref int) { return 2; }

int g11915(    int) { return 1; }
int g11915(out int) { return 2; }

void test11915()
{
    const int n = 1;
    assert(f11915(n) == 1);
    assert(g11915(n) == 1);
}

/***************************************************/
// 11916

auto f11916(T)(    T)            { return 1; }
auto f11916(T)(out T) if (false) { return 2; }

auto g11916(T)(    T) { return 1; }
auto g11916(T)(out T) { return 2; }

void test11916()
{
    const int c = 1;
    int m = 2;

    // 'out const int' is invalid function parameter, so (out T) version will be dropped
    // from overload candidates before template constraint evaluated.
    assert(f11916(c) == 1);

    // Both (T) and (out T) have valid signatures with T == int, but the 2nd overload will be
    // dropped from overload candidates because of the template constraint.
    assert(f11916(m) == 1);

    // 'out const int' parameter is invalid, so non-out version is selected.
    assert(g11916(c) == 1);

    // MATCHconst for (T) version, and MATCHexact for (out T) version.
    assert(g11916(m) == 2);
}

/***************************************************/
// 13783

enum E13783 { a = 5 }

    inout(int) f(    inout(int) t) { return t * 2; }
ref inout(int) f(ref inout(int) t) { return t; }

void test13783()
{
    const E13783 e = E13783.a;
    assert(f(e) == 10);
}

/***************************************************/
// 14858

int foo14858()() { return 1; }
int bar14858(int) { return 2; }

alias foobar14858 = foo14858;
alias foobar14858 = bar14858;

void test14858()
{
    assert(foobar14858() == 1);
    assert(foobar14858(1) == 2); // OK <- NG
}

/***************************************************/
// 14989

template Foo14989(T) if (is(T == int))    { enum Foo14989 = 1; }
template Bar14989(T) if (is(T == double)) { enum Bar14989 = 2; }
template Baz14989(T) if (is(T == string)) { enum Baz14989 = 3; }

alias X14989 = Foo14989;
alias X14989 = Bar14989;
// X is an alias to is OverDeclaration
alias A14989 = X14989;
// first, A->aliassym == X
static if (true)
{
    alias A14989 = Baz14989;
    // A->aliassym = new OverDeclaration('A')
    // then, A->aliassym->overloadInsert(Baz)
}

template Mix14989a() { alias M14989 = Foo14989; }
template Mix14989b() { alias M14989 = Bar14989; }
mixin Mix14989a;
mixin Mix14989b;
alias Y14989 = M14989;
// Y is an alias to OverloadSet
alias B14989 = Y14989;
// first, B->aliassym == Y
static if (true)
{
    alias B14989 = Baz14989;
    // (B->aliassym = new OverloadSet('B')
    // then, B->aliassym->overloadInsert(Baz)
}

void test14989()
{
    static assert(X14989!int    == 1);
    static assert(X14989!double == 2);
    static assert(!__traits(compiles, X14989!string));  // Baz is not in X

    static assert(A14989!int    == 1);
    static assert(A14989!double == 2);
    static assert(A14989!string == 3);  // OK <- error

    static assert(Y14989!int    == 1);
    static assert(Y14989!double == 2);
    static assert(!__traits(compiles, Y14989!string));  // Baz is not in Y

    static assert(B14989!int    == 1);
    static assert(B14989!double == 2);
    static assert(B14989!string == 3);  // OK <- error
}

/***************************************************/
// 14965

auto f14965a1() { return f14965a1(123); }
int f14965a1(int x) { return x; }

int f14965a2(int x) { return x; }
auto f14965a2() { return f14965a2(123); }

auto f14965b1() { int function(int) fp = &f14965b1; return fp(123); }
int f14965b1(int x) { return x; }

int f14965b2(int x) { return x; }
auto f14965b2() { int function(int) fp = &f14965b2; return fp(123); }

auto f14965c1() { auto fp = cast(int function(int))&f14965c1; return fp(123); }
int f14965c1(int x) { return x; }

int f14965c2(int x) { return x; }
auto f14965c2() { auto fp = cast(int function(int))&f14965c2; return fp(123); }

int function(int) f14965d1() { return &f14965d1; }
int f14965d1(int n) { return 10 + n; }

int f14965d2(int n) { return 10 + n; }
int function(int) f14965d2() { return &f14965d2; }

class C
{
    auto fa1() { return this.fa1(123); }
    int fa1(int x) { return x; }

    int fa2(int x) { return x; }
    auto fa2() { return this.fa2(123); }

    auto fb1() { int delegate(int) dg = &this.fb1; return dg(123); }
    int fb1(int x) { return x; }

    int fb2(int x) { return x; }
    auto fb2() { int delegate(int) dg = &this.fb2; return dg(123); }

    auto fc1() { auto dg = cast(int delegate(int))&this.fc1; return dg(123); }
    int fc1(int x) { return x; }

    int fc2(int x) { return x; }
    auto fc2() { auto dg = cast(int delegate(int))&this.fc2; return dg(123); }

    int delegate(int) fd1() { return &fd1; }
    int fd1(int n) { return 10 + n; }

    int fd2(int n) { return 10 + n; }
    int delegate(int) fd2() { return &fd2; }
}

void test14965()
{
    assert(f14965a1() == 123);
    assert(f14965b1() == 123);
    assert(f14965c1() == 123);
    assert(f14965d1()(113) == 123);
    assert(f14965a2() == 123);
    assert(f14965b2() == 123);
    assert(f14965c2() == 123);
    assert(f14965d2()(113) == 123);

    auto c = new C();
    assert(c.fa1() == 123);
    assert(c.fb1() == 123);
    assert(c.fc1() == 123);
    assert(c.fd1()(113) == 123);
    assert(c.fa2() == 123);
    assert(c.fb2() == 123);
    assert(c.fc2() == 123);
    assert(c.fd2()(113) == 123);
}

/***************************************************/

int main()
{
    test1528a();
    test1528b();
    test1528c();
    test1528d();
    test1680();
    test7418();
    test7552();
    test8668();
    test8943();
    test9410();
    test10171();
    test1900a();
    test1900b();
    test1900c();
    test1900d();
    test1900e();
    test7780();
    test7849();
    test8352();
    test8441a();
    test8441b();
    test8441c();
    test9235a();
    test9235b();
    test10658a();
    test10658b();
    test11785();
    test11915();
    test11916();
    test13783();
    test14858();
    test14965();

    printf("Success\n");
    return 0;
}
