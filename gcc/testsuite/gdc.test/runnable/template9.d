/*
REQUIRED_ARGS: -preview=rvaluerefparam
PERMUTE_ARGS:
EXTRA_FILES: imports/testmangle.d
TEST_OUTPUT:
---
func
double
All good 1
All good 2
All good 3
_D7imports10testmangle12detectMangleFPSQBlQBg6DetectZQq
_D7imports10testmangle__T10DetectTmplTiZQpFNaNbNiNfZv
true
false
uint
int[]
int[]
const(K5886)
4 ; const(K5886)
8 ; const(K5886)
K5886
immutable(K5886)
4 ; K5886
4 ; immutable(K5886)
1 ; K5886
2 ; const(K5886)
3 ; immutable(K5886)
8 ; K5886
9 ; const(K5886)
10 ; immutable(K5886)
> U = int, N:$?:64=ulong = 3LU|32=uint = 3u$
K=string, V=int
K=char, V=string
T = SA, E = int, dim = $?:64=5LU|32=5u$
T = DA, E = int
T = AA, K = string, V = int
pure nothrow @nogc @safe void(int t)
pure nothrow @nogc @safe void(int t)
T = byte
T = char
---

RUN_OUTPUT:
---
typeof(T)=double typeof(S)=int
typeof(T)=double typeof(S)=int
typeof(T)=float typeof(S)=int
Success
---
*/

module breaker;

import core.stdc.stdio, core.vararg;

/**********************************/

U foo(T, U)(U i)
{
    return i + 1;
}

int foo(T)(int i)
{
    return i + 2;
}

void test1()
{
    auto i = foo!(int)(2L);
//    assert(i == 4);    // now returns 3
}

/**********************************/

U foo2(T, U)(U i)
{
    return i + 1;
}

void test2()
{
    auto i = foo2!(int)(2L);
    assert(i == 3);
}

/**********************************/

class Foo3
{
    T bar(T,U)(U u)
    {
        return cast(T)u;
    }
}

void test3()
{
  Foo3 foo = new Foo3;
  int i = foo.bar!(int)(1.0);
  assert(i == 1);
}


/**********************************/

T* begin4(T)(T[] a) { return a.ptr; }

void copy4(string pred = "", Ranges...)(Ranges rs)
{
    alias rs[$ - 1] target;
    pragma(msg, typeof(target).stringof);
    auto tb = begin4(target);//, te = end(target);
}

void test4()
{
    int[] a, b, c;
    copy4(a, b, c);
    // comment the following line to prevent compiler from crashing
    copy4!("a > 1")(a, b, c);
}

/**********************************/

template foo5(T,S)
{
    void foo5(T t, S s) {
        const tstr = typeid(T).toString();
        const sstr = typeid(S).toString();
        printf("typeof(T)=%.*s typeof(S)=%.*s\n",
               cast(int)tstr.length, tstr.ptr, cast(int)sstr.length, sstr.ptr);
    }
}

template bar5(T,S)
{
    void bar5(S s) {
        const tstr = typeid(T).toString();
        const sstr = typeid(S).toString();
        printf("typeof(T)=%.*s typeof(S)=%.*s\n",
               cast(int)tstr.length, tstr.ptr, cast(int)sstr.length, sstr.ptr);
    }
}


void test5()
{
    foo5(1.0,33);
    bar5!(double,int)(33);
    bar5!(float)(33);
}

/**********************************/

int foo6(T...)(auto ref T x)
{   int result;

    foreach (i, v; x)
    {
        if (v == 10)
            assert(__traits(isRef, x[i]));
        else
            assert(!__traits(isRef, x[i]));
        result += v;
    }
    return result;
}

void test6()
{   int y = 10;
    int r;
    r = foo6(8);
    assert(r == 8);
    r = foo6(y);
    assert(r == 10);
    r = foo6(3, 4, y);
    assert(r == 17);
    r = foo6(4, 5, y);
    assert(r == 19);
    r = foo6(y, 6, y);
    assert(r == 26);
}

/**********************************/

auto ref min(T, U)(auto ref T lhs, auto ref U rhs)
{
    return lhs > rhs ? rhs : lhs;
}

void test7()
{   int x = 7, y = 8;
    int i;

    i = min(4, 3);
    assert(i == 3);
    i = min(x, y);
    assert(i == 7);
    min(x, y) = 10;
    assert(x == 10);
    static assert(!__traits(compiles, min(3, y) = 10));
    static assert(!__traits(compiles, min(y, 3) = 10));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5946

template TTest8()
{
    int call(){ return this.g(); }
}
class CTest8
{
    int f() { mixin TTest8!(); return call(); }
    int g() { return 10; }
}
void test8()
{
    assert((new CTest8()).f() == 10);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=693

template TTest9(alias sym)
{
    int call(){ return sym.g(); }
}
class CTest9
{
    int f1() { mixin TTest9!(this); return call(); }
    int f2() { mixin TTest9!this; return call(); }
    int g() { return 10; }
}
void test9()
{
    assert((new CTest9()).f1() == 10);
    assert((new CTest9()).f2() == 10);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=1780

template Tuple1780(Ts ...) { alias Ts Tuple1780; }

template Decode1780( T )                            { alias Tuple1780!() Types; }
template Decode1780( T : TT!(Us), alias TT, Us... ) { alias Us Types; }

void test1780()
{
    struct S1780(T1, T2) {}

    // should extract tuple (bool,short) but matches the first specialisation
    alias Decode1780!( S1780!(bool,short) ).Types SQ1780;  // --> SQ2 is empty tuple!
    static assert(is(SQ1780 == Tuple1780!(bool, short)));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=1659

class Foo1659 { }
class Bar1659 : Foo1659 { }

void f1659(T : Foo1659)() { }
void f1659(alias T)() { static assert(false); }

void test1659()
{
    f1659!Bar1659();
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=2025

struct S2025 {}
void f2025() {}

template Foo2025(int i) { enum Foo2025 = 1; }
template Foo2025(TL...) { enum Foo2025 = 2; }
static assert(Foo2025!1 == 1);
static assert(Foo2025!int == 2);
static assert(Foo2025!S2025 == 2);
static assert(Foo2025!f2025 == 2);

template Bar2025(T)    { enum Bar2025 = 1; }
template Bar2025(A...) { enum Bar2025 = 2; }
static assert(Bar2025!1 == 2);
static assert(Bar2025!int == 1);    // 2 -> 1
static assert(Bar2025!S2025 == 1);  // 2 -> 1
static assert(Bar2025!f2025 == 2);

template Baz2025(T)       { enum Baz2025 = 1; }
template Baz2025(alias A) { enum Baz2025 = 2; }
static assert(Baz2025!1 == 2);
static assert(Baz2025!int == 1);
static assert(Baz2025!S2025 == 1);  // 2 -> 1
static assert(Baz2025!f2025 == 2);

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=3608

template foo3608(T, U){}

template BaseTemplate3608(alias TTT : U!V, alias U, V...)
{
    alias U BaseTemplate3608;
}
template TemplateParams3608(alias T : U!V, alias U, V...)
{
    alias V TemplateParams3608;
}

template TyueTuple3608(T...) { alias T TyueTuple3608; }

void test3608()
{
    alias foo3608!(int, long) Foo3608;

    static assert(__traits(isSame, BaseTemplate3608!Foo3608, foo3608));
    static assert(is(TemplateParams3608!Foo3608 == TyueTuple3608!(int, long)));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5015

import breaker;

static if (is(ElemType!(int))){}

template ElemType(T) {
  alias _ElemType!(T).type ElemType;
}

template _ElemType(T) {
    alias r type;
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5185

class C5185(V)
{
    void f()
    {
        C5185!(C5185!(int)) c;
    }
}

void test5185()
{
    C5185!(C5185!(int)) c;
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5893

class C5893
{
    int concatAssign(C5893 other) { return 1; }
    int concatAssign(int other) { return 2; } // to demonstrate overloading

    template opOpAssign(string op) if (op == "~")
    { alias concatAssign opOpAssign; }

    int opOpAssign(string op)(int other) if (op == "+") { return 3; }
}

void test5893()
{
    auto c = new C5893;
    assert(c.opOpAssign!"~"(c) == 1); // works
    assert(c.opOpAssign!"~"(1) == 2); // works
    assert((c ~= 1) == 2);
    assert((c += 1) == 3);  // overload
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5988

template Templ5988(alias T)
{
    alias T!int Templ5988;
}

class C5988a(T) { Templ5988!C5988a foo; }
//Templ5988!C5988a foo5988a;    // Commented version
void test5988a() { C5988a!int a; }  // Was error, now works

class C5988b(T) { Templ5988!C5988b foo; }
Templ5988!C5988b foo5988b;      // Uncomment version
void test5988b() { C5988b!int a; }  // Works

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6404

// receive only rvalue
void rvalue(T)(auto ref T x) if (!__traits(isRef, x)) {}
void rvalueVargs(T...)(auto ref T x) if (!__traits(isRef, x[0])) {}

// receive only lvalue
void lvalue(T)(auto ref T x) if ( __traits(isRef, x)) {}
void lvalueVargs(T...)(auto ref T x) if ( __traits(isRef, x[0])) {}

void test6404()
{
    int n;

    static assert(!__traits(compiles, rvalue(n)));
    static assert( __traits(compiles, rvalue(0)));

    static assert( __traits(compiles, lvalue(n)));
    static assert(!__traits(compiles, lvalue(0)));

    static assert(!__traits(compiles, rvalueVargs(n)));
    static assert( __traits(compiles, rvalueVargs(0)));

    static assert( __traits(compiles, lvalueVargs(n)));
    static assert(!__traits(compiles, lvalueVargs(0)));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=2246

class A2246(T,d){
    T p;
}

class B2246(int rk){
    int[rk] p;
}

class C2246(T,int rk){
    T[rk] p;
}

template f2246(T:A2246!(U,d),U,d){
    void f2246(){ }
}

template f2246(T:B2246!(rank),int rank){
    void f2246(){ }
}

template f2246(T:C2246!(U,rank),U,int rank){
    void f2246(){ }
}

void test2246(){
    A2246!(int,long) a;
    B2246!(2) b;
    C2246!(int,2) c;
    f2246!(A2246!(int,long))();
    f2246!(B2246!(2))();
    f2246!(C2246!(int,2))();
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=2296

void foo2296(size_t D)(int[D] i...){}
void test2296()
{
    foo2296(1, 2, 3);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=1684

template Test1684( uint memberOffset ){}

class MyClass1684 {
    int flags2;
    mixin Test1684!(cast(uint)flags2.offsetof) t1; // compiles ok
    mixin Test1684!(cast(int)flags2.offsetof)  t2; // compiles ok
    mixin Test1684!(flags2.offsetof)           t3; // Error: no property 'offsetof' for type 'int'
}

/**********************************/

void bug4984a(int n)() if (n > 0 && is(typeof(bug4984a!(n-1) ()))) {
}

void bug4984a(int n : 0)() {
}

void bug4984b(U...)(U args) if ( is(typeof( bug4984b(args[1..$]) )) ) {
}

void bug4984b(U)(U u) {
}

void bug4984() {
  // Note: compiling this overflows the stack if dmd is build with DEBUG
  //bug4984a!400();
    bug4984a!200();
    bug4984b(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=2579

void foo2579(T)(T delegate(in Object) dlg)
{
}

void test2579()
{
    foo2579( (in Object o) { return 15; } );
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=2803

auto foo2803(T)(T t = 0) { return t; }

struct S2803 {}
S2803 s2803;
ref S2803 getS2803() { return s2803; }
auto fun2803(T, U)(T t, ref U u = getS2803)
{
    static assert(is(U == S2803));
    return &u;
}

// from the past version of std.conv
template to2803(T) { T to2803(S)(S src) { return T.init; } }
auto toImpl2803a(T, S)(S s, in T left, in T sep = ", ", in T right = "]") {}
auto toImpl2803b(T, S)(S s, in T left = to2803!T(S.stringof~"("), in T right = ")") {}
auto toImpl2803c(T, S)(S s, in T left =          S.stringof~"(" , in T right = ")") {}  // combination with enh 13944

// from std.range.package in 2.067a.
auto enumerate2803(En = size_t, R)(R r, En start = 0)
{
    // The type of 'start' should be size_t, it's the defaultArg of En,
    // rather than the deduced type from its defualtArg '0'.
    static assert(is(typeof(start) == size_t));
    return start;
}

// from std.numeric.
alias ElementType2803(R) = typeof(R.init[0].init);
void normalize2803(R)(R range, ElementType2803!R sum = 1)
{
    // The type of 'sum' should be determined to ElementType!(double[]) == double
    // before the type deduction from its defaultArg '1'.
    static assert(is(typeof(sum) == double));
}

auto foo14468(T)(T[]...) { return 1; }
auto foo14468(bool flag, T)(T[]...) { return 2; }

void test2803()
{
    assert(foo2803() == 0);
    assert(foo2803(1) == 1);

    S2803 s;
    assert(fun2803(1)    is &s2803);
    assert(fun2803(1, s) is &s);

    // regression cases

    toImpl2803a!string(1, "[");

    toImpl2803b! string(1);
    toImpl2803b!wstring(1);
    toImpl2803b!dstring(1);

    toImpl2803c! string(1);
    toImpl2803c!wstring(1); // requires enhancement 13944
    toImpl2803c!dstring(1); // requires enhancement 13944

    enumerate2803([1]);

    double[] a = [];
    normalize2803(a);

    assert(foo14468!int() == 1);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6613

alias Tuple6613(T...) = T;

void f6613(T...)(int x = 0, T xs = Tuple6613!())
{
    assert(x == 0);
    static assert(T.length == 0);
}

void test6613()
{
    f6613();
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=4953

void bug4953(T = void)(short x) {}
static assert(is(typeof(bug4953(3))));

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5886
// https://issues.dlang.org/show_bug.cgi?id=5393

mixin template Foo5886(T)
{
    void foo(U : T, this X)() const { static assert(is(X == const K5886)); }
}

struct K5886
{
    void get1(this T)() const
    {
        pragma(msg, T);
    }
    void get2(int N=4, this T)() const
    {
        pragma(msg, N, " ; ", T);
    }

    mixin Foo5886!double;
    mixin Foo5886!string;

    void test() const
    {
        get1;       // OK
        get2;       // OK
        get2!8;     // NG

        foo!(int);
        foo!(typeof(null));
    }
}

void test5886()
{
    K5886 km;
    const(K5886) kc;
    immutable(K5886) ki;

    km.get1;        // OK
    kc.get1;        // OK
    ki.get1;        // OK
    km.get2;        // OK
    kc.get2;        // OK
    ki.get2;        // OK
    km.get2!(1, K5886);             // Ugly
    kc.get2!(2, const(K5886));      // Ugly
    ki.get2!(3, immutable(K5886));  // Ugly
    km.get2!8;      // Error
    kc.get2!9;      // Error
    ki.get2!10;     // Error
}

// --------

void test5393()
{
    class A
    {
        void opDispatch (string name, this T) () { }
    }

    class B : A {}

    auto b = new B;
    b.foobar();
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5896

struct X5896
{
                 T opCast(T)(){ return 1; }
           const T opCast(T)(){ return 2; }
       immutable T opCast(T)(){ return 3; }
          shared T opCast(T)(){ return 4; }
    const shared T opCast(T)(){ return 5; }
}
void test5896()
{
    auto xm =              X5896  ();
    auto xc =        const(X5896) ();
    auto xi =    immutable(X5896) ();
    auto xs =       shared(X5896) ();
    auto xcs= const(shared(X5896))();
    assert(cast(int)xm == 1);
    assert(cast(int)xc == 2);
    assert(cast(int)xi == 3);
    assert(cast(int)xs == 4);
    assert(cast(int)xcs== 5);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6312

void h6312() {}

class Bla6312
{
    mixin wrap6312!h6312;
}

mixin template wrap6312(alias f)
{
    void blub(alias g = f)()
    {
        g();
    }
}

void test6312()
{
    Bla6312 b = new Bla6312();
    b.blub();
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6825

void test6825()
{
    struct File
    {
        void write(S...)(S args) {}
    }

    void dump(void delegate(string) d) {}

    auto o = File();
    dump(&o.write!string);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6789

template isStaticArray6789(T)
{
    static if (is(T U : U[N], size_t N))    // doesn't match
    {
        pragma(msg, "> U = ", U, ", N:", typeof(N), " = ", N);
        enum isStaticArray6789 = true;
    }
    else
        enum isStaticArray6789 = false;
}

void test6789()
{
    alias int[3] T;
    static assert(isStaticArray6789!T);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=2778

struct ArrayWrapper2778(T)
{
    T[] data;
    alias data this;
}

void doStuffFunc2778(int[] data) {}

void doStuffTempl2778(T)(T[] data) {}

int doStuffTemplOver2778(T)(void* data) { return 1; }
int doStuffTemplOver2778(T)(ArrayWrapper2778!T w) { return 2; }

void test2778()
{
    ArrayWrapper2778!(int) foo;

    doStuffFunc2778(foo);  // Works.

    doStuffTempl2778!(int)(foo);  // Works.

    doStuffTempl2778(foo);  // Error

    assert(doStuffTemplOver2778(foo) == 2);
}

// ----

void test2778aa()
{
    void foo(K, V)(V[K] aa){ pragma(msg, "K=", K, ", V=", V); }

    int[string] aa1;
    foo(aa1);   // OK

    struct SubTypeOf(T)
    {
        T val;
        alias val this;
    }
    SubTypeOf!(string[char]) aa2;
    foo(aa2);   // NG
}

// ----

void test2778get()
{
    void foo(ubyte[]){}

    static struct S
    {
        ubyte[] val = [1,2,3];
        @property ref ubyte[] get() return { return val; }
        alias get this;
    }
    S s;
    foo(s);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6208

int getRefNonref(T)(ref T s){ return 1; }
int getRefNonref(T)(    T s){ return 2; }

int getAutoRef(T)(auto ref T s){ return __traits(isRef, s) ? 1 : 2; }

void getOut(T)(out T s){ {} }

void getLazy1(T=int)(lazy void s){ s(), s(); }
void getLazy2(T)(lazy T s){  s(), s(); }

void test6208a()
{
    int lvalue;
    int rvalue(){ int t; return t; }

    assert(getRefNonref(lvalue  ) == 1);
    assert(getRefNonref(rvalue()) == 2);

    assert(getAutoRef(lvalue  ) == 1);
    assert(getAutoRef(rvalue()) == 2);

    static assert( __traits(compiles, getOut(lvalue  )));
    static assert(!__traits(compiles, getOut(rvalue())));

    int n1; getLazy1(++n1); assert(n1 == 2);
    int n2; getLazy2(++n2); assert(n2 == 2);

    struct X
    {
        int f(T)(auto ref T t){ return 1; }
        int f(T)(auto ref T t, ...){ return -1; }
    }
    auto xm =       X ();
    auto xc = const(X)();
    int n;
    assert(xm.f!int(n) == 1);   // resolved 'auto ref'
    assert(xm.f!int(0) == 1);   // ditto
}

void test6208b()
{
    void foo(T)(const T value) if (!is(T == int)) {}

    int mn;
    const int cn;
    static assert(!__traits(compiles, foo(mn)));    // OK -> OK
    static assert(!__traits(compiles, foo(cn)));    // NG -> OK
}

void test6208c()
{
    struct S
    {
        // Original test case.
        int foo(V)(in V v)                         { return 1; }
        int foo(Args...)(auto ref const Args args) { return 2; }

        // Reduced test cases

        int hoo(V)(const V v)             { return 1; }  // typeof(10) : const V       -> MATCHconst
        int hoo(Args...)(const Args args) { return 2; }  // typeof(10) : const Args[0] -> MATCHconst
        // If deduction matching level is same, tuple parameter is less specialized than others.

        int bar(V)(V v)                   { return 1; }  // typeof(10) : V             -> MATCHexact
        int bar(Args...)(const Args args) { return 2; }  // typeof(10) : const Args[0] -> MATCHconst

        int baz(V)(const V v)             { return 1; }  // typeof(10) : const V -> MATCHconst
        int baz(Args...)(Args args)       { return 2; }  // typeof(10) : Args[0] -> MATCHexact

        inout(int) war(V)(inout V v)            { return 1; }
        inout(int) war(Args...)(inout Args args){ return 2; }

        inout(int) waz(Args...)(inout Args args){ return 0; }   // wild deduction test
    }

    S s;

    int nm = 10;
    assert(s.foo(nm) == 1);
    assert(s.hoo(nm) == 1);
    assert(s.bar(nm) == 1);
    assert(s.baz(nm) == 2);
    assert(s.war(nm) == 1);
    static assert(is(typeof(s.waz(nm)) == int));

    const int nc = 10;
    assert(s.foo(nc) == 1);
    assert(s.hoo(nc) == 1);
    assert(s.bar(nc) == 1);
    assert(s.baz(nc) == 1);
    assert(s.war(nc) == 1);
    static assert(is(typeof(s.waz(nc)) == const(int)));

    immutable int ni = 10;
    assert(s.foo(ni) == 1);
    assert(s.hoo(ni) == 1);
    assert(s.bar(ni) == 1);
    assert(s.baz(ni) == 2);
    assert(s.war(ni) == 1);
    static assert(is(typeof(s.waz(ni)) == immutable(int)));

    static assert(is(typeof(s.waz(nm, nm)) == int));
    static assert(is(typeof(s.waz(nm, nc)) == const(int)));
    static assert(is(typeof(s.waz(nm, ni)) == const(int)));
    static assert(is(typeof(s.waz(nc, nm)) == const(int)));
    static assert(is(typeof(s.waz(nc, nc)) == const(int)));
    static assert(is(typeof(s.waz(nc, ni)) == const(int)));
    static assert(is(typeof(s.waz(ni, nm)) == const(int)));
    static assert(is(typeof(s.waz(ni, nc)) == const(int)));
    static assert(is(typeof(s.waz(ni, ni)) == immutable(int)));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6805

struct T6805
{
    template opDispatch(string name)
    {
        alias int Type;
    }
}
static assert(is(T6805.xxx.Type == int));

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6738

struct Foo6738
{
    int _val = 10;

    @property int val()() { return _val; }
    int get() { return val; }  // fail
}

void test6738()
{
    Foo6738 foo;
    auto x = foo.val;  // ok
    assert(x == 10);
    assert(foo.get() == 10);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7498

template IndexMixin(){
    void insert(T)(T value){  }
}

class MultiIndexContainer{
    mixin IndexMixin!() index0;
    class Index0{
        void baburk(){
            this.outer.index0.insert(1);
        }
    }
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6780

@property int foo6780()(){ return 10; }

int g6780;
@property void bar6780()(int n){ g6780 = n; }

void test6780()
{
    auto n = foo6780;
    assert(n == 10);

    bar6780 = 10;
    assert(g6780 == 10);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6810

int f6810(int n)(int) { return 1;}
int f6810(U...)(U)    { assert(0); }
int f6810(U...)(U a)  { assert(0); }
int f6810(U...)(U)   if (true) { assert(0); }
int f6810(U...)(U a) if (true) { assert(0); }

void test6810()
{
    assert(f6810!0(0) == 1);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6891

struct S6891(int N, T)
{
    void f(U)(S6891!(N, U) u) { }
}

void test6891()
{
    alias S6891!(1, void) A;
    A().f(A());
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6994

struct Foo6994
{
    T get(T)(){ return T.init; }

    T func1(T)()
    if (__traits(compiles, get!T()))
    { return get!T; }

    T func2(T)()
    if (__traits(compiles, this.get!T()))   // add explicit 'this'
    { return get!T; }
}
void test6994()
{
    Foo6994 foo;
    foo.get!int();      // OK
    foo.func1!int();    // OK
    foo.func2!int();    // NG
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6764

enum N6764 = 1; //use const for D1

alias size_t[N6764] T6764; //workaround
void f6764()(T6764 arr...) { }

void g6764()(size_t[1] arr...) { }

void h6764()(size_t[N6764] arr...) { }

void test6764()
{
    f6764(0);    //good
    g6764(0);    //good
    h6764!()(0); //good
    h6764(0);    //Error: template main.f() does not match any function template declaration
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=3467
// https://issues.dlang.org/show_bug.cgi?id=6806

struct Foo3467( uint n )
{
    Foo3467!( n ) bar( ) {
        typeof( return ) result;
        return result;
    }
}
struct Vec3467(size_t N)
{
    void opBinary(string op:"~", size_t M)(Vec3467!M) {}
}
void test3467()
{
    Foo3467!( 4 ) baz;
    baz = baz.bar;// FAIL

    Vec3467!2 a1;
    Vec3467!3 a2;
    a1 ~ a2; // line 7, Error
}

struct TS6806(uint n) { pragma(msg, typeof(n)); }
static assert(is(TS6806!(1u) == TS6806!(1)));

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=4413

struct Foo4413
{
    alias typeof(this) typeof_this;
    void bar1(typeof_this other) {}
    void bar2()(typeof_this other) {}
    void bar3(typeof(this) other) {}
    void bar4()(typeof(this) other) {}
}

void test4413()
{
    Foo4413 f;
    f.bar1(f); // OK
    f.bar2(f); // OK
    f.bar3(f); // OK
    f.bar4(f); // ERR
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=4675

template isNumeric(T)
{
    enum bool test1 = is(T : long);     // should be hidden
    enum bool test2 = is(T : real);     // should be hidden
    enum bool isNumeric = test1 || test2;
}
void test4675()
{
    static assert( isNumeric!int);
    static assert(!isNumeric!string);
    static assert(!__traits(compiles, isNumeric!int.test1));   // should be an error
    static assert(!__traits(compiles, isNumeric!int.test2));   // should be an error
    static assert(!__traits(compiles, isNumeric!int.isNumeric));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5525

template foo5525(T)
{
    T foo5525(T t)      { return t; }
    T foo5525(T t, T u) { return t + u; }
}

void test5525()
{
    alias foo5525!int f;
    assert(f(1) == 1);
    assert(f(1, 2) == 3);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5801

int a5801;
void bar5801(T = double)(typeof(a5801) i) {}
void baz5801(T)(typeof(a5801) i, T t) {}
void test5801()
{
    bar5801(2);  // Does not compile.
    baz5801(3, "baz"); // Does not compile.
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=5832

struct Bar5832(alias v) {}

template isBar5832a(T)
{
    static if (is(T _ : Bar5832!(v), alias v))
        enum isBar5832a = true;
    else
        enum isBar5832a = false;
}
template isBar5832b(T)
{
    static if (is(T _ : Bar5832!(v), alias int v))
        enum isBar5832b = true;
    else
        enum isBar5832b = false;
}
template isBar5832c(T)
{
    static if (is(T _ : Bar5832!(v), alias string v))
        enum isBar5832c = true;
    else
        enum isBar5832c = false;
}
static assert( isBar5832a!(Bar5832!1234));
static assert( isBar5832b!(Bar5832!1234));
static assert(!isBar5832c!(Bar5832!1234));

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=2550

template pow10_2550(long n)
{
    const long pow10_2550 = 0;
    static if (n < 0)
        const long pow10_2550 = 0;
    else
        const long pow10_2550 = 10 * pow10_2550!(n - 1);
}
template pow10_2550(long n:0)
{
    const long pow10_2550 = 1;
}
static assert(pow10_2550!(0) == 1);

/**********************************/
// [2.057] Remove top const in IFTI, 9198

void foo10a(T   )(T)            { static assert(is(T    == const(int)[])); }
void foo10b(T...)(T)            { static assert(is(T[0] == const(int)[])); }

// ref parameter doesn't remove top const
void boo10a(T   )(ref T)        { static assert(is(T    == const(int[]))); }
void boo10b(T...)(ref T)        { static assert(is(T[0] == const(int[]))); }

// auto ref with lvalue doesn't
void goo10a(T   )(auto ref T)   { static assert(is(T    == const(int[]))); }
void goo10b(T...)(auto ref T)   { static assert(is(T[0] == const(int[]))); }

// auto ref with rvalue does
void hoo10a(T   )(auto ref T)   { static assert(is(T    == const(int)[])); }
void hoo10b(T...)(auto ref T)   { static assert(is(T[0] == const(int)[])); }

void bar10a(T   )(T)            { static assert(is(T    == const(int)*)); }
void bar10b(T...)(T)            { static assert(is(T[0] == const(int)*)); }

void test10()
{
    const a = [1,2,3];
    static assert(is(typeof(a) == const(int[])));
    foo10a(a);
    foo10b(a);
    boo10a(a);
    boo10b(a);
    goo10a(a);
    goo10b(a);
    hoo10a(cast(const)[1,2,3]);
    hoo10b(cast(const)[1,2,3]);

    int n;
    const p = &n;
    static assert(is(typeof(p) == const(int*)));
    bar10a(p);
    bar10b(p);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=3092

template Foo3092(A...)
{
    alias A[0] Foo3092;
}
static assert(is(Foo3092!(int, "foo") == int));

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7037

struct Foo7037 {}
struct Bar7037 { Foo7037 f; alias f this; }
void works7037( T )( T value ) if ( is( T : Foo7037 ) ) {}
void doesnotwork7037( T : Foo7037 )( T value ) {}

void test7037()
{
   Bar7037 b;
   works7037( b );
   doesnotwork7037( b );
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7110

struct S7110
{
    int opSlice(int, int) const { return 0; }
    int opSlice()         const { return 0; }
    int opIndex(int, int) const { return 0; }
    int opIndex(int)      const { return 0; }
}

enum e7110 = S7110();

template T7110(alias a) { } // or T7110(a...)

alias T7110!( S7110 ) T71100; // passes
alias T7110!((S7110)) T71101; // passes

alias T7110!( S7110()[0..0]  )  A0; // passes
alias T7110!(  (e7110[0..0]) )  A1; // passes
alias T7110!(   e7110[0..0]  )  A2; // passes

alias T7110!( S7110()[0, 0]  ) B0; // passes
alias T7110!(  (e7110[0, 0]) ) B1; // passes
alias T7110!(   e7110[0, 0]  ) B2; // passes

alias T7110!( S7110()[]  ) C0; // passes
alias T7110!(  (e7110[]) ) C1; // passes
alias T7110!(   e7110[]  ) C2; // fails: e7110 is used as a type

alias T7110!( S7110()[0]  ) D0; // passes
alias T7110!(  (e7110[0]) ) D1; // passes
alias T7110!(   e7110[0]  ) D2; // fails: e7110 must be an array or pointer type, not S7110

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7124

template StaticArrayOf(T : E[dim], E, size_t dim)
{
    pragma(msg, "T = ", T, ", E = ", E, ", dim = ", dim);
    alias E[dim] StaticArrayOf;
}

template DynamicArrayOf(T : E[], E)
{
    pragma(msg, "T = ", T, ", E = ", E);
    alias E[] DynamicArrayOf;
}

template AssocArrayOf(T : V[K], K, V)
{
    pragma(msg, "T = ", T, ", K = ", K, ", V = ", V);
    alias V[K] AssocArrayOf;
}
void test7124()
{
    struct SA { int[5] sa; alias sa this; }
    static assert(is(StaticArrayOf!SA == int[5]));

    struct DA { int[] da; alias da this; }
    static assert(is(DynamicArrayOf!DA == int[]));

    struct AA { int[string] aa; alias aa this; }
    static assert(is(AssocArrayOf!AA == int[string]));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7359

bool foo7359(T)(T[] a ...)
{
    return true;
}

void test7359()
{
    assert(foo7359(1,1,1,1,1,1));               // OK
    assert(foo7359("abc","abc","abc","abc"));   // NG
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7363

template t7363()
{
   enum e = 0;
   static if (true)
       enum t7363 = 0;
}
static assert(!__traits(compiles, t7363!().t7363 == 0)); // Assertion fails
static assert(t7363!() == 0); // Error: void has no value

template u7363()
{
   static if (true)
   {
       enum e = 0;
       enum u73631 = 0;
   }
   alias u73631 u7363;
}
static assert(!__traits(compiles, u7363!().u7363 == 0)); // Assertion fails
static assert(u7363!() == 0); // Error: void has no value

/**********************************/

struct S4371(T ...) { }

alias S4371!("hi!") t;

static if (is(t U == S4371!(U))) { }

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7416

void t7416(alias a)() if(is(typeof(a())))
{}

void test7416() {
    void f() {}
    alias t7416!f x;
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7563

class Test7563
{
    void test(T, bool a = true)(T t)
    {

    }
}

void test7563()
{
    auto test = new Test7563;
    pragma(msg, typeof(test.test!(int, true)).stringof);
    pragma(msg, typeof(test.test!(int)).stringof); // Error: expression (test.test!(int)) has no type
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7572

class F7572
{
    Tr fn7572(Tr, T...)(T t) { return 1; }
}
Tr Fn7572(Tr, T...)(T t) { return 2; }

void test7572()
{
    F7572 f = new F7572();
    int delegate() dg = &f.fn7572!int;
    assert(dg() == 1);

    int function() fn = &Fn7572!int;
    assert(fn() == 2);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7580

struct S7580(T)
{
    void opAssign()(T value) {}
}
struct X7580(T)
{
    private T val;
    @property ref inout(T) get()() inout { return val; }    // template
    alias get this;
}
struct Y7580(T)
{
    private T val;
    @property ref auto get()() inout { return val; }        // template + auto return
    alias get this;
}

void test7580()
{
    S7580!(int) s;
    X7580!int x;
    Y7580!int y;
    s = x;
    s = y;

    shared(X7580!int) sx;
    static assert(!__traits(compiles, s = sx));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7585

extern(C) alias void function() Callback;

template W7585a(alias dg)
{
    //pragma(msg, typeof(dg));
    extern(C) void W7585a() { dg(); }
}

void test7585()
{
    static void f7585a(){}
    Callback cb1 = &W7585a!(f7585a);      // OK
    static assert(!__traits(compiles,
    {
        void f7585b(){}
        Callback cb2 = &W7585a!(f7585b);  // NG
    }));

    Callback cb3 = &W7585a!((){});              // NG -> OK
    Callback cb4 = &W7585a!(function(){});      // OK
    static assert(!__traits(compiles,
    {
        Callback cb5 = &W7585a!(delegate(){});  // NG
    }));

    static int global;  // global data
    Callback cb6 = &W7585a!((){return global;});    // NG -> OK
    static assert(!__traits(compiles,
    {
        int n;
        Callback cb7 = &W7585a!((){return n;});     // NG
    }));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7643

template T7643(A...){ alias A T7643; }

alias T7643!(long, "x", string, "y") Specs7643;

alias T7643!( Specs7643[] ) U7643;  // Error: tuple A is used as a type

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7671

       inout(int)[3]  id7671n1             ( inout(int)[3] );
       inout( U )[n]  id7671x1(U, size_t n)( inout( U )[n] );

shared(inout int)[3]  id7671n2             ( shared(inout int)[3] );
shared(inout  U )[n]  id7671x2(U, size_t n)( shared(inout  U )[n] );

void test7671()
{
    static assert(is( typeof( id7671n1( (immutable(int)[3]).init ) ) == immutable(int[3]) ));
    static assert(is( typeof( id7671x1( (immutable(int)[3]).init ) ) == immutable(int[3]) ));

    static assert(is( typeof( id7671n2( (immutable(int)[3]).init ) ) == immutable(int[3]) ));
    static assert(is( typeof( id7671x2( (immutable(int)[3]).init ) ) == immutable(int[3]) ));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=7672

T foo7672(T)(T a){ return a; }

void test7672(inout(int[]) a = null, inout(int*) p = null)
{
    static assert(is( typeof(        a ) == inout(int[]) ));
    static assert(is( typeof(foo7672(a)) == inout(int)[] ));

    static assert(is( typeof(        p ) == inout(int*) ));
    static assert(is( typeof(foo7672(p)) == inout(int)* ));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7684

       U[]  id7684(U)(        U[]  );
shared(U[]) id7684(U)( shared(U[]) );

void test7684()
{
    shared(int)[] x;
    static assert(is( typeof(id7684(x)) == shared(int)[] ));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7694

void match7694(alias m)()
{
    m.foo();    //removing this line suppresses ice in both cases
}

struct T7694
{
    void foo(){}
    void bootstrap()
    {
    //next line causes ice
        match7694!(this)();
    //while this works:
        alias this p;
        match7694!(p)();
    }
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7755

template to7755(T)
{
    T to7755(A...)(A args)
    {
        return toImpl7755!T(args);
    }
}

T toImpl7755(T, S)(S value)
{
    return T.init;
}

template Foo7755(T){}

struct Bar7755
{
    void qux()
    {
        if (is(typeof(to7755!string(Foo7755!int)))){}
    }
}

/**********************************/

             U[]   id11a(U)(              U[]   );
       inout(U)[]  id11a(U)(        inout(U)[]  );
       inout(U[])  id11a(U)(        inout(U[])  );
inout(shared(U[])) id11a(U)( inout(shared(U[])) );

void test11a(inout int _ = 0)
{
    shared(const(int))[] x;
    static assert(is( typeof(id11a(x)) == shared(const(int))[] ));

    shared(int)[] y;
    static assert(is( typeof(id11a(y)) == shared(int)[] ));

    inout(U)[n] idz(U, size_t n)( inout(U)[n] );

    inout(shared(bool[1])) z;
    static assert(is( typeof(idz(z)) == inout(shared(bool[1])) ));
}

inout(U[]) id11b(U)( inout(U[]) );

void test11b()
{
    alias const(shared(int)[]) T;
    static assert(is(typeof(id11b(T.init)) == const(shared(int)[])));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7769

void f7769(K)(inout(K) value){}
void test7769()
{
    f7769("abc");
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7812

template A7812(T...) {}

template B7812(alias C) if (C) {}

template D7812()
{
    alias B7812!(A7812!(NonExistent!())) D7812;
}

static assert(!__traits(compiles, D7812!()));

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7873

inout(T)* foo(T)(inout(T)* t)
{
    static assert(is(T == int*));
    return t;
}

inout(T)* bar(T)(inout(T)* t)
{
    return foo(t);
}

void test7873()
{
    int *i;
    bar(&i);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=7933

struct Boo7933(size_t dim){int a;}
struct Baa7933(size_t dim)
{
    Boo7933!dim a;
    //Boo7933!1 a; //(1) This version causes no errors
}

auto foo7933()(Boo7933!1 b){return b;}
//auto fuu7933(Boo7933!1 b){return b;} //(2) This line neutralizes the error

void test7933()
{
    Baa7933!1 a; //(3) This line causes the error message
    auto b = foo7933(Boo7933!1(1));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=8094

struct Tuple8094(T...) {}

template getParameters8094(T, alias P)
{
    static if (is(T t == P!U, U...))
        alias U getParameters8094;
    else
        static assert(false);
}

void test8094()
{
    alias getParameters8094!(Tuple8094!(int, string), Tuple8094) args;
}

/**********************************/

struct Tuple12(T...)
{
    void foo(alias P)()
    {
        alias Tuple12 X;
        static if (is(typeof(this) t == X!U, U...))
            alias U getParameters;
        else
            static assert(false);
    }
}

void test12()
{
    Tuple12!(int, string) t;
    t.foo!Tuple12();
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=14290

struct Foo14290(int i) {}
alias Foo14290a = Foo14290!1;
static assert(!is(Foo14290!2 == Foo14290a!T, T...));

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=8125

void foo8125(){}

struct X8125(alias a) {}

template Y8125a(T : A!f, alias A, alias f) {}  //OK
template Y8125b(T : A!foo8125, alias A) {}     //NG

void test8125()
{
    alias Y8125a!(X8125!foo8125) y1;
    alias Y8125b!(X8125!foo8125) y2;
}

/**********************************/

struct A13() {}
struct B13(TT...) {}
struct C13(T1) {}
struct D13(T1, TT...) {}
struct E13(T1, T2) {}
struct F13(T1, T2, TT...) {}

template Test13(alias X)
{
    static if (is(X x : P!U, alias P, U...))
        enum Test13 = true;
    else
        enum Test13 = false;
}

void test13()
{
    static assert(Test13!( A13!() ));
    static assert(Test13!( B13!(int) ));
    static assert(Test13!( B13!(int, double) ));
    static assert(Test13!( B13!(int, double, string) ));
    static assert(Test13!( C13!(int) ));
    static assert(Test13!( D13!(int) ));
    static assert(Test13!( D13!(int, double) ));
    static assert(Test13!( D13!(int, double, string) ));
    static assert(Test13!( E13!(int, double) ));
    static assert(Test13!( F13!(int, double) ));
    static assert(Test13!( F13!(int, double, string) ));
    static assert(Test13!( F13!(int, double, string, bool) ));
}

/**********************************/

struct A14(T, U, int n = 1)
{
}

template Test14(alias X)
{
    static if (is(X x : P!U, alias P, U...))
        alias U Test14;
    else
        static assert(0);
}

void test14()
{
    alias A14!(int, double) Type;
    alias Test14!Type Params;
    static assert(Params.length == 3);
    static assert(is(Params[0] == int));
    static assert(is(Params[1] == double));
    static assert(   Params[2] == 1);
}

/**********************************/
// test for evaluateConstraint assertion

bool canSearchInCodeUnits15(C)(dchar c)
if (is(C == char))
{
    return true;
}

void test15()
{
    int needle = 0;
    auto b = canSearchInCodeUnits15!char(needle);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=8129

class X8129 {}
class A8129 {}
class B8129 : A8129 {}

int foo8129(T : A8129)(X8129 x) { return 1; }
int foo8129(T : A8129)(X8129 x, void function (T) block) { return 2; }

int bar8129(T, R)(R range, T value) { return 1; }

int baz8129(T, R)(R range, T value) { return 1; }
int baz8129(T, R)(R range, Undefined value) { return 2; }

void test8129()
{
    auto x = new X8129;
    assert(x.foo8129!B8129()      == 1);
    assert(x.foo8129!B8129((a){}) == 2);
    assert(foo8129!B8129(x)        == 1);
    assert(foo8129!B8129(x, (a){}) == 2);
    assert(foo8129!B8129(x)              == 1);
    assert(foo8129!B8129(x, (B8129 b){}) == 2);

    ubyte[] buffer = [0, 1, 2];
    assert(bar8129!ushort(buffer, 915) == 1);

    // While deduction, parameter type 'Undefined' shows semantic error.
    static assert(!__traits(compiles, {
        baz8129!ushort(buffer, 915);
    }));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=8238

void test8238()
{
    static struct S { template t(){ int t; } }

    S s1, s2;
    assert(cast(void*)&s1      != cast(void*)&s2     );
    assert(cast(void*)&s1      != cast(void*)&s1.t!());
    assert(cast(void*)&s2      != cast(void*)&s2.t!());
    assert(cast(void*)&s1.t!() == cast(void*)&s2.t!());
    s1.t!() = 256;
    assert(s2.t!() == 256);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=8669

struct X8669
{
    void mfoo(this T)()
    {
        static assert(is(typeof(this) == T));
    }
    void cfoo(this T)() const
    {
        static assert(is(typeof(this) == const(T)));
    }
    void sfoo(this T)() shared
    {
        static assert(is(typeof(this) == shared(T)));
    }
    void scfoo(this T)() shared const
    {
        static assert(is(typeof(this) == shared(const(T))));
    }
    void ifoo(this T)() immutable
    {
        static assert(is(typeof(this) == immutable(T)));
    }
}

void test8669()
{
                 X8669 mx;
           const X8669 cx;
      immutable  X8669 ix;
          shared X8669 sx;
    shared const X8669 scx;

     mx.mfoo();
     cx.mfoo();
     ix.mfoo();
     sx.mfoo();
    scx.mfoo();

     mx.cfoo();
     cx.cfoo();
     ix.cfoo();
     sx.cfoo();
    scx.cfoo();

    static assert(!is(typeof(  mx.sfoo() )));
    static assert(!is(typeof(  cx.sfoo() )));
     ix.sfoo();
     sx.sfoo();
    scx.sfoo();

    static assert(!is(typeof(  mx.scfoo() )));
    static assert(!is(typeof(  cx.scfoo() )));
     ix.scfoo();
     sx.scfoo();
    scx.scfoo();

    static assert(!is(typeof(  mx.ifoo() )));
    static assert(!is(typeof(  cx.ifoo() )));
     ix.ifoo();
    static assert(!is(typeof(  sx.ifoo() )));
    static assert(!is(typeof( scx.ifoo() )));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=8833

template TypeTuple8833(T...) { alias TypeTuple = T; }

void func8833(alias arg)() { }

void test8833()
{
    int x, y;

    alias TypeTuple8833!(
        func8833!(x),
        func8833!(y),
    ) Map;
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=8976

void f8976(ref int) { }

void g8976()()
{
    f8976(0); // line 5
}


void h8976()()
{
    g8976!()();
}

static assert( __traits(compiles, h8976!()() ) ); // causes error
static assert(is(typeof(          h8976!()() )));

void test8976()
{
    static assert( __traits(compiles, h8976!()() ) );
    static assert(is(typeof(          h8976!()() )));
}

/****************************************/
// https://issues.dlang.org/show_bug.cgi?id=8940

const int n8940; // or `immutable`
static this() { n8940 = 3; }

void f8940(T)(ref int val)
{
    assert(val == 3);
    ++val;
}

static assert(!__traits(compiles,  f8940!void(n8940))); // fails
void test8940()
{
    assert(n8940 == 3);
    static assert(!__traits(compiles, f8940!void(n8940)));
    //assert(n8940 == 3); // may pass as compiler caches comparison result
    //assert(n8940 != 4); // may pass but likely will fail
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=6969
// https://issues.dlang.org/show_bug.cgi?id=8990

class A6969() { alias C6969!() C1; }
class B6969   { alias A6969!() A1; }
class C6969() : B6969 {}

struct A8990(T) { T t; }
struct B8990(T) { A8990!T* a; }
struct C8990    { B8990!C8990* b; }

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9018

template Inst9018(alias Template, T)
{
    alias Template!T Inst;
}

template Template9018(T)
{
    enum Template9018 = T;
}

static assert(!__traits(compiles, Inst9018!(Template9018, int))); // Assert passes
static assert(!__traits(compiles, Inst9018!(Template9018, int))); // Assert fails

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9022

class C9022
{
    struct X {}

    alias B = X;
}
class D9022
{
    struct X {}
}

void test9022()
{
    auto c = new C9022();
    auto d = new D9022();
    auto cx = C9022.X();
    auto dx = D9022.X();

    void foo1(T)(T, T.X) { static assert(is(T == C9022)); }
    void foo2(T)(T.X, T) { static assert(is(T == C9022)); }
    foo1(c, cx);
    foo2(cx, c);

    void hoo1(T)(T, T.B) { static assert(is(T == C9022)); }
    void hoo2(T)(T.B, T) { static assert(is(T == C9022)); }
    hoo1(c, cx);
    hoo1(c, cx);

    void bar1(alias A)(A.C9022, A.D9022) { static assert(A.stringof == "module breaker"); }
    void bar2(alias A)(A.D9022, A.C9022) { static assert(A.stringof == "module breaker"); }
    bar1(c, d);
    bar2(d, c);

    void var1(alias A)(A.C9022, A.D9022.X) { static assert(A.stringof == "module breaker"); }
    void var2(alias A)(A.D9022.X, A.C9022) { static assert(A.stringof == "module breaker"); }
    var1(c, dx);
    var2(dx, c);

    void baz(T)(T.X t, T.X u) { }
    static assert(!__traits(compiles, baz(cx, dx)));
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9026

mixin template node9026()
{
    static if (is(this == struct))
        alias typeof(this)* E;
    else
        alias typeof(this) E;
    E prev, next;
}

struct list9026(alias N)
{
    N.E head;
    N.E tail;
}

class A9026
{
    mixin node9026 L1;
    mixin node9026 L2;
}

list9026!(A9026.L1) g9026_l1;
list9026!(A9026.L2) g9026_l2;

void test9026()
{
    list9026!(A9026.L1) l9026_l1;
    list9026!(A9026.L2) l9026_l2;
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9038

mixin template Foo9038()
{
    string data = "default";
}

class Bar9038
{
    string data;
    mixin Foo9038 f;
}

void check_data9038(alias M, T)(T obj)
{
    //writeln(M.stringof);
    assert(obj.data == "Bar");
    assert(obj.f.data == "F");
}

void test9038()
{
    auto bar = new Bar9038;
    bar.data = "Bar";
    bar.f.data = "F";

    assert(bar.data == "Bar");
    assert(bar.f.data == "F");

    check_data9038!(Bar9038)(bar);
    check_data9038!(Bar9038.f)(bar);
    check_data9038!(bar.f)(bar);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9050

struct A9050(T) {}

struct B9050(T)
{
    void f() { foo9050(A9050!int()); }
}

auto foo9050()(A9050!int base) pure
{
    return B9050!int();
}

auto s9050 = foo9050(A9050!int());

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=10936 (dup of 9050)

struct Vec10936(string s)
{
    auto foo(string v)()
    {
        return Vec10936!(v)();
    }

    static void bar()
    {
        Vec10936!"" v;
        auto p = v.foo!"sup";
    }
}

Vec10936!"" v;

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9076

template forward9076(args...)
{
    @property forward9076()(){ return args[0]; }
}

void test9076()
{
    int a = 1;
    int b = 1;
    assert(a == forward9076!b);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9083

template isFunction9083(X...) if (X.length == 1)
{
    enum isFunction9083 = true;
}

struct S9083
{
    static string func(alias Class)()
    {
        foreach (m; __traits(allMembers, Class))
        {
            pragma(msg, m);  // prints "func"
            enum x1 = isFunction9083!(mixin(m));  //NG
            enum x2 = isFunction9083!(func);      //OK
        }
        return "";
    }
}
enum nothing9083 = S9083.func!S9083();

class C9083
{
    int x;  // some class members

    void func()
    {
        void templateFunc(T)(const T obj)
        {
            enum x1 = isFunction9083!(mixin("x"));  // NG
            enum x2 = isFunction9083!(x);           // NG
        }
        templateFunc(this);
    }
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9100

template Id(alias A) { alias Id = A; }
template ErrId(alias A) { static assert(0); }
template TypeTuple9100(TL...) { alias TypeTuple9100 = TL; }

class C9100
{
    int value;

    int fun() { return value; }
    int tfun(T)() { return value; }
    TypeTuple9100!(int, long) field;

    void test()
    {
        this.value = 1;
        auto c = new C9100();
        c.value = 2;

        alias t1a = Id!(c.fun);             // OK
        alias t1b = Id!(this.fun);          // Prints weird error, bad
        // -> internally given TOKdotvar
        assert(t1a() == this.value);
        assert(t1b() == this.value);

        alias t2a = Id!(c.tfun);            // OK
        static assert(!__traits(compiles, ErrId!(this.tfun)));
        alias t2b = Id!(this.tfun);         // No error occurs, why?
        // -> internally given TOKdottd
        assert(t2a!int() == this.value);
        assert(t2b!int() == this.value);

        alias t3a = Id!(foo9100);           // OK
        alias t3b = Id!(mixin("foo9100"));  // Prints weird error, bad
        // -> internally given TOKtemplate
        assert(t3a() == 10);
        assert(t3b() == 10);

        assert(field[0] == 0);
        alias t4a = TypeTuple9100!(field);              // NG
        alias t4b = TypeTuple9100!(GetField9100!());    // NG
        t4a[0] = 1; assert(field[0] == 1);
        t4b[0] = 2; assert(field[0] == 2);
    }
}

int foo9100()() { return 10; }
template GetField9100() { alias GetField9100 = C9100.field[0]; }

void test9100()
{
    (new C9100()).test();
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9101

class Node9101
{
    template ForwardCtorNoId()
    {
        this() {} // default constructor
        void foo() { 0 = 1; }    // wrong code
    }
}
enum x9101 = __traits(compiles, Node9101.ForwardCtorNoId!());

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9124

struct Foo9124a(N...)
{
    enum SIZE = N[0];
    private int _val;

    public void opAssign (T) (T other)
    if (is(T unused == Foo9124a!(_N), _N...))
    {
        _val = other._val;          // compile error
        this._val = other._val;     // explicit this make it work
    }

    public auto opUnary (string op) () if (op == "~") {
        Foo9124a!(SIZE) result = this;
        return result;
    }
}
void test9124a()
{
    Foo9124a!(28) a;
    Foo9124a!(28) b = ~a;
}

// --------

template Foo9124b(T, U, string OP)
{
    enum N = T.SIZE;
    alias Foo9124b = Foo9124b!(false, true, N);
}
struct Foo9124b(bool S, bool L, N...)
{
    enum SIZE = 5;
    long[1] _a = 0;
    void someFunction() const {
        auto data1 = _a;        // Does not compile
        auto data2 = this._a;   // <--- Compiles
    }
    auto opBinary(string op, T)(T) {
        Foo9124b!(typeof(this), T, op) test;
    }
}
void test9124b()
{
    auto p = Foo9124b!(false, false, 5)();
    auto q = Foo9124b!(false, false, 5)();
    p|q;
    p&q;
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9143

struct Foo9143a(bool S, bool L)
{
    auto noCall() {
        Foo9143a!(S, false) x1;         // compiles if this line commented
        static if(S) Foo9143a!(true,  false) x2;
        else         Foo9143a!(false, false) x2;
    }
    this(T)(T other)        // constructor
    if (is(T unused == Foo9143a!(P, Q), bool P, bool Q)) { }
}

struct Foo9143b(bool L, size_t N)
{
    void baaz0() {
        bar!(Foo9143b!(false, N))();    // line 7
        // -> move to before the baaz semantic
    }
    void baaz() {
        bar!(Foo9143b!(false, 2LU))();  // line 3
        bar!(Foo9143b!(true, 2LU))();   // line 4
        bar!(Foo9143b!(L, N))();        // line 5
        bar!(Foo9143b!(true, N))();     // line 6
        bar!(Foo9143b!(false, N))();    // line 7
    }
    void bar(T)()
    if (is(T unused == Foo9143b!(_L, _N), bool _L, size_t _N))
    {}
}

void test9143()
{
    Foo9143a!(false, true) k = Foo9143a!(false, false)();

    auto p = Foo9143b!(true, 2LU)();
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9266

template Foo9266(T...)
{
    T Foo9266;
}
struct Bar9266()
{
    alias Foo9266!int f;
}
void test9266()
{
    Bar9266!() a, b;
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9361

struct Unit9361(A)
{
    void butPleaseDontUseMe()()
    if (is(unitType9361!((this))))  // !
    {}

}
template isUnit9361(alias T) if ( is(T)) {}
template isUnit9361(alias T) if (!is(T)) {}

template unitType9361(alias T) if (isUnit9361!T) {}

void test9361()
{
    Unit9361!int u;
    static assert(!__traits(compiles, u.butPleaseDontUseMe())); // crashes
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9536

struct S9536
{
    static A foo(A)(A a)
    {
        return a * 2;
    }
    int bar() const
    {
        return foo(42);
    }
}

void test9536()
{
    S9536 s;
    assert(s.bar() == 84);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9578

template t9578(alias f) { void tf()() { f(); } }

void g9578a(alias f)()  { f(); }        // Error -> OK
void g9578b(alias ti)() { ti.tf(); }    // Error -> OK

void test9578()
{
    int i = 0;
    int m() { return i; }

    g9578a!(t9578!m.tf)();
    g9578b!(t9578!m)();
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=9596

int foo9596a(K, V)(inout(       V  [K])) { return 1; }
int foo9596a(K, V)(inout(shared(V) [K])) { return 2; }

int foo9596b(K, V)(inout(       V  [K])) { return 1; }
int foo9596b(K, V)(inout( const(V) [K])) { return 3; }

int foo9596c(K, V)(inout(shared(V) [K])) { return 2; }
int foo9596c(K, V)(inout( const(V) [K])) { return 3; }

int foo9596d(K, V)(inout(       V  [K])) { return 1; }
int foo9596d(K, V)(inout(shared(V) [K])) { return 2; }
int foo9596d(K, V)(inout( const(V) [K])) { return 3; }

int foo9596e(K, V)(inout(shared(V) [K])) { return 2; }
int foo9596e(K, V)(inout(       V  [K])) { return 1; }
int foo9596e(K, V)(inout( const(V) [K])) { return 3; }

void test9596()
{
    shared(int)[int] aa;
    static assert(!__traits(compiles, foo9596a(aa)));

    assert(foo9596b(aa) == 1);
    assert(foo9596c(aa) == 2);

    static assert(!__traits(compiles, foo9596d(aa)));
    static assert(!__traits(compiles, foo9596e(aa)));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9806

struct S9806a(alias x)
{
    alias S9806a!0 N;
}
enum expr9806a = 0 * 0;
alias S9806a!expr9806a T9806a;

// --------

struct S9806b(alias x)
{
    template Next()
    {
        enum expr = x + 1;
        alias S9806b!expr Next;
    }
}
alias S9806b!1 One9806b;
alias S9806b!0.Next!() OneAgain9806b;

// --------

struct S9806c(x...)
{
    template Next()
    {
        enum expr = x[0] + 1;
        alias S9806c!expr Next;
    }
}
alias S9806c!1 One9806c;
alias S9806c!0.Next!() OneAgain9806c;

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9837

void test9837()
{
    enum DA : int[] { a = [1,2,3] }
    DA da;
    int[] bda = da;
    static assert(is(DA : int[]));
    void fda1(int[] a) {}
    void fda2(T)(T[] a) {}
    fda1(da);
    fda2(da);

    enum SA : int[3] { a = [1,2,3] }
    SA sa;
    int[3] bsa = sa;
    static assert(is(SA : int[3]));
    void fsa1(int[3] a) {}
    void fsa2(T)(T[3] a) {}
    void fsa3(size_t d)(int[d] a) {}
    void fsa4(T, size_t d)(T[d] a) {}
    fsa1(sa);
    fsa2(sa);
    fsa3(sa);
    fsa4(sa);

    enum AA : int[int] { a = null }
    AA aa;
    int[int] baa = aa;
    static assert(is(AA : int[int]));
    void faa1(int[int] a) {}
    void faa2(V)(V[int] a) {}
    void faa3(K)(int[K] a) {}
    void faa4(K, V)(V[K] a) {}
    faa1(aa);
    faa2(aa);
    faa3(aa);
    faa4(aa);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9874

bool foo9874() { return true; }
void bar9874(T)(T) if (foo9874()) {} // OK
void baz9874(T)(T) if (foo9874)   {} // error

void test9874()
{
    foo9874;                      // OK
    bar9874(0);
    baz9874(0);
}

/******************************************/

void test9885()
{
    void foo(int[1][]) {}
    void boo()(int[1][]){}
    struct X(T...) { static void xoo(T){} }
    struct Y(T...) { static void yoo()(T){} }
    struct Z(T...) { static void zoo(U...)(T, U){} }

    struct V(T...) { static void voo()(T, ...){} }
    struct W(T...) { static void woo()(T...){} }

    struct R(T...) { static void roo(U...)(int, U, T){} }

    // OK
    foo([[10]]);
    boo([[10]]);

    // OK
    X!(int[1][]).xoo([[10]]);

    // NG!
    Y!().yoo();
    Y!(int).yoo(1);
    Y!(int, int[]).yoo(1, [10]);
    static assert(!__traits(compiles, Y!().yoo(1)));
    static assert(!__traits(compiles, Y!(int).yoo("a")));
    static assert(!__traits(compiles, Y!().yoo!(int)()));

    // NG!
    Z!().zoo();
    Z!().zoo([1], [1:1]);
    Z!(int, string).zoo(1, "a");
    Z!(int, string).zoo(1, "a", [1], [1:1]);
    Z!().zoo!()();
    static assert(!__traits(compiles, Z!().zoo!()(1)));     // (none) <- 1
    static assert(!__traits(compiles, Z!(int).zoo!()()));   // int <- (none)
    static assert(!__traits(compiles, Z!(int).zoo!()(""))); // int <- ""
    static assert(!__traits(compiles, Z!().zoo!(int)()));   // int <- (none)
    static assert(!__traits(compiles, Z!().zoo!(int)(""))); // int <- ""

    V!().voo(1,2,3);
    V!(int).voo(1,2,3);
    V!(int, long).voo(1,2,3);
    static assert(!__traits(compiles, V!(int).voo()));          // int <- (none)
    static assert(!__traits(compiles, V!(int, long).voo(1)));       // long <- (none)
    static assert(!__traits(compiles, V!(int, string).voo(1,2,3)));     // string <- 2

    W!().woo();
    //W!().woo(1, 2, 3);    // Access Violation
    {   // this behavior is consistent with:
        //alias TL = TypeTuple!();
        //void foo(TL...) {}
        //foo(1, 2, 3);     // Access Violation
        //pragma(msg, typeof(foo));   // void(...)  -> D-style variadic function?
    }
    W!(int,int[]).woo(1,2,3);
    W!(int,int[2]).woo(1,2,3);
    static assert(!__traits(compiles, W!(int,int,int).woo(1,2,3)));     // int... <- 2
    static assert(!__traits(compiles, W!(int,int).woo(1,2)));           // int... <- 2
    static assert(!__traits(compiles, W!(int,int[2]).woo(1,2)));    // int[2]... <- 2

    R!().roo(1, "", []);
    R!(int).roo(1, "", [], 1);
    R!(int, string).roo(1, "", [], 1, "");
    R!(int, string).roo(1, 2, "");
    static assert(!__traits(compiles, R!(int).roo(1, "", []))); // int <- []
    static assert(!__traits(compiles, R!(int, int).roo(1, "", [])));    // int <- []
    static assert(!__traits(compiles, R!(int, string).roo(1, 2, 3)));   // string <- 3

    // test case
    struct Tuple(T...) { this()(T values) {} }
    alias T = Tuple!(int[1][]);
    auto t = T([[10]]);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9971

void goo9971()()
{
    auto g = &goo9971;
}

struct S9971
{
    void goo()()
    {
        auto g = &goo;
        static assert(is(typeof(g) == delegate));
    }
}

void test9971()
{
    goo9971!()();

    S9971.init.goo!()();
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9977

void test9977()
{
    struct S1(T) { T value; }
    auto func1(T)(T value) { return value; }
    static assert(is(S1!int == struct));
    assert(func1(10) == 10);

    template S2(T) { struct S2 { T value; } }
    template func2(T) { auto func2(T value) { return value; } }
    static assert(is(S2!int == struct));
    assert(func2(10) == 10);

    template X(T) { alias X = T[3]; }
    static assert(is(X!int == int[3]));

    int a;
    template Y(T) { alias Y = T[typeof(a)]; }
    static assert(is(Y!double == double[int]));

    int v = 10;
    template Z() { alias Z = v; }
    assert(v == 10);
    Z!() = 20;
    assert(v == 20);
}

/******************************************/

enum T8848a(int[] a) = a;
enum T8848b(int[int] b) = b;
enum T8848c(void* c) = c;

static assert(T8848a!([1,2,3]) == [1,2,3]);
static assert(T8848b!([1:2,3:4]) == [1:2,3:4]);
static assert(T8848c!(null) == null);

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9990

auto initS9990() { return "hi"; }

class C9990(alias init) {}

alias SC9990 = C9990!(initS9990);

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10067

struct assumeSize10067(alias F) {}

template useItemAt10067(size_t idx, T)
{
    void impl(){ }

    alias useItemAt10067 = assumeSize10067!(impl);
}

useItemAt10067!(0, char) mapS10067;

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=4072

void bug4072(T)(T x)
    if (is(typeof(bug4072(x))))
{}

static assert(!is(typeof(bug4072(7))));

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10074

template foo10074(F)
{
    enum foo10074 = false;
}
bool foo10074(F)(F f)
    if (foo10074!F)
{
    return false;
}

static assert(!is(typeof(foo10074(1))));

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10083

// [a-c] IFTI can find syntactic eponymous member
template foo10083a(T)
{
    int foo10083a(double) { return 1; }
    int foo10083a(T) { return 2; }
}
template foo10083b(T)
{
    int foo10083b(T) { return 1; }
    int foo10083b(T, T) { return 2; }
}
template foo10083c1(T)
{
    int foo10083c1(T) { return 1; }
    static if (true) { int x; }
}
template foo10083c2(T)
{
    int foo10083c2(T) { return 1; }
    static if (true) { int x; } else { int y; }
}

// [d-f] IFTI cannot find syntactic eponymous member
template foo10083d1(T)
{
    static if (true)
    {
        int foo10083d1(T) { return 1; }
    }
    else
    {
    }
}
template foo10083d2(T)
{
    static if (true)
    {
    }
    else
    {
        int foo10083d2(T) { return 1; }
    }
}
template foo10083e(T)
{
    static if (true)
    {
        int foo10083e(double arg) { return 1; }
    }
    int foo10083e(T arg) { return 2; }
}
template foo10083f(T)
{
    static if (true)
    {
        int foo10083f(T) { return 1; }
    }
    else
    {
        int foo10083f(T) { return 2; }
    }
}

void test10083()
{
    assert(foo10083a(1) == 2);
    assert(foo10083a!int(1) == 2);
    assert(foo10083a!int(1.0) == 1);
    static assert(!__traits(compiles, foo10083a!double(1)));
    static assert(!__traits(compiles, foo10083a!double(1.0)));
    static assert(!__traits(compiles, foo10083a!real(1)));
    assert(foo10083a!real(1.0) == 1);
    assert(foo10083a!real(1.0L) == 2);

    assert(foo10083b(2) == 1);
    assert(foo10083b(3, 4) == 2);
    static assert(!__traits(compiles, foo10083b(2, "")));

    assert(foo10083c1(1) == 1);
    assert(foo10083c2(1) == 1);

    static assert(!__traits(compiles, foo10083d1(2)));
    static assert(!__traits(compiles, foo10083d2(2)));
    static assert(!__traits(compiles, foo10083e(3)));
    static assert(!__traits(compiles, foo10083f(3)));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10134

template ReturnType10134(alias func)
{
    static if (is(typeof(func) R == return))
        alias R ReturnType10134;
    else
        static assert(0);
}

struct Result10134(T) {}

template getResultType10134(alias func)
{
    static if(is(ReturnType10134!(func.exec) _ == Result10134!(T), T))
    {
        alias getResultType10134 = T;
    }
}

template f10134(alias func)
{
    Result10134!(getResultType10134!(func)) exec(int i)
    {
        return typeof(return)();
    }
}

template a10134()
{
    Result10134!(double) exec(int i)
    {
        return b10134!().exec(i);
    }
}

template b10134()
{
    Result10134!(double) exec(int i)
    {
        return f10134!(a10134!()).exec(i);
    }
}

pragma(msg, getResultType10134!(a10134!()));

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10313

void test10313()
{
    struct Nullable(T)
    {
        this()(inout T value) inout {}
    }

    struct S { S[] array; }
    S s;
    auto ns = Nullable!S(s);

    class C { C[] array; }
    C c;
    auto nc = Nullable!C(c);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10498

template triggerIssue10498a()
{
    enum triggerIssue10498a = __traits(compiles, { T10498a; });
}

template PackedGenericTuple10498a(Args...)
{
    alias Args Tuple;
    enum e = triggerIssue10498a!();
}

struct S10498a { }

template T10498a()
{
    alias PackedGenericTuple10498a!S10498a T10498a;
}

void test10498a()
{
    alias T10498a!() t;
    static assert(is(t.Tuple[0])); // Fails -> OK
}

// --------

template triggerIssue10498b(A...)
{
    enum triggerIssue10498b = __traits(compiles, { auto a = A[0]; });
}

template PackedGenericTuple10498b(Args...)
{
    alias Args Tuple;
    enum e = triggerIssue10498b!Args;
}

template T10498b()
{
    struct S {} // The fact `S` is in `T` causes the problem
    alias PackedGenericTuple10498b!S T10498b;
}

void test10498b()
{
    alias T10498b!() t;
    static assert(is(t.Tuple[0]));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10537

struct Iota10537
{
    int s,e,i;
    mixin Yield10537!q{ ; };
}

auto skipStrings10537(T)(T source)
{
    return "";
}

mixin template Yield10537(dstring code)
{
    alias X = typeof({ enum x = rewriteCode10537(code); }());
}

dstring rewriteCode10537(dstring code)
{
    skipStrings10537(code);  // IFTI causes forward reference
    return "";
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10558

template Template10558() {}

struct Struct10558(alias T){}

alias bar10558 = foo10558!(Template10558!());

template foo10558(alias T)
{
    alias foobar = Struct10558!T;

    void fun()
    {
        alias a = foo10558!T;
    }
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10592

void test10592()
{
    struct A(E)
    {
        int put()(const(E)[] data)
        {
            return 1;
        }

        int put()(const(dchar)[] data) if (!is(E == dchar))
        {
            return 2;
        }

        int put(C)(const(C)[] data) if (!is(C == dchar) && !is(E == C))
        {
            return 3;
        }
    }

    A!char x;
    assert(x.put("abcde"c) == 1);   // OK: hit 1
    assert(x.put("abcde"w) == 3);   // NG: this should hit 3
    assert(x.put("abcde"d) == 2);   // OK: hit 2
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11242

inout(T[]) fromString11242(T)(inout(char[]) s, T[] dst)
{
    return s;
}

void test11242()
{
    char[] a;
    fromString11242(a, a);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10811

void foo10811a(R1, R2)(R1, R2) {}
template foo10811a(alias pred) { void foo10811a(R1, R2)(R1, R2) {} }

template foo10811b(alias pred) { void foo10811b(R1, R2)(R1, R2) {} }
void foo10811b(R1, R2)(R1, R2) {}

void test10811()
{
    foo10811a(1, 2);
    foo10811a!(a => a)(1, 2);

    foo10811b(1, 2);
    foo10811b!(a => a)(1, 2);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10969

template A10969(T, U...) { alias A10969 = T; }
void foo10969(T, U...)(A10969!(T, U) a) {}

template B10969(T, U) { alias B10969 = T; }
void bar10969(T, U...)(B10969!(T, U[0]) a) {}

void test10969()
{
    foo10969!(int, float)(3);
    bar10969!(int, float)(3);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11271

struct SmartPtr11271(T)
{
    ~this() {}
    void opAssign(U)(auto ref U rh) {}
}

void test11271()
{
    SmartPtr11271!Object a;
    a = SmartPtr11271!Object();
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11533

version (none)
{
struct S11533
{
    void put(alias fun)() { fun!int(); }
}
void test11533a()
{
    static void foo(T)() {}
    S11533 s;
    s.put!foo();
}

void test11533b()
{
    static void bar(alias fun)() { fun(); }
    void nested() {}
    bar!nested();
}

void test11533c()
{
    static struct Foo(alias fun)
    {
        auto call() { return fun(); }
    }
    int var = 1;
    auto getVar() { return var; }
    Foo!getVar foo;
    assert(foo.call() == var);
    var += 1;
    assert(foo.call() == var);
}

void test11533()
{
    test11533a();
    test11533b();
    test11533c();
}
}
else
{
void test11533()
{
}
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11553

struct Pack11553(T ...)
{
    alias Unpack = T;
    enum length = T.length;
}

template isPack11553(TList ...)
{
    static if (TList.length == 1 && is(Pack11553!(TList[0].Unpack) == TList[0]))
    {
        enum isPack11553 = true;
    }
    else
    {
        enum isPack11553 = false;
    }
}

template PartialApply11553(alias T, uint argLoc, Arg ...)
    if (Arg.length == 1)
{
    template PartialApply11553(L ...)
    {
        alias PartialApply11553 = T!(L[0 .. argLoc], Arg, L[argLoc .. $]);
    }
}

template _hasLength11553(size_t len, T)
{
    static if (T.length == len)
    {
        enum _hasLength11553 = true;
    }
    else
    {
        enum _hasLength11553 = false;
    }
}

alias _hasLength11553(size_t len) = PartialApply11553!(._hasLength11553, 0, len);


alias hl11553 = _hasLength11553!1;

// this segfaults
static if (!isPack11553!hl11553) { pragma(msg, "All good 1"); }

// these are fine
static if ( hl11553!(Pack11553!(5))) { pragma(msg, "All good 2"); }

static if (!hl11553!(Pack11553!( ))) { pragma(msg, "All good 3"); }

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11818

enum E11818 { e0, e1 }

struct SortedRange11818
{
    void fun(E11818 e = true ? E11818.e0 : E11818.e1)()
    {
    }
}

void test11818()
{
    SortedRange11818 s;
    s.fun();
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11843

void test11843()
{
    struct Foo
    {
        int[string] x;
    }

    struct Bar(alias foo) {}

    enum bar1 = Bar!(Foo(["a": 1]))();
    enum bar2 = Bar!(Foo(["a": 1]))();
    static assert(is(typeof(bar1) == typeof(bar2)));

    enum foo1 = Foo(["a": 1]);
    enum foo2 = Foo(["b": -1]);
    static assert(!__traits(isSame, foo1, foo2));
    enum bar3 = Bar!foo1();
    enum bar4 = Bar!foo2();
    static assert(!is(typeof(bar3) == typeof(bar4)));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11872

class Foo11872
{
    auto test(int v)() {}
    auto test(int v)(string) {}

    template Bar(T)
    {
        void test(T) {}
    }
}

void test11872()
{
    auto foo = new Foo11872();

    with (foo)
    {
        // ScopeExp(ti) -> DotTemplateInstanceExp(wthis, ti)
        foo.test!2();   // works
        test!2();       // works <- fails
        test!2;         // works <- fails

        // ScopeExp(ti) -> DotTemplateInstanceExp(wthis, ti) -> DotExp(wthis, ScopeExp)
        foo.Bar!int.test(1);    // works
        Bar!int.test(1);        // works <- fails
    }
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12042

struct S12042
{
    int[] t;

    void m()()
    {
        t = null;   // CTFE error -> OK
    }
}

int test12042()
{
    S12042 s;

    with (s)
        m!()();

    return 1;
}

static assert(test12042());

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12077

struct S12077(A) {}

alias T12077(alias T : Base!Args, alias Base, Args...) = Base;
static assert(__traits(isSame, T12077!(S12077!int), S12077));

alias U12077(alias T : Base!Args, alias Base, Args...) = Base;
alias U12077(      T : Base!Args, alias Base, Args...) = Base;
static assert(__traits(isSame, U12077!(S12077!int), S12077));

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12262

template Inst12262(T) { int x; }

enum fqnSym12262(alias a)                      = 1;
enum fqnSym12262(alias a : B!A, alias B, A...) = 2;

static assert(fqnSym12262!(Inst12262!(Object)) == 2);
static assert(fqnSym12262!(Inst12262!(Object).x) == 1);

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12264

struct S12264(A) {}

template AX12264(alias A1)                      { enum AX12264 = 1; }
template AX12264(alias A2 : B!A, alias B, A...) { enum AX12264 = 2; }
template AY12264(alias A1)                  { enum AY12264 = 1; }
template AY12264(alias A2 : B!int, alias B) { enum AY12264 = 2; }
template AZ12264(alias A1)               { enum AZ12264 = 1; }
template AZ12264(alias A2 : S12264!T, T) { enum AZ12264 = 2; }
static assert(AX12264!(S12264!int) == 2);
static assert(AY12264!(S12264!int) == 2);
static assert(AZ12264!(S12264!int) == 2);

template TX12264(T1)                      { enum TX12264 = 1; }
template TX12264(T2 : B!A, alias B, A...) { enum TX12264 = 2; }
template TY12264(T1)                  { enum TY12264 = 1; }
template TY12264(T2 : B!int, alias B) { enum TY12264 = 2; }
template TZ12264(T1)               { enum TZ12264 = 1; }
template TZ12264(T2 : S12264!T, T) { enum TZ12264 = 2; }
static assert(TX12264!(S12264!int) == 2);
static assert(TY12264!(S12264!int) == 2);
static assert(TZ12264!(S12264!int) == 2);

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12122

enum N12122 = 1;

void foo12122(T)(T[N12122]) if(is(T == int)) {}

void test12122()
{
    int[N12122] data;
    foo12122(data);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12186

template map_front12186(fun...)
{
    auto map_front12186(Range)(Range r)
    {
        return fun[0](r[0]);
    }
}

void test12186()
{
    immutable int[][] mat;

    mat.map_front12186!((in r) => 0);              // OK
    mat.map_front12186!((const r) => 0);           // OK
    mat.map_front12186!((immutable int[] r) => 0); // OK
    mat.map_front12186!((immutable r) => 0);       // OK <- Error
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12207

void test12207()
{
    static struct S
    {
        static void f(T)(T) {}
    }

    immutable S s;

    s.f(1);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12263

template A12263(alias a) { int x; }
template B12263(alias a) { int x; }

template fqnSym12263(alias T : B12263!A, alias B12263, A...)
{
    enum fqnSym12263 = true;
}

static assert(fqnSym12263!(A12263!(Object)));
static assert(fqnSym12263!(B12263!(Object)));

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12290

void test12290()
{
    short[] arrS;
    float[] arrF;
    double[] arrD;
    real[] arrR;
    string cstr;
    wstring wstr;
    dstring dstr;
    short[short] aa;

    auto func1a(E)(E[], E) { return E.init; }
    auto func1b(E)(E, E[]) { return E.init; }

    static assert(is(typeof(func1a(arrS, 1)) == short));
    static assert(is(typeof(func1b(1, arrS)) == short));
    static assert(is(typeof(func1a(arrF, 1.0)) == float));
    static assert(is(typeof(func1b(1.0, arrF)) == float));
    static assert(is(typeof(func1a(arrD, 1.0L)) == double));
    static assert(is(typeof(func1b(1.0L, arrD)) == double));
    static assert(is(typeof(func1a(arrR, 1)) == real));
    static assert(is(typeof(func1b(1, arrR)) == real));
    static assert(is(typeof(func1a("str" , 'a')) == immutable  char));
    static assert(is(typeof(func1b('a', "str" )) == immutable  char));
    static assert(is(typeof(func1a("str"c, 'a')) == immutable  char));
    static assert(is(typeof(func1b('a', "str"c)) == immutable  char));
    static assert(is(typeof(func1a("str"w, 'a')) == immutable wchar));
    static assert(is(typeof(func1b('a', "str"w)) == immutable wchar));
    static assert(is(typeof(func1a("str"d, 'a')) == immutable dchar));
    static assert(is(typeof(func1b('a', "str"d)) == immutable dchar));
    static assert(is(typeof(func1a([1,2,3], 1L)) == long));
    static assert(is(typeof(func1b(1L, [1,2,3])) == long));
    static assert(is(typeof(func1a([1,2,3], 1.5)) == double));
    static assert(is(typeof(func1b(1.5, [1,2,3])) == double));
    static assert(is(typeof(func1a(["a","b"], "s"c)) ==  string));
    static assert(is(typeof(func1b("s"c, ["a","b"])) ==  string));
    static assert(is(typeof(func1a(["a","b"], "s"w)) == wstring));
    static assert(is(typeof(func1b("s"w, ["a","b"])) == wstring));
    static assert(is(typeof(func1a(["a","b"], "s"d)) == dstring));
    static assert(is(typeof(func1b("s"d, ["a","b"])) == dstring));

    auto func2a(K, V)(V[K], K, V) { return V[K].init; }
    auto func2b(K, V)(V, K, V[K]) { return V[K].init; }

    static assert(is(typeof(func2a(aa, 1, 1)) == short[short]));
    static assert(is(typeof(func2b(1, 1, aa)) == short[short]));
    static assert(is(typeof(func2a([1:10,2:20,3:30], 1L, 10L)) == long[long]));
    static assert(is(typeof(func2b(1L, 10L, [1:20,2:20,3:30])) == long[long]));

    auto func3a(T)(T, T) { return T.init; }
    auto func3b(T)(T, T) { return T.init; }

    static assert(is(typeof(func3a(arrS, null)) == short[]));
    static assert(is(typeof(func3b(null, arrS)) == short[]));
    static assert(is(typeof(func3a(arrR, null)) == real[]));
    static assert(is(typeof(func3b(null, arrR)) == real[]));
    static assert(is(typeof(func3a(cstr, "str")) ==  string));
    static assert(is(typeof(func3b("str", cstr)) ==  string));
    static assert(is(typeof(func3a(wstr, "str")) == wstring));
    static assert(is(typeof(func3b("str", wstr)) == wstring));
    static assert(is(typeof(func3a(dstr, "str")) == dstring));
    static assert(is(typeof(func3b("str", dstr)) == dstring));
    static assert(is(typeof(func3a("str1" , "str2"c)) ==  string));
    static assert(is(typeof(func3b("str1"c, "str2" )) ==  string));
    static assert(is(typeof(func3a("str1" , "str2"w)) == wstring));
    static assert(is(typeof(func3b("str1"w, "str2" )) == wstring));
    static assert(is(typeof(func3a("str1" , "str2"d)) == dstring));
    static assert(is(typeof(func3b("str1"d, "str2" )) == dstring));

    inout(V) get(K, V)(inout(V[K]) aa, K key, lazy V defaultValue) { return V.init; }

    short[short] hash12220;
    short res12220 = get(hash12220, 1, 1);

    short[short] hash12221;
    enum Key12221 : short { a }
    get(hash12221, Key12221.a, Key12221.a);

    int[][string] mapping13026;
    int[] v = get(mapping13026, "test", []);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12292

void test12292()
{
    void fun(T : string)(T data) {}

    ubyte[3] sa;
    static assert(!__traits(compiles, fun(sa)));
    static assert(!__traits(compiles, { alias f = fun!(ubyte[3]); }));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12376

static auto encode12376(size_t sz)(dchar ch) if (sz > 1)
{
    undefined;
}

void test12376()
{
    enum x = __traits(compiles, encode12376!2(x));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12447

enum   test12447(string str) = str; // [1]
string test12447(T...)(T args) if (T.length) { return args[0]; }    // [2]

// With [1]: The template parameter str cannot be be deduced -> no match
// With [2]: T is deduced to a type tuple (string), then match to the function call.
static assert(test12447("foo") == "foo");

// With [1]: template parameter str is deduced to "bar", then match.
// With [2]: T is deduced to an expression tuple ("bar"), but it will make invalid the function signature (T args).
//           The failure should be masked silently and prefer the 1st version.
static assert(test12447!("bar") == "bar");

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12651

alias TemplateArgsOf12651(alias T : Base!Args, alias Base, Args...) = Args;

struct S12651(T) { }

static assert(!__traits(compiles, TemplateArgsOf12651!(S12651!int, S, float)));

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12719

struct A12719
{
    B12719!int b();
}

struct B12719(T)
{
    A12719 a;
    void m()
    {
        auto v = B12719!T.init;
    }
}

// --------

enum canDoIt12719(R) = is(typeof(W12719!R));

struct W12719(R)
{
    R r;
    static if (canDoIt12719!R) {}
}

W12719!int a12719;

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12746

template foo12746()
{
    void bar()
    {
        static assert(!__traits(compiles, bar(1)));
    }
    alias foo12746 = bar;
}

void foo12746(int)
{
    assert(0);
}

void test12746()
{
    foo12746(); // instantiate
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12748

void foo12748(S, C : typeof(S.init[0]))(S s, C c)
{
}

void test12748()
{
    foo12748("abc", 'd');
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9708

struct S9708
{
    void f()(inout(Object)) inout {}
}

void test9708()
{
    S9708 s;
    s.f(new Object);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12880

void f12880(T)(in T value) { static assert(is(T == string)); }
void test12880() { f12880(string.init); }

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13087

struct Vec13087
{
    int x;
    void m()                      { auto n = component13087!(this, 'x'); }
    void c() const                { auto n = component13087!(this, 'x'); }
    void w() inout                { auto n = component13087!(this, 'x'); }
    void wc() inout const         { auto n = component13087!(this, 'x'); }
    void s() shared               { auto n = component13087!(this, 'x'); }
    void sc() shared const        { auto n = component13087!(this, 'x'); }
    void sw() shared inout        { auto n = component13087!(this, 'x'); }
    void swc() shared inout const { auto n = component13087!(this, 'x'); }
    void i() immutable            { auto n = component13087!(this, 'x'); }
}

template component13087(alias vec, char c)
{
    alias component13087 = vec.x;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13127

/+void test13127(inout int = 0)
{
                       int []   ma1;
                 const(int)[]   ca1;
                 const(int[])   ca2;
           inout(      int)[]  wma1;
           inout(      int[])  wma2;
           inout(const int)[]  wca1;
           inout(const int[])  wca2;
             immutable(int)[]   ia1;
             immutable(int[])   ia2;
    shared(            int)[]  sma1;
    shared(            int[])  sma2;
    shared(      const int)[]  sca1;
    shared(      const int[])  sca2;
    shared(inout       int)[] swma1;
    shared(inout       int[]) swma2;
    shared(inout const int)[] swca1;
    shared(inout const int[]) swca2;

    /* In all cases, U should be deduced to top-unqualified type.
     */

    /* Parameter is (shared) mutable
     */
    U f_m(U)(       U) { return null; }
    U fsm(U)(shared U) { return null; }
    // 9 * 2 - 1
    static assert(is(typeof(f_m(  ma1))  ==                    int []));
    static assert(is(typeof(f_m(  ca1))  ==              const(int)[]));
    static assert(is(typeof(f_m(  ca2))  ==              const(int)[]));
    static assert(is(typeof(f_m( wma1))  ==        inout(      int)[]));
    static assert(is(typeof(f_m( wma2))  ==        inout(      int)[]));
    static assert(is(typeof(f_m( wca1))  ==        inout(const int)[]));
    static assert(is(typeof(f_m( wca2))  ==        inout(const int)[]));
    static assert(is(typeof(f_m(  ia1))  ==          immutable(int)[]));
    static assert(is(typeof(f_m(  ia2))  ==          immutable(int)[]));
    static assert(is(typeof(f_m( sma1))  == shared(            int)[]));
    static assert(is(typeof(f_m( sma2))  == shared(            int)[]));  // <- shared(int[])
    static assert(is(typeof(f_m( sca1))  == shared(      const int)[]));
    static assert(is(typeof(f_m( sca2))  == shared(      const int)[]));  // <- shared(const(int)[])
    static assert(is(typeof(f_m(swma1))  == shared(inout       int)[]));
    static assert(is(typeof(f_m(swma2))  == shared(inout       int)[]));  // <- shared(inout(int[]))
    static assert(is(typeof(f_m(swca1))  == shared(inout const int)[]));
    static assert(is(typeof(f_m(swca2))  == shared(inout const int)[]));  // <- shared(inout(const(int))[])
    // 9 * 2 - 1
    static assert(is(typeof(fsm(  ma1))) == false);
    static assert(is(typeof(fsm(  ca1))) == false);
    static assert(is(typeof(fsm(  ca2))) == false);
    static assert(is(typeof(fsm( wma1))) == false);
    static assert(is(typeof(fsm( wma2))) == false);
    static assert(is(typeof(fsm( wca1))) == false);
    static assert(is(typeof(fsm( wca2))) == false);
    static assert(is(typeof(fsm(  ia1))) == false);
    static assert(is(typeof(fsm(  ia2))) == false);
    static assert(is(typeof(fsm( sma1))  == shared(            int)[]));  // <- NG
    static assert(is(typeof(fsm( sma2))  == shared(            int)[]));
    static assert(is(typeof(fsm( sca1))  == shared(      const int)[]));  // <- NG
    static assert(is(typeof(fsm( sca2))  == shared(      const int)[]));
    static assert(is(typeof(fsm(swma1))  == shared(inout       int)[]));  // <- NG
    static assert(is(typeof(fsm(swma2))  == shared(inout       int)[]));
    static assert(is(typeof(fsm(swca1))  == shared(inout const int)[]));  // <- NG
    static assert(is(typeof(fsm(swca2))  == shared(inout const int)[]));

    /* Parameter is (shared) const
     */
    U f_c(U)(       const U) { return null; }
    U fsc(U)(shared const U) { return null; }
    // 9 * 2 - 1
    static assert(is(typeof(f_c(  ma1))  ==                    int []));
    static assert(is(typeof(f_c(  ca1))  ==              const(int)[]));
    static assert(is(typeof(f_c(  ca2))  ==              const(int)[]));
    static assert(is(typeof(f_c( wma1))  ==        inout(      int)[]));
    static assert(is(typeof(f_c( wma2))  ==        inout(      int)[]));
    static assert(is(typeof(f_c( wca1))  ==        inout(const int)[]));
    static assert(is(typeof(f_c( wca2))  ==        inout(const int)[]));
    static assert(is(typeof(f_c(  ia1))  ==          immutable(int)[]));
    static assert(is(typeof(f_c(  ia2))  ==          immutable(int)[]));
    static assert(is(typeof(f_c( sma1))  == shared(            int)[]));
    static assert(is(typeof(f_c( sma2))  == shared(            int)[]));  // <- shared(int[])
    static assert(is(typeof(f_c( sca1))  == shared(      const int)[]));
    static assert(is(typeof(f_c( sca2))  == shared(      const int)[]));  // <- shared(const(int)[])
    static assert(is(typeof(f_c(swma1))  == shared(inout       int)[]));
    static assert(is(typeof(f_c(swma2))  == shared(inout       int)[]));  // shared(inout(int)[])
    static assert(is(typeof(f_c(swca1))  == shared(inout const int)[]));
    static assert(is(typeof(f_c(swca2))  == shared(inout const int)[]));  // shared(inout(const(int))[])
    // 9 * 2 - 1
    static assert(is(typeof(fsc(  ma1))) == false);
    static assert(is(typeof(fsc(  ca1))) == false);
    static assert(is(typeof(fsc(  ca2))) == false);
    static assert(is(typeof(fsc( wma1))) == false);
    static assert(is(typeof(fsc( wma2))) == false);
    static assert(is(typeof(fsc( wca1))) == false);
    static assert(is(typeof(fsc( wca2))) == false);
    static assert(is(typeof(fsc(  ia1))  ==          immutable(int)[]));  // <- NG
    static assert(is(typeof(fsc(  ia2))  ==          immutable(int)[]));  // <- NG
    static assert(is(typeof(fsc( sma1))  == shared(            int)[]));  // <- NG
    static assert(is(typeof(fsc( sma2))  == shared(            int)[]));
    static assert(is(typeof(fsc( sca1))  == shared(      const int)[]));  // <- NG
    static assert(is(typeof(fsc( sca2))  == shared(      const int)[]));
    static assert(is(typeof(fsc(swma1))  == shared(inout       int)[]));  // <- NG
    static assert(is(typeof(fsc(swma2))  == shared(inout       int)[]));
    static assert(is(typeof(fsc(swca1))  == shared(inout const int)[]));  // <- NG
    static assert(is(typeof(fsc(swca2))  == shared(inout const int)[]));

    /* Parameter is immutable
     */
    U fi(U)(immutable U) { return null; }
    // 9 * 2 - 1
    static assert(is(typeof(fi(  ma1))) == false);
    static assert(is(typeof(fi(  ca1))) == false);
    static assert(is(typeof(fi(  ca2))) == false);
    static assert(is(typeof(fi( wma1))) == false);
    static assert(is(typeof(fi( wma2))) == false);
    static assert(is(typeof(fi( wca1))) == false);
    static assert(is(typeof(fi( wca2))) == false);
    static assert(is(typeof(fi(  ia1))  == immutable(int)[]));  // <- NG
    static assert(is(typeof(fi(  ia2))  == immutable(int)[]));  // <- NG
    static assert(is(typeof(fi( sma1))) == false);
    static assert(is(typeof(fi( sma2))) == false);
    static assert(is(typeof(fi( sca1))) == false);
    static assert(is(typeof(fi( sca2))) == false);
    static assert(is(typeof(fi(swma1))) == false);
    static assert(is(typeof(fi(swma2))) == false);
    static assert(is(typeof(fi(swca1))) == false);
    static assert(is(typeof(fi(swca2))) == false);

    /* Parameter is (shared) inout
     */
    U f_w(U)(       inout U) { return null; }
    U fsw(U)(shared inout U) { return null; }
    // 9 * 2 - 1
    static assert(is(typeof(f_w(  ma1))  ==              int []));
    static assert(is(typeof(f_w(  ca1))  ==              int []));  // <- const(int)[]
    static assert(is(typeof(f_w(  ca2))  ==              int []));  // <- const(int)[]
    static assert(is(typeof(f_w( wma1))  ==              int []));  // <- inout(int)[]
    static assert(is(typeof(f_w( wma2))  ==              int []));  // <- inout(int)[]
    static assert(is(typeof(f_w( wca1))  ==        const(int)[]));  // <- inout(const(int))[]
    static assert(is(typeof(f_w( wca2))  ==        const(int)[]));  // <- inout(const(int))[]
    static assert(is(typeof(f_w(  ia1))  ==              int []));  // <- immutable(int)[]
    static assert(is(typeof(f_w(  ia2))  ==              int []));  // <- immutable(int)[]
    static assert(is(typeof(f_w( sma1))  == shared(      int)[]));
    static assert(is(typeof(f_w( sma2))  == shared(      int)[]));  // <- shared(int[])
    static assert(is(typeof(f_w( sca1))  == shared(      int)[]));  // <- shared(const(int))[]
    static assert(is(typeof(f_w( sca2))  == shared(      int)[]));  // <- shared(const(int)[])
    static assert(is(typeof(f_w(swma1))  == shared(      int)[]));  // <- shared(inout(int))[]
    static assert(is(typeof(f_w(swma2))  == shared(      int)[]));  // <- shared(inout(int)[])
    static assert(is(typeof(f_w(swca1))  == shared(const int)[]));  // <- shared(inout(const(int)))[]
    static assert(is(typeof(f_w(swca2))  == shared(const int)[]));  // <- shared(inout(const(int))[])
    // 9 * 2 - 1
    static assert(is(typeof(fsw(  ma1))) == false);
    static assert(is(typeof(fsw(  ca1))) == false);
    static assert(is(typeof(fsw(  ca2))) == false);
    static assert(is(typeof(fsw( wma1))) == false);
    static assert(is(typeof(fsw( wma2))) == false);
    static assert(is(typeof(fsw( wca1))) == false);
    static assert(is(typeof(fsw( wca2))) == false);
    static assert(is(typeof(fsw(  ia1))  ==              int []));  // <- NG
    static assert(is(typeof(fsw(  ia2))  ==              int []));  // <- NG
    static assert(is(typeof(fsw( sma1))  ==              int []));  // <- NG
    static assert(is(typeof(fsw( sma2))  ==              int []));
    static assert(is(typeof(fsw( sca1))  ==              int []));  // <- NG
    static assert(is(typeof(fsw( sca2))  ==              int []));  // const(int)[]
    static assert(is(typeof(fsw(swma1))  ==              int []));  // <- NG
    static assert(is(typeof(fsw(swma2))  ==              int []));  // inout(int)[]
    static assert(is(typeof(fsw(swca1))  ==        const(int)[]));  // <- NG
    static assert(is(typeof(fsw(swca2))  ==        const(int)[]));  // <- inout(const(int))[]

    /* Parameter is (shared) inout const
     */
    U f_wc(U)(       inout const U) { return null; }
    U fswc(U)(shared inout const U) { return null; }
    // 9 * 2 - 1
    static assert(is(typeof(f_wc(  ma1))  ==        int []));
    static assert(is(typeof(f_wc(  ca1))  ==        int []));  // <- const(int)[]
    static assert(is(typeof(f_wc(  ca2))  ==        int []));  // <- const(int)[]
    static assert(is(typeof(f_wc( wma1))  ==        int []));  // <- inout(int)[]
    static assert(is(typeof(f_wc( wma2))  ==        int []));  // <- inout(int)[]
    static assert(is(typeof(f_wc( wca1))  ==        int []));  // <- inout(const(int))[]
    static assert(is(typeof(f_wc( wca2))  ==        int []));  // <- inout(const(int))[]
    static assert(is(typeof(f_wc(  ia1))  ==        int []));  // <- immutable(int)[]
    static assert(is(typeof(f_wc(  ia2))  ==        int []));  // <- immutable(int)[]
    static assert(is(typeof(f_wc( sma1))  == shared(int)[]));
    static assert(is(typeof(f_wc( sma2))  == shared(int)[]));  // <- shared(int[])
    static assert(is(typeof(f_wc( sca1))  == shared(int)[]));  // <- shared(const(int))[]
    static assert(is(typeof(f_wc( sca2))  == shared(int)[]));  // <- shared(const(int)[])
    static assert(is(typeof(f_wc(swma1))  == shared(int)[]));  // <- shared(inout(int))[]
    static assert(is(typeof(f_wc(swma2))  == shared(int)[]));  // <- shared(inout(int)[])
    static assert(is(typeof(f_wc(swca1))  == shared(int)[]));  // <- shared(inout(const(int)))[]
    static assert(is(typeof(f_wc(swca2))  == shared(int)[]));  // <- shared(inout(const(int))[])
    // 9 * 2 - 1
    static assert(is(typeof(fswc(  ma1))) == false);
    static assert(is(typeof(fswc(  ca1))) == false);
    static assert(is(typeof(fswc(  ca2))) == false);
    static assert(is(typeof(fswc( wma1))) == false);
    static assert(is(typeof(fswc( wma2))) == false);
    static assert(is(typeof(fswc( wca1))) == false);
    static assert(is(typeof(fswc( wca2))) == false);
    static assert(is(typeof(fswc(  ia1))  ==        int []));  // <- NG
    static assert(is(typeof(fswc(  ia2))  ==        int []));  // <- NG
    static assert(is(typeof(fswc( sma1))  ==        int []));  // <- NG
    static assert(is(typeof(fswc( sma2))  ==        int []));
    static assert(is(typeof(fswc( sca1))  ==        int []));  // <- NG
    static assert(is(typeof(fswc( sca2))  ==        int []));  // <- const(int)[]
    static assert(is(typeof(fswc(swma1))  ==        int []));  // <- NG
    static assert(is(typeof(fswc(swma2))  ==        int []));  // <- inout(int)[]
    static assert(is(typeof(fswc(swca1))  ==        int []));  // <- NG
    static assert(is(typeof(fswc(swca2))  ==        int []));  // <- inout(const(int))[]
}+/

void test13127a()
{
    void foo(T)(in T[] src, T[] dst) { static assert(is(T == int[])); }

    int[][] a;
    foo(a, a);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13159

template maxSize13159(T...)
{
    static if (T.length == 1)
    {
        enum size_t maxSize13159 = T[0].sizeof;
    }
    else
    {
        enum size_t maxSize13159 =
            T[0].sizeof >= maxSize13159!(T[1 .. $])
                ? T[0].sizeof
                : maxSize13159!(T[1 .. $]);
    }
}

struct Node13159
{
    struct Pair
    {
        Node13159 value;
    }

    //alias Algebraic!(Node[], int) Value;
    enum n = maxSize13159!(Node13159[], int);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13180

void test13180()
{
    inout(V) get1a(K, V)(inout(V[K]) aa, lazy inout(V) defaultValue)
    {
        static assert(is(V == string));
        static assert(is(K == string));
        return defaultValue;
    }
    inout(V) get1b(K, V)(lazy inout(V) defaultValue, inout(V[K]) aa)
    {
        static assert(is(V == string));
        static assert(is(K == string));
        return defaultValue;
    }

    inout(V) get2a(K, V)(inout(V)[K] aa, lazy inout(V) defaultValue)
    {
        static assert(is(V == string));
        static assert(is(K == string));
        return defaultValue;
    }
    inout(V) get2b(K, V)(lazy inout(V) defaultValue, inout(V)[K] aa)
    {
        static assert(is(V == string));
        static assert(is(K == string));
        return defaultValue;
    }
    string def;
    string[string] aa;
    string s1a = get1a(aa, def);
    string s1b = get1b(def, aa);
    string s2a = get2a(aa, def);
    string s2b = get2b(def, aa);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13204

struct A13204(uint v)
{
    alias whatever = A13204y;
    static assert(is(whatever == A13204));
}
alias A13204x = A13204!1;
alias A13204y = A13204x;

struct B13204(uint v)
{
    alias whatever = B13204z;
    static assert(is(whatever == B13204));
}
alias B13204x = B13204!1;
alias B13204y = B13204x;
alias B13204z = B13204y;

void test13204()
{
    static assert(is(A13204x == A13204!1));
    static assert(is(A13204x == A13204!1.whatever));
    static assert(is(A13204x == A13204y));

    static assert(is(B13204x == B13204!1));
    static assert(is(B13204x == B13204!1.whatever));
    static assert(is(B13204x == B13204y));
    static assert(is(B13204x == B13204z));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8462 (dup of 13204)

alias FP8462 = void function(C8462.Type arg);

class C8462
{
    enum Type { Foo }
    alias funcPtrPtr = FP8462*;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13218

template isCallable13218(T...)
    if (T.length == 1)
{
    static assert(0);
}

template ParameterTypeTuple13218(func...)
    if (func.length == 1 && isCallable13218!func)
{
    static assert(0);
}

struct R13218
{
    private static string mangleFuncPtr(ArgTypes...)()
    {
        string result = "fnp_";
        foreach (T; ArgTypes)
            result ~= T.mangleof;
        return result;
    }
    void function(int) fnp_i;
    double delegate(double) fnp_d;

    void opAssign(FnT)(FnT func)
    {
        mixin(mangleFuncPtr!( ParameterTypeTuple13218!FnT) ~ " = func;");   // parsed as TypeInstance
      //mixin(mangleFuncPtr!(.ParameterTypeTuple13218!FnT) ~ " = func;");   // parsed as DotTemplateInstanceExp -> works
    }
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13219

struct Map13219(V) {}

void test13219a(alias F, VA, VB)(Map13219!VA a, Map13219!VB b)
if (is(VA : typeof(F(VA.init, VB.init))))
{}

void test13219b(alias F)()
{
    test13219a!((a, b) => b)(Map13219!int.init, Map13219!int.init);
}

void test13219()
{
    int x;
    test13219b!x();
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13223

void test13223()
{
    T[] f1(T)(T[] a1, T[] a2)
    {
        static assert(is(T == int));
        return a1 ~ a2;
    }
    T[] f2(T)(T[] a1, T[] a2)
    {
        static assert(is(T == int));
        return a1 ~ a2;
    }
    int[] a = [1, 2];
    static assert(is(typeof(f1(a, [])) == int[]));
    static assert(is(typeof(f2([], a)) == int[]));
    static assert(is(typeof(f1(a, null)) == int[]));
    static assert(is(typeof(f2(null, a)) == int[]));

    T[] f3(T)(T[] a) { return a; }
    static assert(is(typeof(f3([])) == void[]));
    static assert(is(typeof(f3(null)) == void[]));

    T f4(T)(T a) { return a; }
    static assert(is(typeof(f4([])) == void[]));
    static assert(is(typeof(f4(null)) == typeof(null)));

    T[][] f5(T)(T[][] a) { return a; }
    static assert(is(typeof(f5([])) == void[][]));
    static assert(is(typeof(f5(null)) == void[][]));

    void translate(C = immutable char)(const(C)[] toRemove)
    {
        static assert(is(C == char));
    }
    translate(null);
}

void test13223a()
{
    T f(T)(T, T) { return T.init; }

    immutable i = 0;
    const c = 0;
    auto m = 0;
    shared s = 0;

    static assert(is(typeof(f(i, i)) == immutable int));
    static assert(is(typeof(f(i, c)) ==     const int));
    static assert(is(typeof(f(c, i)) ==     const int));
    static assert(is(typeof(f(i, m)) ==           int));
    static assert(is(typeof(f(m, i)) ==           int));
    static assert(is(typeof(f(c, m)) ==           int));
    static assert(is(typeof(f(m, c)) ==           int));
    static assert(is(typeof(f(m, m)) ==           int));
    static assert(is(typeof(f(i, s)) ==    shared int));
    static assert(is(typeof(f(s, i)) ==    shared int));
    static assert(is(typeof(f(c, s)) ==    shared int));
    static assert(is(typeof(f(s, c)) ==    shared int));
    static assert(is(typeof(f(s, s)) ==    shared int));
    static assert(is(typeof(f(s, m)) ==           int));
    static assert(is(typeof(f(m, s)) ==           int));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13235

struct Tuple13235(T...)
{
    T expand;
    alias expand field;

    this(T values)
    {
        field = values;
    }
}
struct Foo13235
{
    Tuple13235!(int, Foo13235)* foo;
}

template Inst13235(T...)
{
    struct Tuple
    {
        T expand;
        alias expand field;

        this(T values)
        {
            field = values;
        }
    }
    alias Inst13235 = Tuple*;
}
struct Bar13235
{
    Inst13235!(int, Bar13235) bar;
}

void test13235()
{
    alias Tup1 = Tuple13235!(int, Foo13235);
    assert(Tup1(1, Foo13235()).expand[0] == 1);

    alias Tup2 = typeof(*Inst13235!(int, Bar13235).init);
    assert(Tup2(1, Bar13235()).expand[0] == 1);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13252

alias TypeTuple13252(T...) = T;

static assert(is(typeof(TypeTuple13252!(cast(int )1)[0]) == int ));
static assert(is(typeof(TypeTuple13252!(cast(long)1)[0]) == long));

static assert(is(typeof(TypeTuple13252!(cast(float )3.14)[0]) == float ));
static assert(is(typeof(TypeTuple13252!(cast(double)3.14)[0]) == double));

static assert(is(typeof(TypeTuple13252!(cast(string  )null)[0]) == string  ));
static assert(is(typeof(TypeTuple13252!(cast(string[])null)[0]) == string[]));  // OK <- NG

static assert(is(typeof(TypeTuple13252!(cast(wstring)"abc")[0]) == wstring));
static assert(is(typeof(TypeTuple13252!(cast(dstring)"abc")[0]) == dstring));

static assert(is(typeof(TypeTuple13252!(cast(int[] )[])[0]) == int[] ));
static assert(is(typeof(TypeTuple13252!(cast(long[])[])[0]) == long[]));        // OK <- NG

struct S13252 { }
static assert(is(typeof(TypeTuple13252!(const     S13252())[0]) ==     const(S13252)));
static assert(is(typeof(TypeTuple13252!(immutable S13252())[0]) == immutable(S13252)));     // OK <- NG

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13294

void test13294()
{
    void f(T)(const ref T src, ref T dst)
    {
        pragma(msg, "T = ", T);
        static assert(!is(T == const));
    }
    {
        const byte src;
              byte dst;
        f(src, dst);
    }
    {
        const char src;
              char dst;
        f(src, dst);
    }

    // https://issues.dlang.org/show_bug.cgi?id=13351
    T add(T)(in T x, in T y)
    {
        T z;
        z = x + y;
        return z;
    }
    const double a = 1.0;
    const double b = 2.0;
    double c;
    c = add(a,b);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13299

struct Foo13299
{
    Foo13299 opDispatch(string name)(int a, int[] b...)
    if (name == "bar")
    {
        return Foo13299();
    }

    Foo13299 opDispatch(string name)()
    if (name != "bar")
    {
        return Foo13299();
    }
}

void test13299()
{
    Foo13299()
        .bar(0)
        .bar(1)
        .bar(2);

    Foo13299()
        .opDispatch!"bar"(0)
        .opDispatch!"bar"(1)
        .opDispatch!"bar"(2);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13333

template AliasThisTypeOf13333(T)
{
    static assert(0, T.stringof);  // T.stringof is important
}

template StaticArrayTypeOf13333(T)
{
    static if (is(AliasThisTypeOf13333!T AT))
        alias X = StaticArrayTypeOf13333!AT;
    else
        alias X = T;

    static if (is(X : E[n], E, size_t n))
        alias StaticArrayTypeOf13333 = X;
    else
        static assert(0, T.stringof~" is not a static array type");
}

enum bool isStaticArray13333(T) = is(StaticArrayTypeOf13333!T);

struct VaraiantN13333(T)
{
    static if (isStaticArray13333!T)
        ~this() { static assert(0); }
}

struct DummyScope13333
{
    alias A = VaraiantN13333!C;

    static class C
    {
        A entity;
    }
}

void test13333()
{
    struct DummyScope
    {
        alias A = VaraiantN13333!C;

        static class C
        {
            A entity;
        }
    }
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13374

int f13374(alias a)()  { return 1; }
int f13374(string s)() { return 2; }

void x13374(int i) {}

void test13374()
{
    assert(f13374!x13374() == 1);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14109

string f14109() { return "a"; }
string g14109()() { return "a"; }

struct S14109(string s) { static assert(s == "a"); }

alias X14109 = S14109!(f14109);
alias Y14109 = S14109!(g14109!());
static assert(is(X14109 == Y14109));

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13378

struct Vec13378(size_t n, T, string as)
{
    T[n] data;
}

void doSome13378(size_t n, T, string as)(Vec13378!(n,T,as) v) {}

void test13378()
{
    auto v = Vec13378!(3, float, "xyz")([1,2,3]);
    doSome13378(v);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13379

void test13379()
{
    match13379("");
}

auto match13379(RegEx )(RegEx  re)
if (is(RegEx == Regex13379!char))       // #1 Regex!char (speculative && tinst == NULL)
{}
auto match13379(String)(String re)
{}

struct Regex13379(Char)
{
    ShiftOr13379!Char kickstart;        // #2 ShiftOr!char (speculative && tinst == Regex!char)
}
struct ShiftOr13379(Char)
{
    this(ref Regex13379!Char re)        // #3 Regex!Char (speculative && tinst == ShiftOr!char)
    {
        uint n_length;
        uint idx;
        n_length = min13379(idx, n_length);
    }
}

template MinType13379(T...)
{
    alias MinType13379 = T[0];
}
MinType13379!T min13379(T...)(T args)   // #4 MinType!uint (speculative && thist == ShiftOr!char)
{
    alias a = args[0];
    alias b = args[$-1];
    return cast(typeof(return)) (a < b ? a : b);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13417

struct V13417(size_t N, E, alias string AS)
{
}

auto f13417(E)(in V13417!(4, E, "ijka"))
{
    return V13417!(3, E, "xyz")();
}

void test13417()
{
    f13417(V13417!(4, float, "ijka")());
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13484

int foo13484()(void delegate() hi) { return 1; }
int foo13484(T)(void delegate(T) hi) { return 2; }

void test13484()
{
    assert(foo13484({}) == 1);          // works
    assert(foo13484((float v){}) == 2); // works <- throws error
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13675

enum E13675;

bool foo13675(T : E13675)()
{
    return false;
}

void test13675()
{
    if (foo13675!E13675)
    {}
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13694

auto foo13694(T)(string A,         T[] G ...) { return 1; }
auto foo13694(T)(string A, long E, T[] G ...) { return 2; }

void test13694()
{
    struct S {}

    S v;
    assert(foo13694("A", v) == 1);      // <- OK
    assert(foo13694("A", 0, v) == 2);   // <- used to be OK but now fails
    assert(foo13694!S("A", 0, v) == 2); // <- workaround solution
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13760

void test13760()
{
    void func(K, V)(inout(V[K]) aa, inout(V) val) {}

    class C {}
    C[int] aa;
    func(aa, new C);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13714

struct JSONValue13714
{
    this(T)(T arg)
    {
    }
    this(T : JSONValue13714)(inout T arg) inout
    {
        //store = arg.store;
    }

    void opAssign(T)(T arg)
    {
    }
}

void test13714()
{
    enum DummyStringEnum
    {
        foo = "bar"
    }

    JSONValue13714[string] aa;
    aa["A"] = DummyStringEnum.foo;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13807

T f13807(T)(inout(T)[] arr)
{
    return T.init;
}

void test13807()
{
    static assert(is(typeof(f13807([1, 2, 3])) == int));    // OK
    static assert(is(typeof(f13807(["a", "b"])) == string));    // OK <- Error
    static assert(is(typeof(f13807!string(["a", "b"])) == string)); // OK
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14174
import imports.testmangle;

struct Config14174(a, b) {}

struct N14174 {}

alias defConfig14174 = Config14174!(N14174, N14174);

@safe @nogc pure nothrow
void accepter14174a(Config : Config14174!(T) = defConfig14174, T...)()
{
    static assert(equalDemangle(accepter14174a.mangleof,
           "_D7breaker131__T14"~
           "accepter14174a"~
           "HTS7breaker51__T11Config14174TS7breaker6N14174TS7breaker6N14174Z11Config14174TS7breaker6N14174TS7breaker6N14174Z14"~
           "accepter14174a"~
           "FNaNbNiNfZv"));
}

@safe @nogc pure nothrow
void accepter14174b(Config : Config14174!(T) = defConfig14174, T...)()
{
    static assert(equalDemangle(accepter14174b.mangleof,
           "_D7breaker131__T14"~
           "accepter14174b"~
           "HTS7breaker51__T11Config14174TS7breaker6N14174TS7breaker6N14174Z11Config14174TS7breaker6N14174TS7breaker6N14174Z14"~
           "accepter14174b"~
           "FNaNbNiNfZv"));
}

void test14174()
{
    accepter14174a!()();

    accepter14174b!()();
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14836

template a14836x(alias B, C...)
{
    int a14836x(D...)()    if (D.length == 0) { return 1; }
    int a14836x(D...)(D d) if (D.length >  0) { return 2; }
}
template a14836y(alias B, C...)
{
    int a14836y(T, D...)(T t)      if (D.length == 0) { return 1; }
    int a14836y(T, D...)(T t, D d) if (D.length >  0) { return 2; }
}

void test14836()
{
    int v;
    assert(a14836x!(v)() == 1);
    assert(a14836x!(v)(1) == 2);
    assert(a14836y!(v)(1) == 1);
    assert(a14836y!(v)(1, 2) == 2);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14357

template Qux14357(T : U*, U : V*, V)
{
    pragma(msg, T);     // no match <- float**
    pragma(msg, U);     // no match <- float*
    pragma(msg, V);     // no match <- int
    enum Qux14357 = T.sizeof + V.sizeof;
}
static assert(!__traits(compiles, Qux14357!(float**, int*)));

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14481

template someT14481(alias e)
{
    alias someT14481 = e;
}

mixin template Mix14481(alias e)
{
    alias SomeAlias = someT14481!e;
}

struct Hoge14481
{
    mixin Mix14481!e;
    enum e = 10;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14520

template M14520(alias  a) { enum M14520 = 1; }
template M14520(string s) { enum M14520 = 2; }

int f14520a();
string f14520b() { assert(0); }
string f14520c() { return "a"; }

static assert(M14520!f14520a == 1);
static assert(M14520!f14520b == 1);
static assert(M14520!f14520c == 1);

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14568

struct Interval14568()
{
    auto left = INVALID;

    auto opAssign()(Interval14568) { left; }
}

auto interval14568(T)(T point)
{
    Interval14568!();
}

alias Instantiate14568(alias symbol, Args...) = symbol!Args;

template Match14568(patterns...)
{
    static if (__traits(compiles, Instantiate14568!(patterns[0])))
    {
        alias Match14568 = patterns[0];
    }
    else static if (patterns.length == 1)
    {}
}

template SubOps14568(Args...)
{
    auto opIndex()
    {
        template IntervalType(T...)
        {
            alias Point() = typeof(T.interval14568);

            alias IntervalType = Match14568!(Point);
        }
        alias Subspace = IntervalType!(Args);
    }
}

struct Nat14568 { mixin SubOps14568!(null); }

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14603
// https://issues.dlang.org/show_bug.cgi?id=14604

struct S14603
{
    template opDispatch(string name)
    {
        void opDispatch()() {}
    }
}
alias a14603 = S14603.opDispatch!"go";  // OK
alias b14603 = S14603.go;               // OK <- NG

struct S14604
{
    template opDispatch(string name)
    {
        void opDispatch()() {}
    }
}
alias Id14604(alias thing) = thing;
alias c14604 = Id14604!(S14604.opDispatch!"go");     // ok
// https://issues.dlang.org/show_bug.cgi?id=14604
alias d14604 = Id14604!(S14604.go);                  // 'Error: template instance opDispatch!"go" cannot resolve forward reference'

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14735

enum CS14735 { yes, no }

int indexOf14735a(Range      )(Range    s, in dchar c) { return 1; }
int indexOf14735a(T, size_t n)(ref T[n] s, in dchar c) { return 2; }

int indexOf14735b(Range      )(Range    s, in dchar c, in CS14735 cs = CS14735.yes) { return 1; }
int indexOf14735b(T, size_t n)(ref T[n] s, in dchar c, in CS14735 cs = CS14735.yes) { return 2; }

void test14735()
{
    char[64] buf;

    // Supported from 2.063: (https://dlang.org/changelog/2.063.html#implicitarraycast)
    assert(indexOf14735a(buf[0..32], '\0') == 2);
    assert(indexOf14735b(buf[0..32], '\0') == 2);

    // Have to work as same as above.
    assert(indexOf14735a(buf[], '\0') == 2);
    assert(indexOf14735b(buf[], '\0') == 2);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14743

class A14743
{
    auto func1 = (A14743 a) { a.func2!int(); };
    auto func2(T)() {}
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14802

void test14802()
{
    auto func(T)(T x, T y) { return x; }

    struct S1 { double x; alias x this; }
    struct S2 { double x; alias x this; }
    S1 s1;
    S2 s2;

    enum E1 : double { a = 1.0 }
    enum E2 : double { a = 1.0 }

    static assert(is(typeof( func(1 , 1 ) ) == int));
    static assert(is(typeof( func(1u, 1u) ) == uint));
    static assert(is(typeof( func(1u, 1 ) ) == uint));
    static assert(is(typeof( func(1 , 1u) ) == uint));

    static assert(is(typeof( func(1.0f, 1.0f) ) == float));
    static assert(is(typeof( func(1.0 , 1.0 ) ) == double));
    static assert(is(typeof( func(1.0 , 1.0f) ) == double));
    static assert(is(typeof( func(1.0f, 1.0 ) ) == double));

    static assert(is(typeof( func(s1, s1) ) == S1));
    static assert(is(typeof( func(s2, s2) ) == S2));
    static assert(is(typeof( func(s1, s2) ) == double));
    static assert(is(typeof( func(s2, s1) ) == double));

    static assert(is(typeof( func(E1.a, E1.a) ) == E1));
    static assert(is(typeof( func(E2.a, E2.a) ) == E2));
    static assert(is(typeof( func(E1.a, 1.0)  ) == double));
    static assert(is(typeof( func(E2.a, 1.0)  ) == double));
    static assert(is(typeof( func(1.0,  E1.a) ) == double));
    static assert(is(typeof( func(1.0,  E2.a) ) == double));
    static assert(is(typeof( func(E1.a, E2.a) ) == double));
    static assert(is(typeof( func(E2.a, E1.a) ) == double));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14886

void test14886()
{
    alias R = int[100_000];

    auto front(T)(T[] a) {}
    front(R.init);

    auto bar1(T)(T, T[] a) { return T.init; }
    auto bar2(T)(T[] a, T) { return T.init; }

    static assert(is(typeof(bar1(1L, R.init)) == long));
    static assert(is(typeof(bar2(R.init, 1L)) == long));
    // <-- T should be deduced to int because R.init is rvalue...?

    ubyte x;
    static assert(is(typeof(bar1(x, R.init)) == int));
    static assert(is(typeof(bar2(R.init, x)) == int));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15156

// https://issues.dlang.org/show_bug.cgi?id=15156
auto f15116a(T)(string s, string arg2) { return 1; }
auto f15116b(T)(int    i, string arg2) { return 2; }

template bish15116(T)
{
    alias bish15116 = f15116a!T;
    alias bish15116 = f15116b!T;
}

void test15116()
{
    alias func = bish15116!string;
    assert(func("", "") == 1);
    assert(func(12, "") == 2);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15152

void test15152()
{
    void func(string M)() { }

    struct S
    {
        enum name = "a";
    }

    enum s = S.init;
    func!(s.name);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15352

struct S15352(T, T delegate(uint idx) supplier)
{
}

auto make15352a(T, T delegate(uint idx) supplier)()
{
    enum local = supplier;      // OK
    S15352!(T, local) ret;
    return ret;
}

auto make15352b(T, T delegate(uint idx) supplier)()
{
    S15352!(T, supplier) ret;   // OK <- Error
    return ret;
}

void test15352()
{
    enum dg = delegate(uint idx) => idx;
    auto s1 = S15352!(uint, dg)();
    auto s2 = make15352a!(uint, dg)();
    auto s3 = make15352b!(uint, dg)();
    assert(is(typeof(s1) == typeof(s2)));
    assert(is(typeof(s1) == typeof(s3)));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15623

struct WithFoo15623a { void foo() {} }
struct WithFoo15623b { void foo() {} }
struct WithFoo15623c { void foo() {} }
struct WithFoo15623d { void foo() {} }

struct WithoutFoo15623a {}
struct WithoutFoo15623b {}
struct WithoutFoo15623c {}
struct WithoutFoo15623d {}

struct CallsFoo15623(T)
{
    T t;
    void bar() { t.foo(); }     // error occurs during TemplateInstance.semantic3
}

// Instantiations outside of function bodies
static assert( is(CallsFoo15623!WithFoo15623a));
static assert(!is(CallsFoo15623!WithoutFoo15623a));                     // OK <- NG
static assert( __traits(compiles, CallsFoo15623!WithFoo15623b));
static assert(!__traits(compiles, CallsFoo15623!WithoutFoo15623b));     // OK <- NG

// Instantiations inside function bodies (OK)
static assert( is(typeof({ alias Baz = CallsFoo15623!WithFoo15623c; return Baz.init; }())));
static assert(!is(typeof({ alias Baz = CallsFoo15623!WithoutFoo15623c; return Baz.init; }())));
static assert( __traits(compiles, { alias Baz = CallsFoo15623!WithFoo15623d; return Baz.init; }()));
static assert(!__traits(compiles, { alias Baz = CallsFoo15623!WithoutFoo15623d; return Baz.init; }()));

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15781

void test15781()
{
    static struct S
    {
        int value;
    }

    T foo(T)(T a, T b)
    {
        return T();
    }

    const S cs;
          S ms;
    static assert(is(typeof(foo(ms, ms)) ==       S));
    static assert(is(typeof(foo(ms, cs)) == const S));
    static assert(is(typeof(foo(cs, ms)) == const S));
    static assert(is(typeof(foo(cs, cs)) == const S));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=16042

struct Foo16042 {}

auto map16042(alias func, T)(T t)
{
    return func(t);
}

auto toChars16042(R)(R r) if (is(R == int[]))
{
    Foo16042 f;
    assert(toChars16042(f) == 1);               // OK
    assert(map16042!(toChars16042)(f) == 1);    // OK <- NG
    assert(map16042!((toChars16042))(f) == 1);  // OK
}

auto toChars16042(Foo16042 f)
{
    return 1;
}

void test16042()
{
    [1].toChars16042();
}

// ---

auto fn16042(R)(R r) if (is(R == int[])) {}
auto fn16042(Foo16042 f) { return 1; }

struct Namespace16042
{
    alias fn = fn16042!(int[]);
}

void test16042b()
{
    Foo16042 f;

    with (Namespace16042)
    {
        static assert(!__traits(compiles, fn(f)));              // NG
        static assert(!__traits(compiles, map16042!(fn)(f)));   // should be NG -> actually NG
        static assert(!__traits(compiles, map16042!((fn))(f))); // NG
    }
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15243

struct S15243(Types...)
{
    void apply1(U)(U delegate(Types[0]) f0) {}

    void apply2(U)(U delegate(Types) f0) {}

    void apply3(U)(U delegate(Types[1..$]) f0) {}
}

void test15243()
{
    int f1(int) { return 0; }
    int f2(int, long) { return 0; }
    int f3(long, string) { return 0; }

    S15243!(int) s1;
    s1.apply1(&f1);
    s1.apply2(&f1);

    S15243!(int, long) s2;
    s2.apply2(&f2);

    S15243!(int, long, string) s3;
    s3.apply3(&f3);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15653

alias TypeTuple15653(T...) = T;

void test15653()
{
    void foo(U, T)(const T x)     { static assert(is(T == U)); }
    void bar(U, T)(immutable T x) { static assert(is(T == U)); }

    struct X { int a; long[2] b; }
    struct Y { int* a; long[] b; }

    foreach (U; TypeTuple15653!( byte,    short,   int,  long,
                                ubyte,   ushort,  uint, ulong,
                                 float,  double,  real,
                                void delegate(),
                                int[2], X, X[2]))
    {
        foo!U(U.init);      // OK
        bar!U(U.init);      // Was error, now OK

        U u;
        foo!U(u);           // OK
        bar!U(u);           // Was error, now OK
    }

    foreach (U; TypeTuple15653!(void*, int**, long[], double*[2]))
    {
        foo!U(U.init);      // OK
        bar!U(U.init);      // Was error, now OK

        U u;
        foo!U(u);
        static assert(!__traits(compiles, bar!U(u)), U.stringof);
    }

    foreach (U; TypeTuple15653!(Object, Y, Y[2], int[int]))
    {
        foo!U(U.init);      // OK
        static assert(!__traits(compiles, bar!U(U.init)), U.stringof);

        U u;
        foo!U(u);           // OK
        static assert(!__traits(compiles, bar!U(u)), U.stringof);
    }
}

/******************************************/

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
    test1780();
    test3608();
    test5893();
    test6404();
    test2246();
    test2296();
    bug4984();
    test2579();
    test2803();
    test6613();
    test5886();
    test5393();
    test5896();
    test6825();
    test6789();
    test2778();
    test2778aa();
    test2778get();
    test6208a();
    test6208b();
    test6208c();
    test6738();
    test6780();
    test6810();
    test6891();
    test6994();
    test6764();
    test3467();
    test4413();
    test5525();
    test5801();
    test10();
    test7037();
    test7124();
    test7359();
    test7416();
    test7563();
    test7572();
    test7580();
    test7585();
    test7671();
    test7672();
    test7684();
    test11a();
    test11b();
    test7769();
    test7873();
    test7933();
    test8094();
    test12();
    test8125();
    test13();
    test14();
    test8129();
    test8238();
    test8669();
    test8833();
    test8976();
    test8940();
    test9022();
    test9026();
    test9038();
    test9076();
    test9100();
    test9124a();
    test9124b();
    test9143();
    test9266();
    test9536();
    test9578();
    test9596();
    test9837();
    test9874();
    test9885();
    test9971();
    test9977();
    test10083();
    test10592();
    test11242();
    test10811();
    test10969();
    test11271();
    test11533();
    test11818();
    test11843();
    test11872();
    test12122();
    test12207();
    test12376();
    test13235();
    test13294();
    test13299();
    test13374();
    test13378();
    test13379();
    test13484();
    test13694();
    test14836();
    test14735();
    test14802();
    test15116();
    test16042();
    test16042b();
    test15243();
    test15653();

    printf("Success\n");
    return 0;
}
