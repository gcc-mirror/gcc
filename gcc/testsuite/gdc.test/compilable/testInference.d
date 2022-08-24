
/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6265

pure nothrow @safe int h6265() {
    return 1;
}
int f6265a(alias g)() {
    return g();
}
pure nothrow @safe int i6265a() {
    return f6265a!h6265();
}

int f6265b()() {
    return h6265();
}
pure nothrow @safe int i6265b() {
    return f6265b();
}

pure nothrow @safe int i6265c() {
    return {
        return h6265();
    }();
}

/***************************************************/
// Make sure a function is not infered as pure if it isn't.

int fNPa() {
    return 1;
}
int gNPa()() {
    return fNPa();
}
static assert( __traits(compiles, function int ()         { return gNPa(); }));
static assert(!__traits(compiles, function int () pure    { return gNPa(); }));
static assert(!__traits(compiles, function int () nothrow { return gNPa(); }));
static assert(!__traits(compiles, function int () @safe   { return gNPa(); }));

/***************************************************/
// Need to ensure the comment in Expression::checkPurity is not violated.

void fECPa() {
    void g()() {
        void h() {
        }
        h();
    }
    static assert( is(typeof(&g!()) == void delegate() pure nothrow @nogc @safe));
    static assert(!is(typeof(&g!()) == void delegate()));
}

void fECPb() {
    void g()() {
        void h() {
        }
        fECPb();
    }
    static assert(!is(typeof(&g!()) == void delegate() pure));
    static assert( is(typeof(&g!()) == void delegate()));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=5635

pure bool foo5635(R = int)(string x)
{
    bool result = false;
    foreach (dchar d; x)
        result = true;
    return result;
}

void test5635()
{
    foo5635("hi");
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=5936

auto bug5936c(R)(R i) @safe pure nothrow {
    return true;
}
static assert( bug5936c(0) );

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6351

void bug6351(alias dg)()
{
    dg();
}

void test6351()
{
    void delegate(int[] a...) deleg6351 = (int[] a...){};
    alias bug6351!(deleg6351) baz6531;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6359

void    impure6359()      nothrow @safe @nogc {}
void throwable6359() pure         @safe @nogc {}
void    system6359() pure nothrow       @nogc {}
void    gcable6359() pure nothrow @safe       {}

int global6359;

void f6359() pure nothrow @safe @nogc
{
    static assert(!__traits(compiles,    impure6359()));
    static assert(!__traits(compiles, throwable6359()));
    static assert(!__traits(compiles,    system6359()));
    static assert(!__traits(compiles,    gcable6359()));
    static assert(!__traits(compiles,    global6359++));

    static assert(!__traits(compiles, {    impure6359(); }()));
    static assert(!__traits(compiles, { throwable6359(); }()));
    static assert(!__traits(compiles, {    system6359(); }()));
    static assert(!__traits(compiles, {    gcable6359(); }()));
    static assert(!__traits(compiles, {    global6359++; }()));
}

void g6359()() pure nothrow @safe @nogc
{
    static assert(!__traits(compiles,    impure6359()));
    static assert(!__traits(compiles, throwable6359()));
    static assert(!__traits(compiles,    system6359()));
    static assert(!__traits(compiles,    gcable6359()));
    static assert(!__traits(compiles,    global6359++));

    static assert(!__traits(compiles, {    impure6359(); }()));
    static assert(!__traits(compiles, { throwable6359(); }()));
    static assert(!__traits(compiles, {    system6359(); }()));
    static assert(!__traits(compiles, {    gcable6359(); }()));
    static assert(!__traits(compiles, {    global6359++; }()));
}

// attribute inference is not affected by the expressions inside __traits(compiles)
void h6359()()
{
    static assert( __traits(compiles,    impure6359()));
    static assert( __traits(compiles, throwable6359()));
    static assert( __traits(compiles,    system6359()));
    static assert( __traits(compiles,    gcable6359()));
    static assert( __traits(compiles,    global6359++));

    static assert( __traits(compiles, {    impure6359(); }()));
    static assert( __traits(compiles, { throwable6359(); }()));
    static assert( __traits(compiles, {    system6359(); }()));
    static assert( __traits(compiles, {    gcable6359(); }()));
    static assert( __traits(compiles, {    global6359++; }()));
}

void test6359() pure nothrow @safe @nogc
{
    f6359();
    g6359();
    h6359();
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7017

template map7017(fun...) if (fun.length >= 1)
{
    auto map7017()
    {
        struct Result {
            this(int dummy){}   // impure member function -> inferred to pure by fixing issue 10329
        }
        return Result(0);   // impure call -> inferred to pure by fixing issue 10329
    }
}

int foo7017(immutable int x) pure nothrow { return 1; }

void test7017a() pure
{
    int bar7017(immutable int x) pure nothrow { return 1; }

    static assert(__traits(compiles, map7017!((){})()));
    static assert(__traits(compiles, map7017!q{ 1 }()));
    static assert(__traits(compiles, map7017!foo7017()));
    static assert(__traits(compiles, map7017!bar7017()));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7017 (little simpler cases)

auto map7017a(alias fun)() { return fun();     }    // depends on purity of fun
auto map7017b(alias fun)() { return;           }    // always pure
auto map7017c(alias fun)() { return yyy7017(); }    // always impure

int xxx7017() pure { return 1; }
int yyy7017() { return 1; }

void test7017b() pure
{
    static assert( __traits(compiles, map7017a!xxx7017() ));
    static assert(!__traits(compiles, map7017a!yyy7017() ));

    static assert( __traits(compiles, map7017b!xxx7017() ));
    static assert( __traits(compiles, map7017b!yyy7017() ));

    static assert(!__traits(compiles, map7017c!xxx7017() ));
    static assert(!__traits(compiles, map7017c!yyy7017() ));
}

/***************************************************/
// Test case from std.process

auto escapeArgumentImpl(alias allocator)()
{
    return allocator();
}

auto escapeShellArgument(alias allocator)()
{
    return escapeArgumentImpl!allocator();
}

pure string escapeShellArguments()
{
    char[] allocator()
    {
        return new char[1];
    }

    /* Both escape!allocator and escapeImpl!allocator are impure,
     * but they are nested template function that instantiated here.
     * Then calling them from here doesn't break purity.
     */
    return escapeShellArgument!allocator();
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8234

void test8234()
{
    immutable int x = 0;

    alias FP = typeof({ enum e = x; return e; });
    static assert(is(FP : int function()));

    auto fp = { enum e = x; return e; };
    static assert(is(typeof(fp) : int function()));

    alias DG = typeof({ auto e = x; return e; });
    static assert(is(DG : int delegate()));

    auto dg = { auto e = x; return e; };
    static assert(is(typeof(dg) : int delegate()));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8504

import core.demangle : demangle;

void foo8504()()
{
    static assert(typeof(foo8504!()).stringof == "void()");
    static assert(typeof(foo8504!()).mangleof == "FZv");
//    static assert(demangle(foo8504!().mangleof) == "void testInference.foo8504!().foo8504()");
}

auto toDelegate8504a(F)(auto ref F fp) { return fp; }
   F toDelegate8504b(F)(auto ref F fp) { return fp; }

extern(C) void testC8504() {}

void test8504()
{
    static assert(typeof(foo8504!()).stringof == "pure nothrow @nogc @safe void()");
    static assert(typeof(foo8504!()).mangleof == "FNaNbNiNfZv");
    static assert(demangle(foo8504!().mangleof) == "pure nothrow @nogc @safe void testInference.foo8504!().foo8504()");

    auto fp1 = toDelegate8504a(&testC8504);
    auto fp2 = toDelegate8504b(&testC8504);
    static assert(is(typeof(fp1) == typeof(fp2)));
    static assert(typeof(fp1).stringof == "extern (C) void function()");
    static assert(typeof(fp2).stringof == "extern (C) void function()");
    static assert(typeof(fp1).mangleof == "PUZv");
    static assert(typeof(fp2).mangleof == "PUZv");
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8751

alias bool delegate(in int) pure Bar8751;
Bar8751 foo8751a(immutable int x) pure
{
    return y => x > y; // OK
}
Bar8751 foo8751b(const int x) pure
{
    return y => x > y; // error -> OK
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8793

alias bool delegate(in int) pure Dg8793;
alias bool function(in int) pure Fp8793;

Dg8793 foo8793fp1(immutable Fp8793 f) pure { return x => (*f)(x); } // OK
Dg8793 foo8793fp2(    const Fp8793 f) pure { return x => (*f)(x); } // OK

Dg8793 foo8793dg1(immutable Dg8793 f) pure { return x => f(x); } // OK
Dg8793 foo8793dg2(    const Dg8793 f) pure { return x => f(x); } // OK <- error

Dg8793 foo8793pfp1(immutable Fp8793* f) pure { return x => (*f)(x); } // OK
Dg8793 foo8793pdg1(immutable Dg8793* f) pure { return x => (*f)(x); } // OK

Dg8793 foo8793pfp2(const Fp8793* f) pure { return x => (*f)(x); } // OK <- error
Dg8793 foo8793pdg2(const Dg8793* f) pure { return x => (*f)(x); } // OK <- error

// general case for the hasPointer type
Dg8793 foo8793ptr1(immutable int* p) pure { return x => *p == x; } // OK

Dg8793 foo8793ptr2(const int* p) pure { return x => *p == x; } // OK <- error

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9072

struct A9072(T)
{
    this(U)(U x) {}
    ~this() {}
}
void test9072()
{
    A9072!int a = A9072!short();
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=5933
// https://issues.dlang.org/show_bug.cgi?id=8504
// Template attribute inferrence doesn't work

int foo5933()(int a) { return a*a; }
struct S5933
{
    double foo()(double a) { return a * a; }
}
// outside function
static assert(typeof(foo5933!()).stringof == "pure nothrow @nogc @safe int(int a)");
static assert(typeof(S5933.init.foo!()).stringof == "pure nothrow @nogc @safe double(double a)");

void test5933()
{
    // inside function
    static assert(typeof(foo5933!()).stringof == "pure nothrow @nogc @safe int(int a)");
    static assert(typeof(S5933.init.foo!()).stringof == "pure nothrow @nogc @safe double(double a)");
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9148

void test9148a() pure
{
    static int g;
    int x;

    void foo1() /+pure+/
    {
        static assert(!__traits(compiles, g++));
        x++;
    }
    void foo2() pure
    {
        static assert(!__traits(compiles, g++));
        x++;
    }
    foo1();
    static assert(is(typeof(&foo1) == void delegate() pure nothrow @nogc @safe));
    foo2();
    static assert(is(typeof(&foo2) == void delegate() pure nothrow @nogc @safe));

    void bar1() immutable /+pure+/
    {
        static assert(!__traits(compiles, g++));
        static assert(!__traits(compiles, x++));
    }
    void bar2() immutable pure
    {
        static assert(!__traits(compiles, g++));
        static assert(!__traits(compiles, x++));
    }
    bar1();
    static assert(is(typeof(&bar1) == void delegate() pure immutable nothrow @nogc @safe));
    bar2();
    static assert(is(typeof(&bar2) == void delegate() pure immutable nothrow @nogc @safe));

    struct S
    {
        void foo1() /+pure+/
        {
            static assert(!__traits(compiles, g++));
            x++;
        }
        void foo2() pure
        {
            static assert(!__traits(compiles, g++));
            x++;
        }
        void bar1() immutable /+pure+/
        {
            static assert(!__traits(compiles, g++));
            static assert(!__traits(compiles, x++));
        }
        void bar2() immutable pure
        {
            static assert(!__traits(compiles, g++));
            static assert(!__traits(compiles, x++));
        }
    }

    S sm;
    sm.foo1();
    static assert(is(typeof(&sm.foo1) == void delegate() pure));
    sm.foo2();
    static assert(is(typeof(&sm.foo2) == void delegate() pure));

    immutable S si;
    si.bar1();
    static assert(is(typeof(&si.bar1) == void delegate() pure immutable));
    si.bar2();
    static assert(is(typeof(&si.bar2) == void delegate() pure immutable));
}

// ----
// inheritance of pure and @safe

void test9148b() pure nothrow @nogc @safe
{
    void nf() {}
    static assert(is(typeof(&nf) == void delegate() pure nothrow @nogc @safe));

    struct NS
    {
        void mf() {}
        static void sf() {}
    }
    NS ns;
    static assert(is(typeof(&ns.mf) == void delegate() pure nothrow @nogc @safe));
    static assert(is(typeof(&NS.sf) == void function() pure nothrow @nogc @safe));

    static void sf() {}
    static assert(is(typeof(&sf) == void function() pure nothrow @nogc @safe));

    static struct SS
    {
        void mf() {}
        static void sf() {}
    }
    SS ss;
    static assert(is(typeof(&ss.mf) == void delegate() pure nothrow @nogc @safe));
    static assert(is(typeof(&SS.sf) == void function() pure nothrow @nogc @safe));
}

void impureSystem9148b() {}
void func9148b()()
{
    void bar()  // do not inherit PUREfwdref
    {
        static assert(is(typeof(&bar) == void delegate()));
        impureSystem9148b();
    }
    static assert(is(typeof(&bar) == void delegate()));
}
static assert(is(typeof(&func9148b!()) == void function() pure nothrow @nogc @safe));

// ----
// from fail_compilation/fail283.d

pure int double_sqr9148c(int x)
{
    int y = x;
    void do_sqr() pure { y *= y; }
    do_sqr();
    return y;
}

void test9148c()
{
    assert(double_sqr9148c(10) == 100);
}

// ----
// from fail_compilation/fail348.d

void test9148d() pure
{
    void g()    // implicitly marked as 'pure'
    {
        void h() pure
        {
            // i() and j() are implicitly marked as 'pure'
            void i() { }
            void j() { i(); g(); }  // can call i() and g()
        }
    }
}

void test9148e()
{
    int x;
    static assert(is(typeof((int a){ return a + x; }) == int delegate(int) pure nothrow @nogc @safe));

    auto dg = (int a){ return a + x; };
    static assert(is(typeof(dg) == int delegate(int) pure nothrow @nogc @safe));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12912

struct S12912(alias fun)
{
    void f() { fun(); }
}

class C12912
{
    int n;

    void f() pure
    {
        S12912!(() => n) s;
        // Here lambda should be inferred to weak purity.

        s.f();
        // And this call will be a pure member function call.
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10002

void impure10002() {}
void remove10002(alias pred, bool impure = false, Range)(Range range)
{
    pred(range[0]);
    static if (impure) impure10002();
}
class Node10002
{
    Node10002 parent;
    Node10002[] children;

    void foo() pure
    {
        parent.children.remove10002!(n => n is parent)();
        remove10002!(n => n is parent)(parent.children);
        static assert(!__traits(compiles, parent.children.remove10002x!(n => n is parent, true)()));
        static assert(!__traits(compiles, remove10002x!(n => n is parent, true)(parent.children)));

        Node10002 p;
        p.children.remove10002!(n => n is p)();
        remove10002!(n => n is p)(p.children);
        static assert(!__traits(compiles, p.children.remove10002x!(n => n is p, true)()));
        static assert(!__traits(compiles, remove10002x!(n => n is p, true)(p.children)));
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10148

void fa10148() {}  // fa is @system

auto fb10148(T)()
{
    struct A(S)
    {
        // [4] Parent function fb is already inferred to @safe, then
        // fc is forcely marked @safe on default until 2.052.
        // But fc should keep attribute inference ability
        // by overriding the inherited @safe-ty from its parent.
        void fc(T2)()
        {
            // [5] During semantic3 process, fc is not @safe on default.
            static assert(is(typeof(&fc) == void delegate()));
            fa10148();
        }
        // [1] this is now inferred to @safe by implementing issue 7511
        this(S a) {}
    }

    // [2] A!int(0) is now calling @safe function, then fb!T also be inferred to @safe
    return A!int(0);
}

void test10148()
{
    fb10148!int.fc!int;  // [0] instantiate fb
                         // [3] instantiate fc

    // [6] After semantic3 done, fc!int is deduced to @system.
    static assert(is(typeof(&fb10148!int.fc!int) == void delegate() @system));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10289

void test10289()
{
    void foo(E)()
    {
        throw new E("");
    }
    void bar(E1, E2)()
    {
        throw new E1("");
        throw new E2("");
    }
    void baz(E1, E2)(bool cond)
    {
        if (cond)
            throw new E1("");
        else
            throw new E2("");
    }

    import core.exception;
    static class MyException : Exception
    {
        this(string) @safe pure nothrow { super(""); }
    }

    static assert( __traits(compiles, () nothrow { foo!Error(); }));
    static assert( __traits(compiles, () nothrow { foo!AssertError(); }));

    static assert(!__traits(compiles, () nothrow { foo!Exception(); }));
    static assert(!__traits(compiles, () nothrow { foo!MyException(); }));

    static assert( __traits(compiles, () nothrow { bar!(Error, Exception)(); }));
    static assert(!__traits(compiles, () nothrow { bar!(Exception, Error)(); }));

    static assert(!__traits(compiles, () nothrow { baz!(Error, Exception)(); }));
    static assert(!__traits(compiles, () nothrow { baz!(Exception, Error)(); }));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10296

void foo10296()()
{
    int[3] a;

    void bar()() { a[1] = 2; }
    bar();
    static assert(typeof(bar!()).stringof == "pure nothrow @nogc @safe void()");    // nothrow @safe void()
}
pure void test10296()
{
    foo10296();
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12025

struct Foo12025
{
    int[5] bar;
}

void test12025a() pure
{
    enum n1 = typeof(Foo12025.bar).length;  // OK
    enum n2 =        Foo12025.bar .length;  // OK <- error

    auto x1 = typeof(Foo12025.bar).length;  // OK
    auto x2 =        Foo12025.bar .length;  // OK <- error
}

void test12025b() pure
{
    static int[5] bar;

    enum n1 = typeof(bar).length;  // OK
    enum n2 =        bar .length;  // OK <- error

    auto x1 = typeof(bar).length;  // OK
    auto x2 =        bar .length;  // OK <- error
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12542

int logOf12542(T)(T n)
{
    if (n)
        return 1 + logOf12542(n/2);
    return 0;
}

void test12542() @safe nothrow pure
{
    int log = logOf12542(9);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12704

void foo12704() @system;
alias FP12704 = typeof(function() { foo12704(); });
static assert(is(FP12704 == void function() @system));

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12970

@system { @safe void f12970a() {} }
@system { void f12970b() @safe {} }
static assert(is(typeof(&f12970a) == void function() @safe));
static assert(is(typeof(&f12970b) == void function() @safe));

@system { @trusted void f12970c() {} }
@system { void f12970d() @trusted {} }
static assert(is(typeof(&f12970c) == void function() @trusted));
static assert(is(typeof(&f12970d) == void function() @trusted));

@safe { @system void f12970e() {} }
@safe { void f12970f() @system {} }
static assert(is(typeof(&f12970e) == void function() @system));
static assert(is(typeof(&f12970f) == void function() @system));

@safe { @trusted void f12970g() {} }
@safe { void f12970h() @trusted {} }
static assert(is(typeof(&f12970g) == void function() @trusted));
static assert(is(typeof(&f12970h) == void function() @trusted));

@trusted { @safe void f12970i() {} }
@trusted { void f12970j() @safe {} }
static assert(is(typeof(&f12970i) == void function() @safe));
static assert(is(typeof(&f12970j) == void function() @safe));

@trusted { @system void f12970k() {} }
@trusted { void f12970l() @system {} }
static assert(is(typeof(&f12970k) == void function() @system));
static assert(is(typeof(&f12970l) == void function() @system));

/***************************************************/
// Parsing prefix STC_FUNCATTR for variable declaration

__gshared immutable pure nothrow @property @nogc @safe void function() prefix_qualified_fp1;
__gshared{immutable{pure{nothrow{@property{@nogc{@safe{void function() prefix_qualified_fp2;}}}}}}}
static assert(typeof(prefix_qualified_fp1).stringof == typeof(prefix_qualified_fp2).stringof);
static assert(typeof(prefix_qualified_fp1).stringof
        == "immutable(void function() pure nothrow @nogc @property @safe)");

const pure nothrow @property @nogc @safe void function()[] prefix_qualified_fp_array1;
const{pure{nothrow{@property{@nogc{@safe{void function()[] prefix_qualified_fp_array2;}}}}}}
static assert(typeof(prefix_qualified_fp_array1).stringof == typeof(prefix_qualified_fp_array2).stringof);
static assert(typeof(prefix_qualified_fp_array1).stringof
        == "const(void function() pure nothrow @nogc @property @safe[])");

/***************************************************/
// Parsing prefix, intermediate, or postfix @safe for alias declaration

@safe alias void function() AliasDecl_FP1;
alias @safe void function() AliasDecl_FP2;    // is not @safe
alias void function() @safe AliasDecl_FP3;
static assert(AliasDecl_FP1.stringof == "void function() @safe");
static assert(AliasDecl_FP2.stringof == "void function()");
static assert(AliasDecl_FP3.stringof == "void function() @safe");

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13217

void writeln13217(string) {}

nothrow void a13217(T)(T x)
{
    try
    {
        () { writeln13217("a"); } ();
    }
    catch (Exception e) {}
}

void test13217()
{
    a13217(1);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13840

struct Foo13840
{
    int opApply(int delegate(int))
    {
        return 0;
    }
}

void func13840()
{
}

void test13840() nothrow
{
    try
    {
        foreach (i; Foo13840()) // generated delegate is throwable
        {
            func13840();        // throwable function call
        }
    }
    catch(Throwable)
    {}
}

// Add more tests regarding inferences later.
