/*
TEST_OUTPUT:
---
S7019(16), 16
S7019(24), 24
S7019(32), 32
---

RUN_OUTPUT:
---
Success
---
*/

extern(C) int printf(const char*, ...);

/***************************************************/
// 6475

class Foo6475(Value)
{
    template T1(size_t n){ alias int T1; }
}

void test6475()
{
    alias Foo6475!(int) C1;
    alias C1.T1!0 X1;
    static assert(is(X1 == int));

    alias const(Foo6475!(int)) C2;
    alias C2.T1!0 X2;
    static assert(is(X2 == int));
}

/***************************************************/
// 6905

void test6905()
{
    auto foo1() { static int n; return n; }
    auto foo2() {        int n; return n; }
    auto foo3() {               return 1; }
    static assert(typeof(&foo1).stringof == "int delegate() nothrow @nogc @safe");
    static assert(typeof(&foo2).stringof == "int delegate() pure nothrow @nogc @safe");
    static assert(typeof(&foo3).stringof == "int delegate() pure nothrow @nogc @safe");

    ref bar1() { static int n; return n; }
  static assert(!__traits(compiles, {
    ref bar2() {        int n; return n; }
  }));
  static assert(!__traits(compiles, {
    ref bar3() {               return 1; }
  }));

    auto ref baz1() { static int n; return n; }
    auto ref baz2() {        int n; return n; }
    auto ref baz3() {               return 1; }
    static assert(typeof(&baz1).stringof == "int delegate() nothrow @nogc ref @safe");
    static assert(typeof(&baz2).stringof == "int delegate() pure nothrow @nogc @safe");
    static assert(typeof(&baz3).stringof == "int delegate() pure nothrow @nogc @safe");
}

/***************************************************/
// 7019

struct S7019
{
    int store;
    this(int n)
    {
        store = n << 3;
    }
}

S7019 rt_gs = 2;
enum S7019 ct_gs = 2;
pragma(msg, ct_gs, ", ", ct_gs.store);

void test7019()
{
    S7019 rt_ls = 3; // this compiles fine
    enum S7019 ct_ls = 3;
    pragma(msg, ct_ls, ", ", ct_ls.store);

    static class C
    {
        S7019 rt_fs = 4;
        enum S7019 ct_fs = 4;
        pragma(msg, ct_fs, ", ", ct_fs.store);
    }

    auto c = new C;
    assert(rt_gs == S7019(2) && rt_gs.store == 16);
    assert(rt_ls == S7019(3) && rt_ls.store == 24);
    assert(c.rt_fs == S7019(4) && c.rt_fs.store == 32);
    static assert(ct_gs == S7019(2) && ct_gs.store == 16);
    static assert(ct_ls == S7019(3) && ct_ls.store == 24);
    static assert(C.ct_fs == S7019(4) && C.ct_fs.store == 32);

    void foo(S7019 s = 5)   // fixing bug 7152
    {
        assert(s.store == 5 << 3);
    }
    foo();
}

/***************************************************/
// 7239

struct vec7239
{
    float x, y, z, w;
    alias x r;  //! for color access
    alias y g;  //! ditto
    alias z b;  //! ditto
    alias w a;  //! ditto
}

void test7239()
{
    vec7239 a = {x: 0, g: 0, b: 0, a: 1};
    assert(a.r == 0);
    assert(a.g == 0);
    assert(a.b == 0);
    assert(a.a == 1);
}

/***************************************************/
struct S10635
{
    string str;

    this(string[] v) { str = v[0]; }
    this(string[string] v) { str = v.keys[0]; }
}

S10635 s10635a = ["getnonce"];
S10635 s10635b = ["getnonce" : "str"];

void test10635()
{
    S10635 sa = ["getnonce"];
    S10635 sb = ["getnonce" : "str"];
}

/***************************************************/
// 8123

void test8123()
{
    struct S { }

    struct AS
    {
        alias S Alias;
    }

    struct Wrapper
    {
        AS as;
    }

    Wrapper w;
    static assert(is(typeof(w.as).Alias == S));         // fail
    static assert(is(AS.Alias == S));                   // ok
    static assert(is(typeof(w.as) == AS));              // ok
    static assert(is(typeof(w.as).Alias == AS.Alias));  // fail
}

/***************************************************/
// 8147

enum A8147 { a, b, c }

@property ref T front8147(T)(T[] a)
if (!is(T[] == void[]))
{
    return a[0];
}

template ElementType8147(R)
{
    static if (is(typeof({ R r = void; return r.front8147; }()) T))
        alias T ElementType8147;
    else
        alias void ElementType8147;
}

void test8147()
{
    auto arr = [A8147.a];
    alias typeof(arr) R;
    auto e = ElementType8147!R.init;
}

/***************************************************/
// 8410

void test8410()
{
    struct Foo { int[15] x; string s; }

    Foo[5] a1 = Foo([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], "hello"); // OK
    Foo f = { s: "hello" }; // OK (not static)
    Foo[5] a2 = { s: "hello" }; // error
}

/***************************************************/
// 8942

alias const int A8942_0;
static assert(is(A8942_0 == const int)); // passes

void test8942()
{
    alias const int A8942_1;
    static assert(is(A8942_1 == const int)); // passes

    static struct S { int i; }
    foreach (Unused; typeof(S.tupleof))
    {
        alias const(int) A8942_2;
        static assert(is(A8942_2 == const int)); // also passes

        alias const int A8942_3;
        static assert(is(A8942_3 == const int)); // fails
        // Error: static assert  (is(int == const(int))) is false
    }
}

/***************************************************/
// 10144

final class TNFA10144(char_t)
{
    enum Act { don }
    const Act[] action_lookup1 = [ Act.don, ];
}
alias X10144 = TNFA10144!char;

class C10144
{
    enum Act { don }
    synchronized { enum x1 = [Act.don]; }
    override     { enum x2 = [Act.don]; }
    abstract     { enum x3 = [Act.don]; }
    final        { enum x4 = [Act.don]; }
    synchronized { static s1 = [Act.don]; }
    override     { static s2 = [Act.don]; }
    abstract     { static s3 = [Act.don]; }
    final        { static s4 = [Act.don]; }
    synchronized { __gshared gs1 = [Act.don]; }
    override     { __gshared gs2 = [Act.don]; }
    abstract     { __gshared gs3 = [Act.don]; }
    final        { __gshared gs4 = [Act.don]; }
}

/***************************************************/

// 10142

class File10142
{
    enum Access : ubyte { Read = 0x01 }
    enum Open : ubyte { Exists = 0 }
    enum Share : ubyte { None = 0 }
    enum Cache : ubyte { None = 0x00 }

    struct Style
    {
        Access  access;
        Open    open;
        Share   share;
        Cache   cache;
    }
    enum Style ReadExisting = { Access.Read, Open.Exists };

    this (const(char[]) path, Style style = ReadExisting)
    {
        assert(style.access == Access.Read);
        assert(style.open   == Open  .Exists);
        assert(style.share  == Share .None);
        assert(style.cache  == Cache .None);
    }
}

void test10142()
{
    auto f = new File10142("dummy");
}

/***************************************************/
// 11421

void test11421()
{
    // AAs in array
    const            a1 = [[1:2], [3:4]];   // ok <- error
    const int[int][] a2 = [[1:2], [3:4]];   // ok
    static assert(is(typeof(a1) == typeof(a2)));

    // AAs in AA
    auto aa = [1:["a":1.0], 2:["b":2.0]];
    static assert(is(typeof(aa) == double[string][int]));
    assert(aa[1]["a"] == 1.0);
    assert(aa[2]["b"] == 2.0);
}

/***************************************************/
// 13776

enum a13776(T) = __traits(compiles, { T; });

enum b13776(A...) = 1;

template x13776s()
{
    struct S;
    alias x13776s = b13776!(a13776!S);
}
template y13776s()
{
    struct S;
    alias x2 = b13776!(a13776!S);
    alias y13776s = x2;
}
template z13776s()
{
    struct S;
    alias x1 = a13776!S;
    alias x2 = b13776!(x1);
    alias z13776s = x2;
}

template x13776c()
{
    class C;
    alias x13776c = b13776!(a13776!C);
}
template y13776c()
{
    class C;
    alias x2 = b13776!(a13776!C);
    alias y13776c = x2;
}
template z13776c()
{
    class C;
    alias x1 = a13776!C;
    alias x2 = b13776!(x1);
    alias z13776c = x2;
}

void test13776()
{
    alias xs = x13776s!();  // ok <- ng
    alias ys = y13776s!();  // ok <- ng
    alias zs = z13776s!();  // ok

    alias xc = x13776c!();  // ok <- ng
    alias yc = y13776c!();  // ok <- ng
    alias zc = z13776c!();  // ok
}

/***************************************************/
// 14090

template Packed14090(Args...)
{
    enum x = __traits(compiles, { Args[0] v; });
    // Args[0] is an opaque struct Empty, so the variable declaration fails to compile.
    // The error message creation calls TypeStruct('_Empty')->toChars(), and
    // it wrongly calls TemplateInstance('RoundRobin!()')->toAlias().
    // Finally it will cause incorrect "recursive template instantiation" error.
}

template Map14090(A...)
{
    alias Map14090 = A[0];
}

template RoundRobin14090()
{
    struct Empty;
    alias RoundRobin14090 = Map14090!(Packed14090!(Empty));
}

alias roundRobin14090 = RoundRobin14090!();

/***************************************************/
// 13950

template Tuple13950(T...) { alias T Tuple13950; }

void f13950(int x = 0, Tuple13950!() xs = Tuple13950!())
{
    assert(x == 0);
    assert(xs.length == 0);
}

void test13950()
{
    f13950();
}

/***************************************************/

int main()
{
    test6475();
    test6905();
    test7019();
    test7239();
    test8123();
    test8147();
    test8410();
    test8942();
    test10142();
    test11421();
    test13950();

    printf("Success\n");
    return 0;
}
