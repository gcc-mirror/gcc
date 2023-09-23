/*
TEST_OUTPUT:
---
const(immutable(char)*)
inout(immutable(char)*)
inout(const(char)*)
inout(const(char))
shared(inout(char))
shared(inout(char))
immutable(char)
immutable(char)
inout(const(char))
inout(const(char))
shared(const(char))
shared(const(char))
inout(char*****)
inout(char****)*
const(char*****)
const(char****)*
immutable(char*****)
immutable(char****)*
shared(char*****)
shared(char****)*
const(shared(char****)*)
shared(const(char*****))
shared(char*****)
immutable(char*****)
inout(shared(char****)*)
inout(shared(char**)***)
shared(inout(char**))
immutable(string)
const(char[])
char[]
shared(foo85)
const(char[26])
const(char[26])
immutable(char[26])
immutable(char[26])
string
int[3]
---
*/

import core.stdc.stdio;

class C { }

int ctfe() { return 3; }

template TypeTuple(T...) { alias T TypeTuple; }

/************************************/

void showf(string f)
{
    printf("%.*s\n", cast(int)f.length, f.ptr);
}

/************************************/

void test1()
{
    const int* p;
    const(int)* cp;
    cp = p;
}

/************************************/

void test2()
{
    const int i = 3;
    const(int) j = 0;
    int k;
//    j = i;
    k = i;
    k = j;
//    assert(k == 3);
}

/************************************/

void test3()
{
    char[3] p;
    const(char)[] q;

    q = p;
}

/************************************/

void test4()
{
    char[] p;
    const(char)[] q;

    q = p;
}

/************************************/

void test5()
{
    const(int**)* p;
    const(int)*** cp;
    p = cp;
}

/************************************/

void test6()
{
    const(int***)[] p;
    const(int)***[] cp;
    p = cp;
}

/************************************/

class C8 { }

void foo8(const char[] s, const C8 c, const int x)
{
}

void test8()
{
    import core.demangle : demangle;
    auto p = &foo8;
    showf(p.mangleof);
    assert(typeof(p).mangleof == "PFxAaxC9testconst2C8xiZv");
    assert(demangle(p.mangleof) == "void function(const(char[]), const(testconst.C8), const(int))* testconst.test8().p");
}

/************************************/

void test9()
{
    int [ const (char[]) ] aa;
    int [ char[] ] ab;
    int [ const char[] ] ac;

    aa["hello"] = 3;
    ab["hello"] = 3;
    ac["hello"] = 3;
}

/************************************/

void test10()
{
    const(int) x = 3;
    auto y = x;
    //y++;
    assert(is(typeof(y) == const(int)));
}

/************************************/

void foo11(in char[] a1)
{
    char[3] c;
    char[] a2 = c[];
    a2[0..2] = a1;
    a2[0..2] = 'c';

    const char b = 'b';
    a2[0..2] = b;
}

void test11()
{
}

/************************************/

void foo12(const char[] a1)
{
}

void test12()
{
    foo12("hello");
}

/************************************/

immutable char[16] hexdigits1 = "0123456789ABCDE1";
immutable char[  ] hexdigits2 = "0123456789ABCDE2";

const char[16] hexdigits3 = "0123456789ABCDE3";
const char[  ] hexdigits4 = "0123456789ABCDE4";

void test13()
{
}

/************************************/

void test14()
{
    string s;
    s = s ~ "hello";
}

/************************************/

class Foo15
{
    const string xxxx;

    this(immutable char[] aaa)
    {
        this.xxxx = aaa;
    }
}

void test15()
{
}

/************************************/

void test16()
{
    auto a = "abc";
    immutable char[3] b = "abc";
    const char[3] c = "abc";
}

/************************************/

void test17()
{
    const(char)[3][] a = (["abc", "def"]);

    assert(a[0] == "abc");
    assert(a[1] == "def");
}

/************************************/

class C18
{
    const(char)[] foo() { return "abc"; }
}

class D18 : C18
{
    override char[] foo() { return null; }
}

void test18()
{
}

/************************************/

void test19()
{
    char[] s;
    if (s == "abc")
        s = null;
    if (s < "abc")
        s = null;
    if (s is "abc")
        s = null;
}

/************************************/

void test20()
{
    string p;
    immutable char[] q;

    p = p ~ q;
    p ~= q;
}

/************************************/

void test21()
{
    string s;
    char[] p;
    p = s.dup;
}

/************************************/

void fill22(const(char)[] s)
{
}

void test22()
{
}

/************************************/


struct S23
{
    int x;
    int* p;
}


void foo23(const(S23) s, const(int) i)
{
    immutable int j = 3;
//    j = 4;
//    i = 4;
//    s.x = 3;
//    *s.p = 4;
}

void test23()
{
}

/************************************/

void test24()
{
    wchar[] r;

    r ~= "\000"w;
}

/************************************/

void test25()
{
    char* p;
    if (p == cast(const(char)*)"abc")
    {}
}

/************************************/

void test26()
{
    struct S
    {
        char[3] a;
    }

    static S s = { "abc" };
}

/************************************/

class C27
{
    int x;

    void foo() { x = 3; }
}

void test27()
{
    C27 d = new C27;
    d.foo();
}

/************************************/

class C28
{
    int x;

    void foo() immutable { }
}

void test28()
{
    immutable(C28) d = cast(immutable)new C28;
    d.foo();
}

/************************************/

struct S29 { }

int foo29(const(S29)* s)
{
    S29 s2;
    return *s == s2;
}

void test29()
{
}

/************************************/

struct S30
{
    int x;

    void foo() { x = 3; }
    void bar() const
    {   //x = 4;
        //this.x = 5;
    }
}

class C30
{
    int x;

    void foo() { x = 3; }
    void bar() const
    {   //x = 4;
        //this.x = 5;
    }
}

void test30()
{   S30 s;

    s.foo();
    s.bar();

    S30 t;
    //t.foo();
    t.bar();

    C30 c = new C30;

    c.foo();
    c.bar();

    C30 d = new C30;
    d.foo();
    d.bar();
}


/************************************/

class Foo31
{
  int x;

  immutable immutable(int)* geti()
  {
    return &x;
  }

  const const(int)* getc()
  {
    return &x;
  }
}

void test31()
{
}

/************************************/

int bar32;

struct Foo32
{
    int func() immutable
    {
        return bar32;
    }
}

void test32()
{
    immutable(Foo32) foo;
    printf("%d", foo.func());
    printf("%d", foo.func());
}

/************************************/

void test33()
{
    string d = "a"  ~  "b"  ~  (1?"a":"");
    assert(d == "aba");
}

/************************************/

struct S34
{
   int value;
}

const S34 s34 = { 5 };

const S34 t34 = s34;
const S34 u34 = s34;

void test34()
{
    assert(u34.value == 5);
}

/************************************/

const int i35 = 20;

template Foo35(alias bar)
{
}

alias Foo35!(i35) foo35;

void test35()
{
}

/************************************/

immutable char[10] digits    = "0123456789";  /// 0..9
immutable char[]  octdigits = digits[0 .. 8]; //// 0..7

void test36()
{
}

/************************************/

void test37()
{
    int i = 3;
    const int x = i;
    i++;
    assert(x == 3);
}

/************************************/

void test38()
{
    static const string s = "hello"[1..$];
    assert(s == "ello");
}

/************************************/

static const int x39;
const int y39;

shared static this()
{
    x39 = 3;
    y39 = 4;
}

void test39()
{
    const int i;
    assert(x39 == 3);
    assert(y39 == 4);
    const p = &x39;
//    assert(*p == 3);
}

/************************************/

struct S40
{
    int a;
    const int b = 3;    // shouldn't be allocated
}

void test40()
{
    assert(S40.sizeof == 8);
    assert(S40.init.b == 3);
}

/************************************/

struct S41
{
    int a;
    const int b;
    static const int c = ctfe() + 1;
}

void test41()
{
    assert(S41.sizeof == 8);
    S41 s;
    assert(s.b == 0);
    assert(S41.c == 4);

    const(int)*p;
    p = &s.b;
    assert(*p == 0);
    p = &s.c;
    assert(*p == 4);
}

/************************************/

class C42
{
    int a = ctfe() - 2;
    const int b;
    const int c = ctfe();
    static const int d;
    static const int e = ctfe() + 2;

    shared static this()
    {
        d = 4;
    }

    this()
    {
        b = 2;
    }
}

void test42()
{
    printf("%zd\n", C42.classinfo.initializer.length);
    assert(C42.classinfo.initializer.length == 12 + (void*).sizeof +
        (void*).sizeof);
    C42 c = new C42;
    assert(c.a == 1);
    assert(c.b == 2);
    assert(c.c == 3);
    assert(c.d == 4);
    assert(c.e == 5);

    const(int)*p;
    p = &c.b;
    assert(*p == 2);
    p = &c.c;
    assert(*p == 3);
    p = &c.d;
    assert(*p == 4);
    p = &c.e;
    assert(*p == 5);
}

/************************************/

template Foo43(T)
{
    alias T Foo43;
}

void test43()
{
  {
    int x;
    alias Foo43!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == int));
    assert(is(f == int));
  }

  {
    const int x;
    alias Foo43!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == const(int)));
    assert(is(f == const(int)));
  }

  {
    immutable int x;
    alias Foo43!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == immutable(int)));
    assert(is(f == immutable(int)));
  }
}

/************************************/

template Foo44(T:T)
{
    alias T Foo44;
}

void test44()
{
  {
    int x;
    alias Foo44!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == int));
    assert(is(f == int));
  }

  {
    const int x;
    alias Foo44!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == const(int)));
    assert(is(f == const(int)));
  }

  {
    immutable int x;
    alias Foo44!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == immutable(int)));
    assert(is(f == immutable(int)));
  }
}

/************************************/

template Foo45(T:const(T))
{
    alias T Foo45;
}

void test45()
{
  {
    int x;
    alias Foo45!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == int));
    assert(is(f == int));
  }

  {
    const int x;
    alias Foo45!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == const(int)));
    assert(is(f == int));
  }

  {
    immutable int x;
    alias Foo45!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == immutable(int)));
    assert(is(f == int));
  }
}

/************************************/

template Foo46(T:immutable(T))
{
    alias T Foo46;
}

void test46()
{
  {
    immutable int x;
    alias Foo46!(typeof(x)) f;
    showf(typeid(f).toString());
    assert(is(typeof(x) == immutable(int)));
    assert(is(f == int));
  }
}

/************************************/

template Foo47(T:T)            { const int Foo47 = 2; }
template Foo47(T:const(T))     { const int Foo47 = 3; }
template Foo47(T:immutable(T)) { const int Foo47 = 4; }

void test47()
{
    int x2;
    const int x3;
    immutable int x4;

    printf("%d\n", Foo47!(typeof(x2)));
    printf("%d\n", Foo47!(typeof(x3)));
    printf("%d\n", Foo47!(typeof(x4)));

    assert(Foo47!(typeof(x2)) == 2);
    assert(Foo47!(typeof(x3)) == 3);
    assert(Foo47!(typeof(x4)) == 4);
}

/************************************/

int foo48(T)(const(T) t) { return 3; }

void test48()
{
    const int x = 4;
    assert(foo48(x) == 3);
}

/************************************/

void foo49(T)(T[] t)
{
    showf(typeid(typeof(t)).toString());
    assert(is(T == immutable(char)));
}

void bar49(T)(const T t)
{
    showf(typeid(T).toString());
    assert(is(T == const(int)) || is(T == immutable(int)) || is(T == int));
}

void test49()
{
    string s;
    foo49(s);
    foo49("hello");

    const int c = 1;
    bar49(c);

    immutable int i = 1;
    bar49(i);

    bar49(1);
}

/************************************/

void foo50(T)(T t)
{
    showf(typeid(typeof(t)).toString());
    assert(is(T == C));
}

void baz50(T)(T t)
{
    showf(typeid(typeof(t)).toString());
    assert(is(T == const(C)));
}

void bar50(T)(const T t)
{
    showf(typeid(T).toString());
    showf(typeid(typeof(t)).toString());
    assert(is(T == C));
    assert(is(typeof(t) == const(C)));
}

void abc50(T)(const T t)
{
    showf(typeid(T).toString());
    showf(typeid(typeof(t)).toString());
    assert(is(T == C));
    assert(is(typeof(t) == const(C)));
}

void test50()
{
    C c = new C;
    const(C) d = new C;

    foo50(c);
    baz50(d);

    bar50(c);
    abc50(d);
}

/************************************/

void test51()
{
    const(C) d = new C;
    //d = new C;
}

/************************************/

template isStaticArray(T)
{
    const bool isStaticArray = false;
}

template isStaticArray(T : T[N], size_t N)
{
    const bool isStaticArray = true;
}

template isDynamicArray(T, U = void)
{
    static const isDynamicArray = false;
}

template isDynamicArray(T : U[], U)
{
  static const isDynamicArray = !isStaticArray!(T);
}

void test52()
{
    immutable(char[5])[int] aa = ([3:"hello", 4:"betty"]);

    showf(typeid(typeof(aa.values)).toString());
    static assert(isDynamicArray!(typeof(aa.values)));
}

/************************************/

void foo53(string n) { }

void read53(in string name)
{
    foo53(name);
}

void test53()
{
    read53("hello");
}

/************************************/

void bar54(const(wchar)[] s) { }

void test54()
{
    const(wchar)[] fmt;
    bar54("Orphan format specifier: %" ~ fmt);
}

/************************************/

struct S55
{
    int foo() { return 1; }
    int foo() const { return 2; }
    int foo() immutable { return 3; }
}

void test55()
{
    S55 s1;
    auto i = s1.foo();
    assert(i == 1);

    const S55 s2;
    i = s2.foo();
    assert(i == 2);

    immutable S55 s3;
    i = s3.foo();
    assert(i == 3);
}

/************************************/

const struct S56 { int a; }

void test56()
{
    S56 s;
    S56 t;
    printf("S56.sizeof = %zd\n", S56.sizeof);
    //t = s;
}

/************************************/

struct S57
{
    const void foo(this T)(int i)
    {
        showf(typeid(T).toString());
        if (i == 1)
            assert(is(T == const));
        if (i == 2)
            assert(!is(T == const));
        if (i == 3)
            assert(is(T == immutable));
    }
}

void test57()
{
    const(S57) s;
    (&s).foo(1);
    S57 s2;
    s2.foo(2);
    immutable(S57) s3;
    s3.foo(3);
}

/************************************/

class C58
{
    const(C58) c;
    const C58 y;

    this()
    {
        y = null;
        c = null;
    }

    const void foo()
    {
        //c = null; // should fail
    }

    void bar()
    {
        //c = null;
    }
}

void test58()
{
}

/************************************/

class A59
{
    int[] a;
    this() { a.length = 1; }

    int* ptr() { return a.ptr; }
    const const(int)* ptr() { return a.ptr; }
    immutable immutable(int)* ptr() { return a.ptr; }
}

void test59()
{
    auto a = new A59;
    const b = cast(const)new A59;
    immutable c = cast(immutable)new A59;
}

/************************************/

int foo60(int i) { return 1; }
int foo60(const int i) { return 2; }
int foo60(immutable int i) { return 3; }

void test60()
{
    int i;
    const int j;
    immutable int k;

    assert(foo60(i) == 1);
    assert(foo60(j) == 2);
    assert(foo60(k) == 3);
}

/************************************/

void foo61(T)(T arg)
{
    alias const(T) CT;
    assert(is(const(int) == CT));
    //writeln(typeid(const(T)));
    //writeln(typeid(CT));
}

void test61()
{
    int x = 42;
    foo61(x);
}

/************************************/

class Foo62(T) { }

void test62()
{
    const(Foo62!(int)) f = new Foo62!(int);
    assert(is(typeof(f) == const(Foo62!(int))));
}

/************************************/

struct S63
{
    int x;
}

void foo63(const ref S63 scheme)
{
    //scheme.x = 3;
}

void test63()
{
        S63 scheme;
        foo63(scheme);
}

/************************************/

struct S64
{
    int a;
    long b;
}

void test64()
{
    S64 s1 = S64(2,3);
    const s2 = S64(2,3);
    immutable S64 s3 = S64(2,3);

    s1 = s1;
    s1 = s2;
    s1 = s3;
}

/************************************/

struct S65
{
    int a;
    long b;
    char *p;
}

void test65()
{   char c;
    S65 s1 = S65(2,3);
    const s2 = S65(2,3);
    immutable S65 s3 = S65(2,3);

    S65 t1 = S65(2,3,null);
    const t2 = S65(2,3,null);
    immutable S65 t3 = S65(2,3,null);
}

/************************************/

struct S66
{
    int a;
    long b;
}

void test66()
{   char c;
    S66 s1 = S66(2,3);
    const s2 = S66(2,3);
    immutable S66 s3 = S66(2,3);

    S66 t1 = s1;
    S66 t2 = s2;
    S66 t3 = s3;

    const(S66) u1 = s1;
    const(S66) u2 = s2;
    const(S66) u3 = s3;

    immutable(S66) v1 = s1;
    immutable(S66) v2 = s2;
    immutable(S66) v3 = s3;
}

/************************************/

struct Foo68
{
    int z;
    immutable int x;
    int y;
}

void test68()
{
    Foo68 bar;
    bar.y = 2;
}

/************************************/

class C69 {}
struct S69 {}

void test69()
{
        immutable(S69)* si;
        S69* sm;
        bool a = si is sm;

        immutable(C69) ci;
        const(C69) cm;
        bool b = ci is cm;
}

/************************************/

struct S70
{
  int i;
}

void test70()
{
  S70 s;
  const(S70) cs = s;
  S70 s1 = cs;
  S70 s2 = cast(S70)cs;
}

/************************************/

void test72()
{
    int a;
    const int b;
    enum { int c = 0 }
    immutable int d = 0;

    assert(__traits(isSame, a, a));
    assert(__traits(isSame, b, b));
    assert(__traits(isSame, c, c));
    assert(__traits(isSame, d, d));
}

/************************************/

void a73(const int [] data...)
{
    a73(1);
}

void test73()
{
}

/************************************/

struct S74 { int x; }

const S74 s_const = {0};

S74 s_mutable = s_const;

void test74()
{
}

/************************************/

struct A75 { int x; }
struct B75 { A75 s1; }

const A75 s1_const = {0};
const B75 s2_const = {s1_const};

void test75()
{
}

/************************************/

void test76()
{
    int[int] array;
    const(int[int]) a = array;
}

/************************************/

void test77()
{
    int[][] intArrayArray;
    int[][][] intArrayArrayArray;

//    const(int)[][] f1 = intArrayArray;
    const(int[])[] f2 = intArrayArray;

//    const(int)[][][] g1 = intArrayArrayArray;
//    const(int[])[][] g2 = intArrayArrayArray;
    const(int[][])[] g3 = intArrayArrayArray;
}

/************************************/

void foo78(T)(const(T)[] arg1, const(T)[] arg2) { }

void test78()
{
    foo78("hello", "world".dup);
    foo78("hello", "world");
    foo78("hello".dup, "world".dup);
    foo78(cast(const)"hello", cast(const)"world");
}

/************************************/

const bool[string] stopWords79;

shared static this()
{
    stopWords79 = [ "a"[]:1 ];
}

void test79()
{
    "abc" in stopWords79;
}

/************************************/

void test80(inout(int) _ = 0)
{
    char x;
    inout(char) y = x;

    const(char)[] c;
    immutable(char)[] i;
    shared(char)[] s;
    const(shared(char))[] sc;
    inout(char)[] w;
    inout(shared(char))[] sw;

    c = c;
    c = i;
    static assert(!__traits(compiles, c = s));
    static assert(!__traits(compiles, c = sc));
    c = w;
    static assert(!__traits(compiles, c = sw));

    static assert(!__traits(compiles, i = c));
    i = i;
    static assert(!__traits(compiles, i = s));
    static assert(!__traits(compiles, i = sc));
    static assert(!__traits(compiles, i = w));
    static assert(!__traits(compiles, i = sw));

    static assert(!__traits(compiles, s = c));
    static assert(!__traits(compiles, s = i));
    s = s;
    static assert(!__traits(compiles, s = sc));
    static assert(!__traits(compiles, s = w));
    static assert(!__traits(compiles, s = sw));

    static assert(!__traits(compiles, sc = c));
    sc = i;
    sc = s;
    sc = sc;
    static assert(!__traits(compiles, sc = w));
    sc = sw;

    static assert(!__traits(compiles, w = c));
    static assert(!__traits(compiles, w = i));
    static assert(!__traits(compiles, w = s));
    static assert(!__traits(compiles, w = sc));
    w = w;
    static assert(!__traits(compiles, w = sw));

    static assert(!__traits(compiles, sw = c));
    static assert(!__traits(compiles, sw = i));
    static assert(!__traits(compiles, sw = s));
    static assert(!__traits(compiles, sw = sc));
    static assert(!__traits(compiles, sw = w));
    sw = sw;
}

/************************************/

void test81(inout(int) _ = 0)
{
    const(char)* c;
    immutable(char)* i;
    shared(char)* s;
    const(shared(char))* sc;
    inout(char)* w;

    c = c;
    c = i;
    static assert(!__traits(compiles, c = s));
    static assert(!__traits(compiles, c = sc));
    c = w;

    static assert(!__traits(compiles, i = c));
    i = i;
    static assert(!__traits(compiles, i = s));
    static assert(!__traits(compiles, i = sc));
    static assert(!__traits(compiles, i = w));

    static assert(!__traits(compiles, s = c));
    static assert(!__traits(compiles, s = i));
    s = s;
    static assert(!__traits(compiles, s = sc));
    static assert(!__traits(compiles, s = w));

    static assert(!__traits(compiles, sc = c));
    sc = i;
    sc = s;
    sc = sc;
    static assert(!__traits(compiles, sc = w));

    static assert(!__traits(compiles, w = c));
    static assert(!__traits(compiles, w = i));
    static assert(!__traits(compiles, w = s));
    static assert(!__traits(compiles, w = sc));
    w = w;
}

/************************************/


void test82(inout(int) _ = 0)
{
    const(immutable(char)*) c;
    pragma(msg, typeof(c));
    static assert(typeof(c).stringof == "const(immutable(char)*)");

    inout(immutable(char)*) d;
    pragma(msg, typeof(d));
    static assert(typeof(d).stringof == "inout(immutable(char)*)");

    inout(const(char)*) e;

    pragma(msg, typeof(e));
    static assert(is(typeof(e) == inout(const(char)*)));
    static assert(typeof(e).stringof == "inout(const(char)*)");

    pragma(msg, typeof(*e));
    static assert(is(typeof(*e) == inout(const(char))));
    static assert(typeof(*e).stringof == "inout(const(char))");

    inout const(char)* f;
    static assert(is(typeof(e) == typeof(f)));

    inout(shared(char)) g;
    pragma(msg, typeof(g));
    static assert(typeof(g).stringof == "shared(inout(char))");

    shared(inout(char)) h;
    pragma(msg, typeof(h));
    static assert(typeof(h).stringof == "shared(inout(char))");

    inout(immutable(char)) i;
    pragma(msg, typeof(i));
    static assert(typeof(i).stringof == "immutable(char)");

    immutable(inout(char)) j;
    pragma(msg, typeof(j));
    static assert(typeof(j).stringof == "immutable(char)");

    inout(const(char)) k;
    pragma(msg, typeof(k));
    static assert(typeof(k).stringof == "inout(const(char))");

    const(inout(char)) l;
    pragma(msg, typeof(l));
    static assert(typeof(l).stringof == "inout(const(char))");

    shared(const(char)) m;
    pragma(msg, typeof(m));
    static assert(typeof(m).stringof == "shared(const(char))");

    const(shared(char)) n;
    pragma(msg, typeof(n));
    static assert(typeof(n).stringof == "shared(const(char))");

    inout(char*****) o;
    pragma(msg, typeof(o));
    static assert(typeof(o).stringof == "inout(char*****)");
    pragma(msg, typeof(cast()o));
    static assert(typeof(cast()o).stringof == "inout(char****)*");

    const(char*****) p;
    pragma(msg, typeof(p));
    static assert(typeof(p).stringof == "const(char*****)");
    pragma(msg, typeof(cast()p));
    static assert(typeof(cast()p).stringof == "const(char****)*");

    immutable(char*****) q;
    pragma(msg, typeof(q));
    static assert(typeof(q).stringof == "immutable(char*****)");
    pragma(msg, typeof(cast()q));
    static assert(typeof(cast()q).stringof == "immutable(char****)*");

    shared(char*****) r;
    pragma(msg, typeof(r));
    static assert(typeof(r).stringof == "shared(char*****)");
    pragma(msg, typeof(cast()r));
    static assert(typeof(cast()r).stringof == "shared(char****)*");
    pragma(msg, typeof(cast(const)r));
    static assert(typeof(cast(const)r).stringof == "const(shared(char****)*)");
    pragma(msg, typeof(cast(const shared)r));
    static assert(typeof(cast(const shared)r).stringof == "shared(const(char*****))");
    pragma(msg, typeof(cast(shared)r));
    static assert(typeof(cast(shared)r).stringof == "shared(char*****)");
    pragma(msg, typeof(cast(immutable)r));
    static assert(typeof(cast(immutable)r).stringof == "immutable(char*****)");
    pragma(msg, typeof(cast(inout)r));
    static assert(typeof(cast(inout)r).stringof == "inout(shared(char****)*)");

    inout(shared(char**)***) s;
    pragma(msg, typeof(s));
    static assert(typeof(s).stringof == "inout(shared(char**)***)");
    pragma(msg, typeof(***s));
    static assert(typeof(***s).stringof == "shared(inout(char**))");
}

/************************************/

void test83(inout(int) _ = 0)
{
    static assert( __traits(compiles, typeid(int* function(inout int))));
    static assert( __traits(compiles, typeid(int* delegate(inout int))));
    static assert(!__traits(compiles, typeid(inout(int*) function(int))));
    static assert(!__traits(compiles, typeid(inout(int*) delegate(int))));
    static assert(!__traits(compiles, typeid(inout(int*) function())));
    static assert(!__traits(compiles, typeid(inout(int*) delegate())));
    inout(int*) function(inout(int)) fp;
    inout(int*) delegate(inout(int)) dg;
}

/************************************/

inout(char[]) foo84(inout char[] s) { return s; }

void test84()
{
    char[] m;
    const(char)[] c;
    string s;
    auto r = foo84(s);
    pragma(msg, typeof(r).stringof);
    static assert(typeof(r).stringof == "immutable(string)");

    pragma(msg, typeof(foo84(c)).stringof);
    static assert(typeof(foo84(c)).stringof == "const(char[])");

    pragma(msg, typeof(foo84(m)).stringof);
    static assert(typeof(foo84(m)).stringof == "char[]");
}

/************************************/

class foo85 { }

alias shared foo85 Y85;

void test85()
{
   pragma(msg, Y85);
   shared(foo85) x = new Y85;
}

/************************************/

struct foo87
{
    int bar(T)(T t){ return 1; }
    int bar(T)(T t) shared { return 2; }
}

void test87()
{
    foo87 x;
    auto i = x.bar(1);
    assert(i == 1);
    shared foo87 y;
    i = y.bar(1);
    assert(i == 2);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=2751


void test88(immutable(int[3]) a)
{
    const(char)[26] abc1 = "abcdefghijklmnopqrstuvwxyz";
    const(char[26]) abc2 = "abcdefghijklmnopqrstuvwxyz";
    immutable(const(char)[26]) abc3 = "abcdefghijklmnopqrstuvwxyz";
    const(immutable(char)[26]) abc4 = "abcdefghijklmnopqrstuvwxyz";

    auto abc5 = cast()"abcdefghijklmnopqrstuvwxyz";

    pragma(msg, typeof(abc1).stringof);
    pragma(msg, typeof(abc2).stringof);
    pragma(msg, typeof(abc3).stringof);
    pragma(msg, typeof(abc4).stringof);
    pragma(msg, typeof(abc5).stringof);

    static assert(is(typeof(abc1) == typeof(abc2)));
    static assert(is(typeof(abc1) == const(char[26])));
    static assert(is(typeof(abc3) == typeof(abc4)));
    static assert(is(typeof(abc3) == immutable(char[26])));

    auto b = cast()a;
    pragma(msg, typeof(b).stringof);
    static assert(is(typeof(b) == int[3]));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=3748

// version = error8;
// version = error11;

class C3748
{
    private int _x;
    this(int x) { this._x = x; }
    @property inout(int)* xptr() inout { return &_x; }
    @property void x(int newval) { _x = newval; }
}

struct S3748
{
    int x;
    immutable int y = 5;
    const int z = 6;
    C3748 c;

    inout(int)* getX() inout return
    {
        static assert(!__traits(compiles, {
            x = 4;
        }));
        return &x;
    }
    inout(int)* getCX(C3748 otherc) inout
    {
        inout(C3748) c2 = c;    // typeof(c) == inout(C3748)
        static assert(!__traits(compiles, {
            inout(C3748) err2 = new C3748(1);
        }));
        static assert(!__traits(compiles, {
            inout(C3748) err3 = otherc;
        }));

        auto v1 = getLowestXptr(c, otherc);
        static assert(is(typeof(v1) == const(int)*));
        auto v2 = getLowestXptr(c, c);
        static assert(is(typeof(v2) == inout(int)*));

        alias typeof(return) R;
        static assert(!__traits(compiles, {
            c.x = 4;
        }));
        static assert(!__traits(compiles, {
            R r = otherc.xptr;
        }));
        static assert(!__traits(compiles, {
            R r = &y;
        }));
        static assert(!__traits(compiles, {
            R r = &z;
        }));

        return c2.xptr;
    }

    version(error8)
        inout(int) err8;    // see fail_compilation/failinout3748a.d
}

inout(int)* getLowestXptr(inout(C3748) c1, inout(C3748) c2)
{
    inout(int)* x1 = c1.xptr;
    inout(int)* x2 = c2.xptr;
    if(*x1 <= *x2)
        return x1;
    return x2;
}

ref inout(int) getXRef(inout(C3748) c1, inout(C3748) c2)
{
    return *getLowestXptr(c1, c2);
}

void test3748()
{
    S3748 s;
    s.c = new C3748(1);
    const(S3748)* sp = &s;
    auto s2 = new S3748;
    s2.x = 3;
    s2.c = new C3748(2);
    auto s3 = cast(immutable(S3748)*) s2;

    auto v1 = s.getX;
    static assert(is(typeof(v1) == int*));
    auto v2 = sp.getX;
    static assert(is(typeof(v2) == const(int)*));
    auto v3 = s3.getX;
    static assert(is(typeof(v3) == immutable(int)*));

    static assert(!__traits(compiles, {
        int *err9 = sp.getX;
    }));
    static assert(!__traits(compiles, {
        int *err10 = s3.getX;
    }));
    version(error11)
        inout(int)* err11;  // see fail_compilation/failinout3748b.d

    auto v4 = getLowestXptr(s.c, s3.c);
    static assert(is(typeof(v4) == const(int)*));
    auto v5 = getLowestXptr(s.c, s.c);
    static assert(is(typeof(v5) == int*));
    auto v6 = getLowestXptr(s3.c, s3.c);
    static assert(is(typeof(v6) == immutable(int)*));

    getXRef(s.c, s.c) = 3;
}

/************************************/

void test3748a(inout int = 1)
{
                 int[]    ma;
           inout(int[])   wa;
           const(int[])   ca;
       immutable(int[])   ia;
          shared(int[])   sa;
    shared(inout(int[])) swa;
    shared(const(int[])) sca;

    static foo1(E)(inout(E[]) a) { return E.init; }
    static assert( is( typeof(foo1( ma)) == int));
    static assert( is( typeof(foo1( wa)) == int));
    static assert( is( typeof(foo1( ca)) == int));
    static assert( is( typeof(foo1( ia)) == int));
    static assert( is( typeof(foo1( sa)) == shared int));
    static assert( is( typeof(foo1(swa)) == shared int));
    static assert( is( typeof(foo1(sca)) == shared int));

    static foo2(E)(shared inout(E[]) a) { return E.init; }
    static assert(!is( typeof(foo2( ma)) ));
    static assert(!is( typeof(foo2( wa)) ));
    static assert(!is( typeof(foo2( ca)) ));
    static assert( is( typeof(foo2( ia)) == int));
    static assert( is( typeof(foo2( sa)) == int));
    static assert( is( typeof(foo2(swa)) == int));
    static assert( is( typeof(foo2(sca)) == int));
}

void test3748b(inout int = 1)
{
    // Top of the parameter type is non-ref & qualified
    static        inout(int[])  foo1(       inout(int[])  a);
    static shared(inout(int[])) bar1(shared(inout(int[])) a);

    // Top of the parameter type is non-ref & un-qualified
    static        inout(int) [] foo2(       inout(int) [] a);
    static shared(inout(int))[] bar2(shared(inout(int))[] a);

    // Top of the argument type is qualified
                 int[]    ma1;
           inout(int[])   wa1;
           const(int[])   ca1;
          shared(int[])   sa1;
    shared(inout(int[])) swa1;
    shared(const(int[])) sca1;
       immutable(int[])   ia1;

    // Top of the argument type is un-qualified
                 int  []  ma2;
           inout(int) []  wa2;
           const(int) []  ca2;
          shared(int) []  sa2;
    shared(inout(int))[] swa2;
    shared(const(int))[] sca2;
       immutable(int) []  ia2;

    // --> non-ref qualified param VS qualified arg
    static assert( is( typeof(foo1( ma1)) == typeof( ma1) ));
    static assert( is( typeof(foo1( wa1)) == typeof( wa1) ));
    static assert( is( typeof(foo1( ca1)) == typeof( ca1) ));
    static assert( is( typeof(bar1( sa1)) == typeof( sa1) ));
    static assert( is( typeof(bar1(swa1)) == typeof(swa1) ));
    static assert( is( typeof(bar1(sca1)) == typeof(sca1) ));
    static assert( is( typeof(foo1( ia1)) == typeof( ia1) ));

    // --> non-ref un-qualified param VS qualified arg
    static assert( is( typeof(foo2( ma1)) == typeof( ma2) ));
    static assert( is( typeof(foo2( wa1)) == typeof( wa2) ));
    static assert( is( typeof(foo2( ca1)) == typeof( ca2) ));
    static assert( is( typeof(bar2( sa1)) == typeof( sa2) ));
    static assert( is( typeof(bar2(swa1)) == typeof(swa2) ));
    static assert( is( typeof(bar2(sca1)) == typeof(sca2) ));
    static assert( is( typeof(foo2( ia1)) == typeof( ia2) ));

    // --> non-ref qualified param VS un-qualified arg
    static assert( is( typeof(foo1( ma2)) == typeof( ma1) ));
    static assert( is( typeof(foo1( wa2)) ));
    static assert( is( typeof(foo1( ca2)) ));
    static assert( is( typeof(bar1( sa2)) == typeof( sa1) ));
    static assert( is( typeof(bar1(swa2)) ));
    static assert( is( typeof(bar1(sca2)) ));
    static assert( is( typeof(foo1( ia2)) ));

    // --> non-ref un-qualified param VS un-qualified arg
    static assert( is( typeof(foo2( ma2)) == typeof( ma2) ));
    static assert( is( typeof(foo2( wa2)) == typeof( wa2) ));
    static assert( is( typeof(foo2( ca2)) == typeof( ca2) ));
    static assert( is( typeof(bar2( sa2)) == typeof( sa2) ));
    static assert( is( typeof(bar2(swa2)) == typeof(swa2) ));
    static assert( is( typeof(bar2(sca2)) == typeof(sca2) ));
    static assert( is( typeof(foo2( ia2)) == typeof( ia2) ));
}

void test3748c(inout int = 1)
{
    // Top of the parameter type is ref & qualified
    static        inout(int[])  foo1(ref        inout(int[])  a);
    static shared(inout(int[])) bar1(ref shared(inout(int[])) a);

    // Top of the parameter type is ref & un-qualified
    static        inout(int) [] foo2(ref        inout(int) [] a);
    static shared(inout(int))[] bar2(ref shared(inout(int))[] a);

    // Top of the argument type is qualified
                 int[]    ma1;
           inout(int[])   wa1;
           const(int[])   ca1;
          shared(int[])   sa1;
    shared(inout(int[])) swa1;
    shared(const(int[])) sca1;
       immutable(int[])   ia1;

    // Top of the argument type is un-qualified
                 int  []  ma2;
           inout(int) []  wa2;
           const(int) []  ca2;
          shared(int) []  sa2;
    shared(inout(int))[] swa2;
    shared(const(int))[] sca2;
       immutable(int) []  ia2;

    // --> ref qualified param VS qualified arg
    static assert( is( typeof(foo1( ma1)) == typeof( ma1) ));
    static assert( is( typeof(foo1( wa1)) == typeof( wa1) ));
    static assert( is( typeof(foo1( ca1)) == typeof( ca1) ));
    static assert( is( typeof(bar1( sa1)) == typeof( sa1) ));
    static assert( is( typeof(bar1(swa1)) == typeof(swa1) ));
    static assert( is( typeof(bar1(sca1)) == typeof(sca1) ));
    static assert( is( typeof(foo1( ia1)) == typeof( ia1) ));

    // --> ref un-qualified param VS qualified arg
    static assert( is( typeof(foo2( ma1)) == typeof( ma2) ));
    static assert(!is( typeof(foo2( wa1)) ));
    static assert(!is( typeof(foo2( ca1)) ));
    static assert(!is( typeof(bar2( sa1)) ));
    static assert(!is( typeof(bar2(swa1)) ));
    static assert(!is( typeof(bar2(sca1)) ));
    static assert(!is( typeof(foo2( ia1)) ));

    // --> ref qualified param VS un-qualified arg
    static assert( is( typeof(foo1( ma2)) == typeof( ma1) ));
    static assert(!is( typeof(foo1( wa2)) ));
    static assert(!is( typeof(foo1( ca2)) ));  // why this is OK? --> [*]
    static assert(!is( typeof(bar1( sa2)) ));
    static assert(!is( typeof(bar1(swa2)) ));
    static assert(!is( typeof(bar1(sca2)) ));
    static assert(!is( typeof(foo1( ia2)) ));

    // --> ref un-qualified param VS un-qualified arg
    static assert( is( typeof(foo2( ma2)) == typeof( ma2) ));
    static assert( is( typeof(foo2( wa2)) == typeof( wa2) ));
    static assert( is( typeof(foo2( ca2)) == typeof( ca2) ));
    static assert( is( typeof(bar2( sa2)) == typeof( sa2) ));
    static assert( is( typeof(bar2(swa2)) == typeof(swa2) ));
    static assert( is( typeof(bar2(sca2)) == typeof(sca2) ));
    static assert( is( typeof(foo2( ia2)) == typeof( ia2) ));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=4968

void test4968()
{
    inout(int) f1(inout(int) i) { return i; }
    int mi;
    const int ci;
    immutable int ii;
    static assert(is(typeof(f1(mi)) == int));
    static assert(is(typeof(f1(ci)) == const(int)));
    static assert(is(typeof(f1(ii)) == immutable(int)));

    inout(int)* f2(inout(int)* p) { return p; }
    int* mp;
    const(int)* cp;
    immutable(int)* ip;
    static assert(is(typeof(f2(mp)) == int*));
    static assert(is(typeof(f2(cp)) == const(int)*));
    static assert(is(typeof(f2(ip)) == immutable(int)*));

    inout(int)[] f3(inout(int)[] a) { return a; }
    int[] ma;
    const(int)[] ca;
    immutable(int)[] ia;
    static assert(is(typeof(f3(ma)) == int[]));
    static assert(is(typeof(f3(ca)) == const(int)[]));
    static assert(is(typeof(f3(ia)) == immutable(int)[]));

    inout(int)[1] f4(inout(int)[1] sa) { return sa; }
    int[1] msa;
    const int[1] csa;
    immutable int[1] isa;
    static assert(is(typeof(f4(msa)) == int[1]));
    static assert(is(typeof(f4(csa)) == const(int)[1]));
    static assert(is(typeof(f4(isa)) == immutable(int)[1]));

    inout(int)[string] f5(inout(int)[string] aa) { return aa; }
    int[string] maa;
    const(int)[string] caa;
    immutable(int)[string] iaa;
    static assert(is(typeof(f5(maa)) == int[string]));
    static assert(is(typeof(f5(caa)) == const(int)[string]));
    static assert(is(typeof(f5(iaa)) == immutable(int)[string]));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=1961

inout(char)[] strstr(inout(char)[] source, const(char)[] pattern)
{
    /*
     * this would be an error, as const(char)[] is not implicitly castable to
     * inout(char)[]
     */
    // return pattern;

    for(int i = 0; i + pattern.length <= source.length; i++)
    {
        inout(char)[] tmp = source[i..pattern.length]; // ok
        if (tmp == pattern)         // ok, tmp implicitly casts to const(char)[]
            return source[i..$];    // implicitly casts back to call-site source
    }
    return source[$..$];            // to be consistent with strstr.
}

void test1961a()
{
    auto a = "hello";
    a = strstr(a, "llo");   // cf (constancy factor) == immutable
    static assert(!__traits(compiles, { char[] b = strstr(a, "llo"); }));
                            // error, cannot cast immutable to mutable
    char[] b = "hello".dup;
    b = strstr(b, "llo");   // cf == mutable (note that "llo" doesn't play a role
                            // because that parameter is not inout)
    const(char)[] c = strstr(b, "llo");
                            // cf = mutable, ok because mutable
                            // implicitly casts to const
    c = strstr(a, "llo");   // cf = immutable, ok immutable casts to const
}

inout(T) min(T)(inout(T) a, inout(T) b)
{
    return a < b ? a : b;
}

void test1961b()
{
    immutable(char)[] i = "hello";
    const(char)[] c = "there";
    char[] m = "Walter".dup;

    static assert(!__traits(compiles, { i = min(i, c); }));
                            // error, since i and c vary in constancy, the result
                            // is const, and you cannot implicitly cast const to immutable.

    c = min(i, c);          // ok, cf == const, because not homogeneous
    c = min(m, c);          // ok, cf == const
    c = min(m, i);          // ok, cf == const
    i = min(i, "blah");     // ok, cf == immutable, homogeneous
    static assert(!__traits(compiles, { m = min(m, c); }));
                            // error, cf == const because not homogeneous.
    static assert(!__traits(compiles, { m = min(m, "blah"); }));
                            // error, cf == const
    m = min(m, "blah".dup); // ok
}

inout(T) min2(int i, int j, T)(inout(T) a, inout(T) b)
{
    //pragma(msg, "(", i, ", ", j, ") = ", T);
    static assert(is(T == char[]));
    return a < b ? a : b;
}

template seq(T...){ alias T seq; }

void test1961c()
{
    immutable(char[]) iia = "hello1";
    immutable(char)[] ima = "hello2";
    const(char[]) cca = "there1";
    const(char)[] cma = "there2";
    char[] mma = "Walter".dup;

    foreach (i, x; seq!(iia, ima, cca, cma, mma))
    foreach (j, y; seq!(iia, ima, cca, cma, mma))
    {
        min2!(i, j)(x, y);
        //pragma(msg, "x: ",typeof(x), ", y: ",typeof(y), " -> ", typeof(min2(x, y)), " : ", __traits(compiles, min2(x, y)));
    }
}

/************************************/

inout(int) function(inout(int))   notinoutfun1() { return null; }
inout(int) function(inout(int))[] notinoutfun2() { return null; }
inout(int) delegate(inout(int))   notinoutfun3() { return null; }
inout(int) delegate(inout(int))[] notinoutfun4() { return null; }
void notinoutfun1(inout(int) function(inout(int))   fn) {}
void notinoutfun2(inout(int) function(inout(int))[] fn) {}
void notinoutfun3(inout(int) delegate(inout(int))   dg) {}
void notinoutfun4(inout(int) delegate(inout(int))[] dg) {}

void test88()
{
    inout(int) function(inout int) fp;
    inout(int) delegate(inout int) dg;

    inout(int) function(inout int)*   fp2p;
    inout(int) function(inout int)[]  fp2a;
    inout(int) function(inout int)[3] fp2s;

    inout(int) delegate(inout int)*   dg3p;
    inout(int) delegate(inout int)[]  dg3a;
    inout(int) delegate(inout int)[3] dg3s;

    int delegate() inout*   dg4p;
    int delegate() inout[]  dg4a;
    int delegate() inout[3] dg4s;

    static assert(!__traits(compiles, { inout(int)* p; }));
    static assert(!__traits(compiles, { inout(int delegate()) dg; }));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=4251

void test4251a()
{
    alias int T;

    static assert(!is( immutable(T)** : const(T)** ));  // NG, tail difference
    static assert( is( immutable(T)** : const(T**) ));  // OK, tail to const

    static assert( is(           T *** :           T *** ));    // OK, tail is same
    static assert(!is(           T *** :     const(T)*** ));
    static assert(!is(           T *** :     const(T*)** ));
    static assert( is(           T *** :     const(T**)* ));    // OK, tail to const
    static assert( is(           T *** :     const(T***) ));    // OK, tail to const
    static assert(!is(           T *** : immutable(T)*** ));
    static assert(!is(           T *** : immutable(T*)** ));
    static assert(!is(           T *** : immutable(T**)* ));
    static assert(!is(           T *** : immutable(T***) ));

    static assert(!is(     const(T)*** :           T *** ));
    static assert( is(     const(T)*** :     const(T)*** ));    // OK, tail is same
    static assert(!is(     const(T)*** :     const(T*)** ));
    static assert( is(     const(T)*** :     const(T**)* ));    // OK, tail to const
    static assert( is(     const(T)*** :     const(T***) ));    // OK, tail to const
    static assert(!is(     const(T)*** : immutable(T)*** ));
    static assert(!is(     const(T)*** : immutable(T*)** ));
    static assert(!is(     const(T)*** : immutable(T**)* ));
    static assert(!is(     const(T)*** : immutable(T***) ));

    static assert(!is(     const(T*)** :           T *** ));
    static assert(!is(     const(T*)** :     const(T)*** ));
    static assert( is(     const(T*)** :     const(T*)** ));    // OK, tail is same
    static assert( is(     const(T*)** :     const(T**)* ));    // OK, tail to const
    static assert( is(     const(T*)** :     const(T***) ));    // OK, tail to const
    static assert(!is(     const(T*)** : immutable(T)*** ));
    static assert(!is(     const(T*)** : immutable(T*)** ));
    static assert(!is(     const(T*)** : immutable(T**)* ));
    static assert(!is(     const(T*)** : immutable(T***) ));

    static assert(!is(     const(T**)* :           T *** ));
    static assert(!is(     const(T**)* :     const(T)*** ));
    static assert(!is(     const(T**)* :     const(T*)** ));
    static assert( is(     const(T**)* :     const(T**)* ));    // OK, tail is same
    static assert( is(     const(T**)* :     const(T***) ));    // OK, tail is same
    static assert(!is(     const(T**)* : immutable(T)*** ));
    static assert(!is(     const(T**)* : immutable(T*)** ));
    static assert(!is(     const(T**)* : immutable(T**)* ));
    static assert(!is(     const(T**)* : immutable(T***) ));

    static assert(!is(     const(T***) :           T *** ));
    static assert(!is(     const(T***) :     const(T)*** ));
    static assert(!is(     const(T***) :     const(T*)** ));
    static assert( is(     const(T***) :     const(T**)* ));    // OK, tail is same
    static assert( is(     const(T***) :     const(T***) ));    // OK, tail is same
    static assert(!is(     const(T***) :           T *** ));
    static assert(!is(     const(T***) : immutable(T)*** ));
    static assert(!is(     const(T***) : immutable(T*)** ));
    static assert(!is(     const(T***) : immutable(T**)* ));
    static assert(!is(     const(T***) : immutable(T***) ));

    static assert(!is( immutable(T)*** :           T *** ));
    static assert(!is( immutable(T)*** :     const(T)*** ));
    static assert(!is( immutable(T)*** :     const(T*)** ));
    static assert( is( immutable(T)*** :     const(T**)* ));    // OK, tail to const
    static assert( is( immutable(T)*** :     const(T***) ));    // OK, tail to const
    static assert( is( immutable(T)*** : immutable(T)*** ));    // OK, tail is same
    static assert(!is( immutable(T)*** : immutable(T*)** ));
    static assert(!is( immutable(T)*** : immutable(T**)* ));
    static assert(!is( immutable(T)*** : immutable(T***) ));

    static assert(!is( immutable(T*)** :           T *** ));
    static assert(!is( immutable(T*)** :     const(T)*** ));
    static assert(!is( immutable(T*)** :     const(T*)** ));
    static assert( is( immutable(T*)** :     const(T**)* ));    // OK, tail to const
    static assert( is( immutable(T*)** :     const(T***) ));    // OK, tail to const
    static assert(!is( immutable(T*)** : immutable(T)*** ));
    static assert( is( immutable(T*)** : immutable(T*)** ));    // OK, tail is same
    static assert(!is( immutable(T*)** : immutable(T**)* ));
    static assert(!is( immutable(T*)** : immutable(T***) ));

    static assert(!is( immutable(T**)* :           T *** ));
    static assert(!is( immutable(T**)* :     const(T)*** ));
    static assert(!is( immutable(T**)* :     const(T*)** ));
    static assert( is( immutable(T**)* :     const(T**)* ));    // OK, tail to const
    static assert( is( immutable(T**)* :     const(T***) ));    // OK, tail to const
    static assert(!is( immutable(T**)* : immutable(T)*** ));
    static assert(!is( immutable(T**)* : immutable(T*)** ));
    static assert( is( immutable(T**)* : immutable(T**)* ));    // OK, tail is same
    static assert( is( immutable(T**)* : immutable(T***) ));    // OK, tail is same

    static assert(!is( immutable(T***) :           T *** ));
    static assert(!is( immutable(T***) :     const(T)*** ));
    static assert(!is( immutable(T***) :     const(T*)** ));
    static assert( is( immutable(T***) :     const(T**)* ));    // OK, tail to const
    static assert( is( immutable(T***) :     const(T***) ));    // OK, tail to const
    static assert(!is( immutable(T***) : immutable(T)*** ));
    static assert(!is( immutable(T***) : immutable(T*)** ));
    static assert( is( immutable(T***) : immutable(T**)* ));    // OK, tail is same
    static assert( is( immutable(T***) : immutable(T***) ));    // OK, tail is same

    static assert( is( immutable(int)** : const(immutable(int)*)* ));   // OK, tail to const

    // shared level should be same
    static assert(!is( shared(T)*** :        const(T***)  ));   // NG, tail to const but shared level is different
    static assert( is( shared(T***) : shared(const(T***)) ));   // OK, tail to const and shared level is same

    // head qualifier difference is ignored
    static assert(is( shared(int)* : shared(int*) ));
    static assert(is( inout (int)* : inout (int*) ));

    //
    static assert(!is( T** : T*** ));
    static assert(!is( T[]** : T*** ));
}

void test4251b()
{
    class C {}
    class D : C {}

    static assert(!is( C[]* : const(C)[]* ));
    static assert( is( C[]* : const(C[])* ));

    // derived class to const(base class) in tail
    static assert( is( D[]  : const(C)[] ));
    static assert( is( D[]* : const(C[])* ));

    static assert( is( D*  : const(C)* ));
    static assert( is( D** : const(C*)* ));

    // derived class to const(base interface) in tail
    interface I {}
    class X : I {}
    static assert(!is( X[] : const(I)[] ));

    // interface to const(base interface) in tail
    interface J {}
    interface K : I, J {}
    static assert( is( K[] : const(I)[] )); // OK, runtime offset is same
    static assert(!is( K[] : const(J)[] )); // NG, runtime offset is different
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=5473

void test5473()
{
    class C
    {
        int b;
        void f(){}
        static int x;
        static void g(){};
    }
    struct S
    {
        int b;
        void f(){}
        static int x;
        static void g(){};
    }

    void dummy();
    alias typeof(dummy) VoidFunc;

    const C c = new C;
    const S s;

    foreach (a; TypeTuple!(c, s))
    {
        alias typeof(a) A;

        static assert(is(typeof(a.b) == const int));    // const(int)
        static assert(is(typeof(a.f) == VoidFunc));
        static assert(is(typeof(a.x) == int));
        static assert(is(typeof(a.g) == VoidFunc));

        static assert(is(typeof((const A).b) == const int));    // int, should be const(int)
        static assert(is(typeof((const A).f) == VoidFunc));
        static assert(is(typeof((const A).x) == int));
        static assert(is(typeof((const A).g) == VoidFunc));
    }
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=5493

void test5493()
{
    // non template function
    void pifun(immutable(char)[]* a) {}
    void rifun(ref immutable(char)[] a) {}

    void pcfun(const(char)[]* a) {}
    void rcfun(ref const(char)[] a) {}

    immutable char[] buf1 = "hello";
    static assert(!__traits(compiles, pifun(buf1)));
    static assert(!__traits(compiles, pcfun(buf1)));
    static assert(!__traits(compiles, rifun(buf1)));
    static assert(!__traits(compiles, rcfun(buf1)));

    immutable char[5] buf2 = "hello";
    static assert(!__traits(compiles, pifun(buf2)));
    static assert(!__traits(compiles, pcfun(buf2)));
    static assert(!__traits(compiles, rifun(buf2)));
    static assert(!__traits(compiles, rcfun(buf2)));

    const char[] buf3 = "hello";
    static assert(!__traits(compiles, pcfun(buf3)));
    static assert(!__traits(compiles, rcfun(buf3)));

    const char[5] buf4 = "hello";
    static assert(!__traits(compiles, pcfun(buf4)));
    static assert(!__traits(compiles, rcfun(buf4)));

    // template function
    void pmesswith(T)(const(T)[]* ts, const(T) t)
    {
        *ts ~= t;
    }
    void rmesswith(T)(ref const(T)[] ts, const(T) t)
    {
        ts ~= t;
    }
    class C
    {
        int x;
        this(int i) immutable { x = i; }
    }
    C[] cs;
    immutable C ci = new immutable(C)(6);
    assert (ci.x == 6);
    static assert(!__traits(compiles, pmesswith(&cs,ci)));
    static assert(!__traits(compiles, rmesswith(cs,ci)));
    //cs[$-1].x = 14;
    //assert (ci.x == 14); //whoops.
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=5493 + inout

void test5493inout()
{
    int m;
    const(int) c;
    immutable(int) i;

    inout(int) ptrfoo(inout(int)** a, inout(int)* b)
    {
        *a = b;
        return 0;   // dummy
    }
    inout(int) reffoo(ref inout(int)* a, inout(int)* b)
    {
        a = b;
        return 0;   // dummy
    }

    // wild matching: inout == mutable
    int* pm;
                                      ptrfoo(&pm, &m);    assert(pm == &m);
    static assert(!__traits(compiles, ptrfoo(&pm, &c)));
    static assert(!__traits(compiles, ptrfoo(&pm, &i)));
                                      reffoo( pm, &m);    assert(pm == &m);
    static assert(!__traits(compiles, reffoo( pm, &c)));
    static assert(!__traits(compiles, reffoo( pm, &i)));

    // wild matching: inout == const
    const(int)* pc;
    ptrfoo(&pc, &m);    assert(pc == &m);
    ptrfoo(&pc, &c);    assert(pc == &c);
    ptrfoo(&pc, &i);    assert(pc == &i);
    reffoo( pc, &m);    assert(pc == &m);
    reffoo( pc, &c);    assert(pc == &c);
    reffoo( pc, &i);    assert(pc == &i);

    // wild matching: inout == immutable
    immutable(int)* pi;
    static assert(!__traits(compiles, ptrfoo(&pi, &m)));
    static assert(!__traits(compiles, ptrfoo(&pi, &c)));
                                      ptrfoo(&pi, &i);    assert(pi == &i);
    static assert(!__traits(compiles, reffoo( pi, &m)));
    static assert(!__traits(compiles, reffoo( pi, &c)));
                                      reffoo( pi, &i);    assert(pi == &i);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6782

struct Tuple6782(T...)
{
    T field;
    alias field this;
}
auto tuple6782(T...)(T field)
{
    return Tuple6782!T(field);
}

struct Range6782a
{
    int *ptr;
    @property inout(int)* front() inout { return ptr; }
    @property bool empty() const { return ptr is null; }
    void popFront() { ptr = null; }
}
struct Range6782b
{
    Tuple6782!(int, int*) e;
    @property front() inout { return e; }
    @property empty() const { return e[1] is null; }
    void popFront() { e[1] = null; }
}

void test6782()
{
    int x = 5;
    auto r1 = Range6782a(&x);
    foreach(p; r1) {}

    auto r2 = Range6782b(tuple6782(1, &x));
    foreach(i, p; r2) {}
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6864

int fn6864( const int n) { return 1; }
int fn6864(shared int n) { return 2; }
inout(int) fw6864(inout int s) { return 1; }
inout(int) fw6864(shared inout int s) { return 2; }

void test6864()
{
    int n;
    assert(fn6864(n) == 1);
    assert(fw6864(n) == 1);

    shared int sn;
    assert(fn6864(sn) == 2);
    assert(fw6864(sn) == 2);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6865

shared(inout(int)) foo6865(shared(inout(int)) n){ return n; }
void test6865()
{
    shared(const(int)) n;
    static assert(is(typeof(foo6865(n)) == shared(const(int))));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6866

struct S6866
{
    const(char)[] val;
    alias val this;
}
inout(char)[] foo6866(inout(char)[] s) { return s; }

void test6866()
{
    S6866 s;
    static assert(is(typeof(foo6866(s)) == const(char)[]));
    // Assertion failure: 'targ' on line 2029 in file 'mtype.c'
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6867

inout(char)[] test6867(inout(char)[] a)
{
   foreach(dchar d; a) // No error if 'dchar' is removed
   {
       foreach(c; a) // line 5
       {
       }
   }
   return [];
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6870

void test6870()
{
   shared(int) x;
   static assert(is(typeof(x) == shared(int))); // pass
   const(typeof(x)) y;
   const(shared(int)) z;
   static assert(is(typeof(y) == typeof(z))); // fail!
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6338
// https://issues.dlang.org/show_bug.cgi?id=6922

alias int T;

static assert(is( immutable(       T )  == immutable(T) ));
static assert(is( immutable( const(T))  == immutable(T) )); // 6922
static assert(is( immutable(shared(T))  == immutable(T) ));
static assert(is( immutable( inout(T))  == immutable(T) ));
static assert(is(        immutable(T)   == immutable(T) ));
static assert(is(  const(immutable(T))  == immutable(T) )); // 6922
static assert(is( shared(immutable(T))  == immutable(T) )); // 6338
static assert(is(  inout(immutable(T))  == immutable(T) ));

static assert(is( immutable(shared(const(T))) == immutable(T) ));
static assert(is( immutable(const(shared(T))) == immutable(T) ));
static assert(is( shared(immutable(const(T))) == immutable(T) ));
static assert(is( shared(const(immutable(T))) == immutable(T) ));
static assert(is( const(shared(immutable(T))) == immutable(T) ));
static assert(is( const(immutable(shared(T))) == immutable(T) ));

static assert(is( immutable(shared(inout(T))) == immutable(T) ));
static assert(is( immutable(inout(shared(T))) == immutable(T) ));
static assert(is( shared(immutable(inout(T))) == immutable(T) ));
static assert(is( shared(inout(immutable(T))) == immutable(T) ));
static assert(is( inout(shared(immutable(T))) == immutable(T) ));
static assert(is( inout(immutable(shared(T))) == immutable(T) ));

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6912

void test6912()
{
    //                 From                      To
    static assert( is(                 int []  :                 int []  ));
    static assert(!is(           inout(int []) :                 int []  ));
    static assert(!is(                 int []  :           inout(int []) ));
    static assert( is(           inout(int []) :           inout(int []) ));

    static assert( is(                 int []  :           const(int)[]  ));
    static assert( is(           inout(int []) :           const(int)[]  ));
    static assert(!is(                 int []  :     inout(const(int)[]) ));
    static assert( is(           inout(int []) :     inout(const(int)[]) ));

    static assert( is(           const(int)[]  :           const(int)[]  ));
    static assert( is(     inout(const(int)[]) :           const(int)[]  ));
    static assert(!is(           const(int)[]  :     inout(const(int)[]) ));
    static assert( is(     inout(const(int)[]) :     inout(const(int)[]) ));

    static assert( is(       immutable(int)[]  :           const(int)[]  ));
    static assert( is( inout(immutable(int)[]) :           const(int)[]  ));
    static assert( is(       immutable(int)[]  :     inout(const(int)[]) ));
    static assert( is( inout(immutable(int)[]) :     inout(const(int)[]) ));

    static assert( is(       immutable(int)[]  :       immutable(int)[]  ));
    static assert( is( inout(immutable(int)[]) :       immutable(int)[]  ));
    static assert( is(       immutable(int)[]  : inout(immutable(int)[]) ));
    static assert( is( inout(immutable(int)[]) : inout(immutable(int)[]) ));

    static assert( is(           inout(int)[]  :           inout(int)[]  ));
    static assert( is(     inout(inout(int)[]) :           inout(int)[]  ));
    static assert( is(           inout(int)[]  :     inout(inout(int)[]) ));
    static assert( is(     inout(inout(int)[]) :     inout(inout(int)[]) ));

    static assert( is(           inout(int)[]  :           const(int)[]  ));
    static assert( is(     inout(inout(int)[]) :           const(int)[]  ));
    static assert( is(           inout(int)[]  :     inout(const(int)[]) ));
    static assert( is(     inout(inout(int)[]) :     inout(const(int)[]) ));

    //                 From                         To
    static assert( is(                 int [int]  :                 int [int]  ));
    static assert(!is(           inout(int [int]) :                 int [int]  ));
    static assert(!is(                 int [int]  :           inout(int [int]) ));
    static assert( is(           inout(int [int]) :           inout(int [int]) ));

    static assert( is(                 int [int]  :           const(int)[int]  ));
    static assert(!is(           inout(int [int]) :           const(int)[int]  ));
    static assert(!is(                 int [int]  :     inout(const(int)[int]) ));
    static assert( is(           inout(int [int]) :     inout(const(int)[int]) ));

    static assert( is(           const(int)[int]  :           const(int)[int]  ));
    static assert(!is(     inout(const(int)[int]) :           const(int)[int]  ));
    static assert(!is(           const(int)[int]  :     inout(const(int)[int]) ));
    static assert( is(     inout(const(int)[int]) :     inout(const(int)[int]) ));

    static assert( is(       immutable(int)[int]  :           const(int)[int]  ));
    static assert(!is( inout(immutable(int)[int]) :           const(int)[int]  ));
    static assert(!is(       immutable(int)[int]  :     inout(const(int)[int]) ));
    static assert( is( inout(immutable(int)[int]) :     inout(const(int)[int]) ));

    static assert( is(       immutable(int)[int]  :       immutable(int)[int]  ));
    static assert(!is( inout(immutable(int)[int]) :       immutable(int)[int]  ));
    static assert(!is(       immutable(int)[int]  : inout(immutable(int)[int]) ));
    static assert( is( inout(immutable(int)[int]) : inout(immutable(int)[int]) ));

    static assert( is(           inout(int)[int]  :           inout(int)[int]  ));
    static assert(!is(     inout(inout(int)[int]) :           inout(int)[int]  ));
    static assert(!is(           inout(int)[int]  :     inout(inout(int)[int]) ));
    static assert( is(     inout(inout(int)[int]) :     inout(inout(int)[int]) ));

    static assert( is(           inout(int)[int]  :           const(int)[int]  ));
    static assert(!is(     inout(inout(int)[int]) :           const(int)[int]  ));
    static assert(!is(           inout(int)[int]  :     inout(const(int)[int]) ));
    static assert( is(     inout(inout(int)[int]) :     inout(const(int)[int]) ));

    // Regression check
    static assert( is( const(int)[] : const(int[]) ) );

    //                 From                     To
    static assert( is(                 int *  :                 int *  ));
    static assert(!is(           inout(int *) :                 int *  ));
    static assert(!is(                 int *  :           inout(int *) ));
    static assert( is(           inout(int *) :           inout(int *) ));

    static assert( is(                 int *  :           const(int)*  ));
    static assert( is(           inout(int *) :           const(int)*  ));
    static assert(!is(                 int *  :     inout(const(int)*) ));
    static assert( is(           inout(int *) :     inout(const(int)*) ));

    static assert( is(           const(int)*  :           const(int)*  ));
    static assert( is(     inout(const(int)*) :           const(int)*  ));
    static assert(!is(           const(int)*  :     inout(const(int)*) ));
    static assert( is(     inout(const(int)*) :     inout(const(int)*) ));

    static assert( is(       immutable(int)*  :           const(int)*  ));
    static assert( is( inout(immutable(int)*) :           const(int)*  ));
    static assert( is(       immutable(int)*  :     inout(const(int)*) ));
    static assert( is( inout(immutable(int)*) :     inout(const(int)*) ));

    static assert( is(       immutable(int)*  :       immutable(int)*  ));
    static assert( is( inout(immutable(int)*) :       immutable(int)*  ));
    static assert( is(       immutable(int)*  : inout(immutable(int)*) ));
    static assert( is( inout(immutable(int)*) : inout(immutable(int)*) ));

    static assert( is(           inout(int)*  :           inout(int)*  ));
    static assert( is(     inout(inout(int)*) :           inout(int)*  ));
    static assert( is(           inout(int)*  :     inout(inout(int)*) ));
    static assert( is(     inout(inout(int)*) :     inout(inout(int)*) ));

    static assert( is(           inout(int)*  :           const(int)*  ));
    static assert( is(     inout(inout(int)*) :           const(int)*  ));
    static assert( is(           inout(int)*  :     inout(const(int)*) ));
    static assert( is(     inout(inout(int)*) :     inout(const(int)*) ));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6930

void test6930a()
{
    inout(const int) f1(inout(const int) i) { return i; }
              int mi;
        const int ci;
    immutable int ii;
    static assert(is(typeof(f1(mi)) ==     const(int)));
    static assert(is(typeof(f1(ci)) ==     const(int)));
    static assert(is(typeof(f1(ii)) == immutable(int)));

    inout(const int)* f2(inout(const int)* p) { return p; }
              int * mp;
        const(int)* cp;
    immutable(int)* ip;
    static assert(is(typeof(f2(mp)) ==     const(int)*));
    static assert(is(typeof(f2(cp)) ==     const(int)*));
    static assert(is(typeof(f2(ip)) == immutable(int)*));

    inout(const int)[] f3(inout(const int)[] a) { return a; }
              int [] ma;
        const(int)[] ca;
    immutable(int)[] ia;
    static assert(is(typeof(f3(ma)) ==     const(int)[]));
    static assert(is(typeof(f3(ca)) ==     const(int)[]));
    static assert(is(typeof(f3(ia)) == immutable(int)[]));

    inout(const int[1]) f4(inout(const int[1]) sa) { return sa; }
              int[1] msa;
        const int[1] csa;
    immutable int[1] isa;
    static assert(is(typeof(f4(msa)) ==     const(int)[1]));
    static assert(is(typeof(f4(csa)) ==     const(int)[1]));
    static assert(is(typeof(f4(isa)) == immutable(int)[1]));

    inout(const int)[string] f5(inout(const int)[string] aa) { return aa; }
              int [string] maa;
        const(int)[string] caa;
    immutable(int)[string] iaa;
    static assert(is(typeof(f5(maa)) ==     const(int)[string]));
    static assert(is(typeof(f5(caa)) ==     const(int)[string]));
    static assert(is(typeof(f5(iaa)) == immutable(int)[string]));
}

inout(const(int[])) foo6930(inout(int)[] x)
{
    bool condition = cast(bool)(x.length / 2);
    return condition ? x : new immutable(int[])(2);
}

void test6930b(inout int = 0)
{
    alias T1 = inout(shared(const(int)));
    static assert(T1.stringof == "shared(inout(const(int)))");
    static assert(is(T1 == shared) && is(T1 == const) && is(T1 == inout));

    alias T2 = const(shared(inout(int)[]));
    static assert(T2.stringof == "shared(const(inout(int)[]))");
    static assert(is(T2 == shared) && is(T2 == const) && !is(T2 == inout) && is(typeof(T2.init[0]) == inout));

              int [] ma;
        const(int)[] ca;
    immutable(int)[] ia;
        inout(int)[] wa;
    static assert(is(typeof(foo6930(ma)) ==       const int[]));
    static assert(is(typeof(foo6930(ca)) ==       const int[]));
    static assert(is(typeof(foo6930(ia)) ==   immutable int[]));
    static assert(is(typeof(foo6930(wa)) == inout const int[]));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=11868

void f11868(A...)(A) { }

void g11868(inout(const(int))[] arr)
{
    f11868(arr[0]);
}

void test11868()
{
    auto arr = [1,2,3];
    g11868(arr);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=11924

inout(StringType) localize11924(StringType)(inout StringType str, string locale)
{
    return str;
}

struct S11924
{
    static menuItem_1(ARGS...)()
    {
        enum itemTitle = ARGS;
    }

    static content_left_1()
    {
        menuItem!(localize11924("Home", ""));
    }
    alias menuItem = menuItem_1;
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=11966

inout(char)[] stripped11966 (inout(char)[] path)
{
    return path;
}

struct PathParser11966
{
    inout(const(char))[] path() inout
    {
        return null;
    }

    inout(const(char))[] pop() inout
    {
        return stripped11966(path);
    }
}

void test11966()
{
    auto a = PathParser11966().pop();
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=14788

auto make14788(K, V)(inout V[K] aa)
{
    static struct Result
    {
        V[K] aa;
        ref front() inout { return aa[1]; }
    }
    return inout Result(aa);
}

void test14788()
{
    int[int] aa = [1:1];
    make14788(aa).front();
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=12089

void foo12089(inout(char[]) a)
{
    validate12089(a);
}
void validate12089(S)(in S str)
{
    decodeImpl12089(str);
}
void decodeImpl12089(S)(auto ref S str)
{}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=12524

inout(int) dup12524(inout(const(int)) val)
{
    return val;
}

void test12524(inout(int))
{
    inout(const(int)) val;

    auto bug = dup12524(val);

    static assert(is(typeof(bug) == inout(int)));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6941

static assert((const(shared(int[])[])).stringof == "const(shared(int[])[])");   // fail
static assert((const(shared(int[])[])).stringof != "const(shared(const(int[]))[])"); // fail

static assert((inout(shared(int[])[])).stringof == "inout(shared(int[])[])");   // fail
static assert((inout(shared(int[])[])).stringof != "inout(shared(inout(int[]))[])");    // fail

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6872

static assert((shared(inout(int)[])).stringof == "shared(inout(int)[])");
static assert((shared(inout(const(int)[]))).stringof == "shared(inout(const(int)[]))");
static assert((shared(inout(const(int)[])[])).stringof == "shared(inout(const(int)[])[])");
static assert((shared(inout(const(immutable(int)[])[])[])).stringof == "shared(inout(const(immutable(int)[])[])[])");

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6939

void test6939()
{
    shared    int* x;
    immutable int* y;
    const     int* z;
    static assert( is(typeof(1?x:y) == shared(const(int))*));  // fail
    static assert(!is(typeof(1?x:y) == const(int)*));          // fail
    static assert(!is(typeof(1?x:z)));                         // fail

    shared    int[] a;
    immutable int[] b;
    const     int[] c;
    static assert( is(typeof(1?a:b) == shared(const(int))[])); // pass (ok)
    static assert(!is(typeof(1?a:b) == const(int)[]));         // pass (ok)
    static assert(!is(typeof(1?a:c)));                         // fail
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6940

void test6940()
{
    immutable(int*)*    x;
    int**               y;
    static assert(is(typeof(x) : const(int*)*)); // ok
    static assert(is(typeof(y) : const(int*)*)); // ok
    static assert(is(typeof(1?x:y) == const(int*)*));

    immutable(int[])[]  a;
    int[][]             b;
    static assert(is(typeof(a) : const(int[])[])); // ok
    static assert(is(typeof(b) : const(int[])[])); // ok
    static assert(is(typeof(1?a:b) == const(int[])[]));

    immutable(int)**    v;
    int**               w;
    static assert(is(typeof(1?v:w) == const(int*)*));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=6982

void test6982()
{
    alias int Bla;
    immutable(Bla[string]) ifiles = ["a":1, "b":2, "c":3];
    static assert(!__traits(compiles, { immutable(Bla)[string] files = ifiles; }));  // (1)
    static assert(!__traits(compiles, { ifiles.remove ("a"); }));                    // (2)

          immutable(int)[int]  maa;
    const(immutable(int)[int]) caa;
    immutable(      int [int]) iaa;
    static assert(!__traits(compiles, { maa = iaa; }));
    static assert(!__traits(compiles, { maa = caa; }));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=7038

static assert(is(S7038 == const));
const struct S7038{ int x; }
static assert(is(S7038 == const));

shared struct S7038b{ int x; }
static assert(is(S7038b == shared));

immutable struct S7038c{ int x; }
static assert(is(S7038c == immutable));

// https://issues.dlang.org/show_bug.cgi?id=22515
// Classes fixed for consistency with structs
static assert(is(C7038 == const));
const class C7038{ int x; }
static assert(is(C7038 == const));

void test7038()
{
    S7038 s;
    static assert(is(typeof(s) == const));
    static assert(is(typeof(s.x) == const int));

    C7038 c;
    static assert(is(typeof(c) == const));
    static assert(is(typeof(c.x) == const int));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=7105

void copy(inout(int)** tgt, inout(int)* src){ *tgt = src; }

void test7105()
{
    int* pm;
    int m;
    copy(&pm, &m);
    assert(pm == &m);

    const(int)* pc;
    const(int) c;
    copy(&pc, &c);
    assert(pc == &c);

    immutable(int)* pi;
    immutable(int) i;
    copy(&pi, &i);
    assert(pi == &i);

    static assert(!__traits(compiles, copy(&pm, &c)));
    static assert(!__traits(compiles, copy(&pm, &i)));

    copy(&pc, &m);
    assert(pc == &m);
    copy(&pc, &i);
    assert(pc == &i);

    static assert(!__traits(compiles, copy(&pi, &m)));
    static assert(!__traits(compiles, copy(&pi, &c)));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=7202

void test7202()
{
    void writeln(string s) @system { printf("%.*s\n", cast(int)s.length, s.ptr); }
    void delegate() @system x = { writeln("I am @system"); };
    void delegate() @safe y = {  };
    auto px = &x;
    auto py = &y;
    static assert(!__traits(compiles, px = py)); // accepts-invalid
    *px = x;
    y(); // "I am @system" -> no output, OK
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=7554

T outer7554(T)(immutable T function(T) pure foo) pure {
    pure int inner() {
        return foo(5);
    }
    return inner();
}
int sqr7554(int x) pure {
    return x * x;
}
void test7554()
{
    assert(outer7554(&sqr7554) == 25);

    immutable(int function(int) pure) ifp = &sqr7554;
}

/************************************/

bool empty(T)(in T[] a)
{
    assert(is(T == shared(string)));
    return false;
}


void test7518() {
    shared string[] stuff;
    stuff.empty();
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=7669

shared(inout U)[n] id7669(U, size_t n)( shared(inout U)[n] );
void test7669()
{
    static assert(is(typeof( id7669((shared(int)[3]).init)) == shared(int)[3]));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=7757

inout(int)      foo7757a(int x, lazy inout(int)      def) { return def; }
inout(int)[]    foo7757b(int x, lazy inout(int)[]    def) { return def; }
inout(int)[int] foo7757c(int x, lazy inout(int)[int] def) { return def; }

inout(T)      bar7757a(T)(T x, lazy inout(T)    def) { return def; }
inout(T)[]    bar7757b(T)(T x, lazy inout(T)[]  def) { return def; }
inout(T)[T]   bar7757c(T)(T x, lazy inout(T)[T] def) { return def; }

inout(Object) get7757(lazy inout(Object) defVal) { return null; }

void test7757()
{
          int       mx1  = foo7757a(1,2);
    const(int)      cx1  = foo7757a(1,2);
          int []    ma1  = foo7757b(1,[2]);
    const(int)[]    ca1  = foo7757b(1,[2]);
          int [int] maa1 = foo7757c(1,[2:3]);
    const(int)[int] caa1 = foo7757c(1,[2:3]);

          int       mx2  = bar7757a(1,2);
    const(int)      cx2  = bar7757a(1,2);
          int []    ma2  = bar7757b(1,[2]);
    const(int)[]    ca2  = bar7757b(1,[2]);
          int [int] maa2 = bar7757c(1,[2:3]);
    const(int)[int] caa2 = bar7757c(1,[2:3]);

    Object defObj = null;
    auto resObj = get7757(defObj);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=8098

class Outer8098
{
    int i = 6;

    class Inner
    {
        int y=0;

        void foo() const
        {
            static assert(is(typeof(this.outer) == const(Outer8098)));
            static assert(is(typeof(i) == const(int)));
            static assert(!__traits(compiles, ++i));
        }
    }

    Inner inner;

    this()
    {
        inner = new Inner;
    }
}

void test8098()
{
    const(Outer8098) x = new Outer8098();
    static assert(is(typeof(x) == const(Outer8098)));
    static assert(is(typeof(x.inner) == const(Outer8098.Inner)));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=8099

void test8099()
{
    static class Outer
    {
        class Inner {}
    }

    auto m = new Outer;
    auto c = new const(Outer);
    auto i = new immutable(Outer);

    auto mm = m.new Inner;            // m -> m  OK
    auto mc = m.new const(Inner);     // m -> c  OK
  static assert(!__traits(compiles, {
    auto mi = m.new immutable(Inner); // m -> i  bad
  }));

  static assert(!__traits(compiles, {
    auto cm = c.new Inner;            // c -> m  bad
  }));
    auto cc = c.new const(Inner);     // c -> c  OK
  static assert(!__traits(compiles, {
    auto ci = c.new immutable(Inner); // c -> i  bad
  }));

  static assert(!__traits(compiles, {
    auto im = i.new Inner;            // i -> m  bad
  }));
    auto ic = i.new const(Inner);     // i -> c  OK
    auto ii = i.new immutable(Inner); // i -> i  OK
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=8201

void test8201()
{
    uint[2] msa;
    immutable uint[2] isa = msa;

    ubyte[] buffer = [0, 1, 2, 3, 4, 5];
    immutable ubyte[4] iArr = buffer[0 .. 4];
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=8212

struct S8212 { int x; }

shared S8212 s8212;

shared int x8212;

void test8212()
{
   int y = x8212;
   S8212 s2 = s8212;
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=8366

class B8366
{
    bool foo(in Object o) const { return true; }
}

class C8366a : B8366
{
    bool foo(in Object o)              { return true; }
  override
    bool foo(in Object o) const        { return false; }
    bool foo(in Object o) immutable    { return true; }
    bool foo(in Object o) shared       { return true; }
    bool foo(in Object o) shared const { return true; }
}

class C8366b : B8366
{
    bool foo(in Object o)              { return false; }
    alias typeof(super).foo foo;
    bool foo(in Object o) immutable    { return false; }
    bool foo(in Object o) shared       { return false; }
    bool foo(in Object o) shared const { return false; }
}

void test8366()
{
    {
              C8366a mca = new C8366a();
        const C8366a cca = new C8366a();
              B8366  mb  = mca;
        const B8366  cb  = cca;
        assert(mca.foo(null) == true);
        assert(cca.foo(null) == false);
        assert(mb .foo(null) == false);
        assert(cb .foo(null) == false);
    }
    {
              C8366b mcb = new C8366b();
        const C8366b ccb = new C8366b();
              B8366  mb  = mcb;
        const B8366  cb  = ccb;
        assert(mcb.foo(null) == false);
        assert(ccb.foo(null) == true);
        assert(mb .foo(null) == true);
        assert(cb .foo(null) == true);
    }
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=8408

template hasMutableIndirection8408(T)
{
    template Unqual(T)
    {
             static if (is(T U == shared(const U))) alias U Unqual;
        else static if (is(T U ==        const U )) alias U Unqual;
        else static if (is(T U ==    immutable U )) alias U Unqual;
        else static if (is(T U ==        inout U )) alias U Unqual;
        else static if (is(T U ==       shared U )) alias U Unqual;
        else                                        alias T Unqual;
    }

    enum hasMutableIndirection8408 = !is(typeof({ Unqual!T t = void; immutable T u = t; }));
}
static assert(!hasMutableIndirection8408!(int));
static assert(!hasMutableIndirection8408!(int[3]));
static assert( hasMutableIndirection8408!(Object));

auto dup8408(E)(inout(E)[] arr) pure @trusted
{
    static if (hasMutableIndirection8408!E)
    {
        auto copy = new E[](arr.length);
        copy[] = cast(E[])arr[];        // assume constant
        return cast(inout(E)[])copy;    // assume constant
    }
    else
    {
        auto copy = new E[](arr.length);
        copy[] = arr[];
        return copy;
    }
}

void test8408()
{
    void test(E, bool constConv)()
    {
                  E[] marr = [E.init, E.init, E.init];
        immutable E[] iarr = [E.init, E.init, E.init];

                  E[] m2m = marr.dup8408();    assert(m2m == marr);
        immutable E[] i2i = iarr.dup8408();    assert(i2i == iarr);

      static if (constConv)
      { // If dup() hss strong purity, implicit conversion is allowed
        immutable E[] m2i = marr.dup8408();    assert(m2i == marr);
                  E[] i2m = iarr.dup8408();    assert(i2m == iarr);
      }
      else
      {
        static assert(!is(typeof({ immutable E[] m2i = marr.dup8408(); })));
        static assert(!is(typeof({           E[] i2m = iarr.dup8408(); })));
      }
    }

    class C {}
    struct S1 { long n; }
    struct S2 { int* p; }
    struct T1 { S1 s; }
    struct T2 { S2 s; }
    struct T3 { S1 s1;  S2 s2; }

/*
    test!(int   , false)();
    test!(int[3], false)();
    test!(C     , false)();
    test!(S1    , false)();
    test!(S2    , false)();
    test!(T1    , false)();
    test!(T2    , false)();
    test!(T3    , false)();
*/
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=8688

void test8688()
{
    alias TypeTuple!(int) T;
    foreach (i; TypeTuple!(0))
    {
        alias const(T[i]) X;
        static assert(!is(X == int));           // fails
        static assert( is(X == const(int)));    // fails
    }
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=10946 (regression by fixing bug 8688, from 2.061)

enum xlen10946 = 4;
alias immutable(char)[xlen10946] e3;
alias immutable(char[xlen10946]) e4; // NG -> OK
immutable vlen10946 = 4;
alias immutable(char)[vlen10946] i3;
alias immutable(char[vlen10946]) i4; // NG -> OK

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=9046

void test9046()
{
    foreach (T; TypeTuple!(byte, ubyte, short, ushort, int, uint, long, ulong, char, wchar, dchar,
                           float, double, real))
    foreach (U; TypeTuple!(T, const T, immutable T, shared T, shared const T, inout T, shared inout T))
    {
        static assert(is(typeof(U.init) == U));
    }

    foreach (T; TypeTuple!(int[], const(char)[], immutable(string[]), shared(const(int)[])[],
                           int[1], const(char)[1], immutable(string[1]), shared(const(int)[1])[],
                           int[int], const(char)[long], immutable(string[string]), shared(const(int)[double])[]))
    foreach (U; TypeTuple!(T, const T, immutable T, shared T, shared const T, inout T, shared inout T))
    {
        static assert(is(typeof(U.init) == U));
    }

    int i;
    enum E { x, y }
    static struct S {}
    static class  C {}
    struct NS { void f(){ i++; } }
    class  NC { void f(){ i++; } }
    foreach (T; TypeTuple!(E, S, C, NS, NC))
    foreach (U; TypeTuple!(T, const T, immutable T, shared T, shared const T, inout T, shared inout T))
    {
        static assert(is(typeof(U.init) == U));
    }

    alias TL = TypeTuple!(int, string, int[int]);
    foreach (U; TypeTuple!(TL, const TL, immutable TL, shared TL, shared const TL, inout TL, shared inout TL))
    {
        static assert(is(typeof(U.init) == U));
    }
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=9090

void test9090()
{
    void test1(T)(auto ref const T[] val) {}

    string a;
    test1(a);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=9461

void test9461()
{
    class A {}
    class B : A {}

    void conv(S, T)(ref S x) { T y = x; }

    // should be NG
    static assert(!__traits(compiles, conv!(inout(B)[],     inout(A)[])));
    static assert(!__traits(compiles, conv!(int[inout(B)],  int[inout(A)])));
    static assert(!__traits(compiles, conv!(inout(B)[int],  inout(A)[int])));
    static assert(!__traits(compiles, conv!(inout(B)*,      inout(A)*)));
    static assert(!__traits(compiles, conv!(inout(B)[1],    inout(A)[])));

    // should be OK
    static assert( __traits(compiles, conv!(inout(B),       inout(A))));
}

/************************************/

struct S9209 { int x; }

void bar9209(const S9209*) {}

void test9209() {
    const f = new S9209(1);
    bar9209(f);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=10758

struct X10758
{
static:
        inout(int)   screwUpVal(ref inout(int) wx) { return wx; }
    ref inout(int)   screwUpRef(return ref inout(int) wx) { return wx; }
        inout(int)*  screwUpPtr(return ref inout(int) wx) { return &wx; }
        inout(int)[] screwUpArr(return ref inout(int) wx) { return (&wx)[0 .. 1]; }
}

struct S10758
{
    int x;
        inout(int)   screwUpVal(ref inout(int) _) inout { return x; }
    ref inout(int)   screwUpRef(ref inout(int) _) inout return { return x; }
        inout(int)*  screwUpPtr(ref inout(int) _) inout return { return &x; }
        inout(int)[] screwUpArr(ref inout(int) _) inout return { return (&x)[0 .. 1]; }
}

void test10758(ref inout(int) wx, inout(int)* wp, inout(int)[] wa, inout(S10758) ws)
{
        inout(int)   screwUpVal(inout(int) _) { return wx; }
    ref inout(int)   screwUpRef(inout(int) _) { return wx; }
        inout(int)*  screwUpPtr(inout(int) _) { return &wx; }
        inout(int)[] screwUpArr(inout(int) _) { return (&wx)[0 .. 1]; }

    struct NS
    {
            inout(int)   screwUpVal() inout { return wx; }
        ref inout(int)   screwUpRef() inout { return wx; }
            inout(int)*  screwUpPtr() inout { return &wx; }
            inout(int)[] screwUpArr() inout { return (&wx)[0 .. 1]; }
    }

              int  mx = 1;
        const(int) cx = 1;
    immutable(int) ix = 1;

    // nested inout function may return an inout reference of the context,
    // so substitude inout to mutable or immutable should be disallowed.
    {
        // value return does not leak any inout reference, so safe.
        screwUpVal(mx);
        screwUpVal(ix);
        screwUpVal(wx);
        screwUpVal(cx);

        static assert(!__traits(compiles, screwUpRef(mx)));
        static assert(!__traits(compiles, screwUpRef(ix)));
        screwUpRef(wx);
        screwUpRef(cx);

        static assert(!__traits(compiles, screwUpPtr(mx)));
        static assert(!__traits(compiles, screwUpPtr(ix)));
        screwUpPtr(wx);
        screwUpPtr(cx);

        static assert(!__traits(compiles, screwUpArr(mx)));
        static assert(!__traits(compiles, screwUpArr(ix)));
        screwUpArr(cx);
        screwUpArr(wx);
    }

    // inout method of the nested struct may return an inout reference of the context,
    {
        (          NS()).screwUpVal();
        (immutable NS()).screwUpVal();
        (    inout NS()).screwUpVal();
        (    const NS()).screwUpVal();

        static assert(!__traits(compiles, (          NS()).screwUpRef()));
        static assert(!__traits(compiles, (immutable NS()).screwUpRef()));
        (inout NS()).screwUpRef();
        (const NS()).screwUpRef();

        static assert(!__traits(compiles, (          NS()).screwUpPtr()));
        static assert(!__traits(compiles, (immutable NS()).screwUpPtr()));
        (inout NS()).screwUpPtr();
        (const NS()).screwUpPtr();

        static assert(!__traits(compiles, (          NS()).screwUpArr()));
        static assert(!__traits(compiles, (immutable NS()).screwUpArr()));
        (inout NS()).screwUpArr();
        (const NS()).screwUpArr();
    }

    // function pointer holds no context, so there's no screw up.
    {
        auto fp_screwUpVal = &X10758.screwUpVal;
        fp_screwUpVal(mx);
        fp_screwUpVal(ix);
        fp_screwUpVal(wx);
        fp_screwUpVal(cx);

        auto fp_screwUpRef = &X10758.screwUpRef;
        fp_screwUpRef(mx);
        fp_screwUpRef(ix);
        fp_screwUpRef(wx);
        fp_screwUpRef(cx);

        auto fp_screwUpPtr = &X10758.screwUpVal;
        fp_screwUpPtr(mx);
        fp_screwUpPtr(ix);
        fp_screwUpPtr(wx);
        fp_screwUpPtr(cx);

        auto fp_screwUpArr = &X10758.screwUpVal;
        fp_screwUpArr(mx);
        fp_screwUpArr(ix);
        fp_screwUpArr(wx);
        fp_screwUpArr(cx);
    }

    // inout delegate behaves same as nested functions.
    {
        auto dg_screwUpVal = &ws.screwUpVal;
        dg_screwUpVal(mx);
        dg_screwUpVal(ix);
        dg_screwUpVal(wx);
        dg_screwUpVal(cx);

        auto dg_screwUpRef = &ws.screwUpRef;
        static assert(!__traits(compiles, dg_screwUpRef(mx)));
        static assert(!__traits(compiles, dg_screwUpRef(ix)));
        dg_screwUpRef(wx);
        dg_screwUpRef(cx);

        auto dg_screwUpPtr = &ws.screwUpPtr;
        static assert(!__traits(compiles, dg_screwUpPtr(mx)));
        static assert(!__traits(compiles, dg_screwUpPtr(ix)));
        dg_screwUpPtr(wx);
        dg_screwUpPtr(cx);

        auto dg_screwUpArr = &ws.screwUpArr;
        static assert(!__traits(compiles, dg_screwUpArr(mx)));
        static assert(!__traits(compiles, dg_screwUpArr(ix)));
        dg_screwUpArr(cx);
        dg_screwUpArr(wx);
    }
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=10761

inout(int)* function(inout(int)*) fptr10761(inout(int)*)
{
    static inout(int)* screwUp(inout(int)* x) { return x; }
    auto fp = &screwUp;
    static assert(is(typeof(fp) == inout(int)* function(inout(int)*) pure nothrow @nogc @safe));
    return fp;
}

inout(int)* delegate(inout(int)*) nest10761(inout(int)* x)
{
    inout(int)* screwUp(inout(int)* _) { return x; }
    auto dg = &screwUp;
    static assert(is(typeof(dg) == inout(int)* delegate(inout(int)*) pure nothrow @nogc @safe));
    return dg;
}

struct S10761
{
    int x;
    inout(int)* screwUp() inout return { return &x; }
}

inout(int)* delegate() inout return memfn10761(inout(int)* x)
{
    auto s = new inout S10761(1);
    auto dg = &s.screwUp;
    static assert(is(typeof(dg) == inout(int)* delegate() inout return));
    return dg;
}

void test10761()
{
              int  mx = 1;
        const(int) cx = 1;
    immutable(int) ix = 1;

    // inout substitution has no effect on function pointer type
    {
        auto fp_m = fptr10761(&mx);
        auto fp_c = fptr10761(&cx);
        auto fp_i = fptr10761(&ix);
        alias FP = inout(int)* function(inout(int)*);
        static assert(is(typeof(fp_m) == FP));
        static assert(is(typeof(fp_c) == FP));
        static assert(is(typeof(fp_i) == FP));
    }

    // inout substitution on delegate type should always
    // modify inout to const.
    {
        auto dg_m = nest10761(&mx);
        auto dg_c = nest10761(&cx);
        auto dg_i = nest10761(&ix);
        alias DG = const(int)* delegate(const(int)*);
        static assert(is(typeof(dg_m) == DG));
        static assert(is(typeof(dg_c) == DG));
        static assert(is(typeof(dg_i) == DG));
    }

    // same as above
    {
        auto dg_m = memfn10761(&mx);
        auto dg_c = memfn10761(&cx);
        auto dg_i = memfn10761(&ix);
        alias DG = const(int)* delegate() return const;
        static assert(is(typeof(dg_m) == DG));
        static assert(is(typeof(dg_c) == DG));
        static assert(is(typeof(dg_i) == DG));
    }
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=11226

void test11226()
{
    typeof(null) m;
    const typeof(null) c = m;
    immutable typeof(null) i = m;

    m = m, m = c, m = i;
    assert(m == c);
    assert(m == i);
    assert(c == i);
    static assert(is(typeof(true ? m : m) ==           typeof(null)));
    static assert(is(typeof(true ? m : c) ==     const typeof(null)));
    static assert(is(typeof(true ? m : i) ==     const typeof(null)));
    static assert(is(typeof(true ? c : m) ==     const typeof(null)));
    static assert(is(typeof(true ? c : c) ==     const typeof(null)));
    static assert(is(typeof(true ? c : i) ==     const typeof(null)));
    static assert(is(typeof(true ? i : m) ==     const typeof(null)));
    static assert(is(typeof(true ? i : c) ==     const typeof(null)));
    static assert(is(typeof(true ? i : i) == immutable typeof(null)));

    static assert(typeof(m).stringof ==           "typeof(null)" );
    static assert(typeof(c).stringof ==     "const(typeof(null))");
    static assert(typeof(i).stringof == "immutable(typeof(null))");
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=11257

struct R11257
{
    union
    {
        const(Object) original;
        Object stripped;
    }
}
void test11257()
{
    const(R11257) cr;
    R11257 mr = cr;  // Error: cannot implicitly convert expression (cr) of type const(R) to R
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=11215

shared(inout(void)**) f11215(inout int);

static assert(is(typeof(f11215(0)) == shared(void**)));
static assert(is(typeof(f11215((const int).init)) == shared(const(void)**)));

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=11489

void test11489(inout int = 0)
{
    static class B {}
    static class D : B {}

                 D [] dm;
           const(D)[] dc;
           inout(D)[] dw;
          shared(D)[] dsm;
    shared(const D)[] dsc;
    shared(inout D)[] dsw;
       immutable(D)[] di;

    static assert(!__traits(compiles, {              B [] b = dm; }));
    static assert( __traits(compiles, {        const(B)[] b = dm; }));
    static assert(!__traits(compiles, {        inout(B)[] b = dm; }));
    static assert(!__traits(compiles, {       shared(B)[] b = dm; }));
    static assert(!__traits(compiles, { shared(const B)[] b = dm; }));
    static assert(!__traits(compiles, { shared(inout B)[] b = dm; }));
    static assert(!__traits(compiles, {    immutable(B)[] b = dm; }));

    static assert(!__traits(compiles, {              B [] b = dc; }));
    static assert( __traits(compiles, {        const(B)[] b = dc; }));
    static assert(!__traits(compiles, {        inout(B)[] b = dc; }));
    static assert(!__traits(compiles, {       shared(B)[] b = dc; }));
    static assert(!__traits(compiles, { shared(const B)[] b = dc; }));
    static assert(!__traits(compiles, { shared(inout B)[] b = dc; }));
    static assert(!__traits(compiles, {    immutable(B)[] b = dc; }));

    static assert(!__traits(compiles, {              B [] b = dw; }));
    static assert( __traits(compiles, {        const(B)[] b = dw; }));
    static assert(!__traits(compiles, {        inout(B)[] b = dw; }));
    static assert(!__traits(compiles, {       shared(B)[] b = dw; }));
    static assert(!__traits(compiles, { shared(const B)[] b = dw; }));
    static assert(!__traits(compiles, { shared(inout B)[] b = dw; }));
    static assert(!__traits(compiles, {    immutable(B)[] b = dw; }));

    static assert(!__traits(compiles, {              B [] b = dsm; }));
    static assert(!__traits(compiles, {        const(B)[] b = dsm; }));
    static assert(!__traits(compiles, {        inout(B)[] b = dsm; }));
    static assert(!__traits(compiles, {       shared(B)[] b = dsm; }));
    static assert( __traits(compiles, { shared(const B)[] b = dsm; }));
    static assert(!__traits(compiles, { shared(inout B)[] b = dsm; }));
    static assert(!__traits(compiles, {    immutable(B)[] b = dsm; }));

    static assert(!__traits(compiles, {              B [] b = dsc; }));
    static assert(!__traits(compiles, {        const(B)[] b = dsc; }));
    static assert(!__traits(compiles, {        inout(B)[] b = dsc; }));
    static assert(!__traits(compiles, {       shared(B)[] b = dsc; }));
    static assert( __traits(compiles, { shared(const B)[] b = dsc; }));
    static assert(!__traits(compiles, { shared(inout B)[] b = dsc; }));
    static assert(!__traits(compiles, {    immutable(B)[] b = dsc; }));

    static assert(!__traits(compiles, {              B [] b = dsw; }));
    static assert(!__traits(compiles, {        const(B)[] b = dsw; }));
    static assert(!__traits(compiles, {        inout(B)[] b = dsw; }));
    static assert(!__traits(compiles, {       shared(B)[] b = dsw; }));
    static assert( __traits(compiles, { shared(const B)[] b = dsw; }));
    static assert(!__traits(compiles, { shared(inout B)[] b = dsw; }));
    static assert(!__traits(compiles, {    immutable(B)[] b = dsw; }));

    static assert(!__traits(compiles, {              B [] b = di; }));
    static assert( __traits(compiles, {        const(B)[] b = di; }));
    static assert(!__traits(compiles, {        inout(B)[] b = di; }));
    static assert(!__traits(compiles, {       shared(B)[] b = di; }));
    static assert( __traits(compiles, { shared(const B)[] b = di; }));
    static assert(!__traits(compiles, { shared(inout B)[] b = di; }));
    static assert( __traits(compiles, {    immutable(B)[] b = di; }));
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=11768

void test11768(inout int = 0)
{
    const(inout(char)) k1;
    inout(const(char)) k2;
    static assert(typeof(k1).stringof == "inout(const(char))"); // OK
    static assert(typeof(k2).stringof == "inout(const(char))"); // fails
    static assert(is(typeof(k1) == typeof(k2)));                // fails
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=12403

void test12403()
{
    void func(K, V)(inout(V[K]) aa)
    {
        static assert(is(V == const int));
        static assert(is(K == int));
    }

    const(int)[int] m;
    func(m);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=13011

void test13011()
{
    static size_t hashOf(int delegate() inout val)
    {
        return 0;
    }

    int delegate() inout dg;
    auto h = hashOf(dg);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=13030

void va13030(Args...)(const Args args) {}

void func13030(int delegate(int n) a)
{
    va13030(a);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=13802
// https://issues.dlang.org/show_bug.cgi?id=13803

static assert((              string     ).stringof ==               "string"       );
static assert((              string[]   ).stringof ==               "string[]"     );
static assert((              string[1]  ).stringof ==               "string[1]"    );
static assert((              string[int]).stringof ==               "string[int]"  );
static assert((       const  string     ).stringof ==         "const(string)"      );
static assert((       const  string[]   ).stringof ==         "const(string[])"    );
static assert((       const  string[1]  ).stringof ==         "const(string[1])"   );
static assert((       const  string[int]).stringof ==         "const(string[int])" );
static assert((shared        string     ).stringof ==        "shared(string)"      );
static assert((shared        string[]   ).stringof ==        "shared(string[])"    );
static assert((shared        string[1]  ).stringof ==        "shared(string[1])"   );
static assert((shared        string[int]).stringof ==        "shared(string[int])" );
static assert((shared const  string     ).stringof ==  "shared(const(string))"     );
static assert((shared const  string[]   ).stringof ==  "shared(const(string[]))"   );
static assert((shared const  string[1]  ).stringof ==  "shared(const(string[1]))"  );
static assert((shared const  string[int]).stringof ==  "shared(const(string[int]))");
static assert((   immutable  string     ).stringof ==     "immutable(string)"      );
static assert((   immutable  string[]   ).stringof ==     "immutable(string[])"    );
static assert((   immutable  string[1]  ).stringof ==     "immutable(string[1])"   );
static assert((   immutable  string[int]).stringof ==     "immutable(string[int])" );

static assert((             wstring     ).stringof ==              "wstring"       );
static assert((             wstring[]   ).stringof ==              "wstring[]"     );
static assert((             wstring[1]  ).stringof ==              "wstring[1]"    );
static assert((             wstring[int]).stringof ==              "wstring[int]"  );
static assert((       const wstring     ).stringof ==        "const(wstring)"      );
static assert((       const wstring[]   ).stringof ==        "const(wstring[])"    );
static assert((       const wstring[1]  ).stringof ==        "const(wstring[1])"   );
static assert((       const wstring[int]).stringof ==        "const(wstring[int])" );
static assert((shared       wstring     ).stringof ==       "shared(wstring)"      );
static assert((shared       wstring[]   ).stringof ==       "shared(wstring[])"    );
static assert((shared       wstring[1]  ).stringof ==       "shared(wstring[1])"   );
static assert((shared       wstring[int]).stringof ==       "shared(wstring[int])" );
static assert((shared const wstring     ).stringof == "shared(const(wstring))"     );
static assert((shared const wstring[]   ).stringof == "shared(const(wstring[]))"   );
static assert((shared const wstring[1]  ).stringof == "shared(const(wstring[1]))"  );
static assert((shared const wstring[int]).stringof == "shared(const(wstring[int]))");
static assert((   immutable wstring     ).stringof ==    "immutable(wstring)"      );
static assert((   immutable wstring[]   ).stringof ==    "immutable(wstring[])"    );
static assert((   immutable wstring[1]  ).stringof ==    "immutable(wstring[1])"   );
static assert((   immutable wstring[int]).stringof ==    "immutable(wstring[int])" );

static assert((             dstring     ).stringof ==              "dstring"       );
static assert((             dstring[]   ).stringof ==              "dstring[]"     );
static assert((             dstring[1]  ).stringof ==              "dstring[1]"    );
static assert((             dstring[int]).stringof ==              "dstring[int]"  );
static assert((       const dstring     ).stringof ==        "const(dstring)"      );
static assert((       const dstring[]   ).stringof ==        "const(dstring[])"    );
static assert((       const dstring[1]  ).stringof ==        "const(dstring[1])"   );
static assert((       const dstring[int]).stringof ==        "const(dstring[int])" );
static assert((shared       dstring     ).stringof ==       "shared(dstring)"      );
static assert((shared       dstring[]   ).stringof ==       "shared(dstring[])"    );
static assert((shared       dstring[1]  ).stringof ==       "shared(dstring[1])"   );
static assert((shared       dstring[int]).stringof ==       "shared(dstring[int])" );
static assert((shared const dstring     ).stringof == "shared(const(dstring))"     );
static assert((shared const dstring[]   ).stringof == "shared(const(dstring[]))"   );
static assert((shared const dstring[1]  ).stringof == "shared(const(dstring[1]))"  );
static assert((shared const dstring[int]).stringof == "shared(const(dstring[int]))");
static assert((   immutable dstring     ).stringof ==    "immutable(dstring)"      );
static assert((   immutable dstring[]   ).stringof ==    "immutable(dstring[])"    );
static assert((   immutable dstring[1]  ).stringof ==    "immutable(dstring[1])"   );
static assert((   immutable dstring[int]).stringof ==    "immutable(dstring[int])" );

/************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test8();
    test9();
    test10();
    test11();
    test12();
    test13();
    test14();
    test15();
    test16();
    test17();
    test18();
    test19();
    test20();
    test21();
    test22();
    test23();
    test24();
    test25();
    test26();
    test27();
    test28();
    test29();
    test30();
    test31();
    test32();
    test33();
    test34();
    test35();
    test36();
    test37();
    test38();
    test39();
    test40();
    test41();
    test42();
    test43();
    test44();
    test45();
    test46();
    test47();
    test48();
    test49();
    test50();
    test51();
    test52();
    test53();
    test54();
    test55();
    test56();
    test57();
    test58();
    test59();
    test60();
    test61();
    test62();
    test63();
    test64();
    test65();
    test66();
    test68();
    test69();
    test70();
    test72();
    test73();
    test74();
    test75();
    test76();
    test77();
    test78();
    test79();
    test80();
    test81();
    test82();
    test83();
    test84();
    test85();
    test87();
    test4968();
    test3748();
    test1961a();
    test1961b();
    test1961c();
    test88();
    test4251a();
    test4251b();
    test5473();
    test5493();
    test5493inout();
    test6782();
    test6864();
    test6865();
    test6866();
    test6870();
    test6912();
    test6930a();
    test6930b();
    test11868();
    test6939();
    test6940();
    test6982();
    test7038();
    test7105();
    test7202();
    test7554();
    test7518();
    test7669();
    test7757();
    test8098();
    test8099();
    test8201();
    test8212();
    test8366();
    test8408();
    test8688();
    test9046();
    test9090();
    test9461();
    test9209();
    test11226();
    test11768();
    test13011();

    printf("Success\n");
    return 0;
}
