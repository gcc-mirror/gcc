// PERMUTE_ARGS: -unittest -O -release -inline -fPIC -g
/*
TEST_OUTPUT:
---
runnable/sdtor.d(36): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
runnable/sdtor.d(59): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
runnable/sdtor.d(93): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
runnable/sdtor.d(117): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
runnable/sdtor.d(143): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
runnable/sdtor.d(177): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
runnable/sdtor.d(203): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
runnable/sdtor.d(276): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
S7353
---
*/

import core.vararg;

extern (C) int printf(const(char*) fmt, ...);

template TypeTuple(T...) { alias TypeTuple = T; }

/**********************************/

int sdtor;

struct S1
{
    ~this() { printf("~S()\n"); sdtor++; }
}

void test1()
{
    S1* s = new S1();
    delete s;
    assert(sdtor == 1);
}

/**********************************/

int sdtor2;

struct S2
{
    ~this() { printf("~S2()\n"); sdtor2++; }
    delete(void* p) { assert(sdtor2 == 1); printf("S2.delete()\n"); sdtor2++; }
}

void test2()
{
    S2* s = new S2();
    delete s;
    assert(sdtor2 == 2);
}

/**********************************/

int sdtor3;

struct S3
{   int a;
    ~this() { printf("~S3()\n"); sdtor3++; assert(a == 3); }
}

struct T3
{
    int i;
    S3 s;
}

void test3()
{
    T3* s = new T3();
    s.s.a = 3;
    delete s;
    assert(sdtor3 == 1);
}

/**********************************/

int sdtor4;

struct S4
{   int a = 3;
    ~this()
    {   printf("~S4()\n");
        if (a == 4)
            assert(sdtor4 == 2);
        else
        {   assert(a == 3);
            assert(sdtor4 == 1);
        }
        sdtor4++;
    }
}

struct T4
{
    int i;
    S4 s;
    ~this() { printf("~T4()\n"); assert(sdtor4 == 0); sdtor4++; }
    S4 t;
}

void test4()
{
    T4* s = new T4();
    s.s.a = 4;
    delete s;
    assert(sdtor4 == 3);
}

/**********************************/

int sdtor5;

template M5()
{  ~this()
   {
        printf("~M5()\n"); assert(sdtor5 == 1); sdtor5++;
   }
}

struct T5
{
    mixin M5 m;
    ~this() { printf("~T5()\n"); assert(sdtor5 == 0); sdtor5++; }
}

void test5()
{
    T5* s = new T5();
    delete s;
    assert(sdtor5 == 2);
}

/**********************************/

int sdtor6;

struct S6
{   int b = 7;
    ~this()
    {
        printf("~S6()\n"); assert(b == 7); assert(sdtor6 == 1); sdtor6++;
    }
}

class T6
{
    int a = 3;
    S6 s;
    ~this() { printf("~T6()\n"); assert(a == 3); assert(sdtor6 == 0); sdtor6++; }
}

void test6()
{
    T6 s = new T6();
    delete s;
    assert(sdtor6 == 2);
}

/**********************************/

int sdtor7;

struct S7
{   int b = 7;
    ~this()
    {
        printf("~S7()\n");
        assert(b == 7);
        assert(sdtor7 >= 1 && sdtor7 <= 3);
        sdtor7++;
    }
}

struct T7
{
    int a = 3;
    S7[3] s;
    ~this()
    {   printf("~T7() %d\n", sdtor7);
        assert(a == 3);
        assert(sdtor7 == 0);
        sdtor7++;
    }
}

void test7()
{
    T7* s = new T7();
    delete s;
    assert(sdtor7 == 4);
}

/**********************************/

int sdtor8;

struct S8
{   int b = 7;
    int c;
    ~this()
    {
        printf("~S8() %d\n", sdtor8);
        assert(b == 7);
        assert(sdtor8 == c);
        sdtor8++;
    }
}

void test8()
{
    S8[] s = new S8[3];
    s[0].c = 2;
    s[1].c = 1;
    s[2].c = 0;
    delete s;
    assert(sdtor8 == 3);
}

/**********************************/

int sdtor9;

struct S9
{   int b = 7;
    ~this()
    {
        printf("~S9() %d\n", sdtor9);
        assert(b == 7);
        sdtor9++;
    }
}

void test9()
{
    {
    S9 s;
    }
    assert(sdtor9 == 1);
}

/**********************************/

int sdtor10;

struct S10
{   int b = 7;
    int c;
    ~this()
    {
        printf("~S10() %d\n", sdtor10);
        assert(b == 7);
        assert(sdtor10 == c);
        sdtor10++;
    }
}

void test10()
{
    {
    S10[3] s;
    s[0].c = 2;
    s[1].c = 1;
    s[2].c = 0;
    }
    assert(sdtor10 == 3);
}

/**********************************/

int sdtor11;

template M11()
{   ~this()
    {
        printf("~M11()\n"); assert(sdtor11 == 1); sdtor11++;
    }
}

class T11
{
    mixin M11 m;
    ~this() { printf("~T11()\n"); assert(sdtor11 == 0); sdtor11++; }
}

void test11()
{
    T11 s = new T11();
    delete s;
    assert(sdtor11 == 2);
}

/**********************************/

int sdtor12;

struct S12
{   int a = 3;
    ~this() { printf("~S12() %d\n", sdtor12); sdtor12++; }
}

void foo12(S12 s)
{
}

void test12()
{
    {
    S12 s;
    foo12(s);
    s.a = 4;
    }
    assert(sdtor12 == 2);
}

/**********************************/

struct S13
{   int a = 3;
    int opAssign(S13 s)
    {
        printf("S13.opAssign(%p)\n", &this);
        a = 4;
        return s.a + 2;
    }
}

void test13()
{
    S13 s;
    S13 t;
    assert((s = t) == 5);
    assert(s.a == 4);
}

/**********************************/

struct S14
{   int a = 3;
    int opAssign(ref S14 s)
    {
        printf("S14.opAssign(%p)\n", &this);
        a = 4;
        return s.a + 2;
    }
}

void test14()
{
    S14 s;
    S14 t;
    assert((s = t) == 5);
    assert(s.a == 4);
}

/**********************************/

struct S15
{   int a = 3;
    int opAssign(ref const S15 s)
    {
        printf("S15.opAssign(%p)\n", &this);
        a = 4;
        return s.a + 2;
    }
}

void test15()
{
    S15 s;
    S15 t;
    assert((s = t) == 5);
    assert(s.a == 4);
}

/**********************************/

struct S16
{   int a = 3;
    int opAssign(S16 s, ...)
    {
        printf("S16.opAssign(%p)\n", &this);
        a = 4;
        return s.a + 2;
    }
}

void test16()
{
    S16 s;
    S16 t;
    assert((s = t) == 5);
    assert(s.a == 4);
}

/**********************************/

struct S17
{   int a = 3;
    int opAssign(...)
    {
        printf("S17.opAssign(%p)\n", &this);
        a = 4;
        return 5;
    }
}

void test17()
{
    S17 s;
    S17 t;
    assert((s = t) == 5);
    assert(s.a == 4);
}

/**********************************/

struct S18
{   int a = 3;
    int opAssign(S18 s, int x = 7)
    {
        printf("S18.opAssign(%p)\n", &this);
        a = 4;
        assert(x == 7);
        return s.a + 2;
    }
}

void test18()
{
    S18 s;
    S18 t;
    assert((s = t) == 5);
    assert(s.a == 4);
}

/**********************************/

struct S19
{
    int a,b,c,d;
    this(this) { printf("this(this) %p\n", &this); }
    ~this() { printf("~this() %p\n", &this); }
}

S19 foo19()
{
    S19 s;
    void bar() { s.a++; }
    bar();
    return s;
}

void test19()
{
    S19 t = foo19();
    printf("-main()\n");
}

/**********************************/

struct S20
{
    static char[] r;
    int a,b,c=2,d;
    this(this) { printf("this(this) %p\n", &this); r ~= '='; }
    ~this() { printf("~this() %p\n", &this); r ~= '~'; }
}

void foo20()
{
    S20 s;
    S20[3] a;
    assert(S20.r == "");
    a = s;
    assert(S20.r == "=~=~=~");
}

void test20()
{
    foo20();
    assert(S20.r == "=~=~=~~~~~");
    printf("-main()\n");
}

/**********************************/

struct S21
{
    static char[] r;
    int a,b,c=2,d;
    this(this) { printf("this(this) %p\n", &this); r ~= '='; }
    ~this() { printf("~this() %p\n", &this); r ~= '~'; }
}

void foo21()
{
    S21 s;
    S21[3] a = s;
    assert(S21.r == "===");
    S21.r = null;
    S21[3][2] b = s;
    assert(S21.r == "======");
    S21.r = null;
}

void test21()
{
    foo21();
    assert(S21.r == "~~~~~~~~~~");
    printf("-main()\n");
}

/**********************************/

struct S22
{
    static char[] r;
    int a,b,c=2,d;
    this(this) { printf("this(this) %p\n", &this); r ~= '='; }
    ~this() { printf("~this() %p\n", &this); r ~= '~'; }
}

void foo22()
{
    S22[3] s;
    S22[3][2] a;
    assert(S22.r == "");
    a = s;
    assert(S22.r == "===~~~===~~~");
    S22.r = null;
}

void test22()
{
    foo22();
    assert(S22.r == "~~~~~~~~~");
    printf("-main()\n");
}


/************************************************/

struct S23
{
    int m = 4, n, o, p, q;

    this(int x)
    {
        printf("S23(%d)\n", x);
        assert(x == 3);
        assert(m == 4 && n == 0 && o == 0 && p == 0 && q == 0);
        q = 7;
    }
}

void test23()
{
  {
    auto s = S23(3);
    printf("s.m = %d, s.n = %d, s.q = %d\n", s.m, s.n, s.q);
    assert(s.m == 4 && s.n == 0 && s.o == 0 && s.p == 0 && s.q == 7);
  }
  {
    auto s = new S23(3);
    printf("s.m = %d, s.n = %d, s.q = %d\n", s.m, s.n, s.q);
    assert(s.m == 4 && s.n == 0 && s.o == 0 && s.p == 0 && s.q == 7);
  }
  {
    S23 s;
    s = S23(3);
    printf("s.m = %d, s.n = %d, s.q = %d\n", s.m, s.n, s.q);
    assert(s.m == 4 && s.n == 0 && s.o == 0 && s.p == 0 && s.q == 7);
  }
}

/************************************************/

struct S24
{
    int m, n, o, p, q;

    this(int x)
    {
        printf("S24(%d)\n", x);
        assert(x == 3);
        assert(m == 0 && n == 0 && o == 0 && p == 0 && q == 0);
        q = 7;
    }
}

void test24()
{
  {
    auto s = S24(3);
    printf("s.m = %d, s.n = %d, s.q = %d\n", s.m, s.n, s.q);
    assert(s.m == 0 && s.n == 0 && s.o == 0 && s.p == 0 && s.q == 7);
  }
  {
    auto s = new S24(3);
    printf("s.m = %d, s.n = %d, s.q = %d\n", s.m, s.n, s.q);
    assert(s.m == 0 && s.n == 0 && s.o == 0 && s.p == 0 && s.q == 7);
  }
  {
    S24 s;
    s = S24(3);
    printf("s.m = %d, s.n = %d, s.q = %d\n", s.m, s.n, s.q);
    assert(s.m == 0 && s.n == 0 && s.o == 0 && s.p == 0 && s.q == 7);
  }
}

/**********************************/

struct S25
{
    this(int s) {}
}

void test25()
{
    auto a = S25();
}

/**********************************/

int z26;

struct A26
{
    int id;
    this(int x) { id = x; printf("Created A from scratch: %d\n", x); z26++; }
    this(this) { printf("Copying A: %d\n", id); z26 += 10; }
    ~this() { printf("Destroying A: %d\n", id); z26 += 100; }
}

struct B26
{
    A26 member;
    this(this) { printf("Copying B: %d\n", member.id); assert(0); }
}

B26 foo26()
{
    A26 a = A26(45);
    printf("1\n");
    assert(z26 == 1);
    return B26(a);
}

void test26()
{
    {
    auto b = foo26();
    assert(z26 == 111);
    printf("2\n");
    }
    assert(z26 == 211);
}

/**********************************/

int z27;

struct A27
{
    int id;
    this(int x) { id = x; printf("Ctor A27: %d\n", x); z27++; }
    this(this) { printf("Copying A27: %d\n", id); z27 += 10; }
    ~this() { printf("Destroying A27: %d\n", id); z27 += 100; }
}

struct B27
{
    A27[2][3] member;
}

void test27()
{
  {
    A27[2][3] a;
    assert(z27 == 0);
    B27 b = B27(a);
    assert(z27 == 60);
  }
  assert(z27 == 1260);
}


/**********************************/

string s28;

struct A28
{
    this(this)
    {
        printf("A's copy\n");
        s28 ~= "A";
    }
}

struct B28
{
    A28 member;
    this(this)
    {
        printf("B's copy\n");
        s28 ~= "B";
    }
}

void test28()
{
    B28 b1;
    B28 b2 = b1;
    assert(s28 == "AB");
}


/**********************************/

string s29;

template Templ29 ()
{
    this(int i) { this(0.0); s29 ~= "i"; }
    this(double d) { s29 ~= "d"; }
}

class C29 { mixin Templ29; }
struct S29 { mixin Templ29; }

void test29()
{
    auto r = S29(1);
    assert(s29 == "di");

    r = S29(2.0);
    assert(s29 == "did");

    auto c = new C29(2);
    assert(s29 == "diddi");

    auto c2 = new C29(2.0);
    assert(s29 == "diddid");
}

/**********************************/

struct S30
{
    int x;
    this(T)(T args) { x = args + 1; }
}

void test30()
{
    auto s = S30(3);
    assert(s.x == 4);
}

/**********************************/

struct S31
{
    int x;
    this(T...)(T args) { x = args[0] + 1; }
}

void test31()
{
    auto s = S31(3);
    assert(s.x == 4);
}

/**********************************/

struct S32
{
    static int x;

    this(int i)
    {
        printf("ctor %p(%d)\n", &this, i);
        x += 1;
    }

    this(this)
    {
        printf("postblit %p\n", &this);
        x += 0x10;
    }

    ~this()
    {
        printf("dtor %p\n", &this);
        x += 0x100;
    }
}

S32 foo32()
{
    printf("test1\n");
    return S32(1);
}

S32 bar32()
{
    printf("test1\n");
    S32 f = S32(1);
    printf("test2\n");
    return f;
}

void test32()
{
  {
    S32 s = foo32();
  }
  assert(S32.x == 0x101);

  S32.x = 0;
  {
    S32 s = bar32();
  }
  assert(S32.x == 0x101);
}

/**********************************/

struct S33
{
    static int x;

    this(int i)
    {
        printf("ctor %p(%d)\n", &this, i);
        x += 1;
    }

    this(this)
    {
        printf("postblit %p\n", &this);
        x += 0x10;
    }

    ~this()
    {
        printf("dtor %p\n", &this);
        x += 0x100;
    }
}

struct T33
{
    S33 foo()
    {
        return t;
    }

    S33 t;
}

void test33()
{
    {
        T33 t;
        S33 s = t.foo();
    }
    printf("S.x = %x\n", S33.x);
    assert(S33.x == 0x210);
}

/**********************************/

struct X34 {
    int i;
    this(this) {
        printf("postblit %p\n", &this);
        ++i;
    }

    ~this() {
        printf("dtor %p\n", &this);
    }
}

void test34()
{
    X34[2] xs;
//  xs[0][0] = X34();
    printf("foreach\n");
    for (int j = 0; j < xs.length; j++) {
        j++,j--;
        auto x = xs[j];
        //foreach(x; xs)
        //printf("foreach x.i = %d\n", x[0].i);
        //assert(x[0].i == 1);
        printf("foreach x.i = %d\n", x.i);
        assert(x.i == 1);
    }
    printf("loop done\n");
}

/**********************************/

struct X35 {
    __gshared int k;
    int i;
    this(this) {
        printf("postblit %p\n", &this);
        ++i;
    }

    ~this() {
        printf("dtor %p\n", &this);
        k++;
    }
}

void test35()
{
    {
        X35[2] xs;
        printf("foreach\n");
        foreach(ref x; xs) {
            printf("foreach x.i = %d\n", x.i);
            assert(x.i == 0);
        }
        printf("loop done\n");
    }
    assert(X35.k == 2);
}

/**********************************/

struct X36 {
    __gshared int k;
    int i;
    this(this) {
        printf("postblit %p\n", &this);
        ++i;
    }

    ~this() {
        printf("dtor %p\n", &this);
        k++;
    }
}

void test36()
{
    {
        X36[2] xs;
        printf("foreach\n");
        foreach(x; xs) {
            printf("foreach x.i = %d\n", x.i);
            assert(x.i == 1);
        }
        printf("loop done\n");
    }
    assert(X36.k == 4);
}

/**********************************/

struct X37 {
    __gshared int k;
    int i;
    this(this) {
        printf("postblit %p\n", &this);
        ++i;
    }

    ~this() {
        printf("dtor %p\n", &this);
        k++;
    }
}

void test37() {
    {
        X37[2][3] xs;
        printf("foreach\n");
        foreach(ref x; xs) {
            printf("foreach x.i = %d\n", x[0].i);
            assert(x[0].i == 0);
        }
        printf("loop done\n");
    }
  assert(X37.k == 6);
}

/**********************************/

struct S38 {
    __gshared int count;
    __gshared int postblit;

    this(int x) {
        printf("this(%d)\n", x);
        assert(this.x == 0);
        this.x = x;
        count++;
    }
    this(this) {
        printf("this(this) with %d\n", x);
        assert(x == 1 || x == 2);
        count++;
        postblit++;
    }
    ~this() {
        printf("~this(%d)\n", x);
        assert(x == 1 || x == 2);
        x = 0;
        count--;
    }
    int x;
}

S38 foo38() {
    auto s = S38(1);
    return s;
}

S38 bar38() {
    return S38(2);
}

void test38()
{
  {
    auto s1 = foo38();
    assert(S38.count == 1);
    assert(S38.postblit == 0);
  }
  assert(S38.count == 0);
  S38.postblit = 0;

  {
    auto s2 = bar38();
    assert(S38.count == 1);
    assert(S38.postblit == 0);
  }
  assert(S38.count == 0);
}


/**********************************/

struct Foo39
{
    int x;
    this(int v){ x = v + 1; }
    void opAssign(int v){
        x = v + 3;
    }
}

void test39()
{
    int y = 5;
    Foo39 f = y;
    assert(f.x == 6);
    f = y;
    assert(f.x == 8);
//  f = Foo39(y);

}

/**********************************/

bool approxEqual(float a, float b)
{
    return a < b ? b-a < .001 : a-b < .001;
}

struct Point {
    float x = 0, y = 0;
    const bool opEquals(const ref Point rhs)
    {
        return approxEqual(x, rhs.x) && approxEqual(y, rhs.y);
    }
}

struct Rectangle {
   Point leftBottom, rightTop;
}

void test40()
{
   Rectangle a, b;
   assert(a == b);
   a.leftBottom.x = 1e-8;
   assert(a == b);
   a.rightTop.y = 5;
   assert(a != b);
}

/**********************************/

struct S41 {
   this(int) immutable {   }
}

void test41()
{
    auto s = new immutable S41(3);
    //writeln(typeid(typeof(s)));
    static assert(is(typeof(s) == immutable(S41)*));

    auto t = immutable S41(3);
    //writeln(typeid(typeof(t)));
    static assert(is(typeof(t) == immutable(S41)));
}

/**********************************/

class C42 {
   this(int) immutable {
   }
}

void test42()
{
    static assert(!__traits(compiles, new C42(3)));
    //writeln(typeid(typeof(c)));
    //static assert(is(typeof(c) == immutable(C42)));

    auto d = new immutable(C42)(3);
    //writeln(typeid(typeof(d)));
    static assert(is(typeof(d) == immutable(C42)));
}

/**********************************/

struct S43 {
    int i;
    int* p;
//  this(int i, int* t) immutable { this.i = i; p = t;  }
}

void test43()
{
    int i;
    assert(!__traits(compiles, immutable(S43)(3, &i)));
    immutable int j = 4;
    auto s = immutable(S43)(3, &j);
    //writeln(typeid(typeof(s)));
    static assert(is(typeof(s) == immutable(S43)));
}

/**********************************/

struct S44 {
    int i;
    immutable(int)* p;
    this(int i, immutable(int)* t) immutable { this.i = i; this.p = t;  }
}

void test44()
{
    int i;
    assert(!__traits(compiles, immutable(S44)(3, &i)));
    immutable int j = 4;
    auto s = immutable(S44)(3, &j);
    //writeln(typeid(typeof(s)));
    static assert(is(typeof(s) == immutable(S44)));
}

/**********************************/

class C45 {
    C45 next;
    this(int[] data) immutable {
        next = new immutable(C45)(data[1 .. $]);
    }
}

void test45()
{
}

/**********************************/
// 3986

struct SiberianHamster
{
   int rat = 813;
   this(string z) { }
}

void test46()
{
   SiberianHamster basil = "cybil";
   assert(basil.rat == 813);
}

/**********************************/
// 8741

struct Vec8741
{
    this(float x)
    {
        m[0] = x;
        m[1] = 58;
    }
    float[2] m;
    static Vec8741 zzz = Vec8741(7);
}

void test8741()
{
    assert(Vec8741.zzz.m[0] == 7);
    assert(Vec8741.zzz.m[1] == 58);
}

/**********************************/

struct Segfault3984
{
    int a;
    this(int x){
        a = x;
    }
}

void test47()
{
    //static
    assert(Segfault3984(3).a == 3);
}

/**********************************/

void test48()
{
    struct B {
        void foo()  {   }
    }
    B x = B.init;
}

/**********************************/

struct Foo49 {
   int z;
   this(int a){z=a;}
}

void bar49(Foo49 a = Foo49(1))
{
    assert(a.z == 1);
}

void test49()
{
    bar49();
    bar49();
}

/**********************************/

struct aStruct{
    int    m_Int;

    this(int a){
        m_Int = a;
    }
}

class aClass{
    void aFunc(aStruct a = aStruct(44))
    {
        assert(a.m_Int == 44);
    }
}

void test50()
{
    aClass a = new aClass();
    a.aFunc();
    a.aFunc();
}

/**********************************/

int A51_a;

struct A51
{
    ~this() { ++A51_a; }
}

void test51()
{
  A51_a = 0; { while(0) A51 a;                      } assert(A51_a == 0);
  A51_a = 0; { if(0) A51 a;                         } assert(A51_a == 0);
  A51_a = 0; { if(1){} else A51 a;                  } assert(A51_a == 0);
  A51_a = 0; { for(;0;) A51 a;                      } assert(A51_a == 0);
  A51_a = 0; { if (1) { A51 a; }                    } assert(A51_a == 1);
  A51_a = 0; { if (1) A51 a;                        } assert(A51_a == 1);
  A51_a = 0; { if(0) {} else A51 a;                 } assert(A51_a == 1);
  A51_a = 0; { if (0) for(A51 a;;) {}               } assert(A51_a == 0);
  A51_a = 0; { if (0) for(;;) A51 a;                } assert(A51_a == 0);
  A51_a = 0; { do A51 a; while(0);                  } assert(A51_a == 1);
  A51_a = 0; { if (0) while(1) A51 a;               } assert(A51_a == 0);
  A51_a = 0; { try A51 a; catch(Error e) {}         } assert(A51_a == 1);
  A51_a = 0; { if (0) final switch(1) A51 a;        } assert(A51_a == 0); // should fail to build
//  A51_a = 0; { if (0) switch(1) { A51 a; default: } } assert(A51_a == 0);
  A51_a = 0; { if (0) switch(1) { default: A51 a; } } assert(A51_a == 0);
//  A51_a = 0; { if (1) switch(1) { A51 a; default: } } assert(A51_a == 1); // should be 0, right?
  A51_a = 0; { if (1) switch(1) { default: A51 a; } } assert(A51_a == 1);
//  A51_a = 0; { final switch(0) A51 a;               } assert(A51_a == 0);
  A51_a = 0; { A51 a; with(a) A51 b;                } assert(A51_a == 2);
}

/**********************************/

string s52;

struct A52
{
    int m;
    this(this)
    {
        printf("this(this) %p\n", &this);
        s52 ~= 'a';
    }
    ~this()
    {
        printf("~this() %p\n", &this);
        s52 ~= 'b';
    }
    A52 copy()
    {
        s52 ~= 'c';
        A52 another = this;
        return another;
    }
}

void test52()
{
  {
    A52 a;
    A52 b = a.copy();
    printf("a: %p, b: %p\n", &a, &b);
  }
    printf("s = '%.*s'\n", s52.length, s52.ptr);
    assert(s52 == "cabb");
}

/**********************************/
// 4339

struct A53 {
    invariant() {   }
    ~this() { }
    void opAssign(A53 a) {}
    int blah(A53 a) { return 0; }
}

/**********************************/

struct S54
{
    int x = 1;

    int bar() { return x; }

    this(int i)
    {
        printf("ctor %p(%d)\n", &this, i);
        t ~= "a";
    }

    this(this)
    {
        printf("postblit %p\n", &this);
        t ~= "b";
    }

    ~this()
    {
        printf("dtor %p\n", &this);
        t ~= "c";
    }

    static string t;
}

void bar54(S54 s) { }

S54 abc54() { return S54(1); }

void test54()
{
    {   S54.t = null;
        S54 s = S54(1);
    }
    assert(S54.t == "ac");

    {   S54.t = null;
        S54 s = S54();
    }
    assert(S54.t == "c");

    {   S54.t = null;
        int b = 1 && (){ bar54(S54(1)); return 1;}();
    }
    assert(S54.t == "ac");

    {   S54.t = null;
        int b = 0 && (){ bar54(S54(1)); return 1;}();
    }
    assert(S54.t == "");

    {   S54.t = null;
        int b = 0 || (){ bar54(S54(1)); return 1;}();
    }
    assert(S54.t == "ac");

    {   S54.t = null;
        int b = 1 || (){ bar54(S54(1)); return 1;}();
    }
    assert(S54.t == "");

    {
    S54.t = null;
    { const S54 s = S54(1); }
        assert(S54.t == "ac");
    }
    {
        S54.t = null;
        abc54();
        assert(S54.t == "ac");
    }
    {
        S54.t = null;
        abc54().x += 1;
        assert(S54.t == "ac");
    }
}

/**********************************/

void test55()
{
    S55 s;
    auto t = s.copy();
    assert(t.count == 1);   // (5)
}

struct S55
{
    int count;
    this(this) { ++count; }
    S55 copy() { return this; }
}

/**********************************/

struct S56
{
    int x = 1;

    int bar() { return x; }

    this(int i)
    {
        printf("ctor %p(%d)\n", &this, i);
        t ~= "a";
    }

    this(this)
    {
        printf("postblit %p\n", &this);
        t ~= "b";
    }

    ~this()
    {
        printf("dtor %p\n", &this);
        t ~= "c";
    }

    static string t;
}

int foo56()
{
    throw new Throwable("hello");
    return 5;
}


void test56()
{
    int i;
    int j;
    try
    {
        j |= 1;
        i = S56(1).x + foo56() + 1;
        j |= 2;
    }
    catch (Throwable o)
    {
        printf("caught\n");
        j |= 4;
    }
    printf("i = %d, j = %d\n", i, j);
    assert(i == 0);
    assert(j == 5);
}

/**********************************/
// 5859

int dtor_cnt = 0;
struct S57
{
    int v;
    this(int n){ v = n; printf("S.ctor v=%d\n", v); }
    ~this(){ ++dtor_cnt; printf("S.dtor v=%d\n", v); }
    bool opCast(T:bool)(){ printf("S.cast v=%d\n", v); return true; }
}
S57 f(int n){ return S57(n); }

void test57()
{
    printf("----\n");
    dtor_cnt = 0;
    if (auto s = S57(10))
    {
        printf("ifbody\n");
    }
    else assert(0);
    assert(dtor_cnt == 1);

    printf("----\n");    //+
    dtor_cnt = 0;
    S57(1), S57(2);
    if (auto s = S57(10))
    {
        assert(dtor_cnt == 2);
        printf("ifbody\n");
    }
    else assert(0);
    assert(dtor_cnt == 3);  // +/

    printf("----\n");
    dtor_cnt = 0;
    try{
        if (auto s = S57(10))
        {
            printf("ifbody\n");
            throw new Exception("test");
        }
        else assert(0);
    }catch (Exception e){}
    assert(dtor_cnt == 1);



    printf("----\n");
    dtor_cnt = 0;
    if (auto s = f(10))
    {
        printf("ifbody\n");
    }
    else assert(0);
    assert(dtor_cnt == 1);

    printf("----\n");    //+
    dtor_cnt = 0;
    f(1), f(2);
    if (auto s = f(10))
    {
        assert(dtor_cnt == 2);
        printf("ifbody\n");
    }
    else assert(0);
    assert(dtor_cnt == 3);  // +/

    printf("----\n");
    dtor_cnt = 0;
    try{
        if (auto s = f(10))
        {
            printf("ifbody\n");
            throw new Exception("test");
        }
        else assert(0);
    }catch (Exception e){}
    assert(dtor_cnt == 1);




    printf("----\n");
    dtor_cnt = 0;
    if (S57(10))
    {
        assert(dtor_cnt == 1);
        printf("ifbody\n");
    }
    else assert(0);

    printf("----\n");
    dtor_cnt = 0;
    S57(1), S57(2);
    if (S57(10))
    {
        assert(dtor_cnt == 3);
        printf("ifbody\n");
    }
    else assert(0);

    printf("----\n");
    dtor_cnt = 0;
    try{
        if (auto s = S57(10))
        {
            printf("ifbody\n");
            throw new Exception("test");
        }
        else assert(0);
    }catch (Exception e){}
    assert(dtor_cnt == 1);



    printf("----\n");
    dtor_cnt = 0;
    if (f(10))
    {
        assert(dtor_cnt == 1);
        printf("ifbody\n");
    }
    else assert(0);

    printf("----\n");
    dtor_cnt = 0;
    f(1), f(2);
    if (f(10))
    {
        assert(dtor_cnt == 3);
        printf("ifbody\n");
    }
    else assert(0);

    printf("----\n");
    dtor_cnt = 0;
    try{
        if (auto s = f(10))
        {
            printf("ifbody\n");
            throw new Exception("test");
        }
        else assert(0);
    }catch (Exception e){}
    assert(dtor_cnt == 1);
}

/**********************************/
// 5574

struct foo5574a
{
    ~this() {}
}
class bar5574a
{
    foo5574a[1] frop;
}

struct foo5574b
{
    this(this){}
}
struct bar5574b
{
    foo5574b[1] frop;
}

/**********************************/
// 5777

int sdtor58 = 0;
S58* ps58;

struct S58
{
    @disable this(this);
    ~this(){ ++sdtor58; }
}

S58 makeS58()
{
    S58 s;
    ps58 = &s;
    return s;
}

void test58()
{
    auto s1 = makeS58();
    assert(ps58 == &s1);
    assert(sdtor58 == 0);
}

/**********************************/
// 6308

struct C59
{
    void oops()
    {
        throw new Throwable("Oops!");
    }

    ~this()
    {
    }
}

void foo59()
{
    C59().oops();
//  auto c = C(); c.oops();
}


void test59()
{
    int i = 0;
    try
        foo59();
    catch (Throwable)
    {   i = 1;
    }
    assert(i == 1);
}

/**********************************/
// 5737

void test5737()
{
    static struct S
    {
        static int destroyed;
        static int copied;

        this(this)
        {
            copied++;
        }

        ~this()
        {
            destroyed++;
        }
    }

    static S s;

    ref S foo()
    {
        return s;
    }

    {
        auto s2 = foo();
    }

    assert(S.copied == 1); // fail, s2 was not copied;
    assert(S.destroyed == 1); // ok, s2 was destroyed
}

/**********************************/
// 6119

void test6119()
{
    int postblit = 0;
    int dtor = 0;

    struct Test
    {
        this(this) { ++postblit; }
        ~this()    { ++dtor; }
    }

    void takesVal(Test x) {}
    ref Test returnsRef(ref Test x) { return x; }

    void f(ref Test x) { takesVal( x ); }

    Test x;

    postblit = dtor = 0;
    takesVal(returnsRef(x));
    assert(postblit == 1);
    assert(dtor == 1);

    postblit = dtor = 0;
    f(x);
    assert(postblit == 1);
    assert(dtor == 1);
}

/**********************************/
// 6364

struct Foo6364
{
    int state = 1;

    ~this()
    {
        state = 0;
    }
}

void testfoo6364()
{
    static Foo6364 foo;
    printf("%d\n", foo.state);
    assert(foo.state == 1);
}

void test6364()
{
    testfoo6364();
    testfoo6364();
}

/**********************************/
// 6499

struct S6499
{
    string m = "<not set>";

    this(string s)
    {
        m = s;
        printf("Constructor - %.*s\n", m.length, m.ptr);
        if (m == "foo") { ++sdtor; assert(sdtor == 1); }
        if (m == "bar") { ++sdtor; assert(sdtor == 2); }
    }
    this(this)
    {
        printf("Postblit    - %.*s\n", m.length, m.ptr);
        assert(0);
    }
    ~this()
    {
        printf("Destructor  - %.*s\n", m.length, m.ptr);
        if (m == "bar") { assert(sdtor == 2); --sdtor; }
        if (m == "foo") { assert(sdtor == 1); --sdtor; }
    }
    S6499 bar()     { return S6499("bar"); }
    S6499 baz()()   { return S6499("baz"); }
}

void test6499()
{
    S6499 foo() { return S6499("foo"); }

    {
        sdtor = 0;
        scope(exit) assert(sdtor == 0);
        foo().bar();
    }
    {
        sdtor = 0;
        scope(exit) assert(sdtor == 0);
        foo().baz();
    }
}

/**********************************/

template isImplicitlyConvertible(From, To)
{
    enum bool isImplicitlyConvertible = is(typeof({
                        void fun(ref From v) {
                            void gun(To) {}
                            gun(v);
                        }
                    }()));
}

void test60()
{
    static struct X1
    {
        void* ptr;
        this(this){}
    }
    static struct S1
    {
        X1 x;
    }

    static struct X2
    {
        int ptr;
        this(this){}
    }
    static struct S2
    {
        X2 x;
    }

    {
              S1  ms;
              S1  ms2 = ms; // mutable to mutable
        const(S1) cs2 = ms; // mutable to const                         // NG -> OK
    }
    {
        const(S1) cs;
        static assert(!__traits(compiles,{                              // NG -> OK
              S1 ms2 = cs;  // field has reference, then const to mutable is invalid
        }));
        const(S1) cs2 = cs; // const to const                           // NG -> OK
    }
    static assert( isImplicitlyConvertible!(      S1 ,       S1 ) );
    static assert( isImplicitlyConvertible!(      S1 , const(S1)) );    // NG -> OK
    static assert(!isImplicitlyConvertible!(const(S1),       S1 ) );
    static assert( isImplicitlyConvertible!(const(S1), const(S1)) );    // NG -> OK


    {
              S2  ms;
              S2  ms2 = ms; // mutable to mutable
        const(S2) cs2 = ms; // mutable to const                         // NG -> OK
    }
    {
        const(S2) cs;
              S2  ms2 = cs; // all fields are value, then const to mutable is OK
        const(S2) cs2 = cs; // const to const                           // NG -> OK
    }

    static assert( isImplicitlyConvertible!(      S2 ,       S2 ) );
    static assert( isImplicitlyConvertible!(      S2 , const(S2)) );    // NG -> OK
    static assert( isImplicitlyConvertible!(const(S2),       S2 ) );
    static assert( isImplicitlyConvertible!(const(S2), const(S2)) );    // NG -> OK
}

/**********************************/
// 4316

struct A4316
{
    this(this) @safe { }
}

@safe void test4316()
{
    A4316 a;
    auto b = a; // Error: safe function 'main' cannot call system function'__cpctor'
}

/**********************************/

struct F6177
{
    ~this()    {}
}

struct G6177
{
    this(F6177[] p...) {}
}

void test6177()
{
    F6177 c;
    auto g = G6177(c);
}


/**********************************/
// 6470

struct S6470
{
    static int spblit;

    this(this){ ++spblit; }
}

void test6470()
{
    S6470[] a1;
    S6470[] a2;
    a1.length = 3;
    a2.length = 3;
    a1[] = a2[];
    assert(S6470.spblit == 3);

    S6470 s;

    S6470[] a3;
    a3.length = 3;
    a3 = [s, s, s];
    assert(S6470.spblit == 6);

    void func(S6470[] a){}
    func([s, s, s]);
    assert(S6470.spblit == 9);
}

/**********************************/
// 6636

struct S6636
{
    ~this()
    {
        ++sdtor;
    }
}

void func6636(S6636[3] sa) {}

void test6636()
{
    sdtor = 0;

    S6636[3] sa;
    func6636(sa);
    assert(sdtor == 3);
}

/**********************************/
// 6637

struct S6637
{
    static int spblit;

    this(this){ ++spblit; }
}

void test6637()
{
    void func(S6637[3] sa){}

    S6637[3] sa;
    func(sa);
    assert(S6637.spblit == 3);
}

/**********************************/
// 7353

struct S7353
{
    static uint ci = 0;
    uint i;

    this(int x) { i = ci++; /*writeln("new: ", i);*/ }
    this(this)  { i = ci++; /*writeln("copy ", i);*/ }
    ~this()     {           /*writeln("del ", i);*/ }

    S7353 save1() // produces 2 copies in total
    {
        S7353 s = this;
        return s;
    }
    auto save2() // produces 3 copies in total
    {
        S7353 s = this;
        return s;
        pragma(msg, typeof(return));
    }
}
void test7353()
{
    {
        auto s = S7353(1), t = S7353(1);
        t = s.save1();
        assert(S7353.ci == 3);
    }
    S7353.ci = 0; //writeln("-");
    {
        auto s = S7353(1), t = S7353(1);
        t = s.save2();
        assert(S7353.ci == 3);
    }
}

/**********************************/
// 8036

struct S8036a
{
    ~this() {}
}
struct S8036b // or class
{
    S8036a[0] s;
}

/**********************************/

struct S61
{
    this(long length)
    {
        this.length = length;
    }

    long length;
}


const(S61) copy(const S61 s)
{
    return s;
}


void test61()
{
    S61 t = S61(42);
    const S61 u = t;

    assert(t == u);
    assert(copy(t) == u);
    assert(t == copy(u));
}

/**********************************/
// 7506

void test7506()
{
    static struct S
    {
        static uint ci = 0;
        static uint di = 0;
        uint i;

        this(int x) { i = ci++; /*writeln("new: ", i);*/ }
        this(this)  { i = ci++; /*writeln("copy ", i);*/ }
        ~this()     { ++di;     /*writeln("del: ", i);*/ }

        S save3()
        {
            return this;
        }
    }

    {
        auto s = S(1), t = S(1);
        assert(S.ci == 2);
        t = s.save3();
        assert(S.ci == 3);  // line 23
    }
    assert(S.di == 3);
}

/**********************************/
// 7516

struct S7516
{
    int val;

    this(int n) { val = n; }
    this(this) { val *= 3; }
}

// CondExp on return statement
void test7516a()
{
    alias S = S7516;
    S s1 = S(1);
    S s2 = S(2);

    S foo(bool f) { return f ?  s1  :  s2;  }
    S hoo(bool f) { return f ? S(1) : S(2); }
    S bar(bool f) { return f ?  s1  : S(2); }
    S baz(bool f) { return f ? S(1) :  s2;  }

    auto r1 = foo(true);    assert(r1.val == 3);
    auto r2 = foo(false);   assert(r2.val == 6);
    auto r3 = hoo(true);    assert(r3.val == 1);
    auto r4 = hoo(false);   assert(r4.val == 2);
    auto r5 = bar(true);    assert(r5.val == 3);
    auto r6 = bar(false);   assert(r6.val == 2);
    auto r7 = baz(true);    assert(r7.val == 1);
    auto r8 = baz(false);   assert(r8.val == 6);
}

// CondExp on function argument
void test7516b()
{
    alias S = S7516;
    S s1 = S(1);
    S s2 = S(2);
    S func(S s) { return s; }

    S foo(bool f) { return func(f ?  s1  :  s2 ); }
    S hoo(bool f) { return func(f ? S(1) : S(2)); }
    S bar(bool f) { return func(f ?  s1  : S(2)); }
    S baz(bool f) { return func(f ? S(1) :  s2 ); }

    auto r1 = foo(true);    assert(r1.val == 3 * 3);
    auto r2 = foo(false);   assert(r2.val == 6 * 3);
    auto r3 = hoo(true);    assert(r3.val == 1 * 3);
    auto r4 = hoo(false);   assert(r4.val == 2 * 3);
    auto r5 = bar(true);    assert(r5.val == 3 * 3);
    auto r6 = bar(false);   assert(r6.val == 2 * 3);
    auto r7 = baz(true);    assert(r7.val == 1 * 3);
    auto r8 = baz(false);   assert(r8.val == 6 * 3);
}

// CondExp on array literal
void test7516c()
{
    alias S = S7516;
    S s1 = S(1);
    S s2 = S(2);

    S[] foo(bool f) { return [f ?  s1  :  s2 ]; }
    S[] hoo(bool f) { return [f ? S(1) : S(2)]; }
    S[] bar(bool f) { return [f ?  s1  : S(2)]; }
    S[] baz(bool f) { return [f ? S(1) :  s2 ]; }

    auto r1 = foo(true);    assert(r1[0].val == 3);
    auto r2 = foo(false);   assert(r2[0].val == 6);
    auto r3 = hoo(true);    assert(r3[0].val == 1);
    auto r4 = hoo(false);   assert(r4[0].val == 2);
    auto r5 = bar(true);    assert(r5[0].val == 3);
    auto r6 = bar(false);   assert(r6[0].val == 2);
    auto r7 = baz(true);    assert(r7[0].val == 1);
    auto r8 = baz(false);   assert(r8[0].val == 6);
}

// CondExp on rhs of cat assign
void test7516d()
{
    alias S = S7516;
    S s1 = S(1);
    S s2 = S(2);

    S[] foo(bool f) { S[] a; a ~= f ?  s1  :  s2 ; return a; }
    S[] hoo(bool f) { S[] a; a ~= f ? S(1) : S(2); return a; }
    S[] bar(bool f) { S[] a; a ~= f ?  s1  : S(2); return a; }
    S[] baz(bool f) { S[] a; a ~= f ? S(1) :  s2 ; return a; }

    auto r1 = foo(true);    assert(r1[0].val == 3);
    auto r2 = foo(false);   assert(r2[0].val == 6);
    auto r3 = hoo(true);    assert(r3[0].val == 1);
    auto r4 = hoo(false);   assert(r4[0].val == 2);
    auto r5 = bar(true);    assert(r5[0].val == 3);
    auto r6 = bar(false);   assert(r6[0].val == 2);
    auto r7 = baz(true);    assert(r7[0].val == 1);
    auto r8 = baz(false);   assert(r8[0].val == 6);
}

// CondExp on struct literal element
void test7516e()
{
    alias S = S7516;
    S s1 = S(1);
    S s2 = S(2);
    struct X { S s; }

    X foo(bool f) { return X(f ?  s1  :  s2 ); }
    X hoo(bool f) { return X(f ? S(1) : S(2)); }
    X bar(bool f) { return X(f ?  s1  : S(2)); }
    X baz(bool f) { return X(f ? S(1) :  s2 ); }

    auto r1 = foo(true);    assert(r1.s.val == 3);
    auto r2 = foo(false);   assert(r2.s.val == 6);
    auto r3 = hoo(true);    assert(r3.s.val == 1);
    auto r4 = hoo(false);   assert(r4.s.val == 2);
    auto r5 = bar(true);    assert(r5.s.val == 3);
    auto r6 = bar(false);   assert(r6.s.val == 2);
    auto r7 = baz(true);    assert(r7.s.val == 1);
    auto r8 = baz(false);   assert(r8.s.val == 6);
}

/**********************************/
// 7530

void test7530()
{
    static struct S
    {
        int val;

        this(int n) { val = n; }
        this(this) { val *= 3; }
    }

    S[] sa = new S[](1);
    sa[0].val = 1;
    S foo()
    {
        return sa[0];
    }
    auto s = foo();
    assert(s.val == 3);
}

/**********************************/

struct S62
{
    this(int length)
    {
        _length = length;
    }

    int opBinary(string op)(in S62 rhs) const
        if(op == "-")
    {
        return this.length - rhs.length;
    }

    @property int length() const
    {
        return _length;
    }

    invariant()
    {
        assert(_length == 1);
    }

    int _length  = 1;
}


void test62()
{
    immutable result = S62.init - S62.init;
}

/**********************************/
// 7579

void test7579a()
{
    static struct S
    {
        // postblit can also have no body because isn't called
        @disable this(this) { assert(0); }
    }

    @property S fs() { return S(); }
    @property S[3] fsa() { return [S(), S(), S()]; }

    S s0;
    S s1 = S();
    static assert(!__traits(compiles, { S s2 = s1; }));         // OK
    S s2 = fs;
    static assert(!__traits(compiles, { s2 = s1; }));           // OK

    // static array
    S[3] sa0;
    S[3] sa1 = [S(), S(), S()];
    static assert(!__traits(compiles, { S[3] sa2 = sa1; }));    // fixed
    S[3] sa2 = fsa;
    static assert(!__traits(compiles, { sa2 = sa1; }));         // fixed
    sa2 = [S(), S(), S()];
    sa2 = fsa;

    S[] da1 = new S[3];
    S[] da2 = new S[3];
    static assert(!__traits(compiles, { da2[] = da1[]; }));     // fixed

    // concatenation and appending
    static assert(!__traits(compiles, { da1 ~= s1; }));         // fixed
    static assert(!__traits(compiles, { da1 ~= S(); }));
    static assert(!__traits(compiles, { da1 ~= fsa; }));
    static assert(!__traits(compiles, { da1 ~= fsa[]; }));
    static assert(!__traits(compiles, { da1 = da1 ~ s1; }));    // fixed
    static assert(!__traits(compiles, { da1 = s1 ~ da1; }));    // fixed
    static assert(!__traits(compiles, { da1 = da1 ~ S(); }));
    static assert(!__traits(compiles, { da1 = da1 ~ fsa; }));
    static assert(!__traits(compiles, { da1 = da1 ~ da; }));
}

void test7579b()
{
    static struct S
    {
        // postblit asserts in runtime
        this(this) { assert(0); }
    }

    @property S fs() { return S(); }
    @property S[3] fsa() { return [S(), S(), S()]; }

    S s0;
    S s1 = S();
    S s2 = fs;

    // static array
    S[3] sa0;
    S[3] sa1 = [S(), S(), S()];
    S[3] sa2 = fsa;
    sa2 = [S(), S(), S()];
    sa2 = fsa;

    S[] da1 = new S[3];
    S[] da2 = new S[3];

    // concatenation and appending always run postblits
}

/**********************************/
// 8335

struct S8335
{
    static int postblit;

    int i;
    this(this) { ++postblit; }
}

void f8335(ref S8335[3] arr)
{
    arr[0].i = 7;
}

void g8335(lazy S8335[3] arr)
{
    assert(S8335.postblit == 0);
    auto x = arr;
    assert(S8335.postblit == 3);
}

void h8335(lazy S8335 s)
{
    assert(S8335.postblit == 0);
    auto x = s;
    assert(S8335.postblit == 1);
}

void test8335()
{
    S8335[3] arr;
    f8335(arr);
    assert(S8335.postblit == 0);
    assert(arr[0].i == 7);

    g8335(arr);
    assert(S8335.postblit == 3);

    S8335.postblit = 0;
    S8335 s;
    h8335(s);
    assert(S8335.postblit == 1);
}

/**********************************/
// 8356

void test8356()
{
    static struct S
    {
        @disable this(this) { assert(0); }
    }

    S s;
    S[3] sa;

  static assert(!__traits(compiles, {
    S fs() { return s; }
  }));

  static assert(!__traits(compiles, {
    S[3] fsa() { return sa; }
  }));
}

/**********************************/
// 8475

T func8475(T)(T x) @safe pure
{
    return T();
}

template X8475(bool something)
{
    struct XY
    {
        this(this) @safe pure {}
        void func(XY x) @safe pure
        {
            XY y = x; //Error: see below
            func8475(x);
            func8475(y);
        }
    }
}

alias X8475!(true).XY Xtrue;

/**********************************/

struct Foo9320 {
    real x;

    this(real x) {
        this.x = x;
    }

    Foo9320 opBinary(string op)(Foo9320 other) {
        return Foo9320(mixin("x" ~ op ~ "other.x"));
    }
}

Foo9320 test9320(Foo9320 a, Foo9320 b, Foo9320 c) {
    return (a + b) / (a * b) - c;
}

/**********************************/
// 9386

struct Test9386
{
    string name;
    static char[25] op;
    static size_t i;

    static @property string sop() { return cast(string)op[0..i]; }

    this(string name)
    {
        this.name = name;
        printf("Created %.*s...\n", name.length, name.ptr);
        assert(i + 1 < op.length);
        op[i++] = 'a';
    }

    this(this)
    {
        printf("Copied %.*s...\n", name.length, name.ptr);
        assert(i + 1 < op.length);
        op[i++] = 'b';
    }

    ~this()
    {
        printf("Deleted %.*s\n", name.length, name.ptr);
        assert(i + 1 < op.length);
        op[i++] = 'c';
    }

    const int opCmp(ref const Test9386 t)
    {
        return op[0] - t.op[0];
    }
}

void test9386()
{
    {
        Test9386.op[] = 0;
        Test9386.i = 0;

        Test9386[] tests =
            [ Test9386("one"),
              Test9386("two"),
              Test9386("three"),
              Test9386("four") ];

        assert(Test9386.sop == "aaaa");
        Test9386.op[] = 0;
        Test9386.i = 0;

        printf("----\n");
        foreach (Test9386 test; tests)
        {
            printf("\tForeach %.*s\n", test.name.length, test.name.ptr);
            Test9386.op[Test9386.i++] = 'x';
        }

        assert(Test9386.sop == "bxcbxcbxcbxc");
        Test9386.op[] = 0;
        Test9386.i = 0;

        printf("----\n");
        foreach (ref Test9386 test; tests)
        {
            printf("\tForeach %.*s\n", test.name.length, test.name.ptr);
            Test9386.op[Test9386.i++] = 'x';
        }
        assert(Test9386.sop == "xxxx");
    }
    printf("====\n");
    {
        Test9386.op[] = 0;
        Test9386.i = 0;

        Test9386[Test9386] tests =
            [ Test9386("1") : Test9386("one"),
              Test9386("2") : Test9386("two"),
              Test9386("3") : Test9386("three"),
              Test9386("4") : Test9386("four") ];

        Test9386.op[] = 0;
        Test9386.i = 0;

        printf("----\n");
        foreach (Test9386 k, Test9386 v; tests)
        {
            printf("\tForeach %.*s : %.*s\n", k.name.length, k.name.ptr,
                                              v.name.length, v.name.ptr);
            Test9386.op[Test9386.i++] = 'x';
        }

        assert(Test9386.sop == "bbxccbbxccbbxccbbxcc");
        Test9386.op[] = 0;
        Test9386.i = 0;

        printf("----\n");
        foreach (Test9386 k, ref Test9386 v; tests)
        {
            printf("\tForeach %.*s : %.*s\n", k.name.length, k.name.ptr,
                                              v.name.length, v.name.ptr);
            Test9386.op[Test9386.i++] = 'x';
        }
        assert(Test9386.sop == "bxcbxcbxcbxc");
        Test9386.op[] = 0;
        Test9386.i = 0;
    }
}

/**********************************/
// 9441

auto x9441 = X9441(0.123);

struct X9441
{
    int a;
    this(double x) { a = cast(int)(x * 100); }
}

void test9441()
{
    assert(x9441.a == 12);
}

/**********************************/

struct Payload
{
    size_t _capacity; //Comment me
    int[] _pay;       //Comment me

    size_t insertBack(Data d)
    {
        immutable newLen   = _pay.length + 3;
        _pay.length = newLen;
        _pay = _pay[0 .. newLen]; //Comment me
        return 3;
    }
}

struct Impl
{
    Payload _payload;
    size_t _count;
}

struct Data
{
    Impl* _store;

    this(int i)
    {
        _store = new Impl;
        _store._payload = Payload.init;
    }

    ~this()
    {
        printf("%d\n", _store._count);
        --_store._count;
    }
}


void test9720()
{
    auto a = Data(1);
    auto b = Data(1);
    a._store._payload.insertBack(b); //Fails
}

/**********************************/
// 9899

struct S9899
{
    @safe pure nothrow ~this() {}
}

struct MemberS9899
{
    S9899 s;
}

void test9899() @safe pure nothrow
{
    MemberS9899 s; // 11
}

/**********************************/
// 9907

void test9907()
{
    static struct SX(bool hasCtor, bool hasDtor)
    {
        int i;
        static size_t assign;
        static size_t dtor;

        static if (hasCtor)
        {
            this(int i) { this.i = i; }
        }

        void opAssign(SX rhs)
        {
            printf("%08X(%d) from Rvalue %08X(%d)\n", &this.i, this.i, &rhs.i, rhs.i);
            ++assign;
        }

        void opAssign(ref SX rhs)
        {
            printf("%08X(%d) from Lvalue %08X(%d)\n", &this.i, this.i, &rhs.i, rhs.i);
            assert(0);
        }

        static if (hasDtor)
        {
            ~this()
            {
                printf("destroying %08X(%d)\n", &this.i, this.i);
                ++dtor;
            }
        }
    }

    S test(S)(int i)
    {
        return S(i);
    }

    foreach (hasCtor; TypeTuple!(false, true))
    foreach (hasDtor; TypeTuple!(false, true))
    {
        alias S = SX!(hasCtor, hasDtor);
        alias test!S foo;

        printf("----\n");
        auto s = S(1);

        // Assignment from two kinds of rvalues
        assert(S.assign == 0);
        s = foo(2);
        static if (hasDtor) assert(S.dtor == 1);
        assert(S.assign == 1);
        s = S(3);
        assert(S.assign == 2);
        static if (hasDtor) assert(S.dtor == 2);
    }
    printf("----\n");
}

/**********************************/
// 9985

struct S9985
{
    ubyte* b;
    ubyte buf[128];
    this(this) { assert(0); }

    static void* ptr;
}
auto ref makeS9985() @system
{
    S9985 s;
    s.b = s.buf.ptr;
    S9985.ptr = &s;
    return s;
}
void test9985()
{
    S9985 s = makeS9985();
    assert(S9985.ptr == &s);    // NRVO

    static const int n = 1;
    static auto ref retN()
    {
        return n;
    }
    auto p = &(retN());        // OK
    assert(p == &n);
    alias pure nothrow @nogc @safe ref const(int) F1();
    static assert(is(typeof(retN) == F1));

    enum const(int) x = 1;
    static auto ref retX()
    {
        return x;
    }
    static assert(!__traits(compiles, { auto q = &(retX()); }));
    alias pure nothrow @nogc @safe const(int) F2();
    static assert(is(typeof(retX) == F2));
}

/**********************************/

// https://issues.dlang.org/show_bug.cgi?id=17457

void delegate() dg17457;

struct S17457 {
    ulong[10] data;

    this(int seconds) {
        dg17457 = &mfunc;
    }
    @disable this(this);
    void mfunc() {}
}

auto foo17457() {
    pragma(inline, false);
    return S17457(18);
}

void test17457()
{
    auto x = foo17457();
    //printf("%p vs %p\n", &x, dg17457.ptr);
    assert(&x == dg17457.ptr);
}

/**********************************/
// 9994

void test9994()
{
    static struct S
    {
        static int dtor;
        ~this() { ++dtor; }
    }

    S s;
    static assert( __traits(compiles, s.opAssign(s)));
    static assert(!__traits(compiles, s.__postblit()));

    assert(S.dtor == 0);
    s = s;
    assert(S.dtor == 1);
}

/**********************************/
// 10053

struct S10053A
{
    pure ~this() {}
}

struct S10053B
{
    S10053A sa;
    ~this() {}
}

/**********************************/
// 10055

void test10055a()
{
    static struct SX { pure nothrow @safe ~this() {} }
    static struct SY { pure nothrow @safe ~this() {} }
    static struct SZ {           @disable ~this() {} }

    // function to check merge result of the dtor attributes
    static void check(S)() { S s; }

    static struct S1 {                                             }
    static struct S2 {                                  ~this() {} }
    static struct SA { SX sx; SY sy;                               }
    static struct SB { SX sx; SY sy; pure nothrow @safe ~this() {} }
    static struct SC { SX sx; SY sy;      nothrow @safe ~this() {} }
    static struct SD { SX sx; SY sy; pure         @safe ~this() {} }
    static struct SE { SX sx; SY sy; pure nothrow       ~this() {} }
    static struct SF { SX sx; SY sy;              @safe ~this() {} }
    static struct SG { SX sx; SY sy;      nothrow       ~this() {} }
    static struct SH { SX sx; SY sy; pure               ~this() {} }
    static struct SI { SX sx; SY sy;                    ~this() {} }
    static assert(is( typeof(&check!S1) == void function() pure nothrow @nogc @safe ));
    static assert(is( typeof(&check!S2) == void function()                    ));
    static assert(is( typeof(&check!SA) == void function() pure nothrow @safe ));
    static assert(is( typeof(&check!SB) == void function() pure nothrow @safe ));
    static assert(is( typeof(&check!SC) == void function()      nothrow @safe ));
    static assert(is( typeof(&check!SD) == void function() pure         @safe ));
    static assert(is( typeof(&check!SE) == void function() pure nothrow       ));
    static assert(is( typeof(&check!SF) == void function()              @safe ));
    static assert(is( typeof(&check!SG) == void function()      nothrow       ));
    static assert(is( typeof(&check!SH) == void function() pure               ));
    static assert(is( typeof(&check!SI) == void function()                    ));

    static struct S1x {                                             SZ sz; }
    static struct S2x {                                  ~this() {} SZ sz; }
    static struct SAx { SX sx; SY sy;                               SZ sz; }
    static struct SBx { SX sx; SY sy; pure nothrow @safe ~this() {} SZ sz; }
    static struct SCx { SX sx; SY sy;      nothrow @safe ~this() {} SZ sz; }
    static struct SDx { SX sx; SY sy; pure         @safe ~this() {} SZ sz; }
    static struct SEx { SX sx; SY sy; pure nothrow       ~this() {} SZ sz; }
    static struct SFx { SX sx; SY sy;              @safe ~this() {} SZ sz; }
    static struct SGx { SX sx; SY sy;      nothrow       ~this() {} SZ sz; }
    static struct SHx { SX sx; SY sy; pure               ~this() {} SZ sz; }
    static struct SIx { SX sx; SY sy;                    ~this() {} SZ sz; }
    foreach (Sx; TypeTuple!(S1x, S2x, SAx, SBx, SCx, SDx, SEx, SFx, SGx, SHx, SIx))
    {
        static assert(!__traits(compiles, &check!Sx));
    }
}

void test10055b()
{
    static struct SX { pure nothrow @safe this(this) {} }
    static struct SY { pure nothrow @safe this(this) {} }
    static struct SZ {           @disable this(this) {} }

    // function to check merge result of the postblit attributes
    static void check(S)() { S s; S s2 = s; }

    static struct S1 {                                               }
    static struct S2 {                                  this(this) {} }
    static struct SA { SX sx; SY sy;                                  }
    static struct SB { SX sx; SY sy; pure nothrow @safe this(this) {} }
    static struct SC { SX sx; SY sy;      nothrow @safe this(this) {} }
    static struct SD { SX sx; SY sy; pure         @safe this(this) {} }
    static struct SE { SX sx; SY sy; pure nothrow       this(this) {} }
    static struct SF { SX sx; SY sy;              @safe this(this) {} }
    static struct SG { SX sx; SY sy;      nothrow       this(this) {} }
    static struct SH { SX sx; SY sy; pure               this(this) {} }
    static struct SI { SX sx; SY sy;                    this(this) {} }
    static assert(is( typeof(&check!S1) == void function() pure nothrow @nogc @safe ));
    static assert(is( typeof(&check!S2) == void function()                    ));
    static assert(is( typeof(&check!SA) == void function() pure nothrow @safe ));
    static assert(is( typeof(&check!SB) == void function() pure nothrow @safe ));
    static assert(is( typeof(&check!SC) == void function()      nothrow @safe ));
    static assert(is( typeof(&check!SD) == void function() pure         @safe ));
    static assert(is( typeof(&check!SE) == void function() pure nothrow       ));
    static assert(is( typeof(&check!SF) == void function()              @safe ));
    static assert(is( typeof(&check!SG) == void function()      nothrow       ));
    static assert(is( typeof(&check!SH) == void function() pure               ));
    static assert(is( typeof(&check!SI) == void function()                    ));

    static struct S1x {                                                SZ sz; }
    static struct S2x {                                  this(this) {} SZ sz; }
    static struct SAx { SX sx; SY sy;                                  SZ sz; }
    static struct SBx { SX sx; SY sy; pure nothrow @safe this(this) {} SZ sz; }
    static struct SCx { SX sx; SY sy;      nothrow @safe this(this) {} SZ sz; }
    static struct SDx { SX sx; SY sy; pure         @safe this(this) {} SZ sz; }
    static struct SEx { SX sx; SY sy; pure nothrow       this(this) {} SZ sz; }
    static struct SFx { SX sx; SY sy;              @safe this(this) {} SZ sz; }
    static struct SGx { SX sx; SY sy;      nothrow       this(this) {} SZ sz; }
    static struct SHx { SX sx; SY sy; pure               this(this) {} SZ sz; }
    static struct SIx { SX sx; SY sy;                    this(this) {} SZ sz; }
    foreach (Sx; TypeTuple!(S1x, S2x, SAx, SBx, SCx, SDx, SEx, SFx, SGx, SHx, SIx))
    {
        static assert(!__traits(compiles, &check!Sx));
    }
}

/**********************************/
// 10160

struct S10160 { this(this) {} }

struct X10160a { S10160 s; const int x;     }
struct X10160b { S10160 s; enum int x = 1; }

void test10160()
{
    X10160a xa;
    X10160b xb;
}

/**********************************/
// 10094

void test10094()
{
    static void* p;
    const string[4] i2s = ()
    {
        string[4] tmp;
        p = &tmp[0];
        for (int i = 0; i < 4; ++i)
        {
            char[1] buf = [cast(char)('0' + i)];
            string str = buf.idup;
            tmp[i] = str;
        }
        return tmp; // NRVO should work
    }();
    assert(p == cast(void*)&i2s[0]);
    assert(i2s == ["0", "1", "2", "3"]);
}

/**********************************/
// 10079

// dtor || postblit
struct S10079a
{
    this(this) pure nothrow @safe {}
}
struct S10079b
{
    ~this() pure nothrow @safe {}
}
struct S10079c
{
    this(this) pure nothrow @safe {}
    ~this() pure nothrow @safe {}
}
struct S10079d
{
    this(this) {}
}
struct S10079e
{
    this(this) {}
    ~this() pure nothrow @safe {}
}

// memberwise
struct S10079f
{
    S10079a a;
    S10079b b;
    S10079c c;
    S10079d d;
    S10079e e;
}

void check10079(S)(ref S s) pure nothrow @safe { s = S(); }

// Assignment is pure, nothrow, and @safe in all cases.
static assert(__traits(compiles, &check10079!S10079a));
static assert(__traits(compiles, &check10079!S10079b));
static assert(__traits(compiles, &check10079!S10079c));
static assert(__traits(compiles, &check10079!S10079d));
static assert(__traits(compiles, &check10079!S10079e));
static assert(__traits(compiles, &check10079!S10079f));

/**********************************/
// 10244

void test10244()
{
    static struct Foo
    {
        string _str;
        long _num;

        template DeclareConstructor(string fieldName)
        {
            enum code =
                `this(typeof(_` ~ fieldName ~ `) value)` ~
                `{ this._` ~ fieldName ~ ` = value; }`;
            mixin(code);
        }

        mixin DeclareConstructor!"str";
        mixin DeclareConstructor!"num";
    }

    Foo value1 = Foo("D");
    Foo value2 = Foo(128);
    assert(value1._str == "D");
    assert(value2._num == 128);
}

/**********************************/
// 10694

struct Foo10694 { ~this() { } }

void test10694() pure
{
    static Foo10694 i1;
    __gshared Foo10694 i2;
    void foo() pure
    {
        static Foo10694 j1;
        __gshared Foo10694 j2;
    }
}

/**********************************/
// 10787

int global10787;

static ~this() nothrow pure @safe
{
    int* p;
    static assert(!__traits(compiles, ++p));
    static assert(!__traits(compiles, ++global10787));
}

shared static ~this() nothrow pure @safe
{
    int* p;
    static assert(!__traits(compiles, ++p));
    static assert(!__traits(compiles, ++global10787));
}

/**********************************/
// 10789

struct S10789
{
    static int count;
    int value;

    this(int)  { value = ++count; }
    ~this()    { --count; }
    this(this) { value = ++count; assert(value == 3); }
}

S10789 fun10789a(bool isCondExp)(bool cond)
{
    S10789 s1 = S10789(42), s2 = S10789(24);
    assert(S10789.count == 2);
    static if (isCondExp)
    {
        return cond ? s1 : s2;
    }
    else
    {
        if (cond)
            return s1;
        else
            return s2;
    }
}

auto fun10789b(bool isCondExp)(bool cond)
{
    S10789 s1 = S10789(42), s2 = S10789(24);
    assert(S10789.count == 2);
    static if (isCondExp)
    {
        return cond ? s1 : s2;
    }
    else
    {
        if (cond)
            return s1;
        else
            return s2;
    }
}

void test10789()
{
    foreach (fun; TypeTuple!(fun10789a, fun10789b))
    foreach (isCondExp; TypeTuple!(false, true))
    {
        {
            S10789 s = fun!isCondExp(true);
            assert(S10789.count == 1);
            assert(s.value == 3);
        }
        assert(S10789.count == 0);
        {
            S10789 s = fun!isCondExp(false);
            assert(S10789.count == 1);
            assert(s.value == 3);
        }
        assert(S10789.count == 0);
    }
}

/**********************************/
// 10972

int test10972()
{
    string result;

    struct A
    {
        this(this)  { result ~= "pA"; version(none) printf("copied A\n"); }
        ~this()     { result ~= "dA"; version(none) printf("destroy A\n"); }
    }
    struct B
    {
        this(this)
        {
            result ~= "(pB)"; version(none) printf("B says what?\n");
            throw new Exception("BOOM!");
        }
        ~this() { result ~= "dB"; version(none) printf("destroy B\n"); }
    }
    struct S
    {
        A a;
        B b;
    }

    result = "{";
    {
        S s1;
        result ~= "[";
        try
        {
            S s3 = s1;
            assert(0);
        }
        catch (Exception e)
        {}
        result ~= "]";
    }
    result ~= "}";
    assert(result == "{[pA(pB)dA]dBdA}", result);

    result = "{";
    {
        S s1;
        S s2;
        result ~= "[";
        try
        {
            s2 = s1;
            assert(0);
        }
        catch (Exception e)
        {}
        result ~= "]";
    }
    result ~= "}";
    assert(result == "{[pA(pB)dA]dBdAdBdA}", result);

    return 1;
}
static assert(test10972()); // CTFE

/**********************************/
// 11134

void test11134()
{
    void test(S)()
    {
        S s;
        S[2] sa;
        S[2][] dsa = [[S(), S()]];
        dsa.reserve(dsa.length + 2);    // avoid postblit calls by GC

        S.count = 0;
        dsa ~= sa;
        assert(S.count == 2);

        S.count = 0;
        dsa ~= [s, s];
        assert(S.count == 2);
    }

    static struct SS
    {
        static int count;
        this(this) { ++count; }
    }
    test!SS();

    struct NS
    {
        static int count;
        this(this) { ++count; }
    }
    test!NS();
}

/**********************************/
// 11197

struct S11197a
{
    this(bool) {}
    this(this) {}
}

struct S11197b
{
    //this(bool) {}
    this(this) {}
}

void test11197()
{
    S11197a[][string] aa1;
    aa1["test"] ~= S11197a.init;

    S11197b[][string] aa2;
    aa2["test"] ~= S11197b.init;
}

/**********************************/

struct S7474 {
  float x;
  ~this() {}
}

void fun7474(T...)() { T x; }
void test7474() { fun7474!S7474(); }

/**********************************/
// 11286

struct A11286
{
    ~this() {}
}

A11286 getA11286() pure nothrow
{
    return A11286();
}

void test11286()
{
    A11286 a = getA11286();
}

/**********************************/
// 11505

struct Foo11505
{
    Bar11505 b;
}

struct Bar11505
{
    ~this() @safe { }
    void* p;
}

void test11505()
{
    Foo11505 f;
    f = Foo11505();
}

/**********************************/
// 12045

bool test12045()
{
    string dtor;
    void* ptr;

    struct S12045
    {
        string val;

        this(this) { assert(0); }
        ~this() { dtor ~= val; }
    }

    auto makeS12045(bool thrown)
    {
        auto s1 = S12045("1");
        auto s2 = S12045("2");
        ptr = &s1;

        if (thrown)
            throw new Exception("");

        return s1;  // NRVO
    }

    dtor = null, ptr = null;
    try
    {
        S12045 s = makeS12045(true);
        assert(0);
    }
    catch (Exception e)
    {
        assert(dtor == "21", dtor);
    }

    dtor = null, ptr = null;
    {
        S12045 s = makeS12045(false);
        assert(dtor == "2");
        if (!__ctfe) assert(ptr is &s);   // NRVO
    }
    assert(dtor == "21");

    return true;
}
static assert(test12045());

/**********************************/
// 12591

struct S12591(T)
{
    this(this)
    {}
}

struct Tuple12591(Types...)
{
    Types expand;
    this(Types values)
    {
        expand[] = values[];
    }
}

void test12591()
{
    alias T1 = Tuple12591!(S12591!int);
}

/**********************************/
// 12660

struct X12660
{
    this(this) @nogc {}
    ~this() @nogc {}
    void opAssign(X12660) @nogc {}
    @nogc invariant() {}
}
struct Y12660
{
    X12660 x;

    this(this) @nogc {}
    ~this() @nogc {}
    @nogc invariant() {}
}
struct Z12660
{
    Y12660 y;
}

class C12660
{
    this() @nogc {}
    @nogc invariant() {}
}

void test12660() @nogc
{
    X12660 x;
    x = x;

    Y12660 y = { x };
    y = y;

    Z12660 z = { y };
    z = z;
}

/**********************************/
// 12686

struct Foo12686
{
    static int count;

    invariant() { ++count; }

    @disable this(this);

    Foo12686 bar()
    {
        Foo12686 f;
        return f;
    }
}

void test12686()
{
    Foo12686 f;
    Foo12686 f2 = f.bar();
    version (unittest)
    { }
    else
        assert(Foo12686.count == 2);
}

/**********************************/
// 13089

struct S13089
{
    @disable this(this);    // non nothrow
    int val;
}

void* p13089;

S13089[1000] foo13089() nothrow
{
    typeof(return) data;
    p13089 = &data;
    return data;
}

void test13089() nothrow
{
    immutable data = foo13089();
    assert(p13089 == &data);
}

/**********************************/

struct NoDtortest11763 {}

struct HasDtortest11763
{
    NoDtortest11763 func()
    {
        return NoDtortest11763();
    }
    ~this() {}
}

void test11763()
{
    HasDtortest11763().func();
}

/**********************************/

struct Buf { }

struct Variant
{
    ~this() { }

    Buf get() { Buf b; return b; }
}

Variant value() { Variant v; return v; }

void test13303()
{
    value.get();
}

/**********************************/

struct S13673
{
    string _name;
    ~this() {}
}

string name13673;

void test13673()
{
    S13673(name13673);
    S13673(name13673);
}

/**********************************/

void test13586()
{
    static struct S {
        __gshared int count;
        ~this() { ++count; printf("~S\n"); }
    }

    static struct T {
        __gshared int count;
        ~this() { ++count; printf("~T\n"); }
    }

    static int foo(bool flag)
    {
        if (flag)
            throw new Exception("hello");
        return 1;
    }

    static void func(S s, int f, T t)
    {
        printf("func()\n");
    }

    static class C
    {
        this(S s, int f, T t)
        {
            printf("C()\n");
        }
    }

  {
    bool threw = false;
    try
    {
        func(S(), foo(true), T());
        printf("not reach\n");
    }
    catch (Exception e)
    {
        threw = true;
    }
    printf("threw %d S %d T %d\n", threw, S.count, T.count);
    assert(threw && S.count == 1 && T.count == 0);
    S.count = 0;
    T.count = 0;
  }
  {
    bool threw = false;
    try
    {
        func(S(), foo(false), T());
        printf("reached\n");
    }
    catch (Exception e)
    {
        threw = true;
    }
    printf("threw %d S %d T %d\n", threw, S.count, T.count);
    assert(!threw && S.count == 1 && T.count == 1);
    S.count = 0;
    T.count = 0;
  }
  {
    bool threw = false;
    try
    {
        new C(S(), foo(true), T());
        printf("not reach\n");
    }
    catch (Exception e)
    {
        threw = true;
    }
    printf("threw %d S %d T %d\n", threw, S.count, T.count);
    assert(threw && S.count == 1 && T.count == 0);
    S.count = 0;
    T.count = 0;
  }
}

/**********************************/
// 14443

T enforce14443(E : Throwable = Exception, T)(T value)
{
    if (!value)
        throw new E("Enforcement failed");
    return value;
}

struct RefCounted14443(T)
if (!is(T == class) && !(is(T == interface)))
{
    struct RefCountedStore
    {
        private struct Impl
        {
            T _payload;
            size_t _count;
        }

        private Impl* _store;

        private void initialize(A...)(auto ref A args)
        {
            import core.stdc.stdlib : malloc;

            // enforce is necessary
            _store = cast(Impl*) enforce14443(malloc(Impl.sizeof));

            // emulate 'emplace'
            static if (args.length > 0)
                _store._payload.tupleof = args;
            else
                _store._payload = T.init;

            _store._count = 1;
        }

        @property bool isInitialized() const nothrow @safe
        {
            return _store !is null;
        }

        void ensureInitialized()
        {
            if (!isInitialized) initialize();
        }

    }
    RefCountedStore _refCounted;

    this(A...)(auto ref A args) if (A.length > 0)
    {
        _refCounted.initialize(args);
    }

    this(this)
    {
        if (!_refCounted.isInitialized)
            return;
        ++_refCounted._store._count;
        //printf("RefCounted count = %d (inc)\n", _refCounted._store._count);
    }

    ~this()
    {
        if (!_refCounted.isInitialized)
            return;
        assert(_refCounted._store._count > 0);
        if (--_refCounted._store._count)
        {
            //printf("RefCounted count = %u\n", _refCounted._store._count);
            return;
        }

        import core.stdc.stdlib : free;
        free(_refCounted._store);
        _refCounted._store = null;
    }

    void opAssign(typeof(this) rhs) { assert(0); }
    void opAssign(T rhs) { assert(0); }

    @property ref T refCountedPayload()
    {
        _refCounted.ensureInitialized();
        return _refCounted._store._payload;
    }

    alias refCountedPayload this;
}

struct Path14443
{
    struct Payload
    {
        int p;
    }
    RefCounted14443!Payload data;
}

struct PathRange14443
{
    Path14443 path;
    size_t i;

    @property PathElement14443 front()
    {
        return PathElement14443(this, path.data.p);
    }
}

struct PathElement14443
{
    PathRange14443 range;

    this(PathRange14443 range, int)
    {
        this.range = range;
    }
}

void test14443()
{
    auto path = Path14443(RefCounted14443!(Path14443.Payload)(12));
    assert(path.data.p == 12);

    @property refCount() { return path.data._refCounted._store._count; }
    assert(refCount == 1);

    {
        auto _r = PathRange14443(path);
        assert(refCount == 2);
        // foreach
        {
            auto element = _r.front;
            assert(refCount == 3);  // fail with 2.067
        }
        assert(refCount == 2);
    }
    assert(refCount == 1);
}

/**********************************/
// 13661, 14022, 14023 - postblit/dtor call on static array assignment

bool test13661()
{
    string op;

    struct S
    {
        char x = 'x';
        this(this) { op ~= x-0x20; }    // upper case
        ~this()    { op ~= x; }         // lower case

        ref auto opAssign(T)(T arg)
        {
            assert(0);
            return this;
        }
    }

    {
        S[2] a;

        a[0].x = 'a';
        a[1].x = 'b';
        a = a.init;
        assert(op == "ab");
        assert(a[0].x == 'x' && a[1].x == 'x');

        a[0].x = 'c';
        a[1].x = 'd';
        a = [S(), S()];   // equivalent a = a.init
        assert(op == "abcd");
        assert(a[0].x == 'x' && a[1].x == 'x');
    }
    assert(op == "abcdxx");

    return true;
}
bool test13661a()
{
    string op;

    struct S
    {
        char x = 'x';
        this(this) { op ~= x-0x20; }    // upper case
        ~this()    { op ~= x; }         // lower case
    }

    {
        S[3] sa = [S('a'), S('b'), S('c')];
        S[2] sb = sa[1..3];
        assert(sa == [S('a'), S('b'), S('c')]);
        assert(sb == [S('b'), S('c')]);
        sb[0].x = 'x';
        sb[1].x = 'y';
        assert(sa != [S('a'), S('x'), S('y')]); // OK <- incorrectly fails
        assert(sa == [S('a'), S('b'), S('c')]); // OK <- incorrectly fails
        assert(sb == [S('x'), S('y')]);
    }
    return true;
}
static assert(test13661());     // CTFE
static assert(test13661a());

bool test14022()
{
    string op;

    struct S
    {
        char x = 'x';
        this(this) { op ~= x-0x20; }    // upper case
        ~this()    { op ~= x; }         // lower case
    }

    S[2] makeSA() { return [S('p'), S('q')]; }

    struct T
    {
        S[2] sb;

        this(ref S[2] sa)
        {
            assert(op == "");
            this.sb = sa;   // TOKconstruct
            assert(op == "BC", op);
            assert(sb == [S('b'), S('c')]);
        }
        void test(ref S[2] sa)
        {
            this.sb = sa;    // dotvar: resolveSlice(newva)
            assert(op == "BxCy");
        }
    }

    op = null;
    {
        S[2] sa = [S('a'), S('b')];
        T t;    t.sb[0].x = 'x';
                t.sb[1].x = 'y';
        assert(op == "");
        t.sb = sa;
        assert(op == "AxBy");
        t.sb = makeSA();
        assert(op == "AxByab");
    }
    assert(op == "AxByabqpba");

    op = null;
    {
        S[3] sa = [S('a'), S('b'), S('c')];
        T t = T(sa[1..3]);
        t.sb[0].x = 'x';
        t.sb[1].x = 'y';
        assert(sa == [S('a'), S('b'), S('c')]);
        assert(t.sb == [S('x'), S('y')]);
        assert(op == "BC");
    }
    assert(op == "BCyxcba");

    op = null;
    {
        S[3] sx = [S('a'), S('b'), S('c')];
        T t;    t.sb[0].x = 'x';
                t.sb[1].x = 'y';
        t.test(sx[1..3]);
        assert(op == "BxCy");
        assert(t.sb == [S('b'), S('c')]);
    }
    assert(op == "BxCycbcba");

    return true;
}
static assert(test14022());

bool test14023()
{
    string op;

    struct S
    {
        char x = 'x';
        this(this) { op ~= x-0x20; }    // upper case
        ~this()    { op ~= x; }         // lower case
    }

    S[2] makeSA() { return [S('p'), S('q')]; }

    struct T
    {
        S[2][1] sb;
        this(ref S[2] sa)
        {
            assert(op == "");
            this.sb[0] = sa;   // TOKconstruct
            assert(sa    == [S('b'), S('c')]);
            assert(sb[0] == [S('b'), S('c')]);
        }
    }

    void test(ref S[2] sa)
    {
        S[2][] a;
        //a.length = 1; // will cause runtine AccessViolation
        a ~= (S[2]).init;
        assert(op == "");
        a[0] = sa;      // index <-- resolveSlice(newva)
        assert(op == "BxCx");
        assert(a[0] == [S('b'), S('c')]);
    }

    op = null;
    {
        S[3] sa = [S('a'), S('b'), S('c')];
        T t = T(sa[1..3]);
        t.sb[0][0].x = 'x';
        t.sb[0][1].x = 'y';
        assert(sa      != [S('a'), S('x'), S('y')]);    // OK <- incorrectly fails
        assert(sa      == [S('a'), S('b'), S('c')]);    // OK <- incorrectly fails
        assert(t.sb[0] == [S('x'), S('y')]);
    }

    op = null;
    {
        S[2] sa = [S('a'), S('b')];
        S[2][] a = [[S('x'), S('y')]];
        assert(op == "");
        a[0] = sa;
        assert(op == "AxBy");
        a[0] = makeSA();
        assert(op == "AxByab");
    }
    assert(op == "AxByabba");

    op = null;
    {
        S[3] sa = [S('a'), S('b'), S('c')];
        test(sa[1..3]);
        assert(op == "BxCx");
    }
    assert(op == "BxCxcba");

    return true;
}
static assert(test14023());

/************************************************/
// 13669 - dtor call on static array variable

bool test13669()
{
    string dtor;

    struct S
    {
        char x = 'x';
        ~this() { dtor ~= x; }
    }

    { S[2] a; }
    assert(dtor == "xx");
    dtor = "";

    { S[2] a = [S('a'), S('b')]; }
    assert(dtor == "ba");   // reverse order. See also: TypeInfo_StaticArray.destroy()

    return true;
}
static assert(test13669());

/**********************************/

__gshared bool b13095 = false;

void bar13095() { throw new Exception(""); }

struct S13095
{
    this(int) { printf("ctor %p\n", &this); bar13095(); }

    ~this() { b13095 = true; printf("dtor %p\n", &this); }
}

void test13095()
{
    try {
        S13095(0);
    } catch(Exception) { printf("catch\n"); }
    assert(!b13095);
}

/**********************************/
// 14264

void test14264()
{
    static int dtor;
    static struct Foo
    {
        ~this() { ++dtor; }
        T opCast(T:bool)() { return true; }
    }

    Foo makeFoo()
    {
        return Foo();
    }

    assert(dtor == 0);

    makeFoo();
    assert(dtor == 1);

    makeFoo;
    assert(dtor == 2);

    if (makeFoo()) {}
    assert(dtor == 3);

    if (makeFoo) {}
    assert(dtor == 4);
}

/**********************************/
// 14686

int test14686()
{
    string r;

    struct S
    {
        int n;
        this(this) { r ~= cast(char)('0' + n); }
    }

    S s1 = S(1);
    S s2 = S(2);
    S[] a1 = [S(1)];

    S[2] sa1 = [s1, s2];
    assert(r == "12", r);       // OK

    r = "";
    S[] a2 = a1 ~ s2;           // runtime concatenation
    assert(r == "12", r);       // OK <- NG only in CTFE

    r = "";
    S[2] sa2a = [s1] ~ s2;
    assert(r == "12", r);       // OK <- NG, s2 is not copied

    r = "";
    S[2] sa2b = s2 ~ [s1];
    assert(r == "21", r);       // OK <- NG, s2 is not copied

    r = "";
    S[3] sa3a = ([s1] ~ [s1]) ~ s2;
    assert(r == "112", r);      // OK <- NG, s2 is not copied

    r = "";
    S[3] sa3b = s2 ~ ([s1] ~ [s1]);
    assert(r == "211", r);      // OK <- NG, s2 is not copied

    return 1;
}
static assert(test14686());

/**********************************/
// 14815

int test14815()
{
    uint dtorCount;

    struct S
    {
        uint x;
        ~this() { ++dtorCount; }
    }

    S[2] sa1;
    sa1[0].x = 42;
    sa1 = (S[2]).init;      // S[2] <- rvalue
    assert(sa1[0].x == 0);
    assert(dtorCount == 2);

    S[2] sa2;
    sa2[0].x = 42;
    S[] da2 = sa2[];
    da2[] = (S[2]).init[];  // S[] <- rvalue slice
    assert(sa2[0].x == 0);
    assert(dtorCount == 4);

    S[2] sa3;
    S[2] sa4;
    sa3[0].x = 42;
    sa3 = sa4;              // S[2] <- lvalue
    assert(sa3[0].x == 0);
    assert(dtorCount == 6);

    S[2] sa5;
    S[] da4 = sa4[];
    da4[] = sa5[];          // S[] <- lvalue slice
    assert(sa4[0].x == 0);
    assert(dtorCount == 8);

    return 1;
}
static assert(test14815());

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=16197

struct Elem {
    static string r;
    int x = -1;
    this(this) { r ~= 'p'; printf("POSTBLIT %d\n", x++); }
    ~this()    { r ~= 'd'; printf("DTOR %d\n"    , x++); }
}

struct Ctr {
    Elem[3] arr;
}

void test16197() {
    { auto p = Ctr(); }
    assert(Elem.r == "ddd");
}

/**********************************/
// 14860

int test14860()
{
    uint dtorCount;

    struct S
    {
        uint x;
        ~this() { ++dtorCount; }
    }

    S[] a = [S(42)];
    a[] = S();

    assert(a[0].x == 0);
    assert(dtorCount == 1);

    return 1;
}
static assert(test14860());

/**********************************/
// 14696

void test14696(int len = 2)
{
    string result;

    struct S
    {
        int n;

        void* get(void* p = null)
        {
            result ~= "get(" ~ cast(char)(n+'0') ~ ").";
            return null;
        }

        ~this()
        {
            result ~= "dtor(" ~ cast(char)(n+'0') ~ ").";
        }
    }

    S makeS(int n)
    {
        result ~= "makeS(" ~ cast(char)(n+'0') ~ ").";
        return S(n);
    }
    void foo(void* x, void* y = null)
    {
        result ~= "foo.";
    }
    void fooThrow(void* x, void* y = null)
    {
        result ~= "fooThrow.";
        throw new Exception("fail!");
    }

    void check(void delegate() dg, string r, string file = __FILE__, size_t line = __LINE__)
    {
        import core.exception;

        result = null;
        try { dg(); } catch (Exception e) {}
        if (result != r)
            throw new AssertError(result, file, line);
    }

    // temporary in condition
    check({ foo(len == 2 ?        makeS(1).get() : null); }, "makeS(1).get(1).foo.dtor(1).");
    check({ foo(len == 2 ? null : makeS(1).get()       ); }, "foo.");
    check({ foo(len != 2 ?        makeS(1).get() : null); }, "foo.");
    check({ foo(len != 2 ? null : makeS(1).get()       ); }, "makeS(1).get(1).foo.dtor(1).");

    // temporary in nesting conditions
    check({ foo(len >= 2 ?        (len == 2 ?        makeS(1).get() : null) : null); }, "makeS(1).get(1).foo.dtor(1).");
    check({ foo(len >= 2 ?        (len == 2 ? null : makeS(1).get()       ) : null); }, "foo.");
    check({ foo(len >= 2 ?        (len != 2 ?        makeS(1).get() : null) : null); }, "foo.");
    check({ foo(len >= 2 ?        (len != 2 ? null : makeS(1).get()       ) : null); }, "makeS(1).get(1).foo.dtor(1).");
    check({ foo(len >= 2 ? null : (len == 2 ?        makeS(1).get() : null)       ); }, "foo.");
    check({ foo(len >= 2 ? null : (len == 2 ? null : makeS(1).get()       )       ); }, "foo.");
    check({ foo(len >= 2 ? null : (len != 2 ?        makeS(1).get() : null)       ); }, "foo.");
    check({ foo(len >= 2 ? null : (len != 2 ? null : makeS(1).get()       )       ); }, "foo.");
    check({ foo(len >  2 ?        (len == 2 ?        makeS(1).get() : null) : null); }, "foo.");
    check({ foo(len >  2 ?        (len == 2 ? null : makeS(1).get()       ) : null); }, "foo.");
    check({ foo(len >  2 ?        (len != 2 ?        makeS(1).get() : null) : null); }, "foo.");
    check({ foo(len >  2 ?        (len != 2 ? null : makeS(1).get()       ) : null); }, "foo.");
    check({ foo(len >  2 ? null : (len == 2 ?        makeS(1).get() : null)       ); }, "makeS(1).get(1).foo.dtor(1).");
    check({ foo(len >  2 ? null : (len == 2 ? null : makeS(1).get()       )       ); }, "foo.");
    check({ foo(len >  2 ? null : (len != 2 ?        makeS(1).get() : null)       ); }, "foo.");
    check({ foo(len >  2 ? null : (len != 2 ? null : makeS(1).get()       )       ); }, "makeS(1).get(1).foo.dtor(1).");

    // temporary in condition and throwing callee
    // check({ fooThrow(len == 2 ?        makeS(1).get() : null); }, "makeS(1).get(1).fooThrow.dtor(1).");
    // check({ fooThrow(len == 2 ? null : makeS(1).get()       ); }, "fooThrow.");
    // check({ fooThrow(len != 2 ?        makeS(1).get() : null); }, "fooThrow.");
    // check({ fooThrow(len != 2 ? null : makeS(1).get()       ); }, "makeS(1).get(1).fooThrow.dtor(1).");

    // temporary in nesting condititions and throwing callee
    // check({ fooThrow(len >= 2 ?        (len == 2 ?        makeS(1).get() : null) : null); }, "makeS(1).get(1).fooThrow.dtor(1).");
    // check({ fooThrow(len >= 2 ?        (len == 2 ? null : makeS(1).get()       ) : null); }, "fooThrow.");
    // check({ fooThrow(len >= 2 ?        (len != 2 ?        makeS(1).get() : null) : null); }, "fooThrow.");
    // check({ fooThrow(len >= 2 ?        (len != 2 ? null : makeS(1).get()       ) : null); }, "makeS(1).get(1).fooThrow.dtor(1).");
    // check({ fooThrow(len >= 2 ? null : (len == 2 ?        makeS(1).get() : null)       ); }, "fooThrow.");
    // check({ fooThrow(len >= 2 ? null : (len == 2 ? null : makeS(1).get()       )       ); }, "fooThrow.");
    // check({ fooThrow(len >= 2 ? null : (len != 2 ?        makeS(1).get() : null)       ); }, "fooThrow.");
    // check({ fooThrow(len >= 2 ? null : (len != 2 ? null : makeS(1).get()       )       ); }, "fooThrow.");
    // check({ fooThrow(len >  2 ?        (len == 2 ?        makeS(1).get() : null) : null); }, "fooThrow.");
    // check({ fooThrow(len >  2 ?        (len == 2 ? null : makeS(1).get()       ) : null); }, "fooThrow.");
    // check({ fooThrow(len >  2 ?        (len != 2 ?        makeS(1).get() : null) : null); }, "fooThrow.");
    // check({ fooThrow(len >  2 ?        (len != 2 ? null : makeS(1).get()       ) : null); }, "fooThrow.");
    // check({ fooThrow(len >  2 ? null : (len == 2 ?        makeS(1).get() : null)       ); }, "makeS(1).get(1).fooThrow.dtor(1).");
    // check({ fooThrow(len >  2 ? null : (len == 2 ? null : makeS(1).get()       )       ); }, "fooThrow.");
    // check({ fooThrow(len >  2 ? null : (len != 2 ?        makeS(1).get() : null)       ); }, "fooThrow.");
    // check({ fooThrow(len >  2 ? null : (len != 2 ? null : makeS(1).get()       )       ); }, "makeS(1).get(1).fooThrow.dtor(1).");

    // temporaries in each conditions
    check({ foo(len == 2 ? makeS(1).get() : null, len == 2 ? makeS(2).get() : null); }, "makeS(1).get(1).makeS(2).get(2).foo.dtor(2).dtor(1).");
    check({ foo(len == 2 ? makeS(1).get() : null, len != 2 ? makeS(2).get() : null); }, "makeS(1).get(1).foo.dtor(1).");
    check({ foo(len != 2 ? makeS(1).get() : null, len == 2 ? makeS(2).get() : null); }, "makeS(2).get(2).foo.dtor(2).");
    check({ foo(len != 2 ? makeS(1).get() : null, len != 2 ? makeS(2).get() : null); }, "foo.");

    // nesting temporaries in conditions
    check({ foo(len == 2 ? makeS(1).get(len == 2 ? makeS(2).get() : null) : null); }, "makeS(1).makeS(2).get(2).get(1).foo.dtor(2).dtor(1).");
    check({ foo(len == 2 ? makeS(1).get(len != 2 ? makeS(2).get() : null) : null); }, "makeS(1).get(1).foo.dtor(1).");
    check({ foo(len != 2 ? makeS(1).get(len == 2 ? makeS(2).get() : null) : null); }, "foo.");
    check({ foo(len != 2 ? makeS(1).get(len != 2 ? makeS(2).get() : null) : null); }, "foo.");
}

/**********************************/
// 14838

int test14838() pure nothrow @safe
{
    int dtor;

    struct S14838(T)
    {
        ~this() { ++dtor; }
    }
    struct X14838
    {
              S14838!int ms;
        const S14838!int cs;

              S14838!int[2] ma;
        const S14838!int[2] ca;

              S14838!int[2][2] ma2x2;
        const S14838!int[2][2] ca2x2;

        // number of S14838 = 1*2 + 2*2 + 4*2 = 14
    }

    void test(Dg)(scope Dg code)
    {
        dtor = 0;
        code();
    }

    test(delegate{       S14838!int a; }); assert(dtor == 1);
    test(delegate{ const S14838!int a; }); assert(dtor == 1);

    test(delegate{       S14838!int[2] a; }); assert(dtor == 2);
    test(delegate{ const S14838!int[2] a; }); assert(dtor == 2);

    test(delegate{       S14838!int[2][2] a; }); assert(dtor == 4);
    test(delegate{ const S14838!int[2][2] a; }); assert(dtor == 4);

    test(delegate{       X14838 a; }); assert(dtor == 1 * 14);
    test(delegate{ const X14838 a; }); assert(dtor == 1 * 14);

    test(delegate{       X14838[2] a; }); assert(dtor == 2 * 14);
    test(delegate{ const X14838[2] a; }); assert(dtor == 2 * 14);

    test(delegate{       X14838[2][2] a; }); assert(dtor == 4 * 14);
    test(delegate{ const X14838[2][2] a; }); assert(dtor == 4 * 14);

    return 1;
}
static assert(test14838());

/**********************************/

struct S63
{
    private long p = 87;

    this(int x)
    {
        assert(p == 87);
        p += x;
    }

    ~this() { }

    this(this) { }

    void funky() { assert(p == 90); }

    static void tester()
    {
        S63(3).funky();
    }
}

void test63()
{
    S63.tester();
}

/**********************************/

struct X64
{
    static int dtor;

    ~this() { ++dtor; }
}

struct S64
{
    int n;
    long[10] dummy;     // S64 needs to be passed by stack
}

S64 foo64()
{
    X64();
    return S64(1);
}

void test64()
{
    auto s = foo64();
    assert(X64.dtor == 1);
}

/**********************************/

struct S65
{
    static string t;

    void bar(int a, int b)
    {
        t ~= "d";
    }
}

S65 foo65a()
{
    S65.t ~= "a";
    return S65();
}

int foo65b()
{
    S65.t ~= "b";
    return 1;
}

int foo65c()
{
    S65.t ~= "c";
    return 2;
}

void test65()
{
    import core.stdc.stdio;
    foo65a().bar(foo65b(), foo65c());
    printf("'%.*s'\n", cast(int)S65.t.length, S65.t.ptr);
    assert(S65.t == "abcd");
}

/**********************************/
// 15661

struct X15661
{
    ~this() {}
}

X15661 createX15661() { return X15661(); }

struct Y15661
{
    static int dtor;

    @disable this();
    @disable this(this);
    this(X15661 a1, X15661 a2) {}
    ~this() { ++dtor; }
}

struct Z15661
{
    this(int)
    {
        b = Y15661(createX15661(), createX15661());
        assert(Y15661.dtor == 0);
    }

    private Y15661 b;
}

void test15661()
{
    {
        auto v = Z15661(5);
        assert(Y15661.dtor == 0);
    }
    assert(Y15661.dtor == 1);
}

/**********************************/

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

    test54();
    test55();
    test56();
    test57();
    test58();
    test59();
    test5737();
    test6119();
    test8741();
    test6364();
    test6499();
    test60();
    test4316();
    test6177();
    test6470();
    test6636();
    test6637();
    test7353();
    test61();
    test7506();
    test7516a();
    test7516b();
    test7516c();
    test7516d();
    test7516e();
    test7530();
    test62();
    test7579a();
    test7579b();
    test8335();
    test8356();
    test9386();
    test9441();
    test9720();
    test9899();
    test9907();
    test9985();
    test17457();
    test9994();
    test10094();
    test10244();
    test10694();
    test10789();
    test10972();
    test11134();
    test11197();
    test7474();
    test11505();
    test12045();
    test12591();
    test12660();
    test12686();
    test13089();
    test11763();
    test13303();
    test13673();
    test13586();
    test14443();
    test13661();
    test13661a();
    test14022();
    test14023();
    test13669();
    test13095();
    test14264();
    test14686();
    test14815();
    test16197();
    test14860();
    test14696();
    test14838();
    test63();
    test64();
    test65();
    test15661();

    printf("Success\n");
    return 0;
}
