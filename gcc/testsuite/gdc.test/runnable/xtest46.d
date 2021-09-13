// RUNNABLE_PHOBOS_TEST
// PERMUTE_ARGS: -unittest -O -release -inline -fPIC -g
/* TEST_OUTPUT:
---
Boo!double
Boo!int
true
int
!! immutable(int)[]
int(int i, long j = 7L)
long
C10390(C10390(C10390(<recursion>)))
tuple(height)
tuple(get, get)
tuple(clear)
tuple(draw, draw)
runnable/xtest46.d(149): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46.d(151): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46.d(152): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46.d(154): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46.d(181): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46.d(183): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46.d(184): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46.d(186): Deprecation: `opDot` is deprecated. Use `alias this`
const(int)
string[]
double[]
double[]
{}
tuple("m")
true
TFunction1: extern (C) void function()
---
*/

import std.stdio;
import core.stdc.stdio;

/******************************************/

struct S
{
    int opStar() { return 7; }
}

void test1()
{
    S s;

    printf("%d\n", *s);
    assert(*s == 7);
}

/******************************************/

void test2()
{
  double[1][2] bar;
  bar[0][0] = 1.0;
  bar[1][0] = 2.0;
  foo2(bar);
}

void foo2(T...)(T args)
{
    foreach (arg; args[0 .. $])
    {
        //writeln(arg);
        bar2!(typeof(arg))(&arg);
    }
}


void bar2(D)(const(void)* arg)
{
    D obj = *cast(D*) arg;
}

/***************************************************/

void test3()
{
    version (unittest)
    {
        printf("unittest!\n");
    }
    else
    {
        printf("no unittest!\n");
    }

    version (assert)
    {
        printf("assert!\n");
    }
    else
    {
        printf("no assert!\n");
    }
}


/***************************************************/

void test4()
{
  immutable int maxi = 8;
  int[][maxi] neighbors = [ cast(int[])[ ], [ 0 ], [ 0, 1], [ 0, 2], [1, 2], [1, 2, 3, 4],
[ 2, 3, 5], [ 4, 5, 6 ] ];
  int[maxi] grid;

  // neighbors[0].length = 0;

  void place(int k, uint mask)
  { if(k<maxi) {
      for(uint m = 1, d = 1; d <= maxi; d++, m<<=1)
        if(!(mask & m)) {
          bool ok = true;
          int dif;
          foreach(nb; neighbors[k])
            if((dif=grid[nb]-d)==1 || dif==-1) {
              ok = false; break;
            }
          if(ok) {
            grid[k] = d;
            place(k+1, mask | m);
          }
        }
    } else {
      printf("  %d\n%d %d %d\n%d %d %d\n  %d\n\n",
             grid[0], grid[1], grid[2], grid[3], grid[4], grid[5], grid[6], grid[7]);
    }
  }
  place(0, 0);
}


/***************************************************/

struct S5
{
  enum S5 some_constant = {2};

  int member;
}

void test5()
{
}

/***************************************************/

struct S6
{
    int a, b, c;
}

struct T6
{
    S6 s;
    int b = 7;

    S6* opDot()
    {
        return &s;
    }
}

void test6()
{
    T6 t;
    t.a = 4;
    t.b = 5;
    t.c = 6;
    assert(t.a == 4);
    assert(t.b == 5);
    assert(t.c == 6);
    assert(t.s.b == 0);
    assert(t.sizeof == 4*4);
    assert(t.init.sizeof == 4*4);
}

/***************************************************/

struct S7
{
    int a, b, c;
}

class C7
{
    S7 s;
    int b = 7;

    S7* opDot()
    {
        return &s;
    }
}

void test7()
{
    C7 t = new C7();
    t.a = 4;
    t.b = 5;
    t.c = 6;
    assert(t.a == 4);
    assert(t.b == 5);
    assert(t.c == 6);
    assert(t.s.b == 0);
    assert(t.sizeof == (void*).sizeof);
    assert(t.init is null);
}

/***************************************************/

void foo8(int n1 = __LINE__ + 0, int n2 = __LINE__, string s = __FILE__)
{
    assert(n1 < n2);
    printf("n1 = %d, n2 = %d, s = %.*s\n", n1, n2, s.length, s.ptr);
}

void test8()
{
    foo8();
}

/***************************************************/

void foo9(int n1 = __LINE__ + 0, int n2 = __LINE__, string s = __FILE__)()
{
    assert(n1 < n2);
    printf("n1 = %d, n2 = %d, s = %.*s\n", n1, n2, s.length, s.ptr);
}

void test9()
{
    foo9();
}

/***************************************************/

int foo10(char c) pure nothrow
{
    return 1;
}

void test10()
{
    int function(char c) fp;
    int function(char c) pure nothrow fq;

    fp = &foo10;
    fq = &foo10;
}

/***************************************************/

class Base11 {}
class Derived11 : Base11 {}
class MoreDerived11 : Derived11 {}

int fun11(Base11) { return 1; }
int fun11(Derived11) { return 2; }

void test11()
{
    MoreDerived11 m;

    auto i = fun11(m);
    assert(i == 2);
}

/***************************************************/

interface ABC {};
interface AB: ABC {};
interface BC: ABC {};
interface AC: ABC {};
interface A: AB, AC {};
interface B: AB, BC {};
interface C: AC, BC {};

int f12(AB ab) { return 1; }
int f12(ABC abc) { return 2; }

void test12()
{
    A a;
    auto i = f12(a);
    assert(i == 1);
}

/***************************************************/

template Foo13(alias x)
{
    enum bar = x + 1;
}

static assert(Foo13!(2+1).bar == 4);

template Bar13(alias x)
{
    enum bar = x;
}

static assert(Bar13!("abc").bar == "abc");

void test13()
{
}

/***************************************************/

template Foo14(alias a)
{
    alias Bar14!(a) Foo14;
}

int Bar14(alias a)()
{
    return a.sizeof;
}

void test14()
{
    auto i = Foo14!("hello")();
    printf("i = %d\n", i);
    assert(i == "hello".sizeof);
    i = Foo14!(1)();
    printf("i = %d\n", i);
    assert(i == 4);
}

/***************************************************/

auto foo15()(int x)
{
    return 3 + x;
}

void test15()
{

    auto bar()(int x)
    {
        return 5 + x;
    }

    printf("%d\n", foo15(4));
    printf("%d\n", bar(4));
}

/***************************************************/

int foo16(int x) { return 1; }
int foo16(ref int x) { return 2; }

void test16()
{
    int y;
    auto i = foo16(y);
    printf("i == %d\n", i);
    assert(i == 2);
    i = foo16(3);
    assert(i == 1);
}

/***************************************************/

class A17 { }
class B17 : A17 { }
class C17 : B17 { }

int foo17(A17, ref int x) { return 1; }
int foo17(B17, ref int x) { return 2; }

void test17()
{
    C17 c;
    int y;
    auto i = foo17(c, y);
    printf("i == %d\n", i);
    assert(i == 2);
}

/***************************************************/

class C18
{
    void foo(int x) { foo("abc"); }
    void foo(string s) { }  // this is hidden, but that's ok 'cuz no overlap
    void bar()
    {
        foo("abc");
    }
}

class D18 : C18
{
    override void foo(int x) { }
}

void test18()
{
    D18 d = new D18();
    d.bar();
}

/***************************************************/

int foo19(alias int a)() { return a; }

void test19()
{
    int y = 7;
    auto i = foo19!(y)();
    printf("i == %d\n", i);
    assert(i == 7);

    i = foo19!(4)();
    printf("i == %d\n", i);
    assert(i == 4);
}

/***************************************************/

template Foo20(int x) if (x & 1)
{
    const int Foo20 = 6;
}

template Foo20(int x) if ((x & 1) == 0)
{
    const int Foo20 = 7;
}

void test20()
{
    int i = Foo20!(3);
    printf("%d\n", i);
    assert(i == 6);
    i = Foo20!(4);
    printf("%d\n", i);
    assert(i == 7);
}

/***************************************************/

template isArray21(T : U[], U)
{
  static const isArray21 = 1;
}

template isArray21(T)
{
  static const isArray21 = 0;
}

int foo21(T)(T x) if (isArray21!(T))
{
    return 1;
}

int foo21(T)(T x) if (!isArray21!(T))
{
    return 2;
}

void test21()
{
    auto i = foo21(5);
    assert(i == 2);
    int[] a;
    i = foo21(a);
    assert(i == 1);
}

/***************************************************/

void test22()
{
    immutable uint x, y;
    foreach (i; x .. y) {}
}

/***************************************************/

const bool foo23 = is(typeof(function void() { }));
const bar23      = is(typeof(function void() { }));

void test23()
{
    assert(foo23 == true);
    assert(bar23 == true);
}

/***************************************************/

ref int foo24(int i)
{
    static int x;
    x = i;
    return x;
}

void test24()
{
    int x = foo24(3);
    assert(x == 3);
}

/***************************************************/

ref int foo25(int i)
{
    static int x;
    x = i;
    return x;
}

int bar25(ref int x)
{
    return x + 1;
}

void test25()
{
    int x = bar25(foo25(3));
    assert(x == 4);
}

/***************************************************/

static int x26;

ref int foo26(int i)
{
    x26 = i;
    return x26;
}

void test26()
{
    int* p = &foo26(3);
    assert(*p == 3);
}

/***************************************************/

static int x27 = 3;

ref int foo27(int i)
{
    return x27;
}

void test27()
{
    foo27(3) = 4;
    assert(x27 == 4);
}

/***************************************************/

ref int foo28(ref int x) { return x; }

void test28()
{
    int a;
    foo28(a);
}

/***************************************************/

void wyda(int[] a) { printf("aaa\n"); }
void wyda(int[int] a) { printf("bbb\n"); }

struct S29
{
    int[] a;

    void wyda()
    {
        a.wyda;
        a.wyda();
    }
}

void test29()
{
    int[] a;
    a.wyda;
    int[5] b;
    b.wyda;
    int[int] c;
    c.wyda;

    S29 s;
    s.wyda();
}

/***************************************************/

void foo30(D)(D arg) if (isIntegral!D)
{
}

struct S30(T) { }
struct U30(int T) { }

alias int myint30;

void test30()
{

    S30!myint30 u;
    S30!int s;
    S30!(int) t = s;

//    U30!3 v = s;
}

/***************************************************/

class A31
{
    void foo(int* p) { }
}

class B31 : A31
{
    override void foo(scope int* p) { }
}

void test31()
{
}

/***************************************************/

void bar32() { }

nothrow void foo32(int* p)
{
    //try { bar32(); } catch (Object o) { }
    try { bar32(); } catch (Throwable o) { }
    try { bar32(); } catch (Exception o) { }
}

void test32()
{   int i;

    foo32(&i);
}

/***************************************************/

struct Integer
{
    this(int i)
    {
        this.i = i;
    }

    this(long ii)
    {
      i = 3;
    }

    const int i;
}

void test33()
{
}

/***************************************************/

void test34()
{
  alias uint Uint;
  foreach(Uint u;1..10) {}
  for(Uint u=1;u<10;u++) {}
}

/***************************************************/

ref int foo35(bool condition, ref int lhs, ref int rhs)
{
        if ( condition ) return lhs;
        return rhs;
}

ref int bar35()(bool condition, ref int lhs, ref int rhs)
{
        if ( condition ) return lhs;
        return rhs;
}

void test35()
{
        int a = 10, b = 11;

        foo35(a<b, a, b) = 42;
        printf("a = %d and b = %d\n", a, b); // a = 42 and b = 11
        assert(a == 42 && b == 11);

        bar35(a<b, a, b) = 52;
        printf("a = %d and b = %d\n", a, b);
        assert(a == 42 && b == 52);
}

/***************************************************/

int foo36(T...)(T ts)
if (T.length > 1)
{
    return T.length;
}

int foo36(T...)(T ts)
if (T.length <= 1)
{
    return T.length * 7;
}

void test36()
{
    auto i = foo36!(int,int)(1, 2);
    assert(i == 2);
    i = foo36(1, 2, 3);
    assert(i == 3);
    i = foo36(1);
    assert(i == 7);
    i = foo36();
    assert(i == 0);
}


/***************************************************/

void test6685()
{
    struct S { int x; }
    with({ return S(); }())
    {
        x++;
    }
}

/***************************************************/

struct A37(alias T)
{
}

void foo37(X)(X x) if (is(X Y == A37!(U), alias U))
{
}

void bar37() {}

void test37()
{
    A37!(bar37) a2;
    foo37(a2);
    foo37!(A37!bar37)(a2);
}

/***************************************************/

struct A38
{
    this(this)
    {
        printf("B's copy\n");
    }
    bool empty() {return false;}
    void popFront() {}
    int front() { return 1; }
//    ref A38 opSlice() { return this; }
}

void test38()
{
    A38 a;
    int i;
    foreach (e; a) { if (++i == 100) break; }
}

/***************************************************/

alias int function() Fun39;
alias ref int function() Gun39;
static assert(!is(Fun39 == Gun39));

void test39()
{
}

/***************************************************/

int x40;

struct Proxy
{
    ref int at(int i)() { return x40; }
}

void test40()
{
    Proxy p;
    auto x = p.at!(1);
}

/***************************************************/

template Foo41(TList...)
{
    alias TList Foo41;
}

alias Foo41!(immutable(ubyte)[], ubyte[]) X41;

void test41()
{
}


/***************************************************/

bool endsWith(A1, A2)(A1 longer, A2 shorter)
{
    static if (is(typeof(longer[0 .. 0] == shorter)))
    {
    }
    else
    {
    }
    return false;
}

void test42()
{
    char[] a;
    byte[] b;
    endsWith(a, b);
}

/***************************************************/

void f43(S...)(S s) if (S.length > 3)
{
}

void test43()
{
    f43(1, 2, 3, 4);
}


/***************************************************/

struct S44(int x = 1){}

void fun()(S44!(1) b) { }

void test44()
{
    S44!() s;
    fun(s);
}

/***************************************************/
// 2006

void test2006()
{
    string [][] aas = [];
    assert(aas.length == 0);
    aas ~= cast (string []) [];
    assert(aas.length == 1);
    aas = aas ~ cast (string []) [];
    assert(aas.length == 2);
}

/***************************************************/
// 8442

void test8442()
{
    enum int[] fooEnum = [];
    immutable fooImmutable = fooEnum;
}

/***************************************************/

class A45
{
  int x;
  int f()
  {
    printf("A\n");
    return 1;
  }
}

class B45 : A45
{
  override const int f()
  {
    printf("B\n");
    return 2;
  }
}

void test45()
{
    A45 y = new B45;
    int i = y.f;
    assert(i == 2);
}

/***************************************************/

void text10682()
{
    ulong x = 1;
    ulong y = 2 ^^ x;
}

/***************************************************/

struct Test46
{
 int foo;
}

void test46()
{
    enum Test46 test = {};
    enum q = test.foo;
}

/***************************************************/

pure int double_sqr(int x) {
    int y = x;
    void do_sqr() { y *= y; }
    do_sqr();
    return y;
}

void test47()
{
   assert(double_sqr(10) == 100);
}

/***************************************************/

void sort(alias less)(string[] r)
{
            bool pred()
            {
                return less("a", "a");
            }
            .sort!(less)(r);
}

void foo48()
{
    int[string] freqs;
    string[] words;

sort!((a, b) { return freqs[a] > freqs[b]; })(words);
sort!((string a, string b) { return freqs[a] > freqs[b]; })(words);
//sort!(bool (a, b) { return freqs[a] > freqs[b]; })(words);
//sort!(function (a, b) { return freqs[a] > freqs[b]; })(words);
//sort!(function bool(a, b) { return freqs[a] > freqs[b]; })(words);
sort!(delegate bool(string a, string b) { return freqs[a] > freqs[b]; })(words);

}

void test48()
{
}

/***************************************************/
// 6408

static assert(!is(typeof(string[0..1].init)));
static assert(is(typeof(string[].init) == string[]));
static assert(is(typeof(string[][].init) == string[][]));
static assert(is(typeof(string[][][].init) == string[][][]));

static assert(is(typeof(string[1].init) == string[1]));
static assert(is(typeof(string[1][1].init) == string[1][1]));
static assert(is(typeof(string[1][1][1].init) == string[1][1][1]));

static assert(is(typeof(string[string].init) == string[string]));
static assert(is(typeof(string[string][string].init) == string[string][string]));
static assert(is(typeof(string[string][string][string].init) == string[string][string][string]));

template TT6408(T...) { alias T TT6408; }
static assert(is(typeof(TT6408!(int, int)[].init) == TT6408!(int, int)));
static assert(is(typeof(TT6408!(int, int)[0..$].init) == TT6408!(int, int)));
static assert(is(typeof(TT6408!(int, int)[$-1].init) == int));

/***************************************************/
// 9409

template TT9409(T...) { alias T TT9409; }

template idxTypes9409(Prefix...)
{
    TT9409!((Prefix[$-1])) idxTypes9409;
}

alias idxTypes9409!(int) Types9409;

/***************************************************/

struct S49
{
    static void* p;

    this( string name )
    {
        printf( "(ctor) &%.*s.x = %p\n", name.length, name.ptr, &x );
        p = cast(void*)&x;
    }

    invariant() {}

    int x;
}

void test49()
{
    auto s = new S49("s2");

    printf( "&s2.x = %p\n", &s.x );
    assert(cast(void*)&s.x == S49.p);
}

/***************************************************/

auto max50(Ts...)(Ts args)
  if (Ts.length >= 2
        && is(typeof(Ts[0].init > Ts[1].init ? Ts[1].init : Ts[0].init)))
{
    static if (Ts.length == 2)
        return args[1] > args[0] ? args[1] : args[0];
    else
        return max50(max50(args[0], args[1]), args[2 .. $]);
}

void test50()
{
   assert(max50(4, 5) == 5);
   assert(max50(2.2, 4.5) == 4.5);
   assert(max50("Little", "Big") == "Little");

   assert(max50(4, 5.5) == 5.5);
   assert(max50(5.5, 4) == 5.5);
}

/***************************************************/

void test51()
{
    static immutable int[2] array = [ 42 ];
    enum e = array[1];
    static immutable int[1] array2 = [ 0: 42 ];
    enum e2 = array2[0];
    assert(e == 0);
    assert(e2 == 42);
}

/***************************************************/

enum ubyte[4] a52 = [5,6,7,8];

void test52()
{
  int x=3;
  assert(a52[x]==8);
}

/***************************************************/

void test53()
{
    size_t func2(immutable(void)[] t)
    {
        return 0;
    }
}

/***************************************************/

void foo54(void delegate(void[]) dg) { }

void test54()
{
    void func(void[] t) pure { }
    foo54(&func);

//    void func2(const(void)[] t) { }
//    foo54(&func2);
}

/***************************************************/

class Foo55
{
    synchronized void noop1() { }
    void noop2() shared { }
}

void test55()
{
    auto foo = new shared(Foo55);
    foo.noop1();
    foo.noop2();
}

/***************************************************/

enum float one56 = 1 * 1;
template X56(float E) { int X56 = 2; }
alias X56!(one56 * one56) Y56;

void test56()
{
    assert(Y56 == 2);
}

/***************************************************/

void test57()
{
    alias shared(int) T;
    assert (is(T == shared));
}

/***************************************************/

struct A58
{
  int a,b;
}

void test58()
{
  A58[2] rg=[{1,2},{5,6}];
  assert(rg[0].a == 1);
  assert(rg[0].b == 2);
  assert(rg[1].a == 5);
  assert(rg[1].b == 6);
}

/***************************************************/

class A59 {
    const foo(int i)  { return i; }
}

/***************************************************/

void test60()
{
    enum real ONE = 1.0;
    real x;
    for (x=0.0; x<10.0; x+=ONE)
        printf("%Lg\n", x);
    printf("%Lg\n", x);
    assert(x == 10);
}

/***************************************************/

pure immutable(T)[] fooPT(T)(immutable(T)[] x, immutable(T)[] y){

  immutable(T)[] fooState;

  immutable(T)[] bar(immutable(T)[] x){
    fooState = "hello ";
    return x ~ y;
  }

  return fooState ~ bar(x);

}


void test61()
{
  writeln(fooPT("p", "c"));
}

/***************************************************/

void test9577()
{
    static int function(int)[] foo = [x => x];
    foo[0](0);
}

/***************************************************/

int[3] foo62(int[3] a)
{
    a[1]++;
    return a;
}

void test62()
{
    int[3] b;
    b[0] = 1;
    b[1] = 2;
    b[2] = 3;
    auto c = foo62(b);
    assert(b[0] == 1);
    assert(b[1] == 2);
    assert(b[2] == 3);

    assert(c[0] == 1);
    assert(c[1] == 3);
    assert(c[2] == 3);
}

/***************************************************/

void test3927()
{
    int[] array;
    assert(array.length++ == 0);
    assert(array.length == 1);
    assert(array.length-- == 1);
    assert(array.length == 0);
}

/***************************************************/

void test63()
{
    int[3] b;
    b[0] = 1;
    b[1] = 2;
    b[2] = 3;
    auto c = b;
    b[1]++;
    assert(b[0] == 1);
    assert(b[1] == 3);
    assert(b[2] == 3);

    assert(c[0] == 1);
    assert(c[1] == 2);
    assert(c[2] == 3);
}

/***************************************************/

void test64()
{
    int[3] b;
    b[0] = 1;
    b[1] = 2;
    b[2] = 3;
    int[3] c;
    c = b;
    b[1]++;
    assert(b[0] == 1);
    assert(b[1] == 3);
    assert(b[2] == 3);

    assert(c[0] == 1);
    assert(c[1] == 2);
    assert(c[2] == 3);
}

/***************************************************/

int[2] foo65(int[2] a)
{
    a[1]++;
    return a;
}

void test65()
{
    int[2] b;
    b[0] = 1;
    b[1] = 2;
    int[2] c = foo65(b);
    assert(b[0] == 1);
    assert(b[1] == 2);

    assert(c[0] == 1);
    assert(c[1] == 3);
}

/***************************************************/

int[1] foo66(int[1] a)
{
    a[0]++;
    return a;
}

void test66()
{
    int[1] b;
    b[0] = 1;
    int[1] c = foo66(b);
    assert(b[0] == 1);
    assert(c[0] == 2);
}

/***************************************************/

int[2] foo67(out int[2] a)
{
    a[0] = 5;
    a[1] = 6;
    return a;
}

void test67()
{
    int[2] b;
    b[0] = 1;
    b[1] = 2;
    int[2] c = foo67(b);
    assert(b[0] == 5);
    assert(b[1] == 6);

    assert(c[0] == 5);
    assert(c[1] == 6);
}

/***************************************************/

void test68()
{
    digestToString(cast(ubyte[16])x"c3fcd3d76192e4007dfb496cca67e13b");
}

void digestToString(const ubyte[16] digest)
{
    assert(digest[0] == 0xc3);
    assert(digest[15] == 0x3b);
}

/***************************************************/

void test69()
{
    digestToString69(cast(ubyte[16])x"c3fcd3d76192e4007dfb496cca67e13b");
}

void digestToString69(ref const ubyte[16] digest)
{
    assert(digest[0] == 0xc3);
    assert(digest[15] == 0x3b);
}

/***************************************************/

void test70()
{
    digestToString70("1234567890123456");
}

void digestToString70(ref const char[16] digest)
{
    assert(digest[0] == '1');
    assert(digest[15] == '6');
}

/***************************************************/

void foo71(out shared int o) {}

/***************************************************/

struct foo72
{
  int bar() shared { return 1; }
}

void test72()
{
  shared foo72 f;
  auto x = f.bar;
}

/***************************************************/

class Foo73
{
  static if (is(typeof(this) T : shared T))
    static assert(0);
  static if (is(typeof(this) U == shared U))
    static assert(0);
  static if (is(typeof(this) U == const U))
    static assert(0);
  static if (is(typeof(this) U == immutable U))
    static assert(0);
  static if (is(typeof(this) U == const shared U))
    static assert(0);

  static assert(!is(int == const));
  static assert(!is(int == immutable));
  static assert(!is(int == shared));

  static assert(is(int == int));
  static assert(is(const(int) == const));
  static assert(is(immutable(int) == immutable));
  static assert(is(shared(int) == shared));
  static assert(is(const(shared(int)) == shared));
  static assert(is(const(shared(int)) == const));

  static assert(!is(const(shared(int)) == immutable));
  static assert(!is(const(int) == immutable));
  static assert(!is(const(int) == shared));
  static assert(!is(shared(int) == const));
  static assert(!is(shared(int) == immutable));
  static assert(!is(immutable(int) == const));
  static assert(!is(immutable(int) == shared));
}

template Bar(T : T)
{
    alias T Bar;
}

template Barc(T : const(T))
{
    alias T Barc;
}

template Bari(T : immutable(T))
{
    alias T Bari;
}

template Bars(T : shared(T))
{
    alias T Bars;
}

template Barsc(T : shared(const(T)))
{
    alias T Barsc;
}


void test73()
{
    auto f = new Foo73;

    alias int T;

    // 5*5 == 25 combinations, plus 2 for swapping const and shared

    static assert(is(Bar!(T) == T));
    static assert(is(Bar!(const(T)) == const(T)));
    static assert(is(Bar!(immutable(T)) == immutable(T)));
    static assert(is(Bar!(shared(T)) == shared(T)));
    static assert(is(Bar!(shared(const(T))) == shared(const(T))));

    static assert(is(Barc!(const(T)) == T));
    static assert(is(Bari!(immutable(T)) == T));
    static assert(is(Bars!(shared(T)) == T));
    static assert(is(Barsc!(shared(const(T))) == T));

    static assert(is(Barc!(T) == T));
    static assert(is(Barc!(immutable(T)) == T));
    static assert(is(Barc!(const(shared(T))) == shared(T)));
    static assert(is(Barsc!(immutable(T)) == T));

    static assert(is(Bars!(const(shared(T))) == const(T)));
    static assert(is(Barsc!(shared(T)) == T));

    Bars!(shared(const(T))) b;
    pragma(msg, typeof(b));

    static assert(is(Bars!(shared(const(T))) == const(T)));
    static assert(is(Barc!(shared(const(T))) == shared(T)));

    static assert(!is(Bari!(T)));
    static assert(!is(Bari!(const(T))));
    static assert(!is(Bari!(shared(T))));
    static assert(!is(Bari!(const(shared(T)))));

    static assert(is(Barc!(shared(T))));

    static assert(!is(Bars!(T)));
    static assert(!is(Bars!(const(T))));
    static assert(!is(Bars!(immutable(T))));
    static assert(!is(Barsc!(T)));
    static assert(!is(Barsc!(const(T))));
}

/***************************************************/

pure nothrow {
  alias void function(int) A74;
}

alias void function(int) pure nothrow B74;
alias pure nothrow void function(int) C74;

void test74()
{
    A74 a = null;
    B74 b = null;
    C74 c = null;
    a = b;
    a = c;
}

/***************************************************/

void test9212()
{
    int[int] aa;
    foreach (const key, const val; aa) {}
    foreach (size_t key, size_t val; aa) {}
}

/***************************************************/

class A75
{
    pure static void raise(string s)
    {
        throw new Exception(s);
    }
}

void test75()
{   int x = 0;
    try
    {
        A75.raise("a");
    } catch (Exception e)
    {
        x = 1;
    }
    assert(x == 1);
}

/***************************************************/

void test76()
{
    int x, y;
    bool which;
    (which ? x : y) += 5;
    assert(y == 5);
}

/***************************************************/

void test77()
{
    auto a = ["hello", "world"];
    pragma(msg, typeof(a));
    auto b = a;
    assert(a is b);
    assert(a == b);
    b = a.dup;
    assert(a == b);
    assert(a !is b);
}

/***************************************************/

void test78()
{
    auto array = [0, 2, 4, 6, 8, 10];
    array = array[0 .. $ - 2];         // Right-shrink by two elements
    assert(array == [0, 2, 4, 6]);
    array = array[1 .. $];             // Left-shrink by one element
    assert(array == [2, 4, 6]);
    array = array[1 .. $ - 1];         // Shrink from both sides
    assert(array == [4]);
}

/***************************************************/

void test79()
{
    auto a = [87, 40, 10];
    a ~= 42;
    assert(a == [87, 40, 10, 42]);
    a ~= [5, 17];
    assert(a == [87, 40, 10, 42, 5, 17]);
}

/***************************************************/

void test6317()
{
    int b = 12345;
    struct nested { int a; int fun() { return b; } }
    static assert(!__traits(compiles, { nested x = { 3, null }; }));
    nested g = { 7 };
    auto h = nested(7);
    assert(g.fun() == 12345);
    assert(h.fun() == 12345);
}

/***************************************************/

void test80()
{
    auto array = new int[10];
    array.length += 1000;
    assert(array.length == 1010);
    array.length /= 10;
    assert(array.length == 101);
    array.length -= 1;
    assert(array.length == 100);
    array.length |= 1;
    assert(array.length == 101);
    array.length ^= 3;
    assert(array.length == 102);
    array.length &= 2;
    assert(array.length == 2);
    array.length *= 2;
    assert(array.length == 4);
    array.length <<= 1;
    assert(array.length == 8);
    array.length >>= 1;
    assert(array.length == 4);
    array.length >>>= 1;
    assert(array.length == 2);
    array.length %= 2;
    assert(array.length == 0);

    int[]* foo()
    {
        static int x;
        x++;
        assert(x == 1);
        auto p = &array;
        return p;
    }

    (*foo()).length += 2;
    assert(array.length == 2);
}

/***************************************************/

void test81()
{
    int[3] a = [1, 2, 3];
    int[3] b = a;
    a[1] = 42;
    assert(b[1] == 2); // b is an independent copy of a
    int[3] fun(int[3] x, int[3] y) {
       // x and y are copies of the arguments
       x[0] = y[0] = 100;
       return x;
    }
    auto c = fun(a, b);         // c has type int[3]
    assert(c == [100, 42, 3]);
    assert(b == [1, 2, 3]);     // b is unaffected by fun
}

/***************************************************/

void test82()
{
    auto a1 = [ "Jane":10.0, "Jack":20, "Bob":15 ];
    auto a2 = a1;                    // a1 and a2 refer to the same data
    a1["Bob"] = 100;                 // Changing a1
    assert(a2["Bob"] == 100);        //is same as changing a2
    a2["Sam"] = 3.5;                 //and vice
    assert(a2["Sam"] == 3.5);        //    versa
}

/***************************************************/

void test7942()
{
    string a = "a";
    wstring b = "b";
    dstring c = "c";

    a ~= "a"c;
    static assert(!is(typeof(a ~= "b"w)));
    static assert(!is(typeof(a ~= "c"d)));
    static assert(!is(typeof(b ~= "a"c)));
    b ~= "b"w;
    static assert(!is(typeof(b ~= "c"d)));
    static assert(!is(typeof(c ~= "a"c)));
    static assert(!is(typeof(c ~= "b"w)));
    c ~= "c"d;

    assert(a == "aa");
    assert(b == "bb");
    assert(c == "cc");
}

/***************************************************/

void bump(ref int x) { ++x; }

void test83()
{
   int x = 1;
   bump(x);
   assert(x == 2);
}

/***************************************************/

interface Test4174
{
    void func(T)() {}
}

/***************************************************/

auto foo84 = [1, 2.4];

void test84()
{
    pragma(msg, typeof([1, 2.4]));
    static assert(is(typeof([1, 2.4]) == double[]));

    pragma(msg, typeof(foo84));
    static assert(is(typeof(foo84) == double[]));
}

/***************************************************/

void test85()
{
    dstring c = "V\u00E4rld";
    c = c ~ '!';
    assert(c == "V\u00E4rld!");
    c = '@' ~ c;
    assert(c == "@V\u00E4rld!");

    wstring w = "V\u00E4rld";
    w = w ~ '!';
    assert(w == "V\u00E4rld!");
    w = '@' ~ w;
    assert(w == "@V\u00E4rld!");

    string s = "V\u00E4rld";
    s = s ~ '!';
    assert(s == "V\u00E4rld!");
    s = '@' ~ s;
    assert(s == "@V\u00E4rld!");
}

/***************************************************/

void test86()
{
    int[][] a = [ [1], [2,3], [4] ];
    int[][] w = [ [1, 2], [3], [4, 5], [] ];
    int[][] x = [ [], [1, 2], [3], [4, 5], [] ];
}

/***************************************************/

// Bugzilla 3379

T1[] find(T1, T2)(T1[] longer, T2[] shorter)
   if (is(typeof(longer[0 .. 1] == shorter) : bool))
{
   while (longer.length >= shorter.length) {
      if (longer[0 .. shorter.length] == shorter) break;
      longer = longer[1 .. $];
   }
   return longer;
}

auto max(T...)(T a)
      if (T.length == 2
         && is(typeof(a[1] > a[0] ? a[1] : a[0]))
       || T.length > 2
         && is(typeof(max(max(a[0], a[1]), a[2 .. $])))) {
   static if (T.length == 2) {
      return a[1] > a[0] ? a[1] : a[0];
   } else {
      return max(max(a[0], a[1]), a[2 .. $]);
   }
}

// Cases which would ICE or segfault
struct Bulldog(T){
    static void cat(Frog)(Frog f) if (true)
    {    }
}

void mouse(){
    Bulldog!(int).cat(0);
}

void test87()
{
    double[] d1 = [ 6.0, 1.5, 2.4, 3 ];
    double[] d2 = [ 1.5, 2.4 ];
    assert(find(d1, d2) == d1[1 .. $]);
    assert(find(d1, d2) == d1[1 .. $]); // Check for memory corruption
    assert(max(4, 5) == 5);
    assert(max(3, 4, 5) == 5);
}

/***************************************************/

template test4284(alias v) { enum test4284 = v.length == 0; }
static assert(test4284!(cast(string)null));
static assert(test4284!(cast(string[])null));

/***************************************************/

struct S88
{
    void opDispatch(string s, T)(T i)
    {
        printf("S.opDispatch('%.*s', %d)\n", s.length, s.ptr, i);
    }
}

class C88
{
    void opDispatch(string s)(int i)
    {
        printf("C.opDispatch('%.*s', %d)\n", s.length, s.ptr, i);
    }
}

struct D88
{
    template opDispatch(string s)
    {
        enum int opDispatch = 8;
    }
}

void test88()
{
    S88 s;
    s.opDispatch!("hello")(7);
    s.foo(7);

    auto c = new C88();
    c.foo(8);

    D88 d;
    printf("d.foo = %d\n", d.foo);
    assert(d.foo == 8);
}

/***************************************************/

void test89() {
    static struct X {
        int x;
        int bar() { return x; }
    }
    X s;
    printf("%d\n", s.sizeof);
    assert(s.sizeof == 4);
}

/***************************************************/

struct S90
{
    void opDispatch( string name, T... )( T values )
    {
        assert(values[0] == 3.14);
    }
}

void test90( )
{   S90 s;
    s.opDispatch!("foo")( 3.14 );
    s.foo( 3.14 );
}

/***************************************************/

struct A7439(int r, int c)
{
    alias r R;
    alias c C;
    alias float[R * C] Data;
    Data _data;
    alias _data this;

    this(Data ar){ _data = ar; }

    pure ref float opIndex(size_t rr, size_t cc){ return _data[cc + rr * C]; }
}

void test7439()
{
    A7439!(2, 2) a = A7439!(2, 2)([8, 3, 2, 9]);
    a[0,0] -= a[0,0] * 2.0;
}

/***************************************************/

void foo91(uint line = __LINE__) { printf("%d\n", line); }

void test91()
{
    foo91();
    printf("%d\n", __LINE__);
}

/***************************************************/

bool fun13468(Object e, typeof(null) needle)
{
    return (e == needle);
}

void test13468()
{
    assert(fun13468(null, null));
}

/***************************************************/

auto ref foo92(ref int x) { return x; }
int bar92(ref int x) { return x; }

void test92()
{
    int x = 3;
    int i = bar92(foo92(x));
    assert(i == 3);
}

/***************************************************/

struct Foo93
{
    public int foo() const
    {
        return 2;
    }
}

void test93()
{
    const Foo93 bar = Foo93();
    enum bla = bar.foo();
    assert(bla == 2);
}

/***************************************************/


extern(C++) class C1687
{
    void func() {}
}

void test1687()
{
    auto c = new C1687();
    assert(c.__vptr[0] == (&c.func).funcptr);
}

/***************************************************/

struct Foo94
{
    int x, y;
    real z;
}

pure nothrow Foo94 makeFoo(const int x, const int y)
{
    return Foo94(x, y, 3.0);
}

void test94()
{
    auto f = makeFoo(1, 2);
    assert(f.x==1);
    assert(f.y==2);
    assert(f.z==3);
}

/***************************************************/

struct T95
{
    @disable this(this)
    {
    }
}

struct S95
{
    T95 t;
}

@disable void foo95() { }

struct T95A
{
    @disable this(this);
}

struct S95A
{
    T95A t;
}

@disable void foo95A() { }

void test95()
{
    S95 s;
    S95 t;
    static assert(!__traits(compiles, t = s));
    static assert(!__traits(compiles, foo95()));

    S95A u;
    S95A v;
    static assert(!__traits(compiles, v = u));
    static assert(!__traits(compiles, foo95A()));
}

/***************************************************/

struct S96(alias init)
{
    int[] content = init;
}

void test96()
{
    S96!([12, 3]) s1;
    S96!([1, 23]) s2;
    writeln(s1.content);
    writeln(s2.content);
    assert(!is(typeof(s1) == typeof(s2)));
}

/***************************************************/

struct A97
{
    const bool opEquals(ref const A97) { return true; }
    ref A97 opUnary(string op)() if (op == "++")
    {
        return this;
    }
}

void test97()
{
    A97 a, b;
    foreach (e; a .. b) {
    }
}

/***************************************************/

void test98()
{
    auto a = new int[2];
    // the name "length" should not pop up in an index expression
    static assert(!is(typeof(a[length - 1])));
}

/***************************************************/

string s99;

void bar99(string i)
{
}

void function(string) foo99(string i)
{
    return &bar99;
}

void test99()
{
    foo99 (s99 ~= "a") (s99 ~= "b");
    assert(s99 == "ab");
}

/***************************************************/
// 5081

void test5081()
{
    static pure immutable(int[]) x1()
    {
        int[] a = new int[](10);
        return a;
    }
    static pure immutable(int[]) x2(int len)
    {
        int[] a = new int[](len);
        return a;
    }
    static pure immutable(int[]) x3(immutable(int[]) org)
    {
        int[] a = new int[](org.length);
        return a;
    }

    immutable a1 = x1();
    immutable a2 = x2(10);
    immutable a3 = x3([1,2]);

    static pure int[] y1()
    {
        return new int[](10);
    }

    immutable b1 = y1();
}

/***************************************************/

void test100()
{
   string s;

   /* Testing order of evaluation */
   void delegate(string, string) fun(string) {
      s ~= "b";
      return delegate void(string x, string y) { s ~= "e"; };
   }
   fun(s ~= "a")(s ~= "c", s ~= "d");
   assert(s == "abcde", s);
}

/***************************************************/

void test101()
{
   int[] d1 = [ 6, 1, 2 ];
   byte[] d2 = [ 6, 1, 2 ];
   assert(d1 == d2);
   d2 ~= [ 6, 1, 2 ];
   assert(d1 != d2);
}

/***************************************************/


void test5403()
{
    struct S
    {
        static int front;
        enum back = "yes!";
        bool empty;
        void popAny() { empty = true; }
        alias popAny popFront;
        alias popAny popBack;
    }
    S.front = 7;
    foreach(int i; S()) assert(i == 7);
    S.front = 2;
    foreach(i; S()) assert(i == 2);
    foreach_reverse(i; S()) assert(i == "yes!");
}

/***************************************************/

static assert([1,2,3] == [1.0,2,3]);

/***************************************************/

int transmogrify(uint) { return 1; }
int transmogrify(long) { return 2; }

void test103()
{
    assert(transmogrify(42) == 1);
}

/***************************************************/

int foo104(int x)
{
    int* p = &(x += 1);
    return *p;
}

int bar104(int *x)
{
    int* p = &(*x += 1);
    return *p;
}

void test104()
{
    auto i = foo104(1);
    assert(i == 2);
    i = bar104(&i);
    assert(i == 3);
}

/***************************************************/

ref int bump105(ref int x) { return ++x; }

void test105()
{
   int x = 1;
   bump105(bump105(x)); // two increments
   assert(x == 3);
}

/***************************************************/

pure int genFactorials(int n) {
    static pure int factorial(int n) {
      if (n==2) return 1;
      return factorial(2);
    }
    return factorial(n);
}

/***************************************************/

void test107()
{
    int[6] a;
    writeln(a);
    writeln(a.init);
    assert(a.init == [0,0,0,0,0,0]);
}

/***************************************************/

class A109 {}

void test109()
{
    immutable(A109) b;
    A109 c;
    auto z = true ? b : c;
    //writeln(typeof(z).stringof);
    static assert(is(typeof(z) == const(A109)));
}

/***************************************************/

template Boo(T) {}
struct Foo110(T, alias V = Boo!T)
{
    pragma(msg, V.stringof);
    static const s = V.stringof;
}
alias Foo110!double B110;
alias Foo110!int A110;

static assert(B110.s == "Boo!double");
static assert(A110.s == "Boo!int");

/***************************************************/

int test11247()
{
    static assert(is(byte[typeof(int.init).sizeof] == byte[4]));
    static assert(is(byte[typeof(return).sizeof] == byte[4]));
    return 0;
}

/***************************************************/

// 3716
void test111()
{
    auto k1 = true ? [1,2] : []; // OK
    auto k2 = true ? [[1,2]] : [[]];
    auto k3 = true ? [] : [[1,2]];
    auto k4 = true ? [[[]]] : [[[1,2]]];
    auto k5 = true ? [[[1,2]]] : [[[]]];
    auto k6 = true ? [] : [[[]]];
    static assert(!is(typeof(true ? [[[]]] : [[1,2]]))); // Must fail
}

/***************************************************/
// 658

void test658()
{
    struct S { int i; }
    class C { int i; }

    S s;
    S* sp = &s;
    with (sp) i = 42;
    assert(s.i == 42);
    with (&s) i = 43;
    assert(s.i == 43);

    C c = new C;
    C* cp = &c;
    with (cp) i = 42;
    assert(c.i == 42);
    with (&c) i = 43;
    assert(c.i == 43);
}

/***************************************************/

void test3069()
{
    ubyte id = 0;
    void[] v = [id] ~ [id];
}

/***************************************************/
// 4303

template foo112() if (__traits(compiles,undefined))
{
    enum foo112 = false;
}

template foo112() if (true)
{
    enum foo112 = true;
}

pragma(msg,__traits(compiles,foo112!()));
static assert(__traits(compiles,foo112!()));

const bool bar112 = foo112!();


/***************************************************/

struct File113
{
    this(int name) { }

    ~this() { }

    void opAssign(File113 rhs) { }

    struct ByLine
    {
        File113 file;

        this(int) { }

    }

    ByLine byLine()
    {
        return ByLine(1);
    }
}

auto filter113(File113.ByLine rs)
{
    struct Filter
    {
        this(File113.ByLine r) { }
    }

    return Filter(rs);
}

void test113()
{
    auto f = File113(1);
    auto rx = f.byLine();
    auto file = filter113(rx);
}

/***************************************************/

template foo114(fun...)
{
    auto foo114(int[] args)
    {
        return 1;
    }
}

pragma(msg, typeof(foo114!"a + b"([1,2,3])));

/***************************************************/
// Bugzilla 3935

struct Foo115 {
    void opBinary(string op)(Foo other) {
        pragma(msg, "op: " ~ op);
        assert(0);
    }
}

void test115()
{
    Foo115 f;
    f = f;
}

/***************************************************/
// Bugzilla 2477

void foo116(T,)(T t) { T x; }

void test116()
{
    int[] data = [1,2,3,];  // OK

    data = [ 1,2,3, ];  // fails
    auto i = data[1,];
    foo116!(int)(3);
    foo116!(int,)(3);
    foo116!(int,)(3,);
}

/***************************************************/

void test1891()
{
    struct C {
        char[8] x = "helloabc";
    }

    int main()
    {
        C* a = new C;
        C*[] b;
        b ~= new C;

        auto g = a ~ b;
        assert(g[0] && g[1] && g[0].x == g[1].x);

        return 0;
    }
}

/***************************************************/
// Bugzilla 4291

void test117() pure
{
    mixin declareVariable;
    var = 42;
    mixin declareFunction;
    readVar();
}
template declareVariable() { int var; }
template declareFunction()
{
    int readVar() { return var; }
}

/***************************************************/
// Bugzilla 4177

pure real log118(real x) {
    if (__ctfe)
        return 0.0;
    else
        return 1.0;
}

enum x118 = log118(4.0);

void test118() {}

/***************************************************/

void bug4465()
{
    const a = 2 ^^ 2;
    int b = a;
}

/***************************************************/

pure void foo(int *p)
{
    *p = 3;
}

pure void test120()
{
    int i;
    foo(&i);
    assert(i == 3);
}

/***************************************************/
// 4866

immutable int[3] statik = [ 1, 2, 3 ];
enum immutable(int)[] dynamic = statik;

static assert(is(typeof(dynamic) == immutable(int)[]));
static if (! is(typeof(dynamic) == immutable(int)[]))
{
    static assert(0);   // (7)
}
pragma(msg, "!! ", typeof(dynamic));

/***************************************************/
// 2943

struct Foo2943
{
    int a;
    int b;
    alias b this;
}

void test122()
{
    Foo2943 foo, foo2;
    foo.a = 1;
    foo.b = 2;
    foo2.a = 3;
    foo2.b = 4;

    foo2 = foo;
    assert(foo2.a == foo.a);
}

/***************************************************/
// 4641

struct S123 {
    int i;
    alias i this;
}

void test123() {
    S123[int] ss;
    ss[0] = S123.init; // This line causes Range Violation.
}

/***************************************************/
// 2451

struct Foo124 {
    int z = 3;
    void opAssign(Foo124 x) { z= 2;}
}

struct Bar124 {
    int z = 3;
    this(this){ z = 17; }
}

void test124() {
    Foo124[string] stuff;
    stuff["foo"] = Foo124.init;
    assert(stuff["foo"].z == 3);
    stuff["foo"] = Foo124.init;
    assert(stuff["foo"].z == 2);

    Bar124[string] stuff2;
    Bar124 q;
    stuff2["dog"] = q;
    assert(stuff2["dog"].z == 17);
}

/***************************************************/

void test3022()
{
    static class Foo3022
    {
        new(size_t)
        {
            assert(0);
        }
    }

    scope x = new Foo3022;
}

/***************************************************/

void doNothing() {}

void bug5071(short d, ref short c) {
   assert(c==0x76);

    void closure() {
        auto c2 = c;
        auto d2 = d;
        doNothing();
    }
    auto useless = &closure;
}

void test125()
{
   short c = 0x76;
   bug5071(7, c);
}

/***************************************************/

struct Foo126
{
   static Foo126 opCall(in Foo126 _f) pure
   {
       return _f;
   }
}

/***************************************************/

void test796()
{
    struct S { invariant() { throw new Exception(""); } }
    S* s;
    try {
        assert(s);
    } catch (Error) {
    }
}

/***************************************************/

void test7077()
{
    if(0) mixin("auto x = 2;");
    auto x = 1;
}

/***************************************************/

struct Tuple127(S...)
{
    S expand;
    alias expand this;
}

alias Tuple127!(int, int) Foo127;

void test127()
{
    Foo127[] m_array;
    Foo127 f;
    m_array ~= f;
}

/***************************************************/

struct Bug4434 {}
alias const Bug4434* IceConst4434;
alias shared Bug4434* IceShared4434;
alias shared Bug4434[] IceSharedArray4434;
alias immutable Bug4434* IceImmutable4434;
alias shared const Bug4434* IceSharedConst4434;

alias int MyInt4434;
alias const MyInt4434[3] IceConstInt4434;

alias immutable string[] Bug4830;

/***************************************************/
// 4254

void bub(const inout int other) {}

void test128()
{
    bub(1);
}


/***************************************************/

pure nothrow @safe auto bug4915a() {  return 0; }
pure nothrow @safe int  bug4915b() { return bug4915a(); }

void bug4915c()
{
    pure nothrow @safe int d() { return 0; }
    int e() pure nothrow @safe { return d(); }
}

/***************************************************/
// 5164

static if (is(int Q == int, Z...))  { }

/***************************************************/
// 5195

alias typeof(foo5195) food5195;
const int * foo5195 = null;
alias typeof(foo5195) good5195;
static assert( is (food5195 == good5195));

/***************************************************/

version (Windows)
{
}
else
{
int[0] var5332;
void test5332() { auto x = var5332; }
}

/***************************************************/
// 5191

struct Foo129
{
    void add(T)(T value) nothrow
    {
        this.value += value;
    }

    this(int value)
    {
        this.value = value;
    }

    int value;
}

void test129()
{
    auto foo = Foo129(5);
    assert(foo.value == 5);

    foo.add(2);
    writeln(foo.value);
    assert(foo.value == 7);

    foo.add(3);
    writeln(foo.value);
    assert(foo.value == 10);

    foo.add(3);
    writeln(foo.value);
    assert(foo.value == 13);

    void delegate (int) nothrow dg = &foo.add!(int);
    dg(7);
    assert(foo.value == 20);
}

/***************************************************/
// 6169

auto ctfefunc6169() { return "{}"; }
enum ctfefptr6169 = &ctfefunc6169;
int ctfefunc6169a() { return 1; }
template x6169(string c) { alias int x6169; }
template TT6169(T...) { alias T TT6169; }
@property ctfeprop6169() { return "g"; }

void test6169() pure @safe
{
    enum a = ctfefunc6169();
    static b = ctfefunc6169();
    x6169!(ctfefunc6169()) tt;
    mixin(ctfefunc6169());
    static if(ctfefunc6169()) {}
    pragma(msg, ctfefunc6169());
    enum xx
    {
        k = 0,
        j = ctfefunc6169a()
    }
    auto g = mixin('"' ~ ctfefunc6169() ~ '"');
    //auto h = import("testx.d" ~ false ? ctfefunc() : "");
    alias TT6169!(int, int)[ctfefunc6169a()..ctfefunc6169a()] i;
    alias TT6169!(int, int)[ctfefunc6169a()] j;
    int[ctfefunc6169a()+1] k;
    alias int[ctfefunc6169a()] l;
    switch(1)
    {
    //case ctfefunc6169a(): // Can't do this because of case variables
    case ctfefunc6169a()+1:
        ..
    case ctfefunc6169a()+2:
    default:
        break;
    }
    static assert(ctfefunc6169a());
    void fun(int i : ctfefunc6169a() = ctfefunc6169a(), alias j)() if (ctfefunc6169a()) {}
    fun!(ctfefunc6169a(), ctfefunc6169())();
    enum z = ctfefptr6169();

    auto p = mixin(ctfeprop6169);
}

/***************************************************/
// 10506

void impureFunc10506() {}
string join10506(RoR)(RoR ror)
{
    impureFunc10506();
    return ror[0] ~ ror[1];
}

void test10506() pure
{
    void foobar() {}

    mixin(["foo", "bar"].join10506()~";");
}

/***************************************************/

const shared class C5107
{
    int x;
}

static assert(is(typeof(C5107.x) ==  const)); // okay
static assert(is(typeof(C5107.x) == shared)); // fails!

/***************************************************/

immutable struct S3598
{
    static void funkcja() { }
}

/***************************************************/
// 4211

@safe struct X130
{
    void func() {  }
}

@safe class Y130
{
    void func() {  }
}

@safe void test130()
{
    X130 x;
    x.func();
    auto y = new Y130;
    y.func();
}

/***************************************************/

template Return(alias fun)
{
    static if (is(typeof(fun) R == return)) alias R Return;
}

interface I4217
{
    int  square(int  n);
    real square(real n);
}
alias Return!( __traits(getOverloads, I4217, "square")[0] ) R4217;
alias Return!( __traits(getOverloads, I4217, "square")[1] ) S4217;

static assert(! is(R4217 == S4217));

/***************************************************/
// 5094

void test131()
{
    S131 s;
    int[] conv = s;
}

struct S131
{
    @property int[] get() { return [1,2,3]; }
    alias get this;
}

/***************************************************/

struct S7545
{
    uint id;
    alias id this;
}

void test7545()
{
    auto id = 0 ? S7545() : -1;
}

/***************************************************/
// 5020

void test132()
{
    S132 s;
    if (!s) {}
}

struct S132
{
    bool cond;
    alias cond this;
}

/***************************************************/
// 5343

struct Tuple5343(Specs...)
{
    Specs[0] field;
}

struct S5343(E)
{
    immutable E x;
}
enum A5343{a,b,c}
alias Tuple5343!(A5343) TA5343;
alias S5343!(A5343) SA5343;

/***************************************************/
// 5365

interface IFactory
{
    void foo();
}

class A133
{
    protected static class Factory : IFactory
    {
        void foo()
        {
        }
    }

    this()
    {
        _factory = createFactory();
    }

    protected IFactory createFactory()
    {
        return new Factory;
    }

    private IFactory _factory;
    @property final IFactory factory()
    {
        return _factory;
    }

    alias factory this;
}

void test133()
{

    IFactory f = new A133;
    f.foo(); // segfault
}

/***************************************************/
// 5365

class B134
{
}

class A134
{

    B134 _b;

    this()
    {
        _b = new B134;
    }

    B134 b()
    {
        return _b;
    }

    alias b this;
}

void test134()
{

    auto a = new A134;
    B134 b = a; // b is null
    assert(a._b is b); // fails
}

/***************************************************/
// 5025

struct S135 {
  void delegate() d;
}

void test135()
{
  shared S135[] s;
  if (0)
    s[0] = S135();
}

/***************************************************/
// 5545

bool enforce136(bool value, lazy const(char)[] msg = null) {
    if(!value) {
        return false;
    }

    return value;
}

struct Perm {
    byte[3] perm;
    ubyte i;

    this(byte[] input) {
        foreach(elem; input) {
            enforce136(i < 3);
            perm[i++] = elem;
            std.stdio.stderr.writeln(i);  // Never gets incremented.  Stays at 0.
        }
    }
}

void test136() {
    byte[] stuff = [0, 1, 2];
    auto perm2 = Perm(stuff);
    writeln(perm2.perm);  // Prints [2, 0, 0]
    assert(perm2.perm[] == [0, 1, 2]);
}

/***************************************************/
// 4097

void foo4097() { }
alias typeof(&foo4097) T4097;
static assert(is(T4097 X : X*) && is(X == function));

static assert(!is(X));

/***************************************************/
// 5798

void assign9(ref int lhs) pure {
    lhs = 9;
}

void assign8(ref int rhs) pure {
    rhs = 8;
}

int test137(){
    int a=1,b=2;
    assign8(b),assign9(a);
    assert(a == 9);
    assert(b == 8);   // <-- fail

    assign9(b),assign8(a);
    assert(a == 8);
    assert(b == 9);   // <-- fail

    return 0;
}

/***************************************************/

// 9366
static assert(!is(typeof((void[]).init ~ cast(void)0)));
static assert(!is(typeof(cast(void)0 ~ (void[]).init)));

/***************************************************/

struct Size138
{
    union
    {
        struct
        {
            int width;
            int height;
        }

        long size;
    }
}

enum Size138 foo138 = {2 ,5};

Size138 bar138 = foo138;

void test138()
{
    assert(bar138.width == 2);
    assert(bar138.height == 5);
}

/***************************************************/

void test3822()
{
    import core.stdc.stdlib;
    int i = 0;
    void* ptr;
    while(i++ != 2)
    {
        auto p = alloca(2);
        assert(p != ptr);
        ptr = p;
    }
}

/***************************************************/

// 5939, 5940

template map(fun...)
{
    auto map(double[] r)
    {
        struct Result
        {
            this(double[] input)
            {
            }
        }

        return Result(r);
    }
}


void test139()
{
    double[] x;
    alias typeof(map!"a"(x)) T;
    T a = void;
    auto b = map!"a"(x);
    auto c = [map!"a"(x)];
    T[3] d = void;
}


/***************************************************/
// 5966

string[] foo5966(string[] a)
{
    a[0] = a[0][0..$];
    return a;
}

enum var5966 = foo5966([""]);

/***************************************************/
// 5975

int foo5975(wstring replace)
{
  wstring value = "";
  value ~= replace;
  return 1;
}

enum X5975 = foo5975("X"w);

/***************************************************/
// 5965

template mapx(fun...) if (fun.length >= 1)
{
    int mapx(Range)(Range r)
    {
        return 1;
    }
}

void test140()
{
   int foo(int i) { return i; }

   int[] arr;
   auto x = mapx!( (int a){return foo(a);} )(arr);
}

/***************************************************/

void bug5976()
{
    int[] barr;
    int * k;
    foreach (ref b; barr)
    {
        scope(failure)
            k = &b;
        k = &b;
    }
}

/***************************************************/
// 5771

struct S141
{
    this(A)(auto ref A a){}
}

void test141()
{
    S141 s = S141(10);
}

/***************************************************/

class test5498_A {}
class test5498_B : test5498_A {}
class test5498_C : test5498_A {}

static assert(is(typeof([test5498_B.init, test5498_C.init]) == test5498_A[]));

/***************************************************/
// 3688

struct S142
{
    int v;
    this(int n) pure { v = n; }
    const bool opCast(T:bool)() { return true; }
}

void test142()
{
    if (int a = 1)
        assert(a == 1);
    else assert(0);

    if (const int a = 2)
        assert(a == 2);
    else assert(0);

    if (immutable int a = 3)
        assert(a == 3);
    else assert(0);

    if (auto s = S142(10))
        assert(s.v == 10);
    else assert(0);

    if (auto s = const(S142)(20))
        assert(s.v == 20);
    else assert(0);

    if (auto s = immutable(S142)(30))
        assert(s.v == 30);
    else assert(0);
}

/***************************************************/
// 6072

static assert({
    if (int x = 5) {}
    return true;
}());

/***************************************************/
// 5959

int n;

void test143()
{
    ref int f(){ return n; }            // NG
    f() = 1;
    assert(n == 1);

    nothrow ref int f1(){ return n; }    // OK
    f1() = 2;
    assert(n == 2);

    auto ref int f2(){ return n; }       // OK
    f2() = 3;
    assert(n == 3);
}

/***************************************************/
// 6119

void startsWith(alias pred) ()   if (is(typeof(pred('c', 'd')) : bool))
{
}

void startsWith(alias pred) ()   if (is(typeof(pred('c', "abc")) : bool))
{
}

void test144()
{
    startsWith!((a, b) { return a == b; })();
}

/***************************************************/

void test145()
{
    import core.stdc.stdio;
    printf("hello world 145\n");
}

void test146()
{
    test1();
    static import core.stdc.stdio;
    core.stdc.stdio.printf("hello world 146\n");
}

/***************************************************/
// 5856

struct X147
{
    void f()       { writeln("X.f mutable"); }
    void f() const { writeln("X.f const"); }

    void g()()       { writeln("X.g mutable"); }
    void g()() const { writeln("X.g const"); }

    void opOpAssign(string op)(int n)       { writeln("X+= mutable"); }
    void opOpAssign(string op)(int n) const { writeln("X+= const"); }
}

void test147()
{
    X147 xm;
    xm.f();     // prints "X.f mutable"
    xm.g();     // prints "X.g mutable"
    xm += 10;   // should print "X+= mutable" (1)

    const(X147) xc;
    xc.f();     // prints "X.f const"
    xc.g();     // prints "X.g const"
    xc += 10;   // should print "X+= const" (2)
}


/***************************************************/

void test3559()
{
    static class A
    {
        int foo(int a)   { return 0; }
        int foo(float a) { return 1; }

        int bar(float a) { return 1; }
        int bar(int a) { return 0; }
    }

    static class B : A
    {
        override int foo(float a) { return 2; }
        alias A.foo foo;

        alias A.bar bar;
        override int bar(float a) { return 2; }
    }

    {
        auto x = new A;
        auto f1 = cast(int delegate(int))&x.foo;
        auto f2 = cast(int delegate(float))&x.foo;
        int delegate(int) f3 = &x.foo;
        int delegate(float) f4 = &x.foo;

        assert(f1(0) == 0);
        assert(f2(0) == 1);
        assert(f3(0) == 0);
        assert(f4(0) == 1);
    }

    {
        auto x = new B;
        auto f1 = cast(int delegate(int))&x.foo;
        auto f2 = cast(int delegate(float))&x.foo;
        int delegate(int) f3 = &x.foo;
        int delegate(float) f4 = &x.foo;

        assert(f1(0) == 0);
        assert(f2(0) == 2);
        assert(f3(0) == 0);
        assert(f4(0) == 2);
    }

    {
        auto x = new A;
        auto f1 = cast(int delegate(int))&x.bar;
        auto f2 = cast(int delegate(float))&x.bar;
        int delegate(int) f3 = &x.bar;
        int delegate(float) f4 = &x.bar;

        assert(f1(0) == 0);
        assert(f2(0) == 1);
        assert(f3(0) == 0);
        assert(f4(0) == 1);
    }

    {
        auto x = new B;
        auto f1 = cast(int delegate(int))&x.bar;
        auto f2 = cast(int delegate(float))&x.bar;
        int delegate(int) f3 = &x.bar;
        int delegate(float) f4 = &x.bar;

        assert(f1(0) == 0);
        assert(f2(0) == 2);
        assert(f3(0) == 0);
        assert(f4(0) == 2);
    }
}

/***************************************************/

extern(C++)
class C13182
{
}

void test13182()
{
    scope C13182 c = new C13182();
}

/***************************************************/
// 5897

struct A148{ int n; }
struct B148{
    int n, m;
    this(A148 a){ n = a.n, m = a.n*2; }
}

struct C148{
    int n, m;
    static C148 opCall(A148 a)
    {
        C148 b;
        b.n = a.n, b.m = a.n*2;
        return b;
    }
}

void test148()
{
    auto a = A148(10);
    auto b = cast(B148)a;
    assert(b.n == 10 && b.m == 20);
    auto c = cast(C148)a;
    assert(c.n == 10 && c.m == 20);
}

/***************************************************/
// 4969

class MyException : Exception
{
    this()
    {
        super("An exception!");
    }
}

void throwAway()
{
    throw new MyException;
}

void cantthrow() nothrow
{
    try
        throwAway();
    catch(MyException me)
        assert(0);
    catch(Exception e)
        assert(0);
}

/***************************************************/
// 2356

void test2356()
{
    int[3] x = [1,2,3];
    printf("x[] = [%d %d %d]\n", x[0], x[1], x[2]);
    assert(x[0] == 1 && x[1] == 2 && x[2] == 3);

    struct S
    {
        static int pblit;
        int n;
        this(this) { ++pblit; printf("postblit: %d\n", n); }
    }
    S s2 = S(2);
    S[3] s = [S(1), s2, S(3)];
    assert(s[0].n == 1 && s[1].n == 2 && s[2].n == 3);
    printf("s[].n = [%d %d %d]\n", s[0].n, s[1].n, s[2].n);
    assert(S.pblit == 1);

    ubyte[1024] v;
    v = typeof(v).init;
    printf("v[] = [%d %d %d, ..., %d]\n", v[0], v[1], v[2], v[$-1]);
    foreach (ref a; v) assert(a == 0);

    int n = 5;
    int[3] y = [n, n, n];
    printf("y[] = [%d %d %d]\n", y[0], y[1], y[2]);
    assert(y[0] == 5 && y[1] == 5 && y[2] == 5);

    S[3] z = [s2, s2, s2];
    assert(z[0].n == 2 && z[1].n == 2 && z[2].n == 2);
    printf("z[].n = [%d %d %d]\n", z[0].n, z[1].n, z[2].n);
    assert(S.pblit == 1 + 3);

    int[0] nsa0 = [];
    void[0] vsa0 = [];

    void foo(T)(T){}
    foo(vsa0);

    ref int[0] bar() { static int[1] sa; return *cast(int[0]*)&sa; }
    bar() = [];
}

/***************************************************/
// 13652

void test13652()
{
    // reduced case
    uint[9][5] arr =
        [[0, 0, 0,  0, 1, 5,  8, 0, 7],
         [0, 3, 8,  0, 2, 0,  0, 6, 0],
         [0, 0, 7,  0, 6, 8,  9, 4, 0],
         [0, 0, 0,  0, 0, 1,  2, 9, 0],
         [9, 7, 0,  0, 0, 0,  0, 8, 3]];
    assert(arr[0][0] == 0 && arr[0][1] == 0 && arr[0][2] == 0 &&
           arr[0][3] == 0 && arr[0][4] == 1 && arr[0][5] == 5 &&
           arr[0][6] == 8 && arr[0][7] == 0 && arr[0][8] == 7);
    assert(arr[1][0] == 0 && arr[1][1] == 3 && arr[1][2] == 8 &&
           arr[1][3] == 0 && arr[1][4] == 2 && arr[1][5] == 0 &&
           arr[1][6] == 0 && arr[1][7] == 6 && arr[1][8] == 0);
    assert(arr[2][0] == 0 && arr[2][1] == 0 && arr[2][2] == 7 &&
           arr[2][3] == 0 && arr[2][4] == 6 && arr[2][5] == 8 &&
           arr[2][6] == 9 && arr[2][7] == 4 && arr[2][8] == 0);
    assert(arr[3][0] == 0 && arr[3][1] == 0 && arr[3][2] == 0 &&
           arr[3][3] == 0 && arr[3][4] == 0 && arr[3][5] == 1 &&
           arr[3][6] == 2 && arr[3][7] == 9 && arr[3][8] == 0);
    assert(arr[4][0] == 9 && arr[4][1] == 7 && arr[4][2] == 0 &&
           arr[4][3] == 0 && arr[4][4] == 0 && arr[4][5] == 0 &&
           arr[4][6] == 0 && arr[4][7] == 8 && arr[4][8] == 3);

    // original case
    uint[9][9] tbl =
        [[0, 0, 0,  0, 1, 5,  8, 0, 7],
         [0, 3, 8,  0, 2, 0,  0, 6, 0],
         [0, 0, 7,  0, 6, 8,  9, 4, 0],
         [0, 0, 0,  0, 0, 1,  2, 9, 0],
         [9, 7, 0,  0, 0, 0,  0, 8, 3],
         [0, 2, 1,  6, 0, 0,  0, 0, 0],
         [0, 6, 9,  5, 4, 0,  3, 0, 0],
         [0, 4, 0,  0, 8, 0,  6, 5, 0],
         [2, 0, 5,  9, 3, 0,  0, 0, 0]];
    assert(tbl[0][0] == 0 && tbl[0][1] == 0 && tbl[0][2] == 0 &&
           tbl[0][3] == 0 && tbl[0][4] == 1 && tbl[0][5] == 5 &&
           tbl[0][6] == 8 && tbl[0][7] == 0 && tbl[0][8] == 7);
    assert(tbl[1][0] == 0 && tbl[1][1] == 3 && tbl[1][2] == 8 &&
           tbl[1][3] == 0 && tbl[1][4] == 2 && tbl[1][5] == 0 &&
           tbl[1][6] == 0 && tbl[1][7] == 6 && tbl[1][8] == 0);
    assert(tbl[2][0] == 0 && tbl[2][1] == 0 && tbl[2][2] == 7 &&
           tbl[2][3] == 0 && tbl[2][4] == 6 && tbl[2][5] == 8 &&
           tbl[2][6] == 9 && tbl[2][7] == 4 && tbl[2][8] == 0);
    assert(tbl[3][0] == 0 && tbl[3][1] == 0 && tbl[3][2] == 0 &&
           tbl[3][3] == 0 && tbl[3][4] == 0 && tbl[3][5] == 1 &&
           tbl[3][6] == 2 && tbl[3][7] == 9 && tbl[3][8] == 0);
    assert(tbl[4][0] == 9 && tbl[4][1] == 7 && tbl[4][2] == 0 &&
           tbl[4][3] == 0 && tbl[4][4] == 0 && tbl[4][5] == 0 &&
           tbl[4][6] == 0 && tbl[4][7] == 8 && tbl[4][8] == 3);
    assert(tbl[5][0] == 0 && tbl[5][1] == 2 && tbl[5][2] == 1 &&
           tbl[5][3] == 6 && tbl[5][4] == 0 && tbl[5][5] == 0 &&
           tbl[5][6] == 0 && tbl[5][7] == 0 && tbl[5][8] == 0);
    assert(tbl[6][0] == 0 && tbl[6][1] == 6 && tbl[6][2] == 9 &&
           tbl[6][3] == 5 && tbl[6][4] == 4 && tbl[6][5] == 0 &&
           tbl[6][6] == 3 && tbl[6][7] == 0 && tbl[6][8] == 0);
    assert(tbl[7][0] == 0 && tbl[7][1] == 4 && tbl[7][2] == 0 &&
           tbl[7][3] == 0 && tbl[7][4] == 8 && tbl[7][5] == 0 &&
           tbl[7][6] == 6 && tbl[7][7] == 5 && tbl[7][8] == 0);
    assert(tbl[8][0] == 2 && tbl[8][1] == 0 && tbl[8][2] == 5 &&
           tbl[8][3] == 9 && tbl[8][4] == 3 && tbl[8][5] == 0 &&
           tbl[8][6] == 0 && tbl[8][6] == 0 && tbl[8][8] == 0);
}

/***************************************************/
// 11238

void test11238()
{
    int[2] m;
    m[0] = 4,m[1] = 6;
    //printf("%d,%d\n", m[0], m[1]);
    assert(m[0] == 4 && m[1] == 6);

    m = [m[1], m[0]];   // swap
    assert(m[0] == 6 && m[1] == 4);
    //printf("%d,%d\n", m[0], m[1]);

    m = [m[1], m[0]];   // swap
    //printf("%d,%d\n", m[0], m[1]);
    assert(m[0] == 4 && m[1] == 6);
}

/***************************************************/

void test11805()
{
    int i;

    i = 47;
    i = 1 && i;
    assert(i == 1);
    i = 0 || i;
    assert(i == 1);
}

/***************************************************/

class A2540
{
    int a;
    int foo() { return 0; }
    alias int X;
}

class B2540 : A2540
{
    int b;
    override super.X foo() { return 1; }

    alias this athis;
    alias this.b thisb;
    alias super.a supera;
    alias super.foo superfoo;
    alias this.foo thisfoo;
}

struct X2540
{
    alias this athis;
}

void test2540()
{
    auto x = X2540.athis.init;
    static assert(is(typeof(x) == X2540));

    B2540 b = new B2540();

    assert(&b.a == &b.supera);
    assert(&b.b == &b.thisb);
    assert(b.thisfoo() == 1);
}

/***************************************************/

class B14348
{
    int foo() { return 0; }
}

class C14348 : B14348
{
    override int foo() { return 1; }

    alias superfoo = typeof(super).foo;
    alias thisfoo = typeof(this).foo;
}

B14348 test14348()
{
    alias foo = typeof(return).foo;  // currently doesn't work.
    assert(&B14348.foo is &C14348.superfoo);
    assert(&C14348.foo is &C14348.thisfoo);
    return null;
}

/***************************************************/
// 7295

struct S7295
{
    int member;
    @property ref int refCountedPayload() { return member; }
    alias refCountedPayload this;
}

void foo7295(S)(immutable S t, int qq) pure { }
void foo7295(S)(S s) pure { }

void bar7295() pure
{
    S7295 b;
    foo7295(b);
}

/***************************************************/
// 5659

void test149()
{
    import std.traits;

    char a;
    immutable(char) b;

    static assert(is(typeof(true ? a : b) == const(char)));
    static assert(is(typeof([a, b][0]) == const(char)));

    static assert(is(CommonType!(typeof(a), typeof(b)) == const(char)));
}


/***************************************************/
// 1373

void func1373a(){}

static assert(typeof(func1373a).stringof == "void()");
static assert(typeof(func1373a).mangleof == "FZv");
static assert(!__traits(compiles, typeof(func1373a).alignof));
static assert(!__traits(compiles, typeof(func1373a).init));
static assert(!__traits(compiles, typeof(func1373a).offsetof));

void func1373b(int n){}

static assert(typeof(func1373b).stringof == "void(int n)");
static assert(typeof(func1373b).mangleof == "FiZv");
static assert(!__traits(compiles, typeof(func1373b).alignof));
static assert(!__traits(compiles, typeof(func1373b).init));
static assert(!__traits(compiles, typeof(func1373b).offsetof));

/***************************************************/

void bar150(T)(T n) {  }

@safe void test150()
{
    bar150(1);
}

/***************************************************/

void test5785()
{
    static struct x { static int y; }
    assert(x.y !is 1);
    assert(x.y !in [1:0]);
}

/***************************************************/

void bar151(T)(T n) {  }

nothrow void test151()
{
    bar151(1);
}

/***************************************************/

@property int coo() { return 1; }
@property auto doo(int i) { return i; }

@property int eoo() { return 1; }
@property auto ref hoo(int i) { return i; }

// 3359

int goo(int i) pure { return i; }
auto ioo(int i) pure { return i; }
auto ref boo(int i) pure nothrow { return i; }

class A152 {
    auto hoo(int i) pure  { return i; }
    const boo(int i) nothrow { return i; }
    auto coo(int i) const { return i; }
    auto doo(int i) immutable { return i; }
    auto eoo(int i) shared { return i; }
}

// 4706

struct Foo152(T) {
    @property auto ref front() {
        return T.init;
    }

    @property void front(T num) {}
}

void test152() {
    Foo152!int foo;
    auto a = foo.front;
    foo.front = 2;
}

/***************************************************/
// 6733

void bug6733(int a, int b) pure nothrow { }
void test6733() {
   int z = 1;
   bug6733(z++, z++);
   assert(z==3);
}

/***************************************************/
// 3799

void test153()
{
    void bar()
    {
    }

    static assert(!__traits(isStaticFunction, bar));
}

/***************************************************/
// 3632


void test154() {
    float f;
    assert(f is float.init);
    double d;
    assert(d is double.init);
    real r;
    assert(r is real.init);

    assert(float.nan is float.nan);
    assert(double.nan is double.nan);
    assert(real.nan is real.nan);
}

/***************************************************/

void test6545()
{
    static int[] func()
    {
        auto a = [1, 2, 3];
        auto b = [2, 3, 4];
        auto c = [3, 4, 5];

        a[] = b[] + c[];

        return a;
    }

    auto a = func();
    enum b = func();
    assert(a == b);
}

/***************************************************/
// 3147


void test155()
{
    byte b = 1;
    short s;
    int i;
    long l;

    s = b + b;
    b = s % b;
    s = s >> b;
    b = 1;
    b = i % b;
    b = b >> i;
}

/***************************************************/
// 2486

void test2486()
{
    void foo(ref int[] arr) {}

    int[] arr = [1,2,3];
    foo(arr);   //OK
    static assert(!__traits(compiles, foo(arr[1..2]))); // should be NG

    struct S
    {
        int[] a;
        auto ref opSlice(){ return a[]; }  // line 4
    }
    S s;
    s[];
    // opSlice should return rvalue
    static assert(is(typeof(&S.opSlice) == int[] function() pure nothrow @nogc @safe));
    static assert(!__traits(compiles, foo(s[])));       // should be NG
}

/***************************************************/

extern(C++) class C15080
{
    uint x = 1;
    uint y = 2;
}

__gshared c15080 = new C15080();

void test15080()
{
    assert(c15080.x == 1);
    assert(c15080.y == 2);
}

/***************************************************/
// 2521

immutable int val = 23;
const int val2 = 23;

ref immutable(int) func2521_() {
    return val;
}
ref immutable(int) func2521_2() {
    return *&val;
}
ref immutable(int) func2521_3() {
    return func2521_;
}
ref const(int) func2521_4() {
    return val2;
}
ref const(int) func2521_5() {
    return val;
}
auto ref func2521_6() {
    return val;
}
ref func2521_7() {
    return val;
}

/***************************************************/

void test5554()
{
    class MA { }
    class MB : MA { }
    class MC : MB { }

    class A { abstract MA foo(); }
    interface I { MB foo(); }
    class B : A
    {
        override MC foo() { return null; }
    }
    class C : B, I
    {
        override MC foo() { return null; }
    }
}

/***************************************************/
// 5962

struct S156
{
          auto g()(){ return 1; }
    const auto g()(){ return 2; }
}

void test156()
{
    auto ms = S156();
    assert(ms.g() == 1);
    auto cs = const(S156)();
    assert(cs.g() == 2);
}

/***************************************************/

void test10724()
{
    const(char)* s = "abc"[0..$-1];
    assert(s[2] == '\0');
}

/***************************************************/

void test6708(const ref int y)
{
    immutable int x;
    test6708(x);
}

/***************************************************/
// 4258

struct Vec4258 {
    Vec4258 opOpAssign(string Op)(auto ref Vec4258 other) if (Op == "+") {
        return this;
    }
    Vec4258 opBinary(string Op:"+")(Vec4258 other) {
        Vec4258 result;
        return result += other;
    }
}
void test4258() {
    Vec4258 v;
    v += Vec4258() + Vec4258(); // line 12
}

// regression fix test

struct Foo4258 {
    // binary ++/--
    int opPostInc()() if (false) { return 0; }

    // binary 1st
    int opAdd(R)(R rhs) if (false) { return 0; }
    int opAdd_r(R)(R rhs) if (false) { return 0; }

    // compare
    int opCmp(R)(R rhs) if (false) { return 0; }

    // binary-op assign
    int opAddAssign(R)(R rhs) if (false) { return 0; }
}
struct Bar4258 {
    // binary commutive 1
    int opAdd_r(R)(R rhs) if (false) { return 0; }

    // binary-op assign
    int opOpAssign(string op, R)(R rhs) if (false) { return 0; }
}
struct Baz4258 {
    // binary commutive 2
    int opAdd(R)(R rhs) if (false) { return 0; }
}
static assert(!is(typeof(Foo4258.init++)));
static assert(!is(typeof(Foo4258.init + 1)));
static assert(!is(typeof(1 + Foo4258.init)));
static assert(!is(typeof(Foo4258.init < Foo4258.init)));
static assert(!is(typeof(Foo4258.init += 1)));
static assert(!is(typeof(Bar4258.init + 1)));
static assert(!is(typeof(Bar4258.init += 1)));
static assert(!is(typeof(1 + Baz4258.init)));

/***************************************************/
// 4539

void test4539()
{
    static assert(!__traits(compiles, "hello" = "red"));

    void foo1(ref string s){}
    void foo2(ref const char[10] s){}
    void foo3(ref char[5] s){}

    void foo4(ref const char[5] s)
    {
        assert(s[0] == 'h');
        assert(s[4] == 'o');
    }
    void foo5(ref const ubyte[5] s)
    {
        assert(s[0] == 0xc3);
        assert(s[4] == 0x61);
    }

    static assert(!__traits(compiles, foo1("hello")));
    static assert(!__traits(compiles, foo2("hello")));
    static assert(!__traits(compiles, foo3("hello")));

    // same as test68, 69, 70
    foo4("hello");
    foo5(cast(ubyte[5])x"c3fcd3d761");

    //import std.conv;
    //static assert(!__traits(compiles, parse!int("10") == 10));
}

/***************************************************/
// 1471

void test1471()
{
    int n;
    string bar = "BOOM"[n..$-1];
    assert(bar == "BOO");
}

/***************************************************/

deprecated @disable int bug6389;
static assert(!is(typeof(bug6389 = bug6389)));

/***************************************************/

void test10927()
{
    static assert( (1+2i) ^^ 3 == -11 - 2i );
    auto a = (1+2i) ^^ 3;
}

/***************************************************/

void test4963()
{
    struct Value {
        byte a;
    }
    Value single()
    {
        return Value();
    }

    Value[] list;
    auto x = single() ~ list;
}

/***************************************************/

pure int test4031()
{
    static const int x = 8;
    return x;
}

/***************************************************/
// 5437

template EnumMembers5437(E)
{
    template TypeTuple(T...){ alias T TypeTuple; }

    alias TypeTuple!("A", "B") EnumMembers5437;
}
template IntValue5437()
{
    int IntValue5437 = 10;
}

void test5437()
{
    enum Foo { A, B }
    alias EnumMembers5437!Foo members;      // OK
    enum n1 = members.length;               // OK
    enum n2 = (EnumMembers5437!Foo).length; // NG, type -> symbol

    enum s1 = IntValue5437!().sizeof;       // OK
    enum s2 = (IntValue5437!()).sizeof;     // NG, type -> expression
}

/***************************************************/
// 1962


void test1962()
{
    class C { abstract void x(); }
    assert(C.classinfo.create() is null);
}

/***************************************************/
// 6228


void test6228()
{
    const(int)* ptr;
    const(int)  temp;
    auto x = (*ptr) ^^ temp;
}

/***************************************************/

int test7544()
{
    try { throw new Exception(""); }
    catch (Exception e) static assert(1);
    return 1;
}

static assert(test7544());

/***************************************************/

struct S6230 {
    int p;
    int q() const pure {
        return p;
    }
    void r() pure {
        p = 231;
    }
}
class C6230 {
    int p;
    int q() const pure {
        return p;
    }
    void r() pure {
        p = 552;
    }
}
int q6230(ref const S6230 s) pure {    // <-- Currently OK
    return s.p;
}
int q6230(ref const C6230 c) pure {    // <-- Currently OK
    return c.p;
}
void r6230(ref S6230 s) pure {
    s.p = 244;
}
void r6230(ref C6230 c) pure {
    c.p = 156;
}
bool test6230pure() pure {
    auto s = S6230(4);
    assert(s.p == 4);
    assert(q6230(s) == 4);
    assert(s.q == 4);

    auto c = new C6230;
    c.p = 6;
    assert(q6230(c) == 6);
    assert(c.q == 6);

    r6230(s);
    assert(s.p == 244);
    s.r();
    assert(s.p == 231);

    r6230(c);
    assert(c.p == 156);
    c.r();
    assert(c.p == 552);

    return true;
}
void test6230() {
    assert(test6230pure());
}

/***************************************************/

void test6264()
{
    struct S { auto opSlice() { return this; } }
    int[] a;
    S s;
    static assert(!is(typeof(a[] = s[])));
    int*[] b;
    static assert(is(typeof(b[] = [new immutable(int)])));
    char[] c = new char[](5);
    c[] = "hello";
}

/***************************************************/
// 5046

void test5046()
{
    auto va = S5046!("", int)();
    auto vb = makeS5046!("", int)();
}

struct S5046(alias p, T)
{
    T s;
    T fun() { return s; }   // (10)
}

S5046!(p, T) makeS5046(alias p, T)()
{
    return typeof(return)();
}

/***************************************************/
// 6335

struct S6335
{
    const int value;
    this()(int n){ value = n; }
}
void test6335()
{
    S6335 s = S6335(10);
}

/***************************************************/

struct S6295(int N) {
    int[N] x;
    const nothrow pure @safe f() { return x.length; }
}

void test6295() {
    auto bar(T: S6295!(N), int N)(T x) {
        return x.f();
    }
    S6295!4 x;
    assert(bar(x) == 4);
}

/***************************************************/

template TT4536(T...) { alias T TT4536; }

void test4536()
{
    auto x = TT4536!(int, long, [1, 2]).init;
    assert(x[0] is int.init);
    assert(x[1] is long.init);
    assert(x[2] is [1, 2].init);
}

/***************************************************/

struct S6284 {
    int a;
}
class C6284 {
    int a;
}
pure int bug6284a() {
    S6284 s = {4};
    auto b = s.a;   // ok
    with (s) {
        b += a;     // should be ok.
    }
    return b;
}
pure int bug6284b() {
    auto s = new S6284;
    s.a = 4;
    auto b = s.a;
    with (*s) {
        b += a;
    }
    return b;
}
pure int bug6284c() {
    auto s = new C6284;
    s.a = 4;
    auto b = s.a;
    with (s) {
        b += a;
    }
    return b;
}
void test6284() {
    assert(bug6284a() == 8);
    assert(bug6284b() == 8);
    assert(bug6284c() == 8);
}

/***************************************************/

class C6293 {
    C6293 token;
}
void f6293(in C6293[] a) pure {
    auto x0 = a[0].token;
    assert(x0 is a[0].token.token.token);
    assert(x0 is (&x0).token);
    auto p1 = &x0 + 1;
    assert(x0 is (p1 - 1).token);
    int c = 0;
    assert(x0 is a[c].token);
}
void test6293() {
    auto x = new C6293;
    x.token = x;
    f6293([x]);
}

/***************************************************/
// 3733

class C3733
{
    int foo()        { return 1; }
    int foo() shared { return 2; }

    int bar()        { return foo(); }
}
void test3733()
{
    auto c = new C3733();
    assert(c.bar() == 1);
}

/***************************************************/
// 4392

class C4392
{
    int foo() const { return 1; }
    int foo()       { return 2; }

    int bar() const { return foo(); }
}
void test4392()
{
    auto c = new C4392();
    assert(c.bar() == 1);
}

/***************************************************/
// 6220

void test6220() {
    struct Foobar { real x; real y; real z;}
    switch("x") {
        foreach(i,member; __traits(allMembers, Foobar)) {
            case member : break;
        }
    default : break;
    }
}

/***************************************************/
// 5799

void test5799()
{
    int a;
    int *u = &(a ? a : (a ? a : a));
    assert(u == &a);
}

/***************************************************/
// 6529

enum Foo6529 : char { A='a' }
ref const(Foo6529) func6529(const(Foo6529)[] arr){ return arr[0]; }

/***************************************************/

void test783()
{
    const arr = [ 1,2,3 ];
    const i = 2;
    auto jhk = new int[arr[i]]; // "need size of rightmost array, not type arr[i]"
}

/***************************************************/

template X157(alias x)
{
    alias x X157;
}

template Parent(alias foo)
{
    alias X157!(__traits(parent, foo)) Parent;
}

template ParameterTypeTuple(alias foo)
{
    static if (is(typeof(foo) P == function))
        alias P ParameterTypeTuple;
    else
        static assert(0, "argument has no parameters");
}

template Mfp(alias foo)
{
    auto Mfp = function(Parent!foo self, ParameterTypeTuple!foo i) { return self.foo(i); };
}

class C157 {
 int a = 3;
 int foo(int i, int y) { return i + a + y; }
}

void test157()
{
    auto c = new C157();
    auto mfp = Mfp!(C157.foo);
    auto i = mfp(c, 1, 7);
    assert(i == 11);
}

/***************************************************/
// 6473

struct Eins6473
{
    ~this() {}
}

struct Zwei6473
{
    void build(Eins6473 devices = Eins6473())
    {
    }
}

void build(Eins6473 devices = Eins6473())
{}

void test6473()
{
    void build(Eins6473 devices = Eins6473())
    {}
}

/***************************************************/

uint rol11417(uint n)(in uint x)
{
    return x << n | x >> 32 - n;
}

uint ror11417(uint n)(in uint x)
{
    return x >> n | x << 32 - n;
}

void test11417()
{
    assert(rol11417!1(0x8000_0000) == 0x1);
    assert(ror11417!1(0x1) == 0x8000_0000);
}

/***************************************************/

void test6578()
{
    static struct Foo
    {
        this(int x) pure {}
    }
    auto f1 = new const(Foo)(1);
    auto f2 = new immutable(Foo)(1);
    auto f3 = new shared(Foo)(1);
    auto f4 = const(Foo)(1);
    auto f5 = immutable(Foo)(1);
    auto f6 = shared(Foo)(1);
    static assert(is(typeof(f1) == const(Foo)*));
    static assert(is(typeof(f2) == immutable(Foo)*));
    static assert(is(typeof(f3) == shared(Foo)*));
    static assert(is(typeof(f4) == const(Foo)));
    static assert(is(typeof(f5) == immutable(Foo)));
    static assert(is(typeof(f6) == shared(Foo)));

    static struct Bar
    {
        this(int x) const pure {}
    }
    auto g1 = new const(Bar)(1);
    auto g2 = new immutable(Bar)(1);
    auto g3 = new shared(Bar)(1);
    auto g4 = const(Bar)(1);
    auto g5 = immutable(Bar)(1);
    auto g6 = shared(Bar)(1);
    static assert(is(typeof(g1) == const(Bar)*));
    static assert(is(typeof(g2) == immutable(Bar)*));
    static assert(is(typeof(g3) == shared(Bar)*));
    static assert(is(typeof(g4) == const(Bar)));
    static assert(is(typeof(g5) == immutable(Bar)));
    static assert(is(typeof(g6) == shared(Bar)));

    static struct Baz
    {
        this()(int x) const pure {}
    }
    auto h1 = new const(Baz)(1);
    auto h2 = new immutable(Baz)(1);
    auto h3 = new shared(const(Baz))(1);
    auto h4 = const(Baz)(1);
    auto h5 = immutable(Baz)(1);
    auto h6 = shared(const(Baz))(1);
    static assert(is(typeof(h1) == const(Baz)*));
    static assert(is(typeof(h2) == immutable(Baz)*));
    static assert(is(typeof(h3) == shared(const(Baz))*));
    static assert(is(typeof(h4) == const(Baz)));
    static assert(is(typeof(h5) == immutable(Baz)));
    static assert(is(typeof(h6) == shared(const(Baz))));
}

/***************************************************/
// 6630

void test6630()
{
    static class B {}

    static class A
    {
        this() { b = new B(); }
        B b;
        alias b this;
    }

    void fun(A a)
    {
        a = null;
        assert(a is null);
    }

    auto a = new A;
    assert(a.b !is null);
    fun(a);
    assert(a !is null);
    assert(a.b !is null);
}

/***************************************************/

int i199 = 1;

void test199()
{
    label:
    {
        int i199 = 2;
    }
    assert(i199 == 1);
}

/***************************************************/
// 6690

T useLazy6690(T)(lazy T val)
{
    return val;
    // val is converted to delegate call, but it is typed as int delegate() - not @safe!
}
void test6690() @safe
{
    useLazy6690(0);
    // Error: safe function 'test6690' cannot call system function 'useLazy6690'
}

/***************************************************/

template Hoge6691()
{
    immutable static int[int] dict;
    immutable static int value;

    static this()
    {
        dict = [1:1, 2:2];
        value = 10;
    }
}
alias Hoge6691!() H6691;

/***************************************************/

void test10626()
{
    double[2] v, x;
    struct Y { double u; }
    double z;
    Y y;
    double[2] r = v[] * x[0];
    //double[2] s = v[] * z++;
    //double[2] t = v[] * z--;
    double[2] a = v[] * ++z;
    double[2] b = v[] * --z;
    double[2] c = v[] * y.u;
    double[2] d = v[] * (x[] = 3, x[0]);
    double[2] e = v[] * (v[] ~ z)[0];
}


/***************************************************/
// 2953

template Tuple2953(T...)
{
    alias T Tuple2953;
}
template Range2953(int b)
{
    alias Tuple2953!(1) Range2953;
}
void foo2953()()
{
    Tuple2953!(int, int) args;
    foreach( x ; Range2953!(args.length) ){ }
}
void test2953()
{
    foo2953!()();
}

/***************************************************/
// 2997

abstract class B2997 { void foo(); }
interface I2997 { void bar(); }
abstract class C2997 : B2997, I2997 {}
//pragma(msg, __traits(allMembers, C).stringof);

void test2997()
{
    enum ObjectMembers = ["toString","toHash","opCmp","opEquals","Monitor","factory"];

    static assert([__traits(allMembers, C2997)] == ["foo"] ~ ObjectMembers ~ ["bar"]);
}

/***************************************************/
// 6596

extern (C) int function() pfunc6596;
extern (C) int cfunc6596(){ return 0; }
static assert(typeof(pfunc6596).stringof == "extern (C) int function()");
static assert(typeof(cfunc6596).stringof == "extern (C) int()");


/***************************************************/
// 4423

struct S4423
{
    this(string phrase, int num)
    {
        this.phrase = phrase;
        this.num = num;
    }

    int opCmp(const ref S4423 rhs)
    {
        if (phrase < rhs.phrase)
            return -1;
        else if (phrase > rhs.phrase)
            return 1;

        if (num < rhs.num)
            return -1;
        else if (num > rhs.num)
            return 1;

        return 0;
    }

    string phrase;
    int    num;
}

enum E4423 : S4423
{
    a = S4423("hello", 1),
    b = S4423("goodbye", 45),
    c = S4423("world", 22),
};

void test4423()
{
    E4423 e;
    assert(e.phrase == "hello");

    e = E4423.b;
    assert(e.phrase == "goodbye");
}

/***************************************************/
// 4647

interface Timer
{
    final int run() { printf("Timer.run()\n"); fun(); return 1; };
    int fun();
}

interface Application
{
    final int run() { printf("Application.run()\n"); fun(); return 2; };
    int fun();
}

class TimedApp : Timer, Application
{
    int funCalls;
    override int fun()
    {
        printf("TimedApp.fun()\n");
        funCalls++;
        return 2;
    }
}

class SubTimedApp : TimedApp
{
    int subFunCalls;

    override int fun()
    {
        printf("SubTimedApp.fun()\n");
        subFunCalls++;
        return 1;
    }
}

void test4647()
{
    //Test access to TimedApps base interfaces
    auto app = new TimedApp();
    assert((cast(Application)app).run() == 2);
    assert((cast(Timer)app).run() == 1);
    assert(app.Timer.run() == 1); // error, no Timer property
    assert(app.Application.run() == 2); // error, no Application property
    assert(app.run() == 1);             // This would call Timer.run() if the two calls
                                        // above were commented out
    assert(app.funCalls == 5);

    assert(app.TimedApp.fun() == 2);
    assert(app.funCalls == 6);

    //Test direct access to SubTimedApp interfaces
    auto app2 = new SubTimedApp();
    assert((cast(Application)app2).run() == 2);
    assert((cast(Timer)app2).run() == 1);
    assert(app2.Application.run() == 2);
    assert(app2.Timer.run() == 1);
    assert(app2.funCalls == 0);
    assert(app2.subFunCalls == 4);

    assert(app2.fun() == 1);
    assert(app2.SubTimedApp.fun() == 1);
    assert(app2.funCalls == 0);
    assert(app2.subFunCalls == 6);

    //Test access to SubTimedApp interfaces via TimedApp
    auto app3 = new SubTimedApp();
    (cast(Timer)cast(TimedApp)app3).run();
    app3.TimedApp.Timer.run();
    assert((cast(Application)cast(TimedApp)app3).run() == 2);
    assert((cast(Timer)cast(TimedApp)app3).run() == 1);
    assert(app3.TimedApp.Application.run() == 2);
    assert(app3.TimedApp.Timer.run() == 1);
    assert(app3.funCalls == 0);
    assert(app3.subFunCalls == 6);
}

/***************************************************/

template T1064(E...) { alias E T1064; }

int[] var1064 = [ T1064!(T1064!(T1064!(1, 2), T1064!(), T1064!(3)), T1064!(4, T1064!(T1064!(T1064!(T1064!(5)))), T1064!(T1064!(T1064!(T1064!())))),6) ];

void test1064()
{
    assert(var1064 == [1,2,3,4,5,6]);
}

/***************************************************/
// 5696

template Seq5696(T...){ alias T Seq5696; }
template Pred5696(T) { alias T Pred5696; }  // TOKtemplate
template Scope5696(int n){ template X(T) { alias T X; } }   // TOKimport
T foo5696(T)(T x) { return x; }
void test5696()
{
    foreach (pred; Seq5696!(Pred5696, Pred5696))
    {
        static assert(is(pred!int == int));
    }

    foreach (scop; Seq5696!(Scope5696!0, Scope5696!1))
    {
        static assert(is(scop.X!int == int));
    }

    alias Seq5696!(foo5696, foo5696) funcs;
    assert(funcs[0](0) == 0);
    assert(funcs[1](1) == 1);
    foreach (i, fn; funcs)
    {
        assert(fn(i) == i);
    }
}

/***************************************************/
// 5933

int dummyfunc5933();
alias typeof(dummyfunc5933) FuncType5933;

struct S5933a { auto x() { return 0; } }
static assert(is(typeof(&S5933a.init.x) == int delegate() pure nothrow @nogc @safe));

struct S5933b { auto x() { return 0; } }
//static assert(is(typeof(S5933b.init.x) == FuncType5933));

struct S5933c { auto x() { return 0; } }
static assert(is(typeof(&S5933c.x) == int function()));

struct S5933d { auto x() { return 0; } }
static assert(is(typeof(S5933d.x) == FuncType5933));


class C5933a { auto x() { return 0; } }
static assert(is(typeof(&(new C5933b()).x) == int delegate()));

class C5933b { auto x() { return 0; } }
//static assert(is(typeof((new C5933b()).x) == FuncType5933));

class C5933c { auto x() { return 0; } }
static assert(is(typeof(&C5933c.x) == int function()));

class C5933d { auto x() { return 0; } }
static assert(is(typeof(C5933d.x) == FuncType5933));

/***************************************************/
// 6084

template TypeTuple6084(T...){ alias T TypeTuple6084; }
void test6084()
{
    int foo(int x)() { return x; }
    foreach(i; TypeTuple6084!(0))
        foo!(i);
}

/***************************************************/
// 6763

template TypeTuple6763(TList...)
{
    alias TList TypeTuple6763;
}

alias TypeTuple6763!(int) T6763;

void f6763(      T6763) { } ///
void c6763(const T6763) { } ///T now is (const int)
void r6763(ref   T6763) { } ///T now is(ref const int)
void i6763(in    T6763) { } ///Uncomment to get an Assertion failure in 'mtype.c'
void o6763(out   T6763) { } ///ditto

void test6763()
{
    int n;

    f6763(0);   //With D2: Error: function main.f ((ref const const(int) _param_0)) is not callable using argument types (int)
    c6763(0);
    r6763(n);   static assert(!__traits(compiles, r6763(0)));
    i6763(0);
    o6763(n);   static assert(!__traits(compiles, o6763(0)));

    // 6755
    static assert(typeof(f6763).stringof == "void(int _param_0)");
    static assert(typeof(c6763).stringof == "void(const(int) _param_0)");
    static assert(typeof(r6763).stringof == "void(ref int _param_0)");
    static assert(typeof(i6763).stringof == "void(const(int) _param_0)");
    static assert(typeof(o6763).stringof == "void(out int _param_0)");
}

/***************************************************/
// 6695

struct X6695
{
    void mfunc()
    {
        static assert(is(typeof(this) == X6695));
    }
    void cfunc() const
    {
        static assert(is(typeof(this) == const(X6695)));
    }
    void ifunc() immutable
    {
        static assert(is(typeof(this) == immutable(X6695)));
    }
    void sfunc() shared
    {
        static assert(is(typeof(this) == shared(X6695)));
    }
    void scfunc() shared const
    {
        static assert(is(typeof(this) == shared(const(X6695))));
    }
    void wfunc() inout
    {
        static assert(is(typeof(this) == inout(X6695)));
    }
    void swfunc() shared inout
    {
        static assert(is(typeof(this) == shared(inout(X6695))));
    }

    static assert(is(typeof(this) == X6695));
}

/***************************************************/
// 6087

template True6087(T)
{
    immutable True6087 = true;
}
struct Foo6087
{
    static assert( True6087!(typeof(this)) );
}

struct Bar6087
{
    static assert( is(typeof(this) == Bar6087) );
}

/***************************************************/
// 6848

class Foo6848 {}

class Bar6848 : Foo6848
{
    void func() immutable
    {
        static assert(is(typeof(this) == immutable(Bar6848)));  // immutable(Bar6848)
        auto t = this;
        static assert(is(typeof(t) == immutable(Bar6848)));     // immutable(Bar6848)

        static assert(is(typeof(super) == immutable(Foo6848))); // Foo6848 instead of immutable(Foo6848)
        auto s = super;
        static assert(is(typeof(s) == immutable(Foo6848)));     // Foo6848 instead of immutable(Foo6848)
    }
}

/***************************************************/

version(none)
{
    cent issue785;
    ucent issue785;
}

static assert(is(cent) && is(ucent) || !is(cent) && !is(ucent));
static if (is(cent))
  static assert(__traits(compiles, { cent x; }));
else
  static assert(!__traits(compiles, { cent x; }));

/***************************************************/
// 6847

template True6847(T)
{
    immutable True6847 = true;
}
class Foo6847
{}

class Bar6847 : Foo6847
{
    static assert( True6847!(typeof(super)) );
    static assert( is(typeof(super) == Foo6847) );
}

/***************************************************/
// http://d.puremagic.com/issues/show_bug.cgi?id=6488

struct TickDuration
{
    template to(T) if (__traits(isIntegral,T))
    {
        const T to()
        {
            return 1;
        }
    }

    template to(T) if (__traits(isFloating,T))
    {
        const T to()
        {
            return 0;
        }
    }

    const long seconds()
    {
        return to!(long)();
    }

}

void test6488()
{
    TickDuration d;
    d.seconds();
}

/***************************************************/
// 6565

void foo6565(out int[2][2] m) {}

void test6565()
{
    int[2][2] mat = [[1, 2], [3, 4]];
    foo6565(mat);
    assert(mat == [[0, 0], [0, 0]]);
}

/***************************************************/
// 6836

template map6836(fun...) if (fun.length >= 1)
{
    auto map6836(Range)(Range r)
    {
    }
}
void test6836()
{
    [1].map6836!"a"();
}

/***************************************************/

string func12864() { return ['a', 'b', 'c']; }

void test12864(string s)
{
    switch (s)
    {
    case func12864():
        break;

    default:
        break;
    }
}

/***************************************************/

void test5448()
{
    int[int][] aaa = [[1: 2]];
    int[string][] a2 = [["cc":0], ["DD":10]];
}

/***************************************************/
// 6837

struct Ref6837a(T)
{
    T storage;
    alias storage this;
}

struct Ref6837b(T)
{
    T storage;
    @property ref T get(){ return storage; }
    alias get this;
}

int front6837(int[] arr){ return arr[0]; }

void popFront6837(ref int[] arr){ arr = arr[1..$]; }

void test6837()
{
    assert([1,2,3].front6837 == 1);

    auto r1 = Ref6837a!(int[])([1,2,3]);
    assert(r1.front6837() == 1);    // ng
    assert(r1.front6837 == 1);      // ok
    r1.popFront6837();              // ng
    r1.storage.popFront6837();      // ok

    auto r2 = Ref6837b!(int[])([1,2,3]);
    assert(r2.front6837() == 1);    // ng
    assert(r2.front6837 == 1);      // ok
    r2.popFront6837();              // ng
    r2.get.popFront6837();          // ng
    r2.get().popFront6837();        // ok
}

/***************************************************/
// 6927

@property int[] foo6927()
{
    return [1, 2];
}
int[] bar6927(int[] a)
{
    return a;
}
void test6927()
{
    bar6927(foo6927); // OK
    foo6927.bar6927(); // line 9, Error
}

/***************************************************/

struct Foo6813(T)
{
    Foo6813 Bar()
    {
        return Foo6813(_indices.abc());
    }

    T _indices;
}

struct SortedRange(alias pred)
{
    SortedRange abc()
    {
        return SortedRange();
    }
}

void test6813() {
    auto ind = SortedRange!({ })();
    auto a = Foo6813!(typeof(ind))();
}

/***************************************************/

struct Interval6753{ int a,b; }
@safe struct S6753
{
    int[] arr;
    @trusted @property auto byInterval() const
    {
        return cast(const(Interval6753)[])arr;
    }
}

/***************************************************/
// 6859

class Parent6859
{
public:
    bool isHage() const @property;

public:
    abstract void fuga()
    out
    {
        assert(isHage);
    }
    body { }
}

class Child6859 : Parent6859
{
    override bool isHage() const @property
    {
        return true;
    }
    override void fuga()
    {
        //nop
    }
}

void test6859()
{
    auto t = new Child6859;
    t.fuga();
    printf("done.\n");
}

/***************************************************/
// 6910

template Test6910(alias i, B)
{
    void fn()
    {
        foreach(t; B.Types)
        {
            switch(i)
            {
                case 0://IndexOf!(t, B.Types):
                {
                    pragma(msg, __traits(allMembers, t));
                    pragma(msg, __traits(hasMember, t, "m"));
                    static assert(__traits(hasMember, t, "m")); // test
                    break;
                }
                default: {}
            }
        }
    }
}
void test6910()
{
    static struct Bag(S...)
    {
        alias S Types;
    }
    static struct A
    {
        int m;
    }

    int i;
    alias Test6910!(i, Bag!(A)).fn func;
}

/***************************************************/

void fun12503()
{
    string b = "abc";
    try
    {
        try
        {
            b = null;
            return;
        }
        catch
        {
        }
    }
    finally
    {
        assert("abc" !is b);
    }
}

void test12503()
{
    fun12503();
}

/***************************************************/
// 6902

void test6902()
{
    static assert(is(typeof({
        return int.init; // int, long, real, etc.
    })));

    int f() pure nothrow { assert(0); }
    alias int T() pure nothrow @safe @nogc;
    static if(is(typeof(&f) DT == delegate))
    {
        static assert(is(DT* == T*));  // ok

        // Error: static assert  (is(pure nothrow int() == pure nothrow int())) is false
        static assert(is(DT == T));
    }
}

/***************************************************/
// 6330

struct S6330
{
    void opAssign(S6330 s) @disable
    {
        assert(0);  // This fails.
    }
}

void test6330()
{
    S6330 s;
    S6330 s2;
    static assert(!is(typeof({ s2 = s; })));
}

/***************************************************/

struct S8269
{
    bool dtor = false;
    ~this()
    {
        dtor = true;
    }
}

void test8269()
{
    with(S8269())
    {
        assert(!dtor);
    }
}

/***************************************************/
// 5311

class C5311
{
    private static int globalData;

    void breaksPure() pure const
    {
        static assert(!__traits(compiles, { globalData++; }));      // SHOULD BE ERROR
        static assert(!__traits(compiles, { C5311.globalData++; }));// SHOULD BE ERROR
        static assert(!__traits(compiles, { this.globalData++; })); // SHOULD BE ERROR

        static assert(!__traits(compiles, { int a = this.globalData; }));
    }
}
static void breaksPure5311a(C5311 x) pure
{
    static assert(!__traits(compiles, { x.globalData++; }));        // SHOULD BE ERROR

    static assert(!__traits(compiles, { int a = x.globalData; }));
}

struct S5311
{
    private static int globalData;

    void breaksPure() pure const
    {
        static assert(!__traits(compiles, { globalData++; }));      // SHOULD BE ERROR
        static assert(!__traits(compiles, { S5311.globalData++; }));// SHOULD BE ERROR
        static assert(!__traits(compiles, { this.globalData++; })); // SHOULD BE ERROR

        static assert(!__traits(compiles, { int a = this.globalData; }));
    }
}
static void breaksPure5311b(S5311 x) pure
{
    static assert(!__traits(compiles, { x.globalData++; }));        // SHOULD BE ERROR

    static assert(!__traits(compiles, { int a = x.globalData; }));
}

/***************************************************/
// 6868

@property bool empty6868(T)(in T[] a) @safe pure nothrow
{
    return !a.length;
}

void test6868()
{
    alias int[] Range;
    static if (is(char[1 + Range.empty6868]))  // Line 9
        enum bool isInfinite = true;

    char[0] s;  // need
}

/***************************************************/
// 2856

struct foo2856    { static void opIndex(int i) { printf("foo\n"); } }
struct bar2856(T) { static void opIndex(int i) { printf("bar\n"); } }

void test2856()
{
    foo2856[1];
    bar2856!(float)[1];     // Error (# = __LINE__)
    alias bar2856!(float) B;
    B[1];                   // Okay
}

/***************************************************/

void test13947()
{
    struct S {}
    static assert(S.sizeof == 1);

    S a;
    S b;
    *cast(ubyte*)&a = 1;
    *cast(ubyte*)&b = 2;
    assert(a == b);
    assert(a is b);
    assert(!(a != b));
    assert(!(a !is b));
    static assert(S() == S());
    static assert(S() is S());
    static assert(!(S() != S()));
    static assert(!(S() !is S()));
}

/***************************************************/
// 3091

void test3091(inout int = 0)
{
    struct Foo {}

    auto  pm = new Foo;                 static assert(is( typeof( pm) ==              Foo  * ));
    auto  pc = new const Foo;           static assert(is( typeof( pc) ==        const(Foo) * ));
    auto  pw = new inout Foo;           static assert(is( typeof( pw) ==        inout(Foo) * ));
    auto psm = new shared Foo;          static assert(is( typeof(psm) ==       shared(Foo) * ));
    auto psc = new shared const Foo;    static assert(is( typeof(psc) == shared(const(Foo))* ));
    auto psw = new shared inout Foo;    static assert(is( typeof(psw) == shared(inout(Foo))* ));
    auto  pi = new immutable Foo;       static assert(is( typeof( pi) ==    immutable(Foo) * ));

    auto  m = Foo();                    static assert(is( typeof( m) ==              Foo   ));
    auto  c = const Foo();              static assert(is( typeof( c) ==        const(Foo)  ));
    auto  w = inout Foo();              static assert(is( typeof( w) ==        inout(Foo)  ));
    auto sm = shared Foo();             static assert(is( typeof(sm) ==       shared(Foo)  ));
    auto sc = shared const Foo();       static assert(is( typeof(sc) == shared(const(Foo)) ));
    auto sw = shared inout Foo();       static assert(is( typeof(sw) == shared(inout(Foo)) ));
    auto  i = immutable Foo();          static assert(is( typeof( i) ==    immutable(Foo)  ));
}

/***************************************************/
// 6837

template Id6837(T)
{
    alias T Id6837;
}
static assert(is(Id6837!(shared const int) == shared const int));
static assert(is(Id6837!(shared inout int) == shared inout int));

/***************************************************/
// 6056 fixup

template ParameterTypeTuple6056(func)
{
    static if (is(func Fptr : Fptr*) && is(Fptr P == function))
        alias P ParameterTypeTuple6056;
    else
        static assert(0, "argument has no parameters");
}

extern(C) alias void function() fpw_t;

alias void function(fpw_t fp) cb_t;

void bar6056(ParameterTypeTuple6056!(cb_t) args) {
      pragma (msg, "TFunction1: " ~ typeof(args[0]).stringof);
}

extern(C) void foo6056() { }

void test6056()
{
    bar6056(&foo6056);
}

/***************************************************/
// 6356

int f6356()(int a)
{
    return a*a;
}

alias f6356!() g6356;     // comment this out to eliminate the errors

pure nothrow @safe int i6356()
{
    return f6356(1);
}

void test6356()
{
    assert(i6356() == 1);
}

/***************************************************/
// 7108

static assert(!__traits(hasMember, int, "x"));
static assert( __traits(hasMember, int, "init"));

/***************************************************/
// 7073

void test7073()
{
    string f(int[] arr...)
    {
        return "";
    }
}

/***************************************************/
// 7104

void test7104()
{
    typeof(new class {}) c;
    c = new typeof(c);
}

/***************************************************/
// 7150

struct A7150
{
    static int cnt;

    this(T)(T thing, int i)
    {
        this(thing, i > 0); // Error: constructor call must be in a constructor
        ++cnt;
    }
    this(T)(T thing, bool b)
    {
        ++cnt;
    }
}

void test7150()
{
    auto a = A7150(5, 5); // Error: template instance constructtest.A.__ctor!(int) error instantiating
    assert(A7150.cnt == 2);
}

/***************************************************/
// 7159

alias void delegate()  Void7159;

class HomeController7159 {
    Void7159 foo() {
        return cast(Void7159)&HomeController7159.displayDefault;
    }
    auto displayDefault() {
        return 1;
    }
}

/***************************************************/
// 7160

class HomeController {
    static if (false) {
        mixin(q{ int a; });
    }
    void foo() {
        foreach (m; __traits(derivedMembers, HomeController)) {
        }
    }
}

void test7160()
{}

/***************************************************/
// 7168

void test7168()
{
    static class X
    {
        void foo(){}
    }
    static class Y : X
    {
        void bar(){}
    }

    enum ObjectMembers = ["toString","toHash","opCmp","opEquals","Monitor","factory"];

    static assert([__traits(allMembers, X)] == ["foo"]~ObjectMembers);          // pass
    static assert([__traits(allMembers, Y)] == ["bar", "foo"]~ObjectMembers);   // fail
    static assert([__traits(allMembers, Y)] != ["bar", "foo"]);                 // fail
}

/***************************************************/
// 7170

T to7170(T)(string x) { return 1; }
void test7170()
{
//  auto i = to7170!int("1");   // OK
    auto j = "1".to7170!int();  // NG, Internal error: e2ir.c 683
}

/***************************************************/
// 7196

auto foo7196(int x){return x;}
auto foo7196(double x){return x;}

void test7196()
{
    auto x = (&foo7196)(1);   // ok
    auto y = (&foo7196)(1.0); // fail
}

/***************************************************/
// 7285

int[2] spam7285()
{
    int[2] ab;
    if (true)
        return (true) ? ab : [0, 0]; // Error
    else
        return (true) ? [0, 0] : ab; // OK
}

void test7285()
{
    auto sa = spam7285();
}

/***************************************************/
// 14737

void test14737()
{
    // compile-time
    enum string[2] a1 = ["d", "e"];
    enum b1x = ["a", "b", "c"] ~ a1;        // Tarray vs Tsarray
    enum b1y = a1 ~ ["a", "b", "c"];        // Tsarray vs Tarray
    static assert(is(typeof(b1x) == string[]));
    static assert(is(typeof(b1y) == string[]));
    static assert(b1x == ["a", "b", "c", "d", "e"]);
    static assert(b1y == ["d", "e", "a", "b", "c"]);

    // runtime
    string[2] a2 = ["d", "e"];
    auto b2x = ["a", "b", "c"] ~ a2;        // Tarray vs Tsarray
    auto b2y = a2 ~ ["a", "b", "c"];        // Tsarray vs Tarray
    static assert(is(typeof(b2x) == string[]));
    static assert(is(typeof(b2y) == string[]));
    assert(b2x == ["a", "b", "c", "d", "e"]);
    assert(b2y == ["d", "e", "a", "b", "c"]);
}

/***************************************************/
// 7321

void test7321()
{
    static assert(is(typeof((){})==void function()pure nothrow @nogc @safe));         // ok
    static assert(is(typeof((){return;})==void function()pure nothrow @nogc @safe));  // fail
}

/***************************************************/

class A158
{
    pure void foo1() { }
    const void foo2() { }
    nothrow void foo3() { }
    @safe void foo4() { }
}

class B158 : A158
{
    override void foo1() { }
    override void foo2() const { }
    override void foo3() { }
    override void foo4() { }
}

/***************************************************/
// 9231

class B9231 { void foo() inout pure {} }
class D9231 : B9231 { override void foo() inout {} }

/***************************************************/
// 3282

class Base3282
{
    string f()
    {
        return "Base.f()";
    }
}
class Derived3282 : Base3282
{
    override string f()
    {
        return "Derived.f()";
    }
  /*override*/ string f() const
    {
        return "Derived.f() const";
    }
}

void test3282()
{
    auto x = new Base3282;
    assert(x.f() == "Base.f()");
    auto y = new Derived3282;
    assert(y.f() == "Derived.f()");// calls "Derived.f() const", but it is expected that be called non-const.
    auto z = new const(Derived3282);
    assert(z.f() == "Derived.f() const");
}

/***************************************************/
// 7534

class C7534
{
    int foo(){ return 1; }
}
class D7534 : C7534
{
    override int foo(){ return 2; }
  /*override*/ int foo() const { return 3; }
    // Error: D.foo multiple overrides of same function
}
void test7534()
{
    C7534 mc = new C7534();
    assert(mc.foo() == 1);

    D7534 md = new D7534();
    assert(md.foo() == 2);
    mc = md;
    assert(mc.foo() == 2);

    const(D7534) cd = new const(D7534)();
    assert(cd.foo() == 3);
    md = cast()cd;
    assert(md.foo() == 2);
}

/***************************************************/
// 7534 + return type covariance

class X7534 {}
class Y7534 : X7534
{
    int value; this(int n){ value = n; }
}

class V7534
{
    X7534 foo(){ return new X7534(); }
}
class W7534 : V7534
{
    override Y7534 foo(){ return new Y7534(1); }
  /*override*/ Y7534 foo() const { return new Y7534(2); }
}

void test7534cov()
{
    auto mv = new V7534();
    assert(typeid(mv.foo()) == typeid(X7534));

    auto mw = new W7534();
    assert(typeid(mw.foo()) == typeid(Y7534));
    assert(mw.foo().value == 1);
    mv = mw;
    assert(typeid(mv.foo()) == typeid(Y7534));
    assert((cast(Y7534)mv.foo()).value == 1);

    auto cw = new const(W7534)();
    assert(typeid(cw.foo()) == typeid(Y7534));
    assert(cw.foo().value == 2);
}

/***************************************************/
// 7562

static struct MyInt
{
    private int value;
    mixin ProxyOf!value;
}
mixin template ProxyOf(alias a)
{
    template X1(){}
    template X2(){}
    template X3(){}
    template X4(){}
    template X5(){}
    template X6(){}
    template X7(){}
    template X8(){}
    template X9(){}
    template X10(){}

    void test1(this X)(){}
    void test2(this Y)(){}
}

/***************************************************/

import core.stdc.stdlib;

void test13427(void* buffer = alloca(100))
{
}

/***************************************************/
// 7583

template Tup7583(E...) { alias E Tup7583; }

struct S7583
{
    Tup7583!(float, char) field;
    alias field this;
    this(int x) {    }
}

int bug7583() {
    S7583[] arr;
    arr ~= S7583(0);
    return 1;
}

static assert (bug7583());

/***************************************************/
// 7618

void test7618(const int x = 1)
{
    int func(ref int x) { return 1; }
    static assert(!__traits(compiles, func(x)));
    // Error: function test.foo.func (ref int _param_0) is not callable using argument types (const(int))

    int delegate(ref int) dg = (ref int x) => 1;
    static assert(!__traits(compiles, dg(x)));
    // --> no error, bad!

    int function(ref int) fp = (ref int x) => 1;
    static assert(!__traits(compiles, fp(x)));
    // --> no error, bad!
}

/***************************************************/
// 7621

void test7621()
{
    enum uint N = 4u;
    char[] A = "hello".dup;
    uint[immutable char[4u]] dict;
    dict[*cast(immutable char[4]*)(A[0 .. N].ptr)] = 0; // OK
    dict[*cast(immutable char[N]*)(A[0 .. N].ptr)] = 0; // line 6, error
}

/***************************************************/
// 7682

template ConstOf7682(T)
{
    alias const(T) ConstOf7682;
}
bool pointsTo7682(S)(ref const S source) @trusted pure nothrow
{
    return true;
}
void test7682()
{
    shared(ConstOf7682!(int[])) x;  // line A

    struct S3 { int[10] a; }
    shared(S3) sh3;
    shared(int[]) sh3sub = sh3.a[];
    assert(pointsTo7682(sh3sub));   // line B
}

/***************************************************/
// 7735

void a7735(void[][] data...)
{
    //writeln(data);
    assert(data.length == 1);
    b7735(data);
}

void b7735(void[][] data...)
{
    //writeln(data);
    assert(data.length == 1);
    c7735(data);
}

void c7735(void[][] data...)
{
    //writeln(data);
    assert(data.length == 1);
}

void test7735()
{
    a7735([]);
    a7735([]);
}

/***************************************************/

struct A7823 {
    long a;
    enum A7823 b = {0};
}

void test7823(A7823 a = A7823.b) { }

/***************************************************/
// 7871

struct Tuple7871
{
    string field;
    alias field this;
}

//auto findSplitBefore(R1)(R1 haystack)
auto findSplitBefore7871(string haystack)
{
    return Tuple7871(haystack);
}

void test7871()
{
    string line = `<bookmark href="https://stuff">`;
    auto a = findSplitBefore7871(line[0 .. $])[0];
}

/***************************************************/
// 7906

void test7906()
{
    static assert(!__traits(compiles, { enum s = [string.min]; }));
}

/***************************************************/
// 7907

template Id7907(E)
{
    alias E Id7907;
}
template Id7907(alias E)
{
    alias E Id7907;
}

void test7907()
{
    static assert(!__traits(compiles, { alias Id7907!([string.min]) X; }));
}

/***************************************************/
// 1175

class A1175
{
    class I1 { }
}

class B1175 : A1175
{
    class I2 : I1 { }

    I1 getI() { return new I2; }
}

/***************************************************/
// 7983

class A7983 {
        void f() {
                g7983(this);
        }
        unittest {
        }
}

void g7983(T)(T a)
{
        foreach (name; __traits(allMembers, T)) {
                pragma(msg, name);
                static if (__traits(compiles, &__traits(getMember, a, name)))
                {
                }
        }
}

/***************************************************/
// 8004

void test8004()
{
    auto n = (int n = 10){ return n; }();
    assert(n == 10);
}

/***************************************************/
// 8064

void test8064()
{
    uint[5] arry;
    ref uint acc(size_t i) {
        return arry[i];
    }
    auto arryacc = &acc;
    arryacc(3) = 5; // same error
}

/***************************************************/
// 8220

void foo8220(int){}
static assert(!__traits(compiles, foo8220(typeof(0)))); // fail

/***************************************************/

void func8105(in ref int x) { }

void test8105()
{
}

/***************************************************/

template ParameterTypeTuple159(alias foo)
{
    static if (is(typeof(foo) P == __parameters))
        alias P ParameterTypeTuple159;
    else
        static assert(0, "argument has no parameters");
}

int func159(int i, long j = 7) { return 3; }

alias ParameterTypeTuple159!func159 PT;

int bar159(PT) { return 4; }

pragma(msg, typeof(bar159));
pragma(msg, PT[1]);

PT[1] boo159(PT[1..2] a) { return a[0]; }

void test159()
{
    assert(bar159(1) == 4);
    assert(boo159() == 7);
}

/***************************************************/
// 8283

struct Foo8283 {
    this(long) { }
}

struct FooContainer {
    Foo8283 value;
}

auto get8283() {
    union Buf { FooContainer result; }
    Buf buf = {};
    return buf.result;
}

void test8283() {
    auto a = get8283();
}


/***************************************************/
// 8395

struct S8395
{
    int v;
    this(T : long)(T x) { v = x * 2; }
}
void test8395()
{
    S8395 ms = 6;
    assert(ms.v == 12);
    const S8395 cs = 7;
    assert(cs.v == 14);
}

/***************************************************/
// 5749

void test5749()
{
    static struct A
    {
        A foo(int x, int i)
        {
            //printf("this = %p, %d: i=%d\n", &this, x, i);
            assert(i == x);
            return this;
        }
        A bar(int x, ref int i)
        {
            //printf("this = %p, %d: i=%d\n", &this, x, i);
            assert(i == x);
            return this;
        }
    }

    static     int inc1(ref int i) { return ++i; }
    static ref int inc2(ref int i) { return ++i; }

    int i;
    A a;
    //printf("&a = %p\n", &a);

    i = 0;  a.foo(1, ++i).foo(2, ++i);          // OK <-- 2 1
    i = 0;  a.bar(1, ++i).bar(2, ++i);          // OK <-- 2 2
    i = 0;  a.foo(1, inc1(i)).foo(2, inc1(i));  // OK <-- 2 1
    i = 0;  a.bar(1, inc2(i)).bar(2, inc2(i));  // OK <-- 2 2
    //printf("\n");

    A getVal() { static A a; return a; }
    i = 0;  getVal().foo(1, ++i).foo(2, ++i);           // OK <-- 2 1
    i = 0;  getVal().bar(1, ++i).bar(2, ++i);           // OK <-- 2 2
    i = 0;  getVal().foo(1, inc1(i)).foo(2, inc1(i));   // OK <-- 2 1
    i = 0;  getVal().bar(1, inc2(i)).bar(2, inc2(i));   // OK <-- 2 2
    //printf("\n");

    ref A getRef() { static A a; return a; }
    i = 0;  getRef().foo(1, ++i).foo(2, ++i);           // OK <-- 2 1
    i = 0;  getRef().bar(1, ++i).bar(2, ++i);           // OK <-- 2 2
    i = 0;  getRef().foo(1, inc1(i)).foo(2, inc1(i));   // OK <-- 2 1
    i = 0;  getRef().bar(1, inc2(i)).bar(2, inc2(i));   // OK <-- 2 2
}

/***************************************************/
// 8396

void test8396()
{
    static int g;

    static extern(C) int bar(int a, int b)
    {
        //printf("a = %d, b = %d\n", a, b);
        assert(b - a == 1);
        return ++g;
    }
    static auto getFunc(int n)
    {
        assert(++g == n);
        return &bar;
    }

    static struct Tuple { int _a, _b; }
    static Tuple foo(int n)
    {
        assert(++g == n);
        return Tuple(1, 2);
    }

    g = 0;
    assert(bar(foo(1).tupleof) == 2);

    g = 0;
    assert(getFunc(1)(foo(2).tupleof) == 3);
}

/***************************************************/

enum E160 : ubyte { jan = 1 }

struct D160
{
    short _year  = 1;
    E160 _month = E160.jan;
    ubyte _day   = 1;

    this(int year, int month, int day) pure
    {
        _year  = cast(short)year;
        _month = cast(E160)month;
        _day   = cast(ubyte)day;
    }
}

struct T160
{
    ubyte _hour;
    ubyte _minute;
    ubyte _second;

    this(int hour, int minute, int second = 0) pure
    {
        _hour   = cast(ubyte)hour;
        _minute = cast(ubyte)minute;
        _second = cast(ubyte)second;
    }
}

struct DT160
{
    D160 _date;
    T160 _tod;

    this(int year, int month, int day,
         int hour = 0, int minute = 0, int second = 0) pure
    {
        _date = D160(year, month, day);
        _tod = T160(hour, minute, second);
    }
}

void foo160(DT160 dateTime)
{
    printf("test7 year %d, day %d\n", dateTime._date._year, dateTime._date._day);
    assert(dateTime._date._year == 1999);
    assert(dateTime._date._day == 6);
}

void test160()
{
    auto dateTime = DT160(1999, 7, 6, 12, 30, 33);
    printf("test5 year %d, day %d\n", dateTime._date._year, dateTime._date._day);
    assert(dateTime._date._year == 1999);
    assert(dateTime._date._day == 6);
    foo160(DT160(1999, 7, 6, 12, 30, 33));
}

/***************************************************/
// 8437

class Cgi8437
{
    struct PostParserState {
        UploadedFile piece;
    }

    static struct UploadedFile {
        string contentFilename;
    }
}

/***************************************************/
// 8665

auto foo8665a(bool val)
{
    if (val)
        return 42;
    else
        return 1.5;
}
auto foo8665b(bool val)
{
    if (!val)
        return 1.5;
    else
        return 42;
}

void test8665()
{
    static assert(is(typeof(foo8665a(true))  == double));
    static assert(is(typeof(foo8665b(false)) == double));
    assert(foo8665a(true) == 42); // assertion failure
    assert(foo8665b(true) == 42); // assertion failure
    assert(foo8665a(false) == 1.5);
    assert(foo8665b(false) == 1.5);

    static assert(foo8665a(true) == 42);
    static assert(foo8665b(true) == 42);
    static assert(foo8665a(false) == 1.5);
    static assert(foo8665b(false) == 1.5);
}

/***************************************************/

int foo8108(int, int);

int foo8108(int a, int b)
{
    return a + b;
}

void test8108()
{
    foo8108(1,2);
}

/***************************************************/
// 8360

struct Foo8360
{
    int value = 0;
    int check = 1337;

    this(int value)
    {
        assert(0);
        this.value = value;
    }

    ~this()
    {
        assert(0);
        assert(check == 1337);
    }

    string str()
    {
        assert(0);
        return "Foo";
    }
}

Foo8360 makeFoo8360()
{
    assert(0);
    return Foo8360(2);
}

void test8360()
{
    size_t length = 0;

    // The message part 'makeFoo().str()' should not be evaluated at all.
    assert(length < 5, makeFoo8360().str());
}

/***************************************************/
// 8361

struct Foo8361
{
    string bar = "hello";
    ~this() {}
}

void test8361()
{
    assert(true, Foo8361().bar);
}

/***************************************************/
// 6141 + 8526

void test6141()
{
    static void takeADelegate(void delegate()) {}
    auto items = new int[1];
    items[0] = 17;
    foreach (ref item; items)
    {
        // both asserts fail
        assert(item == 17);
        assert(&item == items.ptr);

        takeADelegate({ auto x = &item; });
    }

    foreach(ref val; [3])
    {
        auto dg = { int j = val; };
        assert(&val != null); // Assertion failure
        assert(val == 3);
    }

    static void f(lazy int) {}
    int i = 0;
    auto dg = { int j = i; };
    foreach(ref val; [3])
    {
        f(val);
        assert(&val != null); // Assertion failure
        assert(val == 3);
    }
}

void test8526()
{
    static void call(void delegate() dg) { dg(); }

    foreach (i, j; [0])
    {
        call({
            assert(i == 0); // fails, i is corrupted
        });
    }

    foreach (n; 0..1)
    {
        call({
            assert(n == 0); // fails, n is corrupted
        });
    }
}

/***************************************************/

template ParameterTuple(alias func)
{
    static if(is(typeof(func) P == __parameters))
        alias P ParameterTuple;
    else
        static assert(0);
}

int foo161(ref float y);

void test161()
{
    alias PT = ParameterTuple!foo161;
    auto x = __traits(identifier, PT);
    assert(x == "y");
}

/***************************************************/
// 7175

void test7175()
{
    struct S { ubyte[0] arr; }
    S s;
    assert(s.arr.ptr !is null);
    assert(cast(void*)s.arr.ptr is cast(void*)&s);
}

/***************************************************/
// 8819

void test8819()
{
    void[1] sa1 = (void[1]).init;
    assert((cast(ubyte*)sa1.ptr)[0] == 0);

    void[4] sa4 = [cast(ubyte)1,cast(ubyte)2,cast(ubyte)3,cast(ubyte)4];
    assert((cast(ubyte*)sa4.ptr)[0] == 1);
    assert((cast(ubyte*)sa4.ptr)[1] == 2);
    assert((cast(ubyte*)sa4.ptr)[2] == 3);
    assert((cast(ubyte*)sa4.ptr)[3] == 4);

    auto sa22 = (void[2][2]).init;
    static assert(sa22.sizeof == ubyte.sizeof * 2 * 2);
    ubyte[4]* psa22 = cast(ubyte[4]*)sa22.ptr;
    assert((*psa22)[0] == 0);
    assert((*psa22)[1] == 0);
    assert((*psa22)[2] == 0);
    assert((*psa22)[3] == 0);
}

/***************************************************/
// 8897

class C8897
{
    static mixin M8897!(int);
    static class causesAnError  {}
}

template M8897 ( E ) { }

/***************************************************/
// 8917

void test8917()
{
    int[3] a;
    int[3] a2;
    int[3] b = a[] + a2[];
}

/***************************************************/
// 8945

struct S8945 // or `class`, or `union`
{
    struct S0(T) { int i; }
    struct S1(T) { this(int){} }
}

void test8945()
{
    auto cs0a = const S8945.S0!int();  // ok
    auto cs0b = const S8945.S0!int(1); // ok
    auto cs1  = const S8945.S1!int(1); // ok

    auto s0a = S8945.S0!int();  // Error: struct S0 does not overload ()
    auto s0b = S8945.S0!int(1); // Error: struct S0 does not overload ()
    auto s1  = S8945.S1!int(1); // Error: struct S1 does not overload ()
}

/***************************************************/

struct S162
{
    static int generateMethodStubs( Class )()
    {
        int text;

        foreach( m; __traits( allMembers, Class ) )
        {
            static if( is( typeof( mixin( m ) ) ) && is( typeof( mixin( m ) ) == function ) )
            {
                pragma(msg, __traits( getOverloads, Class, m ));
            }
        }

        return text;
    }

    enum int ttt = generateMethodStubs!( S162 )();

    float height();
    int get( int );
    int get( long );
    void clear();

    void draw( int );
    void draw( long );
}

/***************************************************/

void test163() {
    static class C { int x; int y; }

    immutable C c = new C();
    shared C c2 = new C();
    shared const C c3 = new C();

    class D { int x; int y; }
    immutable D d;
    assert(!__traits(compiles, d = new D()));

    static struct S { int x; int y; }

    immutable S* s = new S();
    shared S* s2 = new S();
    shared const S* s3 = new S();

    shared S* s4;
    assert(__traits(compiles, s4 = new immutable(S)()));

    struct T { int x; int y; }
    immutable T* t;
    assert(!__traits(compiles, t = new T()));

    immutable int* pi = new int();
    immutable void* pv = new int();

    immutable int[] ai = new int[1];
    immutable void[] av = new int[2];
}

/***************************************************/
struct S9000
{ ubyte i = ubyte.max; }

enum E9000 = S9000.init;

/***************************************************/

mixin template DefineCoreType(string type)
{
    struct Faulty
    {
        static int x;

        static void instance()
        {
            x = 3;
        }

        X164!() xxx;
    }
}

mixin DefineCoreType!("");


mixin template A164()
{
    static this()
    {
    }
}

struct X164()
{
    mixin A164!();
}


/***************************************************/
// 9428

void test9428()
{
    int[2][] items = [[1, 2]];
    int[2] x = [3, 4];

    auto r1 = items ~ [x];
    assert(r1.length == 2);
    assert(r1[0][0] == 1);
    assert(r1[0][1] == 2);
    assert(r1[1][0] == 3);
    assert(r1[1][1] == 4);

    auto r2 = items ~ x;
    assert(r2.length == 2);
    assert(r2[0][0] == 1);
    assert(r2[0][1] == 2);
    assert(r2[1][0] == 3);
    assert(r2[1][1] == 4);

    auto r3 = [x] ~ items;
    assert(r3.length == 2);
    assert(r3[0][0] == 3);
    assert(r3[0][1] == 4);
    assert(r3[1][0] == 1);
    assert(r3[1][1] == 2);

    auto r4 = x ~ items;
    assert(r4.length == 2);
    assert(r4[0][0] == 3);
    assert(r4[0][1] == 4);
    assert(r4[1][0] == 1);
    assert(r4[1][1] == 2);
}

/***************************************************/
// 9477

template Tuple9477(T...) { alias T Tuple9477; }
template Select9477(bool b, T, U) { static if (b) alias T Select9477; else alias U Select9477; }

void test9477()
{
    static bool isEq (T1, T2)(T1 s1, T2 s2) { return s1 == s2; }
    static bool isNeq(T1, T2)(T1 s1, T2 s2) { return s1 != s2; }

    // Must be outside the loop due to http://d.puremagic.com/issues/show_bug.cgi?id=9748
    int order;
    // Must be outside the loop due to http://d.puremagic.com/issues/show_bug.cgi?id=9756
    auto checkOrder(bool dyn, uint expected)()
    {
        assert(order==expected);
        order++;
        // Use temporary ("v") to work around http://d.puremagic.com/issues/show_bug.cgi?id=9402
        auto v = cast(Select9477!(dyn, string, char[1]))"a";
        return v;
    }

    foreach (b1; Tuple9477!(false, true))
        foreach (b2; Tuple9477!(false, true))
        {
            version (D_PIC) {} else // Work around http://d.puremagic.com/issues/show_bug.cgi?id=9754
            {
                assert( isEq (cast(Select9477!(b1, string, char[0]))"" , cast(Select9477!(b2, string, char[0]))""  ));
                assert(!isNeq(cast(Select9477!(b1, string, char[0]))"" , cast(Select9477!(b2, string, char[0]))""  ));

                assert(!isEq (cast(Select9477!(b1, string, char[0]))"" , cast(Select9477!(b2, string, char[1]))"a" ));
                assert( isNeq(cast(Select9477!(b1, string, char[0]))"" , cast(Select9477!(b2, string, char[1]))"a" ));
            }

            assert( isEq (cast(Select9477!(b1, string, char[1]))"a", cast(Select9477!(b2, string, char[1]))"a" ));
            assert(!isNeq(cast(Select9477!(b1, string, char[1]))"a", cast(Select9477!(b2, string, char[1]))"a" ));

            assert(!isEq (cast(Select9477!(b1, string, char[1]))"a", cast(Select9477!(b2, string, char[1]))"b" ));
            assert( isNeq(cast(Select9477!(b1, string, char[1]))"a", cast(Select9477!(b2, string, char[1]))"b" ));

            assert(!isEq (cast(Select9477!(b1, string, char[1]))"a", cast(Select9477!(b2, string, char[2]))"aa"));
            assert( isNeq(cast(Select9477!(b1, string, char[1]))"a", cast(Select9477!(b2, string, char[2]))"aa"));

            // Note: order of evaluation was not followed before this patch
            // (thus, the test below will fail without the patch).
            // Although the specification mentions that as implementation-defined behavior,
            // I understand that this isn't by design, but rather an inconvenient aspect of DMD
            // that has been moved to the specification.
            order = 0;
            bool result = checkOrder!(b1, 0)() == checkOrder!(b2, 1)();
            assert(result);
            assert(order == 2);
        }

    // need largest natural alignment to avoid unaligned access on
    // some architectures, double in this case.
    align(8) ubyte[64] a1, a2;
    foreach (T; Tuple9477!(void, ubyte, ushort, uint, ulong, char, wchar, dchar, float, double))
    {
        auto s1 = cast(T[])(a1[]);
        auto s2 = cast(T[])(a2[]);
        assert(s1 == s2);
        a2[$-1]++;
        assert(s1 != s2);
        assert(s1[0..$-1]==s2[0..$-1]);
        a2[$-1]--;
    }
}

/***************************************************/
// 9504

struct Bar9504
{
    template Abc(T)
    {
        T y;
    }

    enum size_t num = 123;

    class Def {}
}

template GetSym9504(alias sym) { static assert(__traits(isSame, sym, Bar9504.Abc)); }
template GetExp9504(size_t n) { static assert(n == Bar9504.num); }
template GetTyp9504(T) { static assert(is(T == Bar9504.Def)); }

alias GetSym9504!(typeof(Bar9504.init).Abc) X9504; // NG
alias GetExp9504!(typeof(Bar9504.init).num) Y9504; // NG
alias GetTyp9504!(typeof(Bar9504.init).Def) Z9504;

Bar9504 test9504()
{
    alias GetSym9504!(typeof(return).Abc) V9504; // NG
    alias GetExp9504!(typeof(return).num) W9504; // NG
    alias GetTyp9504!(typeof(return).Def) X9504;
    return Bar9504();
}

/***************************************************/
// 9538

void test9538()
{
    void*[1] x;
    auto ti = typeid(x.ptr);
}

/***************************************************/
// 9539

void test9539()
{
    void f(int** ptr)
    {
        assert(**ptr == 10);
    }
    int* p = new int;
    *p = 10;
    int*[1] x = [p];
    f(&x[0]);

    int*[] arr = [null];
    static assert(!__traits(compiles, p = arr));    // bad!
}

/***************************************************/
// 9700

mixin template Proxy9700(alias a)
{
    auto ref opOpAssign(string op, V)(V v) { return a += v; }  // NG
  //auto ref opOpAssign(string op, V)(V v) { a += v; } // OK
}
struct MyInt9700
{
    int value;
    invariant() { assert(value >= 0); }
    mixin Proxy9700!value;
}
void test9700()
{
    MyInt9700 a = { 2 };
    a *= 3;   // object.Error: Access Violation
}

/***************************************************/
// 9834

struct Event9834
{
    void delegate() dg;
    void set(void delegate() h) pure { dg = h; }  // AV occurs
    void call() { dg(); }
}
void test9834()
{
    Event9834 ev;
    auto a = new class
    {
        Object o;
        this()
        {
            o = new Object;
            ev.set((){ o.toString(); });
        }
    };
    ev.call();
}

/***************************************************/
// 9859

void test9859(inout int[] arr)
{
    auto dg1 = { foreach (i, e; arr) { } };
    dg1();

    void foo() { auto v = arr; auto w = arr[0]; }
    void bar(inout int i) { auto v = arr[i]; }

    auto dg2 =
    {
        auto dg =
        {
            void foo(T)()
            {
                auto dg =
                {
                    auto dg =
                    {
                        auto v = arr;
                    };
                };
            }
            foo!int;
        };
    };

    void qux(T)()
    {
        auto v = arr;
        auto dg1 = { auto v = arr; };
        auto dg2 =
        {
            auto dg =
            {
                auto v = arr;
            };
        };
    }
    qux!int;
}

/***************************************************/
// 9912

template TypeTuple9912(Stuff...)
{
    alias Stuff TypeTuple9912;
}

struct S9912
{
    int i;
    alias TypeTuple9912!i t;

    void testA() {
        auto x = t;
    }

    void testB() {
        auto x = t;
    }
}

/***************************************************/
// 9883

struct S9883
{
    @property size_t p9883(T)() { return 0; }
}

@property size_t p9883(T)() { return 0; }

void test9883()
{
    S9883 s;
    auto n1 = p9883!int; // OK
    auto n2 = s.p9883!int; // OK
    auto a1 = new int[p9883!int]; // Error: need size of rightmost array, not type p!(int)
    auto a2 = new int[s.p9883!int]; // Error: no property 'p!(int)' for type 'S'
}


/***************************************************/
// 10091

struct S10091
{
    enum e = "a";
}

void test10091()
{
    auto arr = cast(ubyte[1]) S10091.e;
}

/***************************************************/

void test12824()
{
label:
    static if (0)
    {
    }
}

/***************************************************/
// 9130

class S9130 { void bar() { } }

import core.stdc.stdio : printf;

struct Function
{
    int[] ai = [1,2,3];
}

@property void meta(alias m)()
{
    static Function md;
    printf("length = %d\n", md.ai.length);
    printf("ptr = %p\n", md.ai.ptr);
    md.ai[0] = 0;
}

void test9130()
{
    meta!(__traits(getOverloads, S9130, "bar")[0]);
    meta!(S9130.bar);
}

/***************************************************/
// 10390

class C10390 { this() { this.c = this; } C10390 c; }
const c10390 = new C10390();
pragma(msg, c10390);

/***************************************************/
// 10542

class B10542
{
    this() nothrow pure @safe { }
}

class D10542 : B10542
{
}

void test10542() nothrow pure @safe
{
    new D10542;
}

/***************************************************/
// 10539

void test10539()
{
    int[2][2] a;
    int* p1 = a.ptr.ptr;    // OK <- error
    int* p2 = (*a.ptr).ptr; // OK
    assert(p1 is p2);
}

/***************************************************/

struct TimeOfDay
{
    ubyte h, m, s;
}

__gshared byte glob;

struct DateTime
{
    this(ubyte _d, ubyte _m, ubyte _y, TimeOfDay _tod = TimeOfDay.init)
    {
        d = _d;
        m = _m;
        y = _y;
        tod = _tod;
    }
    TimeOfDay tod;
    ubyte d, m, y;
}


void test10634()
{
    glob = 123;
    DateTime date1 = DateTime(0, 0, 0);
    DateTime date2;
    assert(date1 == date2);
}

/***************************************************/

immutable(char)[4] bar7254(int i)
{
    if (i)
    {
        immutable(char)[4] r; return r;
    }
    else
        return "1234";
}

void test7254()
{
    assert(bar7254(0) == "1234");
}

/***************************************************/

struct S11075() { int x = undefined_expr; }

class C11075() { int x = undefined_expr; }

interface I11075() { enum int x = undefined_expr; }

void test11075()
{
    static assert(!is(typeof(S11075!().x)));
    static assert(!is(typeof(S11075!().x)));

    static assert(!is(typeof(C11075!().x)));
    static assert(!is(typeof(C11075!().x)));

    static assert(!is(typeof(I11075!().x)));
    static assert(!is(typeof(I11075!().x)));
}

/***************************************************/
// 11181

void test11181()
{
    auto a = ["a", "b"];

    static assert(!is(typeof([a, "x"])));
    static assert(!is(typeof(true ? a : "x")));

    static assert(!is(typeof(true ? a[0 .. $] : "x")));
    static assert(!is(typeof([a[0 .. $], "x"])));
}

/***************************************************/
// 11317

void test11317()
{
    auto ref uint fun()
    {
        return 0;
    }

    void test(ref uint x) {}
    static assert(!__traits(compiles, test(fun())));

    assert(fun() == 0);
}

/***************************************************/
// 11888

void test11888()
{
    static long val;

    static ubyte* foo(size_t* len)
    {
        *len = val.sizeof;
        return cast(ubyte*)&val;
    }

    size_t size;
    ubyte[] t = foo(&size)[0..size];
    assert(t.ptr is cast(void*)&val);
    assert(t.length == 8);

    // regression test
    int[3] sa1 = [1,2,3];
    int[1] sa2 = sa1[1..2]; // convert slice to Tsarray
    assert(sa2.length == 1);
    assert(sa2[0] == 2);
}

/***************************************************/
// 12036

template T12036(alias a)
{
    string value;
}

struct S12036
{
    auto fun() { }
    mixin T12036!fun;
}

void test12036()
{
    S12036 s;
    assert(s.value == "");
}

/***************************************************/
// 12153

void test12153()
{
    int[1] i, j;
    bool b = true;
    (b ? i : j)[] = [4];
    assert(i == [4]);

    // regression test
    int[1][1] k, l;
    (b ? k : l)[0..1][0..1] = [4];
    assert(k == [[4]]);
}

/***************************************************/
// 12498

string a12498()
{
    string b;
    while (b) { }
    for (; b; ) { }
    return "";
}

void test12498()
{
    enum t = a12498();
    string x = t;
}

/***************************************************/
// 12900

struct A12900
{
    char[1] b;
}

void test12900()
{
    A12900 c;
    if (*c.b.ptr)
        return;
}

/***************************************************/
// 12937

void test12937()
{
    void[1] sa2 = cast(void[])[cast(ubyte)1];   // ICE!
    assert((cast(ubyte[])sa2[])[0] == 1);
}

/***************************************************/
// 13154

void test13154()
{
    int[3] ints      = [2   , 1   , 0   , 1   ][0..3];
    float[3] floats0 = [2f  , 1f  , 0f  , 1f  ][0..3];
    float[3] floats1 = [2.0 , 1.0 , 0.0 , 1.0 ][0..3];  // fails!
    float[3] floats2 = [2.0f, 1.0f, 0.0f, 1.0f][0..3];
    assert(ints == [2, 1, 0]);
    assert(floats0 == [2, 1, 0]);
    assert(floats1 == [2, 1, 0]); // fail!
    assert(floats1 != [0, 0, 0]); // fail!
    assert(floats2 == [2, 1, 0]);
}

/***************************************************/
// 13437

ubyte[4] foo13437() { return [1,2,3,4]; }

void test13437()
{
    auto n = cast(ubyte[4])foo13437()[];  // OK <- ICE: e2ir.c 4616
    static assert(is(typeof(n) == ubyte[4]));
    assert(n == [1,2,3,4]);
}

/***************************************************/
// 13472

class A13472
{
    int a;
}

void test13472()
{
    A13472[] test;
    test.length = 4;
    auto b = test[0..2] ~ null ~ test[2..$];
    assert(b.length == 5);
}

/***************************************************/
// 13476

template ParameterTypeTuple13476(func...)
{
    static if (is(typeof(*func[0]) P == function))
        alias ParameterTypeTuple13476 = P;
    else
        static assert(0, "argument has no parameters");
}

int flag13476;

__gshared extern(C) void function(int) nothrow someFunc13476 = &Stub13476!someFunc13476;

extern(C) auto Stub13476(alias func)(ParameterTypeTuple13476!func args)
{
    ++flag13476;
    extern(C) void function(int) nothrow impl = (i) { };
    return (func = impl)(args);
}

__gshared extern(C) void function(int) nothrow  someFunc13476Alt = &Stub13476Alt!someFunc13476AltP;
__gshared extern(C) void function(int) nothrow* someFunc13476AltP = &someFunc13476Alt;

extern(C) auto Stub13476Alt(alias func)(int args) nothrow
{
    ++flag13476;
    extern(C) void function(int) nothrow impl = (i) {};
    return (*func = impl)(args);
}

void test13476()
{
    assert(flag13476 == 0);

    someFunc13476(42);
    assert(flag13476 == 1);
    someFunc13476(43);
    assert(flag13476 == 1);

    someFunc13476Alt(42);
    assert(flag13476 == 2);
    someFunc13476Alt(43);
    assert(flag13476 == 2);
}

/***************************************************/
// 14038

static immutable ubyte[string] wordsAA14038;
static this()
{
    wordsAA14038["zero"] = 0;
}

/***************************************************/
// 14192

void test14192()
{
    shared int[int] map;
    map[1] = 1;
}

/***************************************************/
// 13720

struct FracSec13720
{
    this(int hnsecs) {}
}

struct SysTime13720
{
    this(TimeOfDay13720 dateTime, FracSec13720 fracSec)
    {
    }
}

struct TimeOfDay13720
{
    ~this() { }
}

void assertThrown13720(T)(lazy T) {}

void test13720()
{
    assertThrown13720(SysTime13720(TimeOfDay13720.init, FracSec13720(-1)));
}

/***************************************************/
// 13952

struct Reg13952
{
    ubyte type;
    ubyte regNo;
    ushort size;
}

struct Imm13952
{
    ulong imm;
}

struct Opnd13952
{
    union
    {
        Reg13952 reg; // size == 4
        Imm13952 imm; // size == 8
    }
    ubyte tag;

    this(Reg13952 r) { reg = r; }
}

Opnd13952 opnd13952(Reg13952 reg)
{
    return Opnd13952(reg);
}

void test13952()
{
    Reg13952 reg;
    auto op = opnd13952(reg);
    auto buf = (cast(ubyte*)&op)[0 .. op.sizeof];
    //debug
    //{
    //    import std.stdio;
    //    writefln("op.reg = [%(%02x %)]", (cast(ubyte*)&op.reg)[0 .. Reg13952.sizeof]);
    //    writefln("op.imm = [%(%02x %)]", (cast(ubyte*)&op.imm)[0 .. Imm13952.sizeof]);
    //}
    foreach (e; buf) assert(e == 0);
}

/***************************************************/
// 14165

class Foo14165
{
    @disable this();
    this(int i) {}
}

/***************************************************/
// 13985

interface I13985
{
    void m1();
    void m2();
    void m3();

    final void mf()
    {
        m3();
    }
}

class C13985 : I13985
{
    void m1() {}
    void m2() {}
    void m3() {}
}

class D13985 : C13985
{
    void ml()
    {
        super.mf();
    }
}

void test13985()
{
    auto d = new D13985();
    d.ml();
}

/***************************************************/
// 14211

extern(C++) // all derived classes won't have invariants
class B14211
{
    void func()
    {
    }
}

final class C14211 : B14211
{
}

void test14211()
{
    auto c = new C14211();
    *cast(void**)c = null;
    c.func();   // called without vtbl access
}

/***************************************************/
// 14552

template map14552(fun...)
{
    template AppliedReturnType(alias f)
    {
        alias typeof(f(0)) AppliedReturnType;
    }

    auto map14552(int[] r)
    {
        assert(!is(AppliedReturnType!fun));
        return MapResult14552!fun();
    }
}

struct MapResult14552(alias fun)
{
    @property front()
    {
        fun(0);
    }
}

class Outer14552
{
    auto test()
    {
        [1].map14552!(j => new Inner);
    }
    class Inner {}
}

/***************************************************/
// 14853

struct Queue14853(T)
{
    struct Node
    {
        T mfPayload = T.init;
        union
        {
                   typeof(this)*  mfPrev;
            shared(typeof(this)*) mfShPrev;
        }
        union
        {
                   typeof(this)*  mfNext;
            shared(typeof(this)*) mfShNext;
        }
    }

    Node root;

    void pfPut(T v, Node* r = null)
    {
        shared n = new Node(v);    // problem!
    }
}

void test14853()
{
    auto b1 = new Queue14853!uint;
}

/********************************************************/
// 15045

void test15045()
{
    void testName(T, bool r, string name)()
    {
        T t;

        static assert(r ==          is(typeof(mixin("T."~name))));
        static assert(r ==          is(typeof(mixin("t."~name))));
        static assert(r == __traits(compiles, mixin("T."~name)));
        static assert(r == __traits(compiles, mixin("t."~name)));
        static assert(r == mixin("__traits(compiles, T."~name~")"));
        static assert(r == mixin("__traits(compiles, t."~name~")"));

        static assert(r ==                       __traits(hasMember, T, name) );
        static assert(r ==                       __traits(hasMember, t, name) );
        static assert(r == __traits(compiles,    __traits(getMember, T, name) ));
        static assert(r == __traits(compiles,    __traits(getMember, t, name) ));
        static assert(r == __traits(compiles, __traits(getOverloads, T, name) ));
        static assert(r == __traits(compiles, __traits(getOverloads, t, name) ));
    }
    void test(T, bool r)()
    {
        testName!(T, r, "__ctor")();
        testName!(T, r, "__dtor")();
        testName!(T, r, "__xdtor")();
        testName!(T, r, "__postblit")();
        testName!(T, r, "__xpostblit")();
    }

    static struct X
    {
        this(int) {}
        this(this) {}
        ~this() {}
    }

    static struct S1
    {
        auto opDispatch(string name, A...)(A args) { }
    }
    static struct S2
    {
        X get() { return X(); };
        alias get this;
    }
    static struct S3
    {
        X opDot() { return X(); };
    }

    test!(X, true)();
    test!(S1, false)();
    test!(S2, false)();
    test!(S3, false)();
}

/***************************************************/
// 15116

alias TypeTuple15116(T...) = T;

template Mix15116()
{
    TypeTuple15116!(int, int) tup;
}

struct S15116
{
    mixin Mix15116 mix;
}

void test15116()
{
    S15116 s;
    auto x1 = s.tup;        // OK
    auto x2 = s.mix.tup;    // OK <- NG
}

/***************************************************/
// 15117

template Mix15117()
{
    int y = { typeof(this)* s; return s ? s.mix.y : 0; }();
}

struct S15117
{
    int x = { typeof(this)* s; return s ? s.x : 0; }(); // OK

    mixin Mix15117 mix;     // OK <- NG
}

/***************************************************/
// 15126

struct Json15126
{
    ubyte[16] m_data;
    int opDispatch(string prop)() const { return 0; }
    int opDispatch(string prop)() { return 0; }
}

template isCustomSerializable15126(T)
{
    enum isCustomSerializable15126 = T.init.toRepresentation();
}

alias bug15126 = isCustomSerializable15126!Json15126;

/***************************************************/
// 15141

class A15141
{
    abstract void method();
}

class B15141 : A15141 { }

void test15141()
{
    auto a = Object.factory(__MODULE__ ~ ".A15141");
    assert(a is null);
    auto b = Object.factory(__MODULE__ ~ ".B15141");
    assert(b is null); // OK <- oops
}

/***************************************************/
// 15366

enum E15366 : bool { A, B };

struct S15366
{
    void func1(E15366 e) {}

    void func2(E15366 a, E15366 b)
    {
        func1(cast(E15366)(a && b));
        func1(cast(E15366)(a || b));

        auto x1 = cast(E15366)(a && b);
        auto x2 = cast(E15366)(a || b);
    }
}

/***************************************************/
// 15369

struct MsgTable15369
{
    const(char)[] ident;
    const(char)* name;
};

MsgTable15369[] msgTable15369 =
[
    { "empty", "" },
];

void test15369()
{
    auto id = msgTable15369[0].ident;
    auto p = msgTable15369[0].name;

    // a string literal "" should be zero-terminated
    assert(*p == '\0');
}

void test15638()
{
    class A {}
    class B : A {}
    class C : A {}

    B b;
    C c;
    const(B) cb;
    const(C) cc;
    immutable(B) ib;
    immutable(C) ic;

    // Common type for const derived classes
    auto constCommon = true ? cb : cc;
    static assert(is(typeof(constCommon) == const(A)));

    // Common type for immutable derived classes
    auto immutableCommon = true ? ib : ic;
    static assert(is(typeof(immutableCommon) == immutable(A)));

    // Common type for mixed const/immutable derived classes
    auto mixed1 = true ? cb : ic;
    static assert(is(typeof(mixed1) == const(A)));
    auto mixed2 = true ? ib : cc;
    static assert(is(typeof(mixed2) == const(A)));

    // Common type for mixed mutable/immutable derived classes
    auto mixed3 = true ? b : ic;
    static assert(is(typeof(mixed3) == const(A)));
    auto mixed4 = true ? ib : c;
    static assert(is(typeof(mixed4) == const(A)));

    // Array literal type deduction
    auto arr1 = [ new immutable(B), new C ];
    auto arr2 = [ new B,            new const(C) ];
    auto arr3 = [ new immutable(B), new immutable(C) ];
    static assert(is(typeof(arr1) == const(A)[]));
    static assert(is(typeof(arr2) == const(A)[]));
    static assert(is(typeof(arr3) == immutable(A)[]));
}

/***************************************************/
// 15961

struct SliceOverIndexed15961(T)
{
    enum assignableIndex = T.init;
}

struct Grapheme15961
{
    SliceOverIndexed15961!Grapheme15961 opSlice()
    {
        assert(0);
    }

    struct
    {
        ubyte* ptr_;
    }
}

/***************************************************/
// 16022

bool test16022()
{
    enum Type { Colon, Comma }
    Type type;
    return type == Type.Colon, type == Type.Comma;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=16233

enum valueConvertible(T1, T2) = blah;

struct Checked(T, Hook)
{
    bool opEquals(U)(Checked!(U, Hook) rhs)
    {
        alias R = typeof(payload + rhs.payload);
        static if (valueConvertible!(T, R))
        {
        }
        return false;
    }
}

void test16233()
{
    Checked!(Checked!(int, void), void) x1;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=16466

void test16466()
{
    static struct S
    {
        real r;
    }
    real r;
    printf("S.alignof: %x, r.alignof: %x\n", S.alignof, r.alignof);
    assert(S.alignof == r.alignof);
}

/***************************************************/

// https://issues.dlang.org/show_bug.cgi?id=16408

char[1] SDL_GetKeyName_buffer;

const(char)[] SDL_GetKeyName(char k)
{
    pragma(inline, false);
    SDL_GetKeyName_buffer[0] = k;
    return SDL_GetKeyName_buffer[];
}

void formattedWrite(const(char)[] strW, const(char)[] strA, const(char)[] strC)
{
    pragma(inline, false);

    assert(strW == "W");
    assert(strA == "A");
    assert(strC == "C");
}

void test16408()
{
    pragma(inline, false);
    formattedWrite(
        SDL_GetKeyName('W').idup,
        SDL_GetKeyName('A').idup,
        SDL_GetKeyName('C').idup
    );
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=17349

void test17349()
{
    static struct S
    {
        int bar(void delegate(ref int*)) { return 1; }
        int bar(void delegate(ref const int*)) const { return 2; }
    }

    void dg1(ref int*) { }
    void dg2(ref const int*) { }
    S s;
    int i;
    i = s.bar(&dg1);
    assert(i == 1);
    i = s.bar(&dg2);
    assert(i == 2);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=17915

void test17915()
{
    static class MyClass
    {
        S17915!MyClass m_member;
    }
}

struct S17915(T)
{
    T owner;
}

/***************************************************/

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
    test796();
    test50();
    test51();
    test52();
    test53();
    test54();
    test55();
    test56();
    test57();
    test58();

    test60();
    test61();
    test62();
    test63();
    test64();
    test65();
    test66();
    test67();
    test68();
    test69();
    test70();

    test5785();
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
    test3559();
    test84();
    test85();
    test2006();
    test8442();
    test86();
    test87();
    test2486();
    test5554();
    test88();
    test7545();
    test89();
    test90();
    test91();
    test92();
    test4536();
    test93();
    test94();
    test95();
    test5403();
    test96();
    test97();
    test98();
    test99();
    test100();
    test101();

    test103();
    test104();
    test105();
    test3927();
    test107();

    test109();

    test111();

    test113();

    test115();
    test116();
    test117();
    test3822();
    test6545();
    test118();
    test5081();

    test120();
    test10724();
    test122();
    test123();
    test124();
    test125();
    test6763();

    test127();
    test128();
    test1891();
    test129();
    test130();
    test1064();
    test131();
    test132();
    test133();
    test134();
    test135();
    test136();
    test137();
    test138();
    test1962();
    test139();
    test140();
    test141();
    test6317();
    test142();
    test143();
    test144();
    test145();
    test146();
    test147();
    test6685();
    test148();
    test149();
    test2356();
    test13652();
    test11238();
    test2540();
    test14348();
    test150();
    test151();
    test152();
    test153();
    test154();
    test155();
    test156();
    test658();
    test4258();
    test4539();
    test4963();
    test4031();
    test5437();
    test6230();
    test6264();
    test6284();
    test6295();
    test6293();
    test5046();
    test1471();
    test6335();
    test1687();
    test6228();
    test3733();
    test4392();
    test7942();
    test6220();
    test5799();
    test157();
    test6473();
    test6630();
    test6690();
    test2953();
    test2997();
    test4423();
    test4647();
    test5696();
    test6084();
    test6488();
    test6565();
    test6836();
    test6837();
    test6927();
    test6733();
    test6813();
    test6859();
    test3022();
    test6910();
    test6902();
    test6330();
    test6868();
    test2856();
    test3091();
    test6056();
    test6356();
    test7073();
    test7104();
    test7150();
    test7160();
    test7168();
    test7170();
    test7196();
    test7285();
    test14737();
    test7321();
    test3282();
    test7534();
    test7534cov();
    test7618();
    test7621();
    test11417();
    test7682();
    test7735();
    test7823();
    test7871();
    test7906();
    test7907();
    test12503();
    test8004();
    test8064();
    test8105();
    test159();
    test12824();
    test8283();
    test13182();
    test8269();
    test8395();
    test13427();
    test5749();
    test8396();
    test160();
    test8665();
    test8108();
    test8360();
    test9577();
    test6141();
    test199();
    test8526();
    test161();
    test7175();
    test8819();
    test8917();
    test8945();
    test11805();
    test14192();
    test163();
    test9428();
    test9477();
    test9538();
    test9700();
    test9834();
    test13947();
    test9883();
    test10091();
    test9130();
    test10542();
    test10539();
    test10634();
    test15080();
    test7254();
    test13468();
    test11075();
    test11181();
    test11317();
    test11888();
    test12036();
    test12153();
    test12937();
    test13154();
    test13437();
    test13472();
    test13476();
    test13720();
    test13952();
    test13985();
    test14211();
    test15141();
    test15369();
    test15638();
    test16233();
    test16466();
    test16408();
    test17349();
    test17915();

    printf("Success\n");
    return 0;
}
