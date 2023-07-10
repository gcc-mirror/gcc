alias TypeTuple(T...) = T;

class A { }
class B : A { }
class C : B { }

/***************************************/

template Foo(int a, int b, int c)
{
    const int Foo = 1;
}

template Foo(A...)
{
    const int Foo = 2;
}

void test1()
{
    int y = Foo!(1,2,3);
    assert(y == 1);

    y = Foo!(1,2);
    assert(y == 2);

    y = Foo!(1,2,3,4);
    assert(y == 2);
}

/***************************************/

template Foo2(int a, int b, int c)
{
    const int Foo2 = 1;
}

template Foo2(int a, int b, int c, A...)
{
    const int Foo2 = 2;
}

void test2()
{
    int y = Foo2!(1,2,3);
    assert(y == 1);

    y = Foo2!(1,2,3,4);
    assert(y == 2);
}

/***************************************/

void bar3(int x, int y)
{
    assert(x == 2);
    assert(y == 3);
}

template Foo3(T, A...)
{
    int Foo3(T t, A a)
    {
        assert(A.length == 2);
        assert(a.length == 2);
        bar3(a);
        assert([a] == [2, 3]);
        assert([cast(double)a] == [2.0, 3.0]);
        assert(a[0] == 2);
        assert(a[1] == 3);
        assert(a[$ - 2] == 2);
        assert(a[$ - 1] == 3);
        static if (1 || a[6])
            assert(1);
        assert([a[]] == [2, 3]);
        assert([a[0 .. $]] == [2, 3]);
        assert([a[0 .. $ - 1]] == [2]);
        return 3;
    }
}

void test3()
{
    int y = Foo3(1,2,3);
    assert(y == 3);
}

/***************************************/


void foo4(A...)()
{
    int[] ai;
    int[] aa;

    aa = null;
    foreach (a; A)
    {
        aa ~= a;
    }
    assert(aa == [7,4,9]);

    aa = null;
    foreach (int a; A)
    {
        aa ~= a;
    }
    assert(aa == [7,4,9]);

    ai = null;
    aa = null;
    foreach (int i, a; A)
    {
        ai ~= i;
        aa ~= a;
    }
    assert(ai == [0,1,2]);
    assert(aa == [7,4,9]);

    ai = null;
    aa = null;
    foreach_reverse (uint i, a; A)
    {
        ai ~= i;
        aa ~= a;
    }
    assert(ai == [2,1,0]);
    assert(aa == [9,4,7]);

    ai = null;
    aa = null;
    foreach_reverse (i, a; A)
    {
        ai ~= i;
        aa ~= a;
    }
    assert(ai == [2,1,0]);
    assert(aa == [9,4,7]);

    ai = null;
    aa = null;
    foreach (int i, a; A)
    {
        ai ~= i;
        aa ~= a;
        if (i == 1)
            break;
        continue;
    }
    assert(ai == [0,1]);
    assert(aa == [7,4]);
}

void test4()
{
    foo4!(7,4,9)();
}

/***************************************/

int a12(TypeTuple!(int, int) t)
{
    return t[0] + t[1];
}

int b12(TypeTuple!(TypeTuple!(int), TypeTuple!(int)) t)
{
    return t[0] + t[1];
}

int c12(TypeTuple!(TypeTuple!(int), TypeTuple!(TypeTuple!(), int), TypeTuple!()) t)
{
    return t[0] + t[1];
}

void test12()
{
    assert(a12(1, 2) == 3);
    assert(b12(1, 2) == 3);
    assert(c12(1, 2) == 3);
}

/***************************************/


int plus13(TypeTuple!(int, long, float)[0 .. 2] t)
{
    typeof(t)[0] e;
    assert(typeid(typeof(e)) == typeid(int));
    typeof(t)[1] f;
    assert(typeid(typeof(f)) == typeid(long));
    return t[0] + cast(int)t[1];
}

void test13()
{
    assert(plus13(5, 6) == 11);
}

/***************************************/

int plus14(TypeTuple!(int, long, float)[0 .. $ - 1] t)
{
    typeof(t)[$ - 2] e;
    assert(typeid(typeof(e)) == typeid(int));
    typeof(t)[1] f;
    assert(typeid(typeof(f)) == typeid(long));
    return t[0] + cast(int)t[1];
}

void test14()
{
    assert(plus14(5, 6) == 11);
}

/***************************************/

void returnAndArgs(T, U...) (T delegate(U) dg)
{
    static if (U.length == 0)
        assert(dg() == 0);
    else static if (U.length == 1)
        assert(dg(false) == 1);
    else
        assert(dg(false, 63L) == 2);
}

void test24()
{
    returnAndArgs(delegate int(){ return 0; });
    returnAndArgs(delegate int(bool b){ return 1; });
    returnAndArgs(delegate int(bool b, long c){ return 2; });
}

/***************************************/

void test28()
{
    alias TypeTuple!(int, long, double) TL;

    foreach (int i, T; TL)
    {
        switch (i)
        {
            case 0: assert(is(T == int));    break;
            case 1: assert(is(T == long));   break;
            case 2: assert(is(T == double)); break;
            default:assert(0);
        }
    }
}

/***************************************/

template g32(alias B)
{
    int g32 = 2;
}

int f32(A...)(A a)
{
    return g32!(a);
}

void test32()
{
    assert(f32(4) == 2);
}

/***************************************/

struct S34
{
    int x;
    long y;
    double z;
}

void foo34(int x, long y, double z)
{
    assert(x == 3);
    assert(y == 8);
    assert(z == 6.8);
}

void test34()
{
    S34 s;

    s.x = 3;
    s.y = 8;
    s.z = 6.8;
    foo34(s.tupleof);
}

/***************************************/

alias TypeTuple!(int, long, double) TL35;

struct S35
{
    TL35 tl;
}

void foo35(int x, long y, double z)
{
    assert(x == 3);
    assert(y == 8);
    assert(z == 6.8);
}

void test35()
{
    S35 s;

    s.tl[0] = 3;
    s.tl[1] = 8;
    s.tl[2] = 6.8;
    foo35(s.tupleof);
    foo35(s.tl);
}

/***************************************/

alias TypeTuple!(int, long, double) TL36;

class C36
{
    TL36 tl;
}

void foo36(int x, long y, double z)
{
    assert(x == 3);
    assert(y == 8);
    assert(z == 6.8);
}

void test36()
{
    C36 s = new C36;

    s.tl[0] = 3;
    s.tl[1] = 8;
    s.tl[2] = 6.8;
    foo36(s.tupleof);
    foo36(s.tl);
}

/***************************************/


alias TypeTuple!(int, long, double) TL37;

class C37
{
    TL37 tl;
}

void foo37(int x, long y, double z)
{
    assert(x == 3);
    assert(y == 8);
    assert(z == 6.8);
}

void test37()
{
    C37 s = new C37;

    s.tl[0] = 3;
    s.tl[1] = 8;
    s.tl[2] = 6.8;
    foo37(s.tupleof);

    TL37 x;
    assert(x[0] == 0);
    x[0] = 3;
    assert(x[0] == 3);
    assert(x[1] == 0);
    x[1] = 8;
    x[2] = 6.8;
    foo37(x);
}

/***************************************/

interface I38A { }
interface I38B { }

alias TypeTuple!(I38A, I38B) IL38;

class C38 : IL38
{
}

void test38()
{
    auto c = new C38;
}

/***************************************/

void test39()
{
    static const string a = "\x01";
    static const char b = a[0];
    static const string c = "test";
    static assert(c[a[0]] == 'e');

    alias TypeTuple!(ulong,uint,ushort,ubyte) tuple;
    static assert(is(tuple[1] == uint));
    static assert(is(tuple[a[0]] == uint));
}

/***************************************/

struct Foo45
{
    static TypeTuple!(int) selements1;
    TypeTuple!(int) elements1;
    static TypeTuple!() selements0;
    TypeTuple!() elements0;
}

void test45()
{
    Foo45 foo;

    static assert(Foo45.selements1.length == 1);
    static assert(Foo45.elements1.length == 1);
    static assert(Foo45.selements0.length == 0);
    static assert(Foo45.elements0.length == 0);

    static assert(foo.selements1.length == 1);
    static assert(foo.elements1.length == 1);
    static assert(foo.selements0.length == 0);
    static assert(foo.elements0.length == 0);
}

/***************************************/

template Tuple46(E ...) { alias E Tuple46; }

alias Tuple46!(float, float, 3) TP46;
alias TP46[1..$] TQ46;

void test46()
{
    TQ46[0] f = TQ46[1];
    assert(is(typeof(f) == float));
    assert(f == 3);
}

/***************************************/

template Foo47(T, Args...)
{
    void bar(Args args, T t)
    {
    }
}

void test47()
{
    alias Foo47!(int) aFoo;
}

/***************************************/

template Tuple48(E...)
{
    alias E Tuple48;
}

void VarArg48(T...)(T args)
{
}

void test48()
{
    VarArg48( );
    VarArg48( Tuple48!(1,2,3) );
    VarArg48( Tuple48!()      );
}

/***************************************/

alias TypeTuple!(int, long) TX49;

void foo49(TX49 t)
{
    TX49 s;
    s = t;
    assert(s[0] == 1);
    assert(s[1] == 2);
}

void test49()
{
    foo49(1, 2);
}

/***************************************/

void foo51(U...)(int t, U u)
{
    assert(t == 1);
    assert(u[0] == 2);
    assert(u[1] == 3);
}

void bar51(U...)(U u, int t)
{
    assert(u[0] == 1);
    assert(u[1] == 2);
    assert(t == 3);
}

void abc51(U...)(int s, U u, int t)
{
    assert(s == 1);
    assert(u[0] == 2);
    assert(u[1] == 3);
    assert(t == 4);
}

void test51()
{
  foo51(1, 2, 3);
  bar51(1, 2, 3);
  bar51!(int, int)(1, 2, 3);
  abc51(1,2,3,4);
}

/***************************************/

string to55(U, V)(V s) { return "he"; }

private S wyda(S, T...)(T args)
{
    S result;
    foreach (i, arg; args)
    {
        result ~= to55!(S)(args[i]);
    }
    return result;
}

string giba(U...)(U args)
{
    return wyda!(string, U)(args);
}

void test55()
{
    assert(giba(42, ' ', 1.5, ": xyz") == "hehehehe");
}

/***************************************/

private template implicitlyConverts(U, V)
{
    enum bool implicitlyConverts = V.sizeof >= U.sizeof
        && is(typeof({U s; V t = s;}()));
}

T to56(T, S)(S s)
    if (!implicitlyConverts!(S, T) /*&& isSomeString!(T)
        && isSomeString!(S)*/)
{
    return T.init;
}

void test56()
{
    auto x = to56!(int)("4");
    assert(x == 0);
    assert(!implicitlyConverts!(const(char)[], string));
    assert(implicitlyConverts!(string, const(char)[]));
}

/***************************************/

struct A57(B...) {}

void test57()
{
    alias A57!(int, float) X;
    static if (!is(X Y == A57!(Z), Z...))
    {
        static assert(false);
    }
}

/***************************************/

struct A58(B...) {}

void test58()
{
    alias A58!(int, float) X;
    static if (!is(X Y == A58!(Z), Z...))
    {
        static assert(false);
    }
}

/***************************************/

struct Tuple59(T...)
{
    T field;
}

template reduce(fun...)
{
    alias Reduce!(fun).reduce reduce;
}

template Reduce(fun...)
{
    Tuple59!(double, double)
    reduce(Range)(Range r)
    {
        typeof(Tuple59!(double,double).field)[0] y;
        typeof(typeof(return).field)[0] x;
        Tuple59!(double, double) s;
        return s;
    }
}

void test59()
{
    double[] a = [ 3.0, 4, 7, 11, 3, 2, 5 ];
    static double sum(double a, double b) {return a + b;}
    auto r = reduce!((a, b) { return a + b; },
                     (a, b) { return a + b; })(a);
}

/***************************************/

template tuple60(T...)
{
    alias T tuple60;
}

template Foo60(S : void delegate(tuple60!(int))) {}
template Foo60(S : void delegate(tuple60!(int, int))) {}

alias Foo60!(void delegate(int)) Bar60;

void test60()
{
}

/***************************************/

template TypeTuple61(TList...){
    alias TList TypeTuple61;
}
template List61(lst...) { alias lst list; }
alias TypeTuple61!(List61!(void)) A61;
alias TypeTuple61!(A61[0].list) B61;

void test61()
{
}

/***************************************/

template Tuple63(T...){
    alias T Tuple63;
}
// https://issues.dlang.org/show_bug.cgi?id=3336
static assert(!is(int[ Tuple63!(int, int) ]));

void test63()
{
}

/***************************************/

template Tuple1411(T ...) { alias T Tuple1411; }

void test1411()
{
    int delegate(ref Tuple1411!(int, char[], real)) dg; // (*)
    int f(ref int a, ref char[] b, ref real c) { return 77; }
    dg = &f;
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=4444

void test4444()
{
    alias TypeTuple!(1) index;
    auto arr = new int[4];
    auto x = arr[index];    // error
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=13864

struct Tuple13864(T...)
{
    T expand;
    alias expand this;
}
auto tuple13864(T...)(T args)
{
    return Tuple13864!T(args);
}

void test13864()
{
    int[] x = [2,3,4];
    auto y = x[tuple13864(0).expand];
    assert(y == 2);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=4884

struct A4884(T...)
{
    void foo(T) {}
    void bar(bool, T) {}
}

void test4884()
{
    auto a1 = A4884!(int)();
    auto a2 = A4884!(int, long)();
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=4920

struct Test4920(parameters_...)
{
    alias parameters_ parameters;
}

void test4920()
{
    Test4920!(10, 20, 30) test;
    static assert(typeof(test).parameters[1] == 20); // okay
    static assert(       test .parameters[1] == 20); // (7)
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=4940

template Tuple4940(T...)
{
    alias T Tuple4940;
}

struct S4940
{
    Tuple4940!(int, int) x;
    this(int) { }
}

void test4940()
{
    auto w = S4940(0).x;
}

//----

struct S4940add
{
    string s;
    long x;
}

ref S4940add get4940add(return ref S4940add s){ return s; }

void test4940add()
{
    S4940add s;
    get4940add(s).tupleof[1] = 20;
    assert(s.x == 20);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=6530

struct S6530
{
    int a, b, c;
}

struct HasPostblit6530
{
    this(this) {}  // Bug goes away without this.
}

auto toRandomAccessTuple6530(T...)(T input, HasPostblit6530 hasPostblit)
{
    return S6530(1, 2, 3);
}

void doStuff6530(T...)(T args)
{
    HasPostblit6530 hasPostblit;

    // Bug goes away without the .tupleof.
    auto foo = toRandomAccessTuple6530(args, hasPostblit).tupleof;
}

void test6530()
{
    doStuff6530(1, 2, 3);
}

/***************************************/

import core.stdc.stdarg;

extern(C)
void func9495(int a, string format, ...)
{
    va_list ap;
    va_start(ap, format);
    auto a1 = va_arg!int(ap);
    auto a2 = va_arg!int(ap);
    auto a3 = va_arg!int(ap);
    assert(a1 == 0x11111111);
    assert(a2 == 0x22222222);
    assert(a3 == 0x33333333);
    va_end(ap);
}

void test9495()
{
    func9495(0, "", 0x11111111, 0x22222222, 0x33333333);
}

/***************************************/

void copya(int a, string format, ...)
{
    va_list ap;
    va_start(ap, format);

    va_list ap2;
    va_copy(ap2, ap);

    auto a1 = va_arg!int(ap);
    auto a2 = va_arg!int(ap);
    auto a3 = va_arg!int(ap);

    assert(a1 == 0x11111111);
    assert(a2 == 0x22222222);
    assert(a3 == 0x33333333);

    auto b1 = va_arg!int(ap2);
    auto b2 = va_arg!int(ap2);
    auto b3 = va_arg!int(ap2);

    assert(b1 == 0x11111111);
    assert(b2 == 0x22222222);
    assert(b3 == 0x33333333);

    va_end(ap);
    va_end(ap2);
}

void testCopy()
{
    copya(0, "", 0x11111111, 0x22222222, 0x33333333);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=6700

template bug6700(TList ...) {
    const int bug6700 = 2;
}
TypeTuple!(int, long) TT6700;

static assert(bug6700!( (TT6700[1..$]) )==2);

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=6966

template X6966(T...)
{
    alias const(T[0]) X6966;
}
static assert(is(X6966!(int) == const(int)));
static assert(is(X6966!(int, 0) == const(int)));

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=7233

struct Foo7233 { int x, y; }
Foo7233[] front7233(Foo7233[][] a)
{
    return a[0];
}

class Bar7233 { int x, y; }
Bar7233[] front7233(Bar7233[][] a)
{
    return a[0];
}

void test7233()
{
    Foo7233[][] b1 = [[Foo7233()]];
    auto xy1 = b1.front7233[0].tupleof;

    Bar7233[][] b2 = [[new Bar7233()]];
    auto xy2 = b2.front7233[0].tupleof;
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=7263

template TypeTuple7263(T...){ alias T TypeTuple7263; }

struct tuple7263
{
    TypeTuple7263!(int, int) field;
    alias field this;
}

auto front7263(T)(ref T arr){ return arr[0]; }

void test7263()
{
    auto bars = [tuple7263(0, 0), tuple7263(1, 1)];
    auto spam1 = bars.front7263[1];
    auto spam2 = bars.front7263[1..2];
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=8244

TypeTuple!(int,int)[] x8244;
static assert(is(typeof(x8244) == TypeTuple!(int, int)));

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=9017

template X9017(Args...)
{
    static if(__traits(compiles, { enum e = Args; }))
        enum e = Args;
}
alias X9017!0 x9017;
static assert(x9017.e[0] == 0);

void test9017()
{
    enum tup1 = TypeTuple!(11, 22);
    enum tup2 = TypeTuple!("one", "two");
    static assert(tup1 == TypeTuple!(11, 22));
    static assert(tup2 == TypeTuple!("one", "two"));
    static assert(tup1[0] == 11 && tup1[1] == 22);
    static assert(tup2[0] == "one" && tup2[1] == "two");

    shared const tup3 = TypeTuple!(10, 3.14);
    immutable    tup4 = TypeTuple!("a", [1,2]);
    static assert(is(typeof(tup3[0]) == shared const int));
    static assert(is(typeof(tup3[1]) == shared const double));
    static assert(is(typeof(tup4[0]) == immutable string));
    static assert(is(typeof(tup4[1]) == immutable int[]));
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=10279

void foo10279(int[][] strs...) @trusted { }
void bar10279() @safe { foo10279(); }

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=13508

struct S13508
{
    this(T)(T[] t...) {}
}

template make13508(T)
{
    T make13508(Args...)(Args args)
    {
        return T(args);
    }
}

void test13508() @safe @nogc
{
    S13508 s = make13508!S13508(5);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=14395

int v2u14395(uint[1] ar...)
{
    return ar[0];
}

void print14395(int size = v2u14395(7))
{
    assert(size == 7);
}

void test14395()
{
    print14395();
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=10414

void foo10414(void delegate()[] ...) { }

void bar10414() { }

void test10414()
{
    foo10414
    (
        { bar10414(); },
        { bar10414(); },
    );
}

/***************************************/

import core.stdc.stdarg;

struct S14179
{
    const(char)* filename;
    uint linnum;
    uint charnum;
}

extern(C++) const(char)* func14179(S14179 x, const(char)* string, ...)
{
    return string;
}

void test14179()
{
    const(char)* s = "hello";
    assert(func14179(S14179(), s) == s);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=10722

struct S10722
{
    int x;
}

template GetSomething10722(S...)
{
    alias GetSomething = int;
}

void test10722()
{
    alias X10722 = GetSomething10722!(S10722.tupleof[0]);
}

/***************************************/

void testx15417(ulong c1, ...)
{
    check(c1, _argptr, _arguments);
}

class C15417
{
    private void method ()
    {
        void test1 (ulong c1, ...)
        {
            check(c1, _argptr, _arguments);
        }

        void test2 (ulong c1, ...)
        {
            va_list ap;
            va_start(ap, c1);

            check(c1, ap, _arguments);
        }

        testx15417(4242UL, char.init);
        test1(4242UL, char.init);
        test2(4242UL, char.init);
    }
}

void check (ulong c1, va_list arglist, TypeInfo[] ti)
{
    assert(ti.length == 1);
    assert(ti[0].toString() == "char");
    assert(char.init == va_arg!(char)(arglist));
}

void test15417()
{
    auto c = new C15417;
    c.method;
}


/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=21425

import core.stdc.stdarg;
import core.stdc.stdio;

extern(C) void f5(int dummy, ...)
{
    va_list ap;

    va_start(ap, dummy);
    int x = va_arg!int(ap);
    assert(x == 5);
    va_end(ap);

    va_start(ap, dummy);
    int y = va_arg!int(ap);
    assert(y == 5);
    va_end(ap);
}

void test21425()
{
    f5(0, 5);
}

/*********************************************/
// https://issues.dlang.org/show_bug.cgi?id=23409

import core.stdc.string;

void printf10(const(char)* fmt, ...){
    char[30] s;
    for(int i = 0; i < 10; i++){
        va_list args;
        va_start(args, fmt);
        vsprintf(s.ptr, fmt, args);
        va_end(args);
        assert(strcmp(s.ptr, "Hello world\n") == 0);
    }
}

void test23409()
{
    printf10("Hello %s\n".ptr, "world".ptr);
}

/***************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test12();
    test13();
    test14();
    test24();
    test28();
    test32();
    test34();
    test35();
    test36();
    test37();
    test38();
    test39();
    test45();
    test46();
    test47();
    test48();
    test49();
    test51();
    test55();
    test56();
    test57();
    test58();
    test59();
    test60();
    test61();
    test63();
    test1411();
    test4444();
    test13864();
    test4884();
    test4920();
    test4940();
    test4940add();
    test6530();
    test7233();
    test7263();
    test9017();
    test14395();
    test10414();
    test9495();
    testCopy();
    test14179();
    test15417();
    test21425();
    test23409();

    return 0;
}
