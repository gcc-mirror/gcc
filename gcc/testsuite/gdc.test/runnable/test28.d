module test;

import core.vararg;
import std.stdio;
import std.string;

extern(C) int printf(const char*, ...);

/*******************************************/

struct S1
{
    void* function(void*) fn;
}

template M1()
{
    S1 s;
}

void test1()
{
    S1 s2;
    mixin M1;
    assert(s.fn == null);
}

/*******************************************/

enum Qwert { yuiop }

int asdfg(Qwert hjkl) { return 1; }
int asdfg(uint zxcvb) { return 2; }

void test2()
{
    int nm = 2;

    assert(asdfg(nm) == 2);
    assert(asdfg(cast(int) nm) == 2);
    assert(asdfg(3) == 2);
    assert(asdfg(cast(int) 3) == 2);
    assert(asdfg(3L) == 2);
    assert(asdfg(cast(int) 3L) == 2);
    assert(asdfg(3 + 2) == 2);
    assert(asdfg(cast(int) (3 + 2)) == 2);
    assert(asdfg(nm + 2) == 2);
    assert(asdfg(cast(int) (nm + 2)) == 2);
    assert(asdfg(3 + nm) == 2);
    assert(asdfg(cast(int) (3 + nm)) == 2);

}

/*******************************************/

template Qwert3(string yuiop) {
    immutable string Qwert3 = cast(string)yuiop;
}

template Asdfg3(string yuiop) {
    immutable string Asdfg3 = cast(string)Qwert3!(cast(string)(cast(string)yuiop ~ cast(string)"hjkl"));
}

void test3()
{
    string zxcvb = Asdfg3!(null);
    assert(zxcvb == "hjkl");
    assert(zxcvb == "hjkl" ~ null);
}

/*******************************************/

template Qwert4(string yuiop)
{
    immutable string Qwert4 = cast(string)(yuiop ~ "asdfg" ~ yuiop);
}

void test4()
{
    string hjkl = Qwert4!(null);
    assert(hjkl == "asdfg");
}

/*******************************************/

void test6()
{
    struct Foo
    {
        void foo() { }
    }

    alias Foo Bar;

    Bar a;
    a.foo();
}

/*******************************************/

void test7()
{
    struct Foo
    {
        alias typeof(this) ThisType;
        alias typeof(this) ThatType;
    }

    assert(is(Foo.ThisType == Foo));
    assert(is(Foo.ThatType == Foo));
}

/*******************************************/

void test8()
{
   int[] test;
   test.length = 10;
   // Show address of array start and its length (10)
   writefln("%s %s", cast(uint)test.ptr, test.length);

   test.length = 1;
   // Show address of array start and its length (1)
   writefln("%s %s", cast(uint)test.ptr, test.length);

   test.length = 8;
   // Show address of array start and its length (8)
   writefln("%s %s", cast(uint)test.ptr, test.length);

   test.length = 0;
   // Shows 0 and 0!
   writefln("%s %s", cast(uint)test.ptr, test.length);
   assert(test.length == 0);
   assert(test.ptr != null);
}

/*******************************************/

cdouble y9;

cdouble f9(cdouble x)
{
    return (y9 = x);
}

void test9()
{
    f9(1.0+2.0i);
    assert(y9 == 1.0+2.0i);
}

/*******************************************/

class CBase10
{
    this() { }
}

void foo10( CBase10 l )
{
}

void test10()
{
    if (1)
    {
        foo10( new class() CBase10
               {
                    this() { super(); }
               }
             );
    }
    return;
}

/*******************************************/

struct Foo11
{
    static
        int func(T)(T a) { assert(a == 1); return 0; }
}

void test11()
{
        auto a = Foo11.init.func(1);
        a = Foo11.init.func!(int)(1);
        a = Foo11.func(1);
        a = Foo11.func!(int)(1);
}

/*******************************************/

void test12()
{
    class ExceptioN { }

    class ExceptioX { }

    static assert(ExceptioN.mangleof[0 ..$-1] == ExceptioX.mangleof[0 .. $-1]);
}

/*******************************************/

template check( char ch1, char ch2)
{
    const bool check = ch1 == ch2;
}

void test13()
{
        const char[] s = "123+456" ;
        assert(check!( '+', s[3] ) == true);
}

/*******************************************/

void test14()
{
    static const char[] test=['a','b','c','d'];
    static assert(test==['a','b','c','d']);
    static assert(['a','b','c','d']== test);
}

/*******************************************/

void func15(...)
in {
    writefln("Arguments len = %d\n", _arguments.length);
    assert(_arguments.length == 2);
}
body {

}

void test15()
{
    func15(1, 2);
}

/*******************************************/

void test17()
{
    void delegate() y = { };
    y();
}

/*******************************************/

abstract class Pen { int foo(); }

class Penfold : Pen {
    override int foo() { return 1; }
}

class Pinky : Pen {
    override int foo() { return 2; }
}

class Printer {
    void vprint(Pen obj) {
        assert(obj.foo() == 1 || obj.foo() == 2);
    }

    C print(C)(C obj) {
        assert(obj.foo() == 1 || obj.foo() == 2);
        return obj;
    }

}

void test18()
{
    Printer p = new Printer;
    p.print(new Pinky);
    p.print(new Penfold);
    with (p)
    {
        vprint(new Pinky);
        vprint(new Penfold);

        print!(Pinky)(new Pinky);
        print!(Penfold)(new Penfold);

        p.print(new Pinky);
        p.print(new Penfold);

        print(new Pinky);
        print(new Penfold);
    }
}

/*******************************************/


class A19
{
    void s() {}
}

class B19 : A19
{
    alias A19.s s;
    static void s(int i) {}
    override void s() {}
}

class C19
{
    void f() {
        B19.s(0);
    }
}

void test19()
{
}


/*******************************************/

class U {}
class T : U {}

void test20()
{
        T*   ptr;
        T[2] sar;
        T[]  dar;

        // all of the following should work according to the "Implicit
        // Conversions" section of the spec

        tPtr(ptr);
        tPtr(sar.ptr);
        tPtr(dar.ptr);
        tDar(sar);

//      uPtr(ptr);      // T* => U*
//      uPtr(sar);      // T[2] => U*
//      uPtr(dar);      // T[] => U*
//      uSar(sar);      // T[2] => U[2]
//      uDar(sar);      // T[2] => U[]

        uDar(dar);      // T[] => const(U)[]
        vPtr(ptr);      // T* => void*
        vPtr(sar.ptr);
        vPtr(dar.ptr);

        vDar(sar);
        vDar(dar);      // works, but T[] => void[] isn't mentioned in the spec
}

void tPtr(T*t){}
void tDar(T[]t){}
void uPtr(U*u){}
void uSar(U[2]u){}
void uDar(const(U)[]u){}
void vPtr(void*v){}
void vDar(void[]v){}


/*******************************************/

struct Foo21
{
    int i;
}

template some_struct_instance(T)
{
    static Foo21 some_struct_instance =
    {
        5,
    };
}

void test21()
{
    alias some_struct_instance!(int) inst;
    assert(inst.i == 5);
}

/*******************************************/

struct Foo22(T) {}

void test22()
{
    int i;

    if ((Foo22!(char)).init == (Foo22!(char)).init)
        i = 1;
    assert(i == 1);
}


/*******************************************/

void test23()
{
    auto t=['a','b','c','d'];
    writeln(typeid(typeof(t)));
    assert(is(typeof(t) == char[]));

    const t2=['a','b','c','d','e'];
    writeln(typeid(typeof(t2)));
    assert(is(typeof(t2) == const(const(char)[])));
}

/*******************************************/

int foo24(int i ...)
{
    return i;
}

void test24()
{
    assert(foo24(3) == 3);
}

/*******************************************/

void test25()
{
    ireal x = 4.0Li;
    ireal y = 4.0Li;
    ireal z = 4Li;
    creal c = 4L + 0Li;
}

/*******************************************/

struct Foo26
{
    int a;

    static Foo26 opCall(int i)
    {   Foo26 f;
        f.a += i;
        return f;
    }
}

void test26()
{   Foo26 f;

    f = cast(Foo26)3;
    assert(f.a == 3);

    Foo26 g = 3;
    assert(g.a == 3);
}

/*******************************************/

struct S27
{
    int x;

    void opAssign(int i)
    {
        x = i + 1;
    }
}

void test27()
{
    S27 s;
    s = 1;
    assert(s.x == 2);
}

/*******************************************/

class C28
{
    int x;

    void opAssign(int i)
    {
        x = i + 1;
    }
}

void test28()
{
// No longer supported for 2.0
//    C28 s = new C28;
//    s = 1;
//    assert(s.x == 2);
}

/*******************************************/

struct S29
{
    static S29 opCall(int v)
    {
        S29 result;
        result.v = v;
        return result;
    }
    int a;
    int v;
    int x,y,z;
}

int foo29()
{
   auto s = S29(5);
   return s.v;
}

void test29()
{
    int i = foo29();
    printf("%d\n", i);
    assert(i == 5);
}

/*******************************************/

struct S30
{
    static S30 opCall(int v)
    {
        S30 result;

        void bar()
        {
            result.v += 1;
        }

        result.v = v;
        bar();
        return result;
    }
    int a;
    int v;
    int x,y,z;
}

int foo30()
{
   auto s = S30(5);
   return s.v;
}

void test30()
{
    int i = foo30();
    printf("%d\n", i);
    assert(i == 6);
}

/*******************************************/

struct S31
{
    static void abc(S31 *r)
    {
        r.v += 1;
    }

    static S31 opCall(int v)
    {
        S31 result;

        void bar()
        {
            abc(&result);
        }

        result.v = v;
        bar();
        return result;
    }
    int a;
    int v;
    int x,y,z;
}

int foo31()
{
   auto s = S31(5);
   return s.v;
}

void test31()
{
    int i = foo31();
    printf("%d\n", i);
    assert(i == 6);
}

/*******************************************/


struct T32
{
    int opApply(int delegate(ref int i) dg)
    {
        int i;
        return dg(i);
    }
}

struct S32
{
    static void abc(S32 *r)
    {
        r.v += 1;
    }

    static S32 opCall(int v)
    {
        S32 result;
        T32 t;

        result.v = v;
        foreach (i; t)
        {
            result.v += 1;
            break;
        }
        return result;
    }
    int a;
    int v;
    int x,y,z;
}

int foo32()
{
   auto s = S32(5);
   return s.v;
}

void test32()
{
    int i = foo32();
    printf("%d\n", i);
    assert(i == 6);
}

/*******************************************/

class Confectionary
{
    this(int sugar)
    {
        //if (sugar < 500)
        //    tastiness = 200;

        //for (int i = 0; i < 10; ++i)
        //    tastiness = 300;

        //int[] tastinesses_array;

        //foreach (n; tastinesses_array)
        //    tastiness = n;

        //int[int] tastinesses_aa;

        //foreach (n; tastinesses_aa)
        //    tastiness = n;

        tastiness = 1;
    }

   const int tastiness;
}

void test33()
{
}

/*******************************************/

template a34(string name, T...)
{
    string a34(string name,T t)
    {
        string localchar;
        foreach (a34; T)
        {
            writefln(`hello`);
            localchar ~= a34.mangleof;
        }
        return localchar;
    }
}

void test34()
{
    writeln(a34!("Adf"[], typeof("adf"),uint)("Adf"[],"adf",1234));
}

/*******************************************/

template a35(string name, T...)
{
    int a35(M...)(M m)
    {
        return 3;
    }
}

void test35()
{
    assert(a35!("adf")() == 3);
}

/*******************************************/

template a36(AnotherT,string name,T...){
    AnotherT a36(M...)(M){
        AnotherT localchar;
        foreach(a;T)
        {
            writefln(`hello`);
            localchar~=a.mangleof;
        }
        return cast(AnotherT)localchar;
    }
}

void test36()
{
    string b="adf";
    uint i=123;
    char[3] c="Adf";
    writeln(a36!(typeof(b),"Adf")());
}

/*******************************************/

struct Q37 {
    Y37 opCast() {
        return Y37.init;
    }
}

struct Y37 {
    Q37 asdfg() {
        return Q37.init;
    }

    void hjkl() {
        Q37 zxcvb = asdfg();  // line 13
    }
}

void test37()
{
}

/*******************************************/



class C38 { }

const(Object)[] foo38(C38[3] c) @system
{   const(Object)[] x = c;
    return x;
}

void test38()
{
}

/*******************************************/

void test39()
{
    void print(string[] strs)
    {
        writeln(strs);
        assert(format("%s", strs) == `["Matt", "Andrew"]`);
    }

    print(["Matt", "Andrew"]);
}

/*******************************************/

void test40()
{
    class C
    {
        Object propName()
        {
            return this;
        }
    }

    auto c = new C;

    with (c.propName)
    {
        writeln(toString());
    }

    auto foo = c.propName;
}

/*******************************************/

void test41()
{
    auto test = new char [2];

    int x1, x2, x3;
    char[] foo1() { x1++; return test; }
    int foo2() { x2++; return 0; }
    int foo3() { x3++; return 1; }

    test [] = 'a';
    test = test [0 .. 1];

    foo1() [foo2() .. foo3()] = 'b';
    assert(x1 == 1);
    assert(x2 == 1);
    assert(x3 == 1);

    //test [0 .. 2] = 'b'; // this line should assert
    writef ("%s\n", test.ptr [0 .. 2]);
}

/*******************************************/

void test42()
{
    struct X { int x; }

    X x;
    assert(x.x == 0);
    x = x.init;
    assert(x.x == 0);
}

/*******************************************/

struct A43
{
    static const MY_CONST_STRING = "hello";

    void foo()
    {
        // This will either print garbage or throw a UTF exception.
        // But if never_called() is commented out, then it will work.
        writefln("%s", MY_CONST_STRING);
    }
}

void never_called43()
{
    // This can be anything; there just needs to be a reference to
    // A43.MY_CONST_STRING somewhere.
    writefln("%s", A43.MY_CONST_STRING);
}

void test43()
{
    A43 a;
    a.foo();
}

/*******************************************/

class A44
{
    static const MY_CONST_STRING = "hello";

    this()
    {
        // This will either print garbage or throw a UTF exception.
        // But if never_called() is commented out, then it will work.
        writefln("%s", MY_CONST_STRING);
    }
}

void never_called44()
{
    // This can be anything; there just needs to be a reference to
    // A44.MY_CONST_STRING somewhere.
    writefln("%s", A44.MY_CONST_STRING);
}

void test44()
{
    A44 a = new A44();
}

/*******************************************/

class C45
{
    void func(lazy size_t x)
    {
        (new C45).func(super.toHash());
    }
}

void test45()
{
}

/*******************************************/

template T46(double v)
{
    double T46 = v;
}

void test46()
{
    double g = T46!(double.nan) + T46!(-double.nan);
}

/*******************************************/

void test47()
{
    uint* where = (new uint[](5)).ptr;

    where[0 .. 5] = 1;
    assert(where[2] == 1);
    where[0 .. 0] = 0;
    assert(where[0] == 1);
    assert(where[2] == 1);
}

/*******************************************/

void test48()
{
    Object o = new Object();
    printf("%.*s\n", typeof(o).classinfo.name.length, typeof(o).classinfo.name.ptr);
    printf("%.*s\n", (typeof(o)).classinfo.name.length, (typeof(o)).classinfo.name.ptr);
    printf("%.*s\n", (Object).classinfo.name.length, (Object).classinfo.name.ptr);
}

/*******************************************/

void test49()
{
    foo49();
}

void foo49()
{
    char[] bar;
    assert(true, bar ~ "foo");
}

/*******************************************/

void test50()
{
    foo50("foo");
}

void foo50(string bar)
{
    assert(true, bar ~ "foo");
}

/*******************************************/

struct Foo51
{
    static Foo51 opCall()
    {
      return Foo51.init;
    }
    private char[] _a;
    private bool _b;
}

void test51()
{
}

/*******************************************/

template A52(T ...) { }
mixin A52!(["abc2", "def"]);

void test52()
{
}

/*******************************************/

enum: int
{
        AF_INET53 =       2,
        PF_INET53 =       AF_INET53,
}

enum: int
{
        SOCK_STREAM53 =     1,
}

struct sockaddr_in53
{
        int sin_family = AF_INET53;
}

enum AddressFamily53: int
{
        INET =       AF_INET53,
}

enum SocketType53: int
{
        STREAM =     SOCK_STREAM53,
}


class Socket53
{
        this(AddressFamily53 af, SocketType53 type)
        {
        }
}

void test53()
{
    new Socket53(AddressFamily53.INET, SocketType53.STREAM);
}

/*******************************************/

void test54()
{
    int[2][] a;
    a ~= [1,2];
    assert(a.length == 1);
    assert(a[0][0] == 1);
    assert(a[0][1] == 2);
}

/*******************************************/

void test55()
{
        float[][] a = new float [][](1, 1);

        if((a.length != 1) || (a[0].length != 1)){
                assert(0);
        }
        if (a[0][0] == a[0][0]){
                assert(0);
        }
}

/*******************************************/

void test58()
{
    struct S
    {
        int i;
        int[4] bar = 4;
        float[4] abc;
    }

    static S a = {i: 1};
    static S b;

    writefln("a.bar: %s, %s", a.bar, a.abc);
    assert(a.i == 1);
    assert(a.bar[0] == 4);
    assert(a.bar[1] == 4);
    assert(a.bar[2] == 4);
    assert(a.bar[3] == 4);
    writefln("b.bar: %s, %s", b.bar, b.abc);
    assert(b.i == 0);
    assert(b.bar[0] == 4);
    assert(b.bar[1] == 4);
    assert(b.bar[2] == 4);
    assert(b.bar[3] == 4);
}


/*******************************************/

void bug59(string s)()
{
  writeln(s);
  writeln(s.length);
}

void test59()
{
    bug59!("1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234")();
}

/*******************************************/

class Foo60(T)
{
    this() { unknown_identifier; }
}

void test60()
{
    bool foobar = is( Foo60!(int) );
    assert(!foobar);
}


/*******************************************/

void repeat( int n, void delegate() dg )
{
    printf("n = %d\n", n);
    if( n&1 ) dg();
    if( n/2 ) repeat( n/2, {dg();dg();} );
}

void test61()
{
    repeat( 10, {printf("Hello\n");} );
}

/*******************************************/

void test62()
{
    Vector62 a;
    a.set(1,2,24);
    a = a * 2;
    writeln(a.x, a.y, a.z);
    assert(a.x == 2);
    assert(a.y == 4);
    assert(a.z == 48);
}


struct Vector62
{
    float x,y,z;

    // constructor
    void set(float _x, float _y, float _z)
    {
      x = _x;
      y = _y;
      z = _z;
    }

    Vector62 opMul(float s)
    {
      Vector62 ret;
      ret.x = x*s;
      ret.y = y*s;
      ret.z = z*s;
      return ret;
    }
}

/*******************************************/

struct Data63
{
    int x, y;

    /// To make size > 8 so NRVO is used.
    /// Program runs correctly with this line commented out:
    byte filler;
}

Data63 frob(ref Data63 d)
{
    Data63 ret;
    ret.y = d.x - d.y;
    ret.x = d.x + d.y;
    return ret;
}

void test63()
{
    Data63 d; d.x = 1; d.y = 2;
    d = frob(d);
    writeln(d.x);
    writeln(d.y);
    assert(d.x == 3 && d.y == -1);
}

/*******************************************/

class Foo64
{
    this() { writefln("Foo64 created"); }
    ~this() { writefln("Foo64 destroyed"); }
}

template Mix64()
{
    void init() {
        ptr = new Foo64;
    }
    Foo64 ptr;
}

class Container64
{
    this() { init(); }
    mixin Mix64;
}


void test64()
{
    auto x = new Container64;

    assert(!(x.classinfo.flags & 2));
}

/*******************************************/

struct Vector65(T, uint dim)
{
  T[dim]  data;
}

T dot65(T, uint dim)(Vector65!(T,dim) a, Vector65!(T,dim) b)
{
  T tmp;
  for ( int i = 0; i < dim; ++i )
    tmp += a.data[i] * b.data[i];
  return tmp;
}

void test65()
{
  Vector65!(double,3u) a,b;
  auto t = dot65(a,b);
}


/*******************************************/

void main()
{
    printf("Start\n");

    test1();
    test2();
    test3();
    test4();
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
    test58();
    test59();
    test60();
    test61();
    test62();
    test63();
    test64();
    test65();

    printf("Success\n");
}

