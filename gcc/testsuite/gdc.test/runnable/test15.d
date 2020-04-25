// RUNNABLE_PHOBOS_TEST
// REQUIRED_ARGS:
// EXTRA_FILES: extra-files/test15.txt

import std.array;
import core.stdc.math : cos, fabs, sin, sqrt;
import core.vararg;
import std.math: rndtol, rint;
import std.string;
import std.stdio : File;

extern (C)
{
    int printf(const char*, ...);
}

struct A { int x; }
struct B { int x = 22; }

void test5()
{
    A* a = new A;
    assert(a.x == 0);
    B* b = new B;
    assert(b.x == 22);
}

/************************************/

void test6()
{
    assert('\x12'.sizeof == 1);
    assert('\u1234'.sizeof == 2);
    assert('\U00105678'.sizeof == 4);

    assert('\x12' == 0x12);
    assert('\u1234' == 0x1234);
    assert('\U00105678' == 0x105678);

    assert("abc\\def" == r"abc\def");
}


/************************************/

void test7()
{
    string s = `hello"there'you`;
    printf("s = '%.*s'\n", s.length, s.ptr);
    assert(s == "hello\"there'you");
    ubyte[] b = cast(ubyte[])x"8B 7D f4 0d";
    for (int i = 0; i < b.length; i++)
        printf("b[%d] = x%02x\n", i, b[i]);
    assert(b.length == 4);
    assert(b[0] == 0x8B);
    assert(b[1] == 0x7D);
    assert(b[2] == 0xF4);
    assert(b[3] == 0x0D);
}


/************************************/


void foo8(out bool b)
{
    b = true;
}


void test8()
{
   bool b;
   bool *pb = &b;

   assert(b == false);
   *pb = true;
   assert(b == true);
   *pb = false;
   assert(b == false);
   foo8(b);
   assert(b == true);
}


/************************************/

struct Pair
{
   int a;
   int b;

   Pair abs ()
   {
       return this;
   }

   Pair opDiv(Pair other)
   {
       Pair result;

        result.a = a + other.a;
        result.b = b + other.b;
       return result;
   }
}

void test9()
{
   Pair t;

   t.a = 5;
   t.b = 23;
   t = t.abs () / t;
   printf("a = %d, b = %d\n", t.a, t.b);
   assert(t.a == 10);
   assert(t.b == 46);
}


/************************************/

void test10()
{
    int b = 0b_1_1__1_0_0_0_1_0_1_0_1_0_;

    printf("b = %d\n", b);
    assert(b == 3626);

    b = 1_2_3_4_;
    printf("b = %d\n", b);
    assert(b == 1234);

    b = 0x_1_2_3_4_;
    printf("b = %d\n", b);
    assert(b == 4660);

    b = 0_;
    printf("b = %d\n", b);
    assert(b == 0);

    double d = 0_._2_3_4_;
    printf("d = %g\n", d);
    assert(d == 0.234);

    d = 0_._2_3_4_e+3_5_;
    printf("d = %g\n", d);
    assert(d == 0.234e+35);

    d = 0_._2_3_4_e3_5_;
    printf("d = %g\n", d);
    assert(d == 0.234e+35);
}

/************************************/

class CA14 { }
class CB14 : CA14 { }

class A14 {
    int show( CA14 a ) { printf("A14::show( CA14 )\n"); return 1; }
    int boat( CA14 a ) { printf("A14::boat( CA14 )\n"); return 1; }
}

class B14 : A14 {
    int show( CA14 a, CB14 b ) { printf("B14::show(CA14, CB14)\n"); return 2; }
    int boat( CA14 a, CB14 b ) { printf("B14::boat(CA14, CB14)\n"); return 2; }
}

class C14 : B14 {
    override int show( CA14 a ) { printf("C::show( CA14 )\n"); return 3; }
    alias B14.show show;

    alias B14.boat boat;
    override int boat( CA14 a ) { printf("C::boat( CA14 )\n"); return 3; }
}

class D14 : C14  {
}


void test14()
{
    D14 b = new D14();
    int i;

    i = b.show( new CA14(), new CB14() );
    assert(i == 2);
    i = b.show( new CA14() );
    assert(i == 3);

    i = b.boat( new CA14(), new CB14() );
    assert(i == 2);
    i = b.boat( new CA14() );
    assert(i == 3);
}


/************************************/

class A15
{
    void foo()
    {
        List1.rehash;
        List2.rehash;
    }
  private:
    int delegate(in int arg1) List1[char[]];
    int  List2[char []];
}

void test15()
{
}


/************************************/

void test16()
{
    char[] a=new char[0];
    uint c = 200000;
    while (c--)
        a ~= 'x';
    //printf("a = '%.*s'\n", a.length, a.ptr);
}


/************************************/

class A17 { }
class B17 : A17 { }

void foo17(const(A17)[] a) { }

void test17()
{
    B17[] b;
    foo17(b);
}


/************************************/

void test18()
{
    uint a;
    real b=4;
    a=cast(uint)b;
}

/************************************/

abstract class Foo19
{
    int bar() { return 1; }
}

void test19()
{
    Foo19 f;
}

/************************************/

int foo20(string s,char d) {  return 1; }
int foo20(string s,double d) {  return 2; }
int foo20(string s,cdouble d) {  return 3; }

void test20()
{
    int i;
    double x;
    i = foo20("test=",x);
    assert(i == 2);
}

/************************************/

void test21()
{
    int[string] esdom;
    auto f = File("runnable/extra-files/test15.txt", "r");

    foreach(it; f.byLine())
        esdom[it.idup] = 0;

    esdom.rehash;
}

/************************************/

int foo22(char* p) { return 1; }
int foo22(char[] s) { return 2; }

void test22()
{
    int i;

    i = foo22(cast(char*)"abc");
    assert(i == 1);
    i = foo22(cast(char[])"abc");
    assert(i == 2);
}


/************************************/

void test23()
{
    uint v;
    int b;
    long l;
    real e;

    e = cos(e);
    e = fabs(e);
    e = rint(e);
    l = rndtol(e);
    e = sin(e);
    e = sqrt(e);
}


/************************************/

abstract class dtortest24
{
  this() {}
  ~this() {}
}

void test24()
{
}


/************************************/

abstract class bar25 {

  this() {}

  void foo() {}

}

class subbar25 : bar25 {

  this() {}

}

void test25()
{
   new subbar25();
}


/************************************/

void test26()
{
    string[] instructions = std.array.split("a;b;c", ";");

    foreach(ref string instr; instructions)
    {
        std.string.strip(instr);
    }

    foreach(string instr; instructions)
    {
        printf("%.*s\n", instr.length, instr.ptr);
    }
}


/************************************/

void foo27(ClassInfo ci) { }

class A27
{
}

class B27 : A27
{
    static this()
    {
        foo27(B27.classinfo);
        foo27(A27.classinfo);
    }
}

void test27()
{
}


/************************************/

void foo28(ClassInfo ci)
{
    printf("%.*s\n", ci.name.length, ci.name.ptr);

    static int i;
    switch (i++)
    {
        case 0:
        case 2: assert(ci.name == "test15.A28");
                break;
        case 1: assert(ci.name == "test15.B28");
                break;
        default: assert(0);
    }
}

class A28
{
    static this() {
    foo28(A28.classinfo );
    }
}

class B28 : A28
{
    static this() {
        foo28(B28.classinfo );
        (new B28()).bodge_it();
    }
    void bodge_it() {
        foo28(A28.classinfo );
    }
}

void test28()
{
    A28 a,b;
    a = new A28();
    b = new B28();
}


/************************************/

void test29()
{
    static void delegate() dg;

    dg = null;
}


/************************************/

string foo30(int i)
{
    return i ? "three" : "two";
}

string bar30(int i)
{
    return i ? "one" : "five";
}

void test30()
{
    string s;

    s = foo30(0);
    assert(s == "two");
    s = foo30(1);
    assert(s == "three");

    s = bar30(0);
    assert(s == "five");
    s = bar30(1);
    assert(s == "one");
}


/************************************/
// http://www.digitalmars.com/d/archives/18204.html
// DMD0.050 also failed with alias.

alias int recls_bool_t;

class Entry31
{

    recls_bool_t IsReadOnly()
    {
        return cast(recls_bool_t)0;
    }
}


void test31()
{
    Entry31 entry = new Entry31();

    if (entry.IsReadOnly)
    {
    }
}


/************************************/

class A32
{
    alias int delegate() mfunc;

    this( mfunc initv )
    {
    }
}

class foo32
{
    static void getMemberBar()
    {
        //foo32 f = new foo32(); new A32( &(f.bar) );
        new A32( &((new foo32()).bar) );
    }

    int bar()
    {
        return 0;
    }
}


void test32()
{
    foo32.getMemberBar();
}


/************************************/

void[] foo33(int[] b)
{
    return b;
}

void test33()
{
    int[6] c;
    void[] v;

    v = foo33(c);
    assert(v.length == 6 * int.sizeof);
}

/************************************/

void test34()
{
    version (D_Bits)
    {
        bool[8] a8;
        assert(a8.sizeof == 4);
        bool[16] a16;
        assert(a16.sizeof == 4);
        bool[32] a32;
        assert(a32.sizeof == 4);
        bool[256] a256;
        assert(a256.sizeof == 32);
    }
}

/************************************/

void test35()
{
    typeof(1 + 2) i;

    assert(i.sizeof == int.sizeof);
    assert(typeof('c').sizeof == char.sizeof);
    assert(typeof(1).sizeof == int.sizeof);
    assert(typeof(1.0F).sizeof == float.sizeof);
    assert(typeof(1.0).sizeof == double.sizeof);
    assert(typeof(1.0L).sizeof == real.sizeof);

    //assert(((typeof(1.0L))i).sizeof == real.sizeof);
    assert((cast(typeof(1.0L))i).sizeof == real.sizeof);
}

/************************************/

void test36x()
{
    version (Win32)
    {
//      stdin.getch();
    }
}

void test36()
{
}


/************************************/

struct T37
{
    char x;

    T37 create()
    {   T37 t;
        t.x = 3;
        return t;
    }

    bool test1()
    {
        return create() == this;
    }

    bool test2()
    {
        return this == create();
    }
}


void test37()
{
    T37 t;

    assert(!t.test1());
    t.x = 3;
    assert(t.test1());

    t.x = 0;
    assert(!t.test2());
    t.x = 3;
    assert(t.test2());
}

/************************************/

void test38()
{
    uint f(uint n) { return n % 10; }

    printf("%u\n", uint.max);
    printf("%u\n", f(uint.max));
    assert(f(uint.max) == 5);
}

/************************************/

void test39()
{
    short s=HIWORD (0x0FFFDDDD);
    short t=LOWORD (0xFFFFCCCC);
    short v=HIWORD_(0x0FFFEEEE);
    short x=HIWORD_(0xFFFFAAAA);

    printf("%x %x %x %x\n",s,t,v,x);
    assert(s == 0xFFF);
    assert(t == 0xFFFFCCCC);
    assert(v == 0xFFF);
    assert(x == 0xFFFFFFFF);
}

short HIWORD(uint i)
{
    return cast(short)(( i >> 16) & 0xFFFF);
}

short LOWORD(uint i)
{

    return cast(short)i;
}
extern (C) short HIWORD_(uint i)
{
    return cast(short)(( i >> 16) & 0xFFFF);
}


/************************************/

class caller40 {
   caller40 opCall (out int i) {
      i = 10;
      return this;
   }
}

void test40()
{
   caller40 c = new caller40;
   int x,y;
   c(x)(y);
   assert(x == 10);
   assert(y == 10);
}

/************************************/

class Foo41 { void print() {printf("Foo41\n");} }

class Bar41 : Foo41
{
    void test()
    {
        void printFoo41()
        {
            super.print();      // DMD crashes here
        }
        printFoo41();
    }
}

void test41()
{
    Bar41 b = new Bar41();
    b.test();
}

/************************************/

interface Interface
{
    void foo42();
}

void bar42(Interface i)
{
    i.foo42();
}

class Abstract : Interface
{
    abstract void foo42();
}

class Concrete : Abstract
{
    override void foo42() { printf("Concrete.foo42(this = %p)\n", this); }
}

class Sub : Concrete
{
}

void test42()
{
    Sub s = new Sub();
    s.foo42();
    bar42(s);
}

/************************************/

class A43
{
    int foo() { return 6; }
}

int bar43(A43 a)
{
    return a.foo;
}

void test43()
{
    A43 a = new A43();
    assert(bar43(a) == 6);
}


/************************************/

class C44
{
    const char[][] arrArr=["foo"];
}

void test44()
{
  C44 c= new C44();
  printf("%.*s\n", c.arrArr[0].length, c.arrArr[0].ptr);
  assert(c.arrArr[0] == "foo");
}


/************************************/

void test45()
{
    void* p;
    void[] data;

    data = p[0 .. 5];
}

/************************************/

union A46
{
    char c;
    struct { short s; }
    struct { long  l; }
    int a;
    struct { float f; }
}

void test46()
{
    A46 a;
    printf("%d\n", cast(byte*)&a.c - cast(byte*)&a);
    printf("%d\n", cast(byte*)&a.s - cast(byte*)&a);
    printf("%d\n", cast(byte*)&a.l - cast(byte*)&a);
    printf("%d\n", cast(byte*)&a.a - cast(byte*)&a);
    printf("%d\n", cast(byte*)&a.f - cast(byte*)&a);

    assert(cast(byte*)&a.c == cast(byte*)&a);
    assert(cast(byte*)&a.s == cast(byte*)&a);
    assert(cast(byte*)&a.l == cast(byte*)&a);
    assert(cast(byte*)&a.a == cast(byte*)&a);
    assert(cast(byte*)&a.f == cast(byte*)&a);
}


/************************************/

class Bug47
{

    void foo()
    {
    }

    static void foo(int i)
    {
    }

    static void bar()
    {
        foo(1);
    }
}

void test47()
{
}


/************************************/

int[2] x48 = 3;
float y48 = 0.0f;

void test48()
{
    printf("%d, %d\n", x48[0], x48[1]);
    assert(x48[0] == 3 && x48[1] == 3);

    y48 = -100;

    printf("%d, %d\n", x48[0], x48[1]);
    assert(x48[0] == 3 && x48[1] == 3);
}

/************************************/

struct Baz49 { int x,y,z,t; }

void foo49(out Baz49 x, out int y)
{
}

void test49()
{
    int y = 3;
    Baz49 b;
    assert(b.x == 0);
    assert(b.y == 0);
    assert(b.z == 0);
    assert(b.t == 0);
    b.x = 1;
    b.y = 6;
    b.z = 10;
    b.t = 11;
    foo49(b, y);
    assert(b.x == 0);
    assert(b.y == 0);
    assert(b.z == 0);
    assert(b.t == 0);
    assert(y == 0);
}

/************************************/

void foo50(int[] f, ...)
{
    foreach(int i, TypeInfo ti; _arguments) { }
}

void bar50(out int[] f, ...)
{
    foreach(int i, TypeInfo ti; _arguments) { }
}

void test50()
{
    int[] a;

    foo50(a, 1, 2, 3);
    bar50(a, 1, 2, 3);
}


/************************************/

deprecated int tri(int x)
{
    return x*(x+1)/2;
}

deprecated int pent(int x)
{
    return x*x + tri(x) - x;
}

deprecated class Qwert
{
    int yuiop;

    this(int y) { yuiop = y; }

    int twiceYuiop()
    {
        return 2 * yuiop;
    }

    invariant()
    {
        assert (yuiop < 100);
    }
}

void test51()
{
}

/************************************/

void foo52(double d) {}
deprecated void foo52(int i) {}

deprecated void bar52(int i) {}
void bar52(double d) {}

void test52()
{
//    foo52(1);
    foo52(1.0);
    bar52(1.0);
}

/************************************/

class X53
{
    void foo(double d) {}
    deprecated void foo(int i) {}

    deprecated void bar(int i) {}
    void bar(double d) {}
}

void test53()
{
    X53 x = new X53();
    //x.foo(1);
    //x.bar(1);
    x.foo(1.0);
    x.bar(1.0);
}

/************************************/

interface B54 : A54
{
  A54 getParent();
}

interface A54
{
  void parse(char[] systemId);
}

void test54()
{
}


/************************************/

class Writer
{
    int put (bool x){ return 1; }
    int put (int x){ return 2; }
}

class MyWriter : Writer
{
    alias Writer.put put;

    override int put (bool x){ return 3; }
}

void test55()
{
    MyWriter m = new MyWriter();

    assert(m.put(false) == 3);
    assert(m.put(1) == 2);
}


/************************************/

class Foo56
{
    alias int baseType;
}

void test56()
{
    Foo56 f = new Foo56;

    f.baseType s = 10;
}


/************************************/

void det(float mat[][])
{
    float newmat[][];

    size_t i = newmat[0 .. (mat.length - 1)].length;
}

void test57()
{
}


/************************************/

int foo58 (int a, int t) { return 2; }

class A58
{
    int foo58 ( ) { return 3; }
    alias .foo58 foo58;
}

void test58()
{   int y, x;

    with ( new A58 )
    {   y = foo58(0,1);
        x = foo58();
    }
    assert(y == 2);
    assert(x == 3);
}


/************************************/

void test59()
{
    struct data
    {
        int b1=-1;
        int b2=2;
    }

    data d;
    assert(d.b1 == -1);
    assert(d.b2 == 2);
}


/************************************/

class Foo60
{
   int x;
static:
   this() { x = 3; }
   ~this() { }
}


void test60()
{
    Foo60 f = new Foo60();

    assert(f.x == 3);
}


/************************************/

class StdString
{
     alias std.string.format toString;
}

void test61()
{
    int i = 123;
    StdString g = new StdString();
    string s = g.toString("%s", i);
    printf("%.*s\n", s.length, s.ptr);
    assert(s == "123");
}


/************************************/

void test62()
{   char[4] a;

    assert(a[0] == 0xFF);
    assert(a[1] == 0xFF);
    assert(a[2] == 0xFF);
    assert(a[3] == 0xFF);
}


/************************************/

void test63()
{
    bool b;
    real r;
    int i;

    i=cast(double) b ? 1 : 4;
    r=cast(real) b ? 1.0 : 2.0;
}


/************************************/

struct MyStruct64
{
    int test(short s){printf("dynamic short\n"); return 1; }
    int test(int i){printf("dynamic int\n"); return 2; }
    static int staticTest(short s){printf("static short\n"); return 3; }
    static int staticTest(int i){printf("static int\n"); return 4; }
}

void test64()
{
    MyStruct64 S;
    int j;

    short s = 1;
    int i = 1;

    j = S.test(s);
    assert(j == 1);
    j = S.test(i);
    assert(j == 2);

    j = S.staticTest(s);
    assert(j == 3);
    j = S.staticTest(i);
    assert(j == 4);

}

/************************************/

void test65()
{
    int[8] qwert;
    int[] yuiop = qwert[2..5] = 4;

    assert(yuiop.length == 3);
    assert(yuiop[0] == 4);
    assert(yuiop[1] == 4);
    assert(yuiop[2] == 4);
    yuiop[1] = 2;
    assert(qwert[0] == 0);
    assert(qwert[1] == 0);
    assert(qwert[2] == 4);
    assert(qwert[3] == 2);
    assert(qwert[4] == 4);
    assert(qwert[5] == 0);
    assert(qwert[6] == 0);
    assert(qwert[7] == 0);
}


/************************************/

void foo66(ref bool b)
{
    b = true;
}

void test66()
{
    bool[3] a;

    foo66(a[0]);
    assert(a[0] == true);
    assert(a[1] == false);
    assert(a[2] == false);
}


/************************************/

class FuBar
{
        void foo ()
        {
                printf ("should never get here\n");
                assert(0);
        }

        const(void)[] get ()
        {
                return "weqweqweqweqwee";
        }

        void test (void* dst)
        {
                uint count = 7;
                while (count)
                      {
                      // get as much as there is available in the buffer
                      uint available = 10;

                      // cap bytes read
                      if (available > count)
                          available = count;

                      // copy them over
                      dst[0..available] = get ()[0..available];

                      // bump counters
                      dst += available;
                      if ((count -= available) > 0)
                           foo ();
                      }
        }
}


void test67()
{
    FuBar b = new FuBar();
    char[10] dst;
    b.test(&dst[0]);
}


/************************************/

struct Foo68 { int a,b,c,d; }

void bar68(out Foo68 f)
{
    f.a = 28;
}

void test68()
{
    Foo68 f;
    bar68(f);
    assert(f.a == 28);
}

/************************************/

class ConduitStyle
{

//      static ConduitStyle     Read;
//      static ConduitStyle     ReadWrite;

        static ConduitStyle     Read, ReadWrite;
}

void test69()
{
}


/************************************/

void test70()
{
    printf("-5/3 prints: %d\n", -5/3);
    printf("-5/2 prints: %d\n", -5/2);
    printf("-7/3 prints: %d\n", -7/3);
    printf("-7/4 prints: %d\n", -7/4);
    printf("-7/7 prints: %d\n", -7/7);
    printf("-8/7 prints: %d\n", -8/7);
    printf("-12/6 prints: %d\n", -12/6);
    printf("12/6 prints: %d\n", 12/6);
    printf("-9/7 prints: %d\n", -9/7);
    printf("-11/8 prints: %d\n", -11/8);
    printf("-7/9 prints: %d\n", -7/9);

    assert(-5/3 == -1);
    assert(-5/2 == -2);
    assert(-7/3 == -2);
    assert(-7/4 == -1);
    assert(-7/7 == -1);
    assert(-8/7 == -1);
    assert(-12/6 == -2);
    assert(12/6 == 2);
    assert(-9/7 == -1);
    assert(-11/8 == -1);
    assert(-7/9 == 0);
}

/************************************/

void insertText(string str)
{
    assert(str == "a ");
}

char getCharAt()
{
    return 'a';
}

void test71()
{
    insertText(getCharAt() ~ " ");
}

/************************************/

public class Foo72
{
    public this() { }
}

void test72()
{
    Foo72[] foos;

    foos = new Foo72() ~ foos[];
    assert(foos.length == 1);
}


/************************************/

int main()
{
    test5();
    test6();
    test7();
    test8();
    test9();
    test10();
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
    test67();
    test68();
    test69();
    test70();
    test71();
    test72();

    printf("Success\n");
    return 0;
}
