// REQUIRED_ARGS:

module test;

import core.vararg;
import core.stdc.stdlib;
import std.stdio;
import std.string;
import core.stdc.stdlib;


/*******************************************/

struct S
{
    int opSliceAssign(int v, size_t i, size_t j)
    {
        assert(v == 5);
        assert(i == 9);
        assert(j == 10);
        return 3;
    }

    int opSliceAssign(int v)
    {
        assert(v == 6);
        return 11;
    }
}

void test1()
{
    S s;

    assert((s[9 .. 10] = 5) == 3);
    assert((s[] = 6) == 11);
}

/*******************************************/

static int i2 = 1;

void test2()
{
    synchronized { int i2 = 2; }
    assert(i2 == 1);
}

/*******************************************/

void test3()
{
    size_t border = 8;

    for(ulong i = 0; i < border; i++)
    {
        ulong test = 1;
        test <<= i;
        double r = test;
        ulong result = cast(ulong)r;

        if (result != test)
        {
            assert(0);
        }
    }
}

/*******************************************/

void test4()
{
    writeln("",true);
}

/*******************************************/

void test5()
{
    int[] qwert = new int[6];
    int[] yuiop;
    yuiop = qwert[2..5] = 3;
    assert(yuiop.length == 3);
    assert(yuiop[0] == 3);
    assert(yuiop[1] == 3);
    assert(yuiop[2] == 3);
}

/*******************************************/

struct Foo6
{
    static int x;

    static int[] farray()
    {
        printf("farray\n");
        assert(x == 0);
        x++;
        return new int[6];
    }

    static int flwr()
    {
        printf("flwr\n");
        assert(x == 1);
        x++;
        return 2;
    }

    static int fupr()
    {
        printf("fupr\n");
        assert(x == 2);
        x++;
        return 1;
    }
}

void test6()
{
    int[] yuiop;
    yuiop =
        Foo6.farray()[Foo6.flwr() .. $ - Foo6.fupr()] = 3;
    assert(Foo6.x == 3);
    assert(yuiop.length == 3);
    assert(yuiop[0] == 3);
    assert(yuiop[1] == 3);
    assert(yuiop[2] == 3);
}

/*******************************************/

void test7()
{
    real a = 3.40483; // this is treated as 3.40483L
    real b;
    b = 3.40483;
    assert(a==b);
    assert(a==3.40483);
    assert(a==3.40483L);
    assert(a==3.40483F);
}

/*******************************************/

void test8()
{
   real [5][5] m = 1;
   m[1][1..3] = 2;

   for (size_t i = 0; i < 5; i++)
        for (size_t j = 0; j < 5; j++)
        {
            if (i == 1 && (j >= 1 && j < 3))
                assert(m[i][j] == 2);
            else
                assert(m[i][j] == 1);
        }
}

/*******************************************/

class ClassOf(Type)
{
    Type val;

    template refx()
    {
        alias val refx;
    }
}

struct StructOf
{
    int val;

    template refx()
    {
        alias val refx;
    }
}

void test9()
{
    ClassOf!(int)  c = new ClassOf!(int)();
    StructOf s;
    int x = 10;

    c.refx!() = x;
    x = c.refx!();
    assert(x == 10);

    x = 11;
    s.refx!() = x;
    x = s.refx!();
    assert(x == 11);
}


/*******************************************/

void test10()
{
    static if( int.mangleof.length > 1 && int.mangleof[1] == 'x' )
        printf("'x' as second char\n");
}

/*******************************************/

class Foo11 : Bar11 { }

class Foo11T(V)
{
    public void foo() {}
}

class Bar11
{
    public this(){
        f = new Foo11T!(int);
    }
    Foo11T!(int) f;
}

void test11()
{
    Foo11 fooIt = new Foo11();
    if (fooIt !is null)
        writefln("fooIt should be valid");
    fooIt.f.foo();
    writefln("it worked");
}

/*******************************************/

struct A12 {
        int a;
        union {
                int c;
                B12 b;
        }
}

struct B12 {
        int b1;
        int b2;
}

void test12()
{
        A12 a;
        printf("%d\n", A12.sizeof);
        assert(A12.sizeof == 12);
}

/*******************************************/

template declare13(X) { X declare13; }

typeof(declare13!(int[0]).ptr[0]) x13;
typeof(declare13!(typeof(""))[0..$]) y13;

void test13()
{
}

/*******************************************/

interface Father {}

class Mother {
     Father test() {
         writefln("Called Mother.test!");
         return new Child(42);
     }
}

class Child : Mother, Father {
     int data;

     this(int d) { data = d; }

     override Child test() {
         writefln("Called Child.test!");
         return new Child(69);
     }
}

void test14()
{
     Child aChild = new Child(105);
     Mother childsMum = aChild;
     Child childsChild = aChild.test();
     Child mumsChild = cast(Child) childsMum.test();
     writefln("Success2");
}


/*******************************************/

class A15
{
        int a = 3;

        class B
        {
            void bar()
            {
                assert(a == 3);
            }
        }

        void fork()
        {
                assert(a == 3);
                B b = new B();  // This is okay
                b.bar();

                void knife()
                {
                        assert(a == 3);
                        B b = new B();  // No 'this' for nested class B
                        b.bar();
                }
        }
}

void test15()
{
    A15 a = new A15();
    a.fork();
}

/*******************************************/

creal x16;

void foo16()
{
        x16 = -x16;
}

void bar16()
{
        return foo16();
}

void test16()
{
        x16 = 2.0L + 0.0Li;
        bar16();
        assert(x16 == -2.0L + 0.0Li);
}

/*******************************************/

void test17()
{
    version(D_InlineAsm_X86_64)
        enum AsmX86 = true;
    else version(D_InlineAsm_X86)
        enum AsmX86 = true;
    else
        enum AsmX86 = false;

    version (OSX)
    {
    }
    else
    {
        const f = 1.2f;
        float g = void;

        static if (AsmX86)
        {
            asm
            {
                fld f;  // doesn't work with PIC
                fstp g;
            }
        }
        else
        {
            g = f;
        }
        assert(g == 1.2f);
    }
}

/*******************************************/

class Foo18 : Bar18 {}
class FooT18(V){}
class Bar18 : FooT18!(int) {}

void test18()
{
}

/*******************************************/

struct STRUCTA19
{
  union {
    int a;
    long b;
  }
  STRUCTB19 c;
}

struct STRUCTB19
{
  int a;
}

void test19()
{
}

/*******************************************/

class Foo20
{
        void bar (void * src)
        {
                void baz (void function (void *, size_t) xyz)
                {
                        size_t foo (void [] dst)
                        {
                                size_t len = dst.length;
                                dst [0 .. len] = src [0 .. len];
                                xyz (dst.ptr, len);
                                return len;
                        }
                }
        }
}

void test20()
{
}

/*******************************************/

class Baz21
{
        int opApply (int delegate(ref int) dg)
        {
                int i;
                return dg(i);
        }
}

class Foo21
{
        Baz21    baz;

        int foo (int delegate() dg)
        {
                foreach (b; baz)
                         if (bar ())
                            if (dg ())
                                break;
                return 0;
        }

        bool bar ()
        { return true; }
}

void test21()
{
}

/*******************************************/

struct Bar22 {
    union {
        struct {
            union {
                Foo22 A;
            }
        }
    }
}

struct Foo22 {
    double d = 3;
}

void test22()
{
    printf("Bar22.sizeof = %zd, double.sizeof = %zd\n", Bar22.sizeof, double.sizeof);
    assert(Bar22.sizeof == double.sizeof);
    Bar22 b;
    assert(b.A.d == 3);
}

/*******************************************/

struct Ag
{
    static void func(){}

    static void foo()
    {
        void function() fnp;
        Ag a;

        fnp = &func;
        fnp = &Ag.func;
        with(a)    fnp = &Ag.func;

        with(a) fnp = &func;
    }
}

class Ah
{
    static void func(){}

    static void foo()
    {
        void function() fnp;
        Ah a;

        fnp = &func;
        fnp = &Ah.func;
        with(a)    fnp = &Ah.func;

        with(a) fnp = &func;
    }
}

void test23()
{
}

/*******************************************/

void test24()
{
    uint[10] arr1;
    ulong idx = 3;
    uint[] arr3 = arr1[ cast(int)(idx) .. (cast(int) idx) + 3  ]; // OK
    uint[] arr4 = arr1[ cast(int) idx  ..  cast(int) idx  + 3  ]; // OK
    uint[] arr5 = arr1[ cast(int)(idx) ..  cast(int)(idx) + 3  ]; // C style cast illegal, use cast(idx)+3
    uint[] arr6 = arr1[ cast(int)(idx) ..  cast(int)(idx  + 3) ]; // OK
}

/*******************************************/

void test25()
{
  char[6] cstr = "123456"c;
  auto str1 = cast(wchar[3])(cstr);

  writefln("str1: ", (cast(char[])str1).length , " : ", (cast(char[])str1));
  assert(cast(char[])str1 == "123456"c);

  auto str2 = cast(wchar[3])("789abc"c);
  writefln("str2: ", (cast(char[])str2).length , " : ", (cast(char[])str2));
  assert(cast(char[])str2 == "789abc"c);

  auto str3 = cast(wchar[3])("defghi");
  writefln("str3: ", (cast(char[])str3).length , " : ", (cast(char[])str3));
  assert(cast(char[])str3 == "d\000e\000f\000"c);
}

/*******************************************/

void test26()
{
    assert(foo26(5) == 25);
}

int foo26(int i)
{
    if (auto j = i * i)
      return j;
    else
      return 10;
}

/*******************************************/

class A27
{
    int am;

    class B
    {
        this()
        {
            assert(am == 3);
        }
    }

    void fork()
    {
        B b = new B();  // This is okay

        void knife()
        {
                B b = new B();  // No 'this' for nested class B
                assert(am == 3);
        }
    }
}


void test27()
{
    A27 a = new A27();
    a.am = 3;

    a.fork();
}

/*******************************************/

uint intRes()
{
        return 4;
}

void test28()
{
        auto s = std.string.format("%s", "abc123"[intRes() % $] );
        writefln( "%s", s );
        assert(s == "2");

        static const char[] foo = "abc123";
        s = std.string.format("%s", foo[intRes() % $] );
        assert(s == "2");


        static string bar = "abc123";
        s = std.string.format("%s", bar[intRes() % $] );
        assert(s == "2");

        const char[] abc = "abc123";
        s = std.string.format("%s", abc[intRes() % $] );
        assert(s == "2");

        string def = "abc123";
        s = std.string.format("%s", def[intRes() % $] );
        assert(s == "2");
}

/*******************************************/

class UA {
    A29 f() { return null; }
}

class UB : UA {
    override B29 f() { return null; }
}

class A29
{
}

class B29 : A29
{
}

void test29()
{
}

/*******************************************/

class Foo30 : Bar30 {}

class FooT30(V) {}

class Bar30 : FooT30!(int) {}

void test30()
{
}


/*******************************************/

int y31;

struct X31 { static void opCall() { y31 = 3; } }

void test31()
{
    X31 x;
    typeof(x)();
    assert(y31 == 3);
}

/*******************************************/

class Foo32
{
    static void* ps;

    new (size_t sz)
    {
        void* p = core.stdc.stdlib.malloc(sz);
        printf("new(sz = %d) = %p\n", sz, p);
        ps = p;
        return p;
    }

    delete(void* p)
    {
        printf("delete(p = %p)\n", p);
        assert(p == ps);
        if (p) core.stdc.stdlib.free(p);
    }
}

void test32()
{
    Foo32 f = new Foo32;
    delete f;
}

/*******************************************/

class Foo33
{
//    this() { printf("this()\n"); }
//    ~this() { printf("~this()\n"); }

    static void* ps;
    static int del;

    new (size_t sz, int i)
    {
        void* p = core.stdc.stdlib.malloc(sz);
        printf("new(sz = %d) = %p\n", sz, p);
        ps = p;
        return p;
    }

    delete(void* p)
    {
        printf("delete(p = %p)\n", p);
        assert(p == ps);
        if (p) core.stdc.stdlib.free(p);
        del += 1;
    }
}

void foo33()
{
    scope Foo33 f = new(3) Foo33;
}

void test33()
{
    foo33();
    assert(Foo33.del == 1);
}

/*******************************************/

struct o_O { int a; }
union  O_O { int a; }
class  O_o { int a; }

struct Foo34
{
    int ok;
    o_O foo;
    O_O bar;
    O_o baz;
}

void test34()
{
    int o1 = Foo34.ok.offsetof;
    assert(o1 == 0);
    int o2 = Foo34.foo.offsetof;
    assert(o2 == 4);
    int o3 = Foo34.bar.offsetof;
    assert(o3 == 8);
    int o4 = Foo34.baz.offsetof;
    assert((o4 % (void*).sizeof) == 0);
    assert(o4 > o3);
}

/*******************************************/

class Foo37
{
    float[4] array = 1.0;
    int count = 10;
}

void test37()
{
    Foo37 f = new Foo37();

    writefln("Foo.array[0] = %s", f.array[0] );
    writefln("Foo.array[1] = %s", f.array[1] );
    writefln("Foo.array[2] = %s", f.array[2] );
    writefln("Foo.array[3] = %s", f.array[3] );
    writefln("Foo.count = %s", f.count );

    assert(f.array[0] == 1.0);
    assert(f.array[1] == 1.0);
    assert(f.array[2] == 1.0);
    assert(f.array[3] == 1.0);
    assert(f.count == 10);
}

/*******************************************/

void test38()
in
{
    static void checkParameters()
    {
        return;
    }

    checkParameters();
}
body
{
}

/*******************************************/

void delegate() foo39()
{
        return &(new class
        {

                int a;

                this() { a = 3; }

                void dg()
                {
                        writefln("delegate!");
                        assert(a == 3);
                }
        }).dg;
}

void test39()
{
    void delegate() dg = foo39();

    dg();
}

/*******************************************/

void test40()
{
    assert( typeid(int) == typeid(int) );
    assert( (typeid(int) != typeid(int)) == false );

    int x;

    bool b1 = (typeid(typeof(x)) != typeid(int));

    TypeInfo t1 = typeid(typeof(x));
    TypeInfo t2 = typeid(int);

    bool b2 = (t1 != t2);

    assert(b1 == b2);
}

/*******************************************/

int foo41(string s)
{
        short shift = cast(short)(s.length * 3);
        int answer;

        for (size_t i = 0; i < s.length; i++){
                answer = s[i] << shift;
        }

        return answer;
}

void test41()
{
        if(foo41("\u0001") != 8){
                assert(0);
        }
}

/*******************************************/

struct S42
{
        int i;

        static S42 foo(int x){
                S42 s;

                s.i = x;

                return s;
        }
}

void test42()
{
        S42[] s;

        s = s ~ S42.foo(6);
        s = s ~ S42.foo(1);

        if(s.length != 2){
                assert(0);
        }
        if(s[0].i != 6){
                assert(0);
        }
        if(s[1].i != 1){
                assert(0);
        }
}

/*******************************************/

struct S43
{
        int i,j;

        static S43 foo(int x){
                S43 s;

                s.i = x;

                return s;
        }
}

void test43()
{
        S43[] s;

        s = s ~ S43.foo(6);
        s = s ~ S43.foo(1);

        if(s.length != 2){
                assert(0);
        }
        if(s[0].i != 6){
                assert(0);
        }
        if(s[1].i != 1){
                assert(0);
        }
}

/*******************************************/

struct S44
{
        int i,j,k;

        static S44 foo(int x){
                S44 s;

                s.i = x;

                return s;
        }
}

void test44()
{
        S44[] s;

        s = s ~ S44.foo(6);
        s = s ~ S44.foo(1);

        if(s.length != 2){
                assert(0);
        }
        if(s[0].i != 6){
                assert(0);
        }
        if(s[1].i != 1){
                assert(0);
        }
}

/*******************************************/

void test45()
{
   char[] buffer = "abcdefghijklmnopqrstuvwxyz".dup;
   foreach(ref char c; buffer)
   {
       if('a' <= c && c <= 'z')
       {
           c -= cast(char)'a' - 'A'; // segfault here
       }
   }
   for(int i = 0; i < buffer.length; i++)
   {
       if('a' <= buffer[i] && buffer[i] <= 'z')
       {
           buffer[i] -= cast(char)'a' - 'A'; // segfault here
       }
   }
   writeln(buffer);
}

/*******************************************/

struct st46
{
    template t1()
    {
        template t2(int n2) { }
    }
}

alias st46.t1!().t2 a46;

void test46()
{
}

/*******************************************/

struct A47
{
    static int y;
    void opSliceAssign(int x)
    {
        printf("x = %d\n", x);
        y = x;
    }
    A47 d() { return this; }
}

void test47()
{
    A47 a;
    a[] = 3;
    printf("y = %d\n", a.y);
    a.d()[] = 5;
    printf("y = %d\n", a.y);
    assert(a.y == 5);
    a.d[] = 6;
    printf("y = %d\n", a.y);
    assert(a.y == 6);
}

/*******************************************/

static uint[] sarray48 = void;

void test48()
{
    static uint[] array = void;

    assert(sarray48 == null);
    assert(array == null);
}

/*******************************************/

int x = 2, y = 1;

void foo50(int z)
{
    static int t;
    t++;
    assert(t == z);
}

void test50()
{
    printf("test50()\n");
    int res = 0;
    for(int i = 0; i < 10; i++)
    {
        res = res + x - y;
        foo50(res);
    }
}

/*******************************************/

void test52()
{
    printf("test52()\n");
    char[] s;
    s = ['a', 'b', 'c'];
    assert(s == "abc");

    int[] x;
    x = [17, 18u, 29, 33];
    assert(x.length == 4);
    assert(x[0] == 17);
    assert(x[1] == 18);
    assert(x[2] == 29);
    assert(x[3] == 33);
    assert(x == [17, 18, 29, 33]);
}

/*******************************************/

void test54()
{
    printf("test54()\n");
    uint[500][] data;

    data.length = 1;
    assert(data.length == 1);
    foreach (ref foo; data)
    {
        assert(foo.length == 500);
        foreach (ref u; foo)
        {   //printf("u = %u\n", u);
            assert(u == 0);
            u = 23;
        }
    }
    foreach (ref foo; data)
    {
        assert(foo.length == 500);
        foreach (u; foo)
        {   assert(u == 23);
            auto v = u;
            v = 23;
        }
    }
}

/*******************************************/

class Base56
{
        private string myfoo;
        private string mybar;

        // Get/set properties that will be overridden.
        void foo(string s) { myfoo = s; }
        string foo() { return myfoo; }

        // Get/set properties that will not be overridden.
        void bar(string s) { mybar = s; }
        string bar() { return mybar; }
}

class Derived56: Base56
{
        alias Base56.foo foo; // Bring in Base56's foo getter.
        override void foo(string s) { super.foo = s; } // Override foo setter.
}

void test56()
{
        Derived56 d = new Derived56;
        with (d)
        {
                foo = "hi";
                d.foo = "hi";
                bar = "hi";
                assert(foo == "hi");
                assert(d.foo == "hi");
                assert(bar == "hi");
        }
}

/*******************************************/

bool[void[]] reg57;

void addToReg57(const(void)[] a, int b, bool v)
{
    if (!v)
        writefln("X");
    auto key = a~(cast(void*)&b)[0..4];
    reg57[cast(immutable(void)[])key] = v;
    writefln("OK");
}

void test57()
{
        addToReg57("test", 1024, true);
}

/*******************************************/

int bar58( string msg ){
    return 1;
}

int foo58( lazy string dg ){
    return bar58( dg() );
}

void test58()
{
    printf("test58()\n");
    try{
    }
    finally{
        foo58("");
    }
}

/*******************************************/

struct S59
{
    string toString()
    {
        return "foo";
    }
}

void test59()
{   S59 s;
    writefln("s = %s", s);

    string p;
    p = std.string.format("s = %s", s);
    assert(p == "s = foo");
}

/*******************************************/

void test60()
{
    int[2][] a;
    a = [ [-1,2], [3,4] ];
    assert(a[0][0] == -1);
    assert(a[0][1] == 2);
    assert(a[1][0] == 3);
    assert(a[1][1] == 4);

    int[][] b;
    b = [ [-1,2], [3,4] ];
    assert(b[0][0] == -1);
    assert(b[0][1] == 2);
    assert(b[1][0] == 3);
    assert(b[1][1] == 4);
}

/*******************************************/

void test61()
{
    int[][] f = [[1,2],[3,4]];
    assert(f[0][0] == 1);
    assert(f[0][1] == 2);
    assert(f[1][0] == 3);
    assert(f[1][1] == 4);
    writeln(f);
}

/*******************************************/

struct List62 {
        void get() {}
}
struct Array62 {
        interface Model {
                List62 list();
        }
        List62 list() {
                return model ? model.list() : List62.init;
        }
        void item() {
                list.get();
        }
        private Model model;
}

void test62()
{
}

/*******************************************/

void foo63(...)
{
}

void test63()
{
        int[] arr;
        arr = [1] ~ 2;

        // runtime crash, the length == 1
        printf("%d\n", arr.length);
        assert (arr.length == 2);
        assert(arr[0] == 1);
        assert(arr[1] == 2);

        arr = 2 ~ [1];
        assert(arr.length == 2);
        assert(arr[0] == 2);
        assert(arr[1] == 1);

        arr = [2, 3] ~ [1];
        assert(arr.length == 3);
        assert(arr[0] == 2);
        assert(arr[1] == 3);
        assert(arr[2] == 1);

        foo63([1] ~ 2, 2 ~ [1], [1,2] ~ [3,4,5]);
}

/*******************************************/

void test64()
{
    printf("test64()\n");
    int[] x = [1,2,3,4];
    int j = 4;

    foreach_reverse(v; x)
    {
        writeln(v);
        assert(j == v);
        j--;
    }
    assert(j == 0);

    j = 4;
    foreach_reverse(i, v; x)
    {
        writefln("[%s] = %s", i, v);
        assert(i + 1 == j);
        assert(j == v);
        j--;
    }
    assert(j == 0);
    printf("-test64()\n");
}

/*******************************************/

void test65()
{
    // Bugzilla Issue 407.
    int i = *cast(int*)cast(char[4])['0', '0', '0', '0']; // compiler seg-fault
    printf("i = %x\n", i);
}

/*******************************************/

void test66()
{
    int[]  ia;
    ia ~= 3;
    byte[] data = new byte[ia[0]];
    byte[] data2 = new byte[ cast(int)(ia[0])];
}

/*******************************************/

class C68
{
    static int value;
}

void test68()
{
    auto v1 = test.C68.value;
    auto v2 = C68.classinfo;
    auto v3 = test.C68.classinfo;
    assert(v2 == v3);
}

/*******************************************/

void test69()
{
    class Bug
    {
      char[12] str = "";
      uint t = 1;
    }

    class NoBug
    {
      uint t = 2;
      char[12] str = "";
    }

    class NoBug2
    {
      char[12] str;
      uint t = 3;
    }

    auto b = new Bug;
    auto n = new NoBug;
    auto n2 = new NoBug2;

    writefln("bug %d", b.t);
    assert(b.t == 1);
    writefln("nobug %d", n.t);
    assert(n.t == 2);
    writefln("nobug2 %d", n2.t);
    assert(n2.t == 3);
}

/*******************************************/

void test70()
{
    void foo(char[0] p)
    {
    }

    static const char[0] altsep;
    string s = std.string.format("test%spath", altsep);
    assert(s == "testpath");
    foo(altsep);
}

/*******************************************/


class C71
{
    static int cnt;
    this() { printf("C()\n"); cnt++; }
    ~this() { printf("~C()\n"); cnt--; }
}

class D71
{
    static int cnt;
    this() { printf("D()\n"); cnt++; }
    ~this() { printf("~D()\n"); cnt--; }
}

class E71
{
    static int cnt;
    this() { printf("E()\n"); cnt++; }
    ~this() { printf("~E()\n"); cnt--; }
}

void test71()
{
  {
    int i = 0;
    printf("start\n");
    scope D71 d = new D71();
    assert(D71.cnt == 1);
    for (scope E71 e = new E71(); i < 5; i++)
    {
        assert(D71.cnt == 1);
        assert(E71.cnt == 1);
        scope c = new C71();
        assert(C71.cnt == 1);
    }
    assert(C71.cnt == 0);
    assert(E71.cnt == 0);
    assert(D71.cnt == 1);
    printf("finish\n");
  }
  assert(D71.cnt == 0);
}

/*******************************************/

size_t getLength(int[] arr) { return arr.length; }

void test13237()
{
        int[] arr = [0];
        immutable size_t len = getLength(arr);

        arr.length--;

        assert(len == 1); // ok
        if (len) { auto l = len; }
        assert(len == 1); // len cannot be changed, but produces Assertion failure with "-O -inline"
}

/*******************************************/

void main()
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
    test50();
    test52();
    test54();
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
    test71();
    test13237();

    printf("Success\n");
}
