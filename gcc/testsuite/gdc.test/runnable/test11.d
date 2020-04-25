// RUNNABLE_PHOBOS_TEST
// REQUIRED_ARGS:

extern(C) int printf(const char*, ...);
extern(C) size_t strlen(const char*);

/**************************************/

alias strlen foo1;

void test1()
{
    const(char) *p = "bar";
    size_t i = foo1(p);
    assert(i == 3);
}

/**************************************/

template Foo2(T)
{
    alias T t;
}

alias Foo2!(int) t1;
alias Foo2!(int).t t2;
alias t1.t t3;
alias t2 t4;
alias Foo2!(int) t5;

void test2()
{
    t1.t v1;
    t2 v2;
    t3 v3;
    t4 v4;
    t5.t v5;
    int *p;

    p = &v1;
    p = &v2;
    p = &v3;
    p = &v4;
    p = &v5;
}


/**************************************/

debug = stdchar;

debug(mychar)
{
    alias byte mychar;
}

void test3()
{
    debug(mychar)
    {
    mychar[] line=cast(mychar[])cast(char[])"It is a long line.";
    mychar[] delimiter=cast(mychar[])cast(string)"is";
    }

    debug(stdchar)
    {
    string line="It is a long line.";
    string delimiter="is";
    }

    debug(stdbyte)
    {
    byte[] line=cast(byte[])cast(string)"It is a long line.";
    byte[] delimiter=cast(byte[])cast(string)"is";
    }

    debug(stdwchar)
    {
    wstring line="It is a long line.";
    wstring delimiter="is";
    }
    int ptr=3;

    size_t dl=delimiter.length;
    size_t pl=ptr+dl;

    assert(line[ptr..pl]==delimiter[]);
}


/**************************************/

void test4()
{
    byte* p;

    version(D_LP64)
        assert(p.sizeof == 8);
    else
        assert(p.sizeof == 4);
}


/**************************************/


class Foo6
{
}

void test6()
{
    Foo6 foo = new Foo6();

     with (foo)
     {
         int x;

         x = 4;
     }
}

/**************************************/

int i7 = 3;

void test7()
{
    switch (i7)
    {
    default: assert(0);
    case 3:
        int x;

        x = 4;
    }
}

/**************************************/

void test8()
{
    string a = "a
b
c";
    assert(a.length == 5);
    assert(a[1] == '\n');
}

/**************************************/


struct Foo9 { char c; char bar() { return c; } }

Foo9 makeFoo() { Foo9 f; return f; }

void callFoo (Foo9 a)
{
    a.bar();
}

void test9()
{
    callFoo(makeFoo ());
}

/**************************************/


struct Foo10 { }

Foo10 makeFoo10() { Foo10 f; return f; }

void callFoo (Foo10 a)
{
}

void test10()
{
    callFoo(makeFoo10());
}

/**************************************/

struct Color
{ int x; }

Color[3] colors;

Color eval(float x, float y)
{
    colors[1].x = 7;
    return colors[1];
}

void test11()
{
    Color c;

    c = eval(1.0, 2.0);
    assert(c.x == 7);
}

/**************************************/

struct Size12
{
  int width;
  int height;
}

int x12;

void foo12(out Size12 sz)
{
  sz.width = 2;

  if(sz.width == 2)
    x12 = 1;
}


void test12()
{
  Size12 sz;

  foo12(sz);
  assert(x12 == 1);
  assert(sz.width == 2);
}

/**************************************/


interface D13
{
    void setHostFrame();
}

class A13 : D13
{
    void setHostFrame()
    {
    }

    char         group;
}


void setLayout(D13 lo)
{
    printf("lo = %p\n", lo);
    lo.setHostFrame();
    printf("ok\n");
}


void test13()
{
    A13 a = new A13();
    printf("a  = %p\n", a);
    setLayout(a);
}

/**************************************/

void test14()
{
    while(false)
    {
        static int a;
    }
}


/**************************************/

alias void delegate(int) t_func;

class Foo15
{
   t_func func1;
   int x;

   void dothis()
   {
     if (func1)
        func1(4);
     else
        x = 3;
   }

   void func(int num) { x = num; }
}

void test15()
{
   Foo15 a = new Foo15;
   a.dothis();
   assert(a.x == 3);
   a.func1 = &a.func;
   a.dothis();
   assert(a.x == 4);
}


/**************************************/

int[] foo16(byte[] a)
{
    return cast(int[])a;
}

void test16()
{
    byte[12] b;
    int[] i;

    i = foo16(b);
    assert(i.length == 3);
}

/**************************************/

void test17()
{
  {
    float x = 10;
    x %= 4;
    printf("x = %g\n", x);
    assert(x == 2);
    x = 10;
    x = x % 4;
    printf("x = %g\n", x);
    assert(x == 2);
    x = 4;
    x = 10 % x;
    printf("x = %g\n", x);
    assert(x == 2);
  }
  {
    double y = 10;
    y %= 4;
    printf("y = %g\n", y);
    assert(y == 2);
    y = 10;
    y = y % 4;
    printf("y = %g\n", y);
    assert(y == 2);
    y = 4;
    y = 10 % y;
    printf("y = %g\n", y);
    assert(y == 2);
  }
  {
    real z = 10;
    z %= 4;
    printf("z = %Lg\n", z);
    assert(z == 2);
    z = 10;
    z = z % 4;
    printf("z = %Lg\n", z);
    assert(z == 2);
    z = 4;
    z = 10 % z;
    printf("z = %Lg\n", z);
    assert(z == 2);
  }
}


/**************************************/

struct Bar18 { }

struct Foo18
{
     static Bar18 x = { };
}

void test18()
{
     const Bar18 b = Foo18.x;
}


/**************************************/

int x19 = 10;

void test19()
{   bool b;

    b = cast(bool)x19;
    assert(b == true);
}

/**************************************/

class A20
{
  int abc() { return 3; }

  alias abc def;
}

void test20()
{
    int i;
    A20 a = new A20();

    i = a.def();
    assert(i == 3);
}


/**************************************/

void test21()
{
    string s;
    s = 1 ? "a" : "b";
    assert(s == "a");
}

/**************************************/

class Foo22
{
}

class Bar22 : Foo22
{
}

class Abc22
{
    Foo22 test() { return null; }
}

class Def22 : Abc22
{
    override Bar22 test() { return new Bar22; }
}

void testx22(Abc22 a)
{
    assert(a.test() !is null);
}

void test22()
{
    Def22 d = new Def22();

    testx22(d);
}

/**************************************/

struct foo23
{
   static struct bar
   {
      int x;
   }
}

void test23()
{
   //printf ("%d\n", foo23.bar.sizeof);
   assert(foo23.bar.sizeof == int.sizeof);
}


/**************************************/

void test24()
{
  struct Test
  {
    int i;

    bool bar(int a)
    {
      i = a;
      return true;
    }
  }

  Test t;
  assert(t.bar(3));
}


/**************************************/

void test25()
{
    {   const int [] list = [ 1, 2 ];
      assert(list[0] == 1);
      assert(list[1] == 2);
    }

    {   const int [] list = [ 3, 4 ];
      assert(list[0] == 3);
      assert(list[1] == 4);
    }
}


/**************************************/

void test26()
{
   while (0)
   {
      int x;
   }

   while (0)
   {
      int x;
   }
}


/**************************************/

struct NODE27 {
    int data;
    shared(NODE27) *next;
}

static shared NODE27 nodetbl[3] =
[
    {   0,cast(shared(NODE27)*)nodetbl + 1},
    {   0,cast(shared(NODE27)*)nodetbl + 2},
    {   0,null}
];

static shared NODE27 nodetbl2[3] = [
    {   0,&nodetbl2[1]},
    {   0,&nodetbl2[2]},
    {   0,null}
];

void test27()
{
}


/**************************************/

class Foo28
{
    protected int x;

    static class Bar
    {
       Foo28 f;

       int method () { return f.x; }
    }
}

void test28()
{
}


/**************************************/

void test29()
{
  int[immutable(byte)[]] foo;

  static immutable(byte)[] bar = [ 65, 66, 67 ];

  foo[bar] = 1;
  assert(foo[bar] == 1);
}

/**************************************/

class A30
{
   static class Child
   {
   }
}


class B30
{
   static class Child
   {
      static int value = 6;
   }
}

void test30()
{
   printf ("%d\n", B30.Child.value);
   assert(B30.Child.value == 6);
}


/**************************************/

void test31()
{
    float b;
    b -= 1.0;
    b += 1.0;
}

/**************************************/

class Foo32
{
    struct Bar
    {
        int x;
    }
}

void test32()
{
    with (new Foo32)
    {
        Bar z;
        z.x = 5;
    }
}


/**************************************/

string[2][] foo33;

void test33()
{
    string[2] bar;

    bar[1] = "hello";
    foo33 ~= bar;
    assert(foo33[0][1] == "hello");
}


/**************************************/


void test34()
{
 try {
  int i = 0;
  printf( "i:%d\n", i );
 } finally {
  printf( "Done\n" );
 }
 try {
  int i = 1;
  printf( "i:%d\n", i );
 } finally {
  printf( "Done\n" );
 }
}


/**************************************/

class Bar35 {}

template Foo35( T )
{
    void func() { };
}

void test35()
{
 try {
  alias Foo35!( Bar35 ) filter;
 } catch (Exception e) {
  printf( "Exception %.*s", e.msg.length, e.msg.ptr );
 } finally {
  printf( "Done0." );
 }
}


/**************************************/

void test36()
{
    enum {A=1}
    enum {B=A?0:1}
    assert(A == 1);
    assert(B == 0);
}

/**************************************/

struct A37
{
    int a;
}

struct B37
{
    int a;
    int b;
}

struct C37
{
    int a;
    int b;
    int c;
}

struct D37
{
    byte a,b,c;
}

void test37()
{
    A37 a;
    B37 b;
    C37 c;
    D37 d;

    assert(a.a == 0);
    assert(b.a == 0 && b.b == 0);
    assert(c.a == 0 && c.b == 0 && c.c == 0);
    assert(d.a == 0 && d.b == 0 && d.c == 0);
}


/**************************************/

int function() fp18;

extern(Windows) int func18()
{
    static int otherfunc()
    {   return 18; }

    fp18 = &otherfunc;
    return fp18();
}

void test38()
{
    assert(func18() == 18);
}


/**************************************/

class bar39
{
  struct _sub
  {
    bool a;
    string d;
  };
  _sub mySub;
};

class foo39
{
  bar39._sub[] subArray;

  this(bar39[] arr)
  {
    for(int i=0; i<arr.length; i++)
      subArray ~= arr[i].mySub;
  };
};


void test39()
{
}


/**************************************/

void test40()
{
    void* h;

    h = h.init;
    assert(h == cast(void*)0);
}

/**************************************/

int test41()
{
 label:
 int foo;
 foo = 0;
 return foo;
}


/**************************************/

struct A42
{
    invariant()
    {
    }

    B42 *findPool()
    {
        return null;
    }

}

struct B42
{
    int cmp(B42 *p2)
    {
        return 0;
    }
}

void test42()
{
}


/**************************************/

void test43()
{
    real a = 0.9;
    ulong b = cast(ulong) a;
    printf("%u", cast(uint) b);
    assert(cast(uint) b == 0);

    int c = cast(int) a;
    printf("%i", c);
    assert(c == 0);
}


/**************************************/

void test44()
{
   switch("asdf")
   {
   case "asdf":
     printf("asdf\n");
     break;


   case "jkl":
     printf("jkl\n");
     assert(0);
     break;


   default:
     printf("default\n");
     assert(0);
   }
}


/**************************************/

void func45(string a)
{
    assert(a.length == 5);
}

void test45()
{
    char[5] foo;

    foo[] = "hello";
    printf("'%.*s'\n", foo.length, foo.ptr);
    func45(cast(string)foo);
}

/**************************************/

struct Foo46
{
    int x;
}

void test46()
{
    Foo46 f;

    with (f)
    {
        x = 3;
    }
    assert(f.x == 3);
}

/**************************************/

struct Bar48
{
    uint k;
    ubyte m;
}

Bar48 makebar48() { Bar48 b; return b; }

void test48()
{
    Bar48 b = makebar48();
}

/**************************************/

void testx49() { printf("testx49()\n"); }

void test49() { return testx49(); }

/**************************************/

int testx50() { printf("testx50()\n"); return 3; }

void test50() { return cast(void)testx50(); }

/**************************************/

class A51
{
    static typeof(this) foo()
    {
        return new A51();
    }

    this()
    {
        bar = 3;
    }

    int bar;
}

class B51 : A51
{
    static typeof(super) b;
}

struct C51
{
    typeof(&this) x;
}

void test51()
{
    A51 a = A51.foo();
    assert(a.bar == 3);

    B51.b = a;
    assert(B51.b.bar == 3);
    assert(B51.b.classinfo == A51.classinfo);

    C51 c;
    c.x = &c;
}


/**************************************/

class A52
{
    char get() { return 'A'; }

    char foo() { return typeof(this).get(); }
    char bar() { return A52.get(); }
}

class B52 : A52
{
    override char get() { return 'B'; }
}

void test52()
{
    B52 b = new B52();

    assert(b.foo() == 'A');
    assert(b.bar() == 'A');
    assert(b.get() == 'B');
}

/**************************************/

struct A53
{
    int b() { return 1; }
}

int x53()
{
    A53 a;
    return a.b;
}

void test53()
{
    assert(x53() == 1);
}

/**************************************/

class A54
{
    void a()
    {
        printf("A54.a\n");
    }

    void b()
    {
        typeof(this).a();
    }
}

class B54 : A54
{
    override void a()
    {
        printf("B54.a\n");
        assert(0);
    }
}

void test54()
{
    B54 b = new B54();

    b.b();
}


/**************************************/

int foo55(int x = 5)
{
    printf("x = %d\n", x);
    return x;
}

void test55()
{   int i;

    i = foo55(6);
    assert(i == 6);
    i = foo55();
    assert(i == 5);
}

/**************************************/

class A56
{
    int foo(int x = 5)
    {
        printf("A56.x = %d\n", x);
        return x;
    }
}

class B56 : A56
{
    override int foo(int x = 7)
    {
        printf("B56.x = %d\n", x);
        return x;
    }
}


void test56()
{   int i;
    B56 b = new B56();

    i = b.foo(6);
    assert(i == 6);
    i = b.foo();
    assert(i == 7);
}


/**************************************/

void test57()
{
    char c;
    wchar w;
    dchar d;

    printf("c = %x, w = %x, d = %x\n", c, w, d);
    assert(c == 0xFF);
    assert(w == 0xFFFF);
    assert(d == 0xFFFF);
}

/**************************************/

void test58()
{
    static int x;

    static class S
    {
        static this()
        {
            printf ("static constructor\n");
            x = 10;
        }

        this()
        {
            printf ("class constructor\n");
        }
    }

    assert(x == 10);
    new S;
}

/**************************************/

struct S61 {
    int a, b, c, d;
}

void rec(int n, S61 t)
{
 if (n > 0) {
  t.b++;
  rec(n-1,t);
 }
}

void test61()
{
    S61 F;

    rec(100, F);
}

/**************************************/

class A62
{
    static A62 test(int q=0) {
        return null;
    }
}

A62 foo62()
{
    return A62.test;
}

void test62()
{
    foo62();
}


/**************************************/

class A63
{
     private import std.file;
     alias std.file.getcwd getcwd;
}

void test63()
{
     A63 f = new A63();
     auto s = f.getcwd();
     printf("%.*s\n", s.length, s.ptr);
}


/**************************************/

debug = 3;

void test64()
{
    debug(5)
    {
        assert(0);
    }
    debug(3)
    {
        int x = 3;
    }
    assert(x == 3);
}

/**************************************/

version = 3;

void test65()
{
    version(5)
    {
        assert(0);
    }
    version(3)
    {
        int x = 3;
    }
    assert(x == 3);
}

/**************************************/
// 8809

void test8809()
{
    static class B
    {
        char foo() { return 'B'; }
    }
    static class C : B
    {
        char test1Bx() { return B.foo(); }
        char test1Cx() { return C.foo(); }
        char test1Dx() { return   foo(); }
        char test1By() { return this.B.foo(); }
        char test1Cy() { return this.C.foo(); }   // cannot compile -> OK
        char test1Dy() { return this.  foo(); }
        char test1Bz() { return typeof(super).foo(); }
        char test1Cz() { return typeof(this). foo(); }
      //char test1Dz();

        char test2Bx() { return { return B.foo(); }(); }
        char test2Cx() { return { return C.foo(); }(); }
        char test2Dx() { return { return   foo(); }(); }
        char test2By() { return { return this.B.foo(); }(); }
        char test2Cy() { return { return this.C.foo(); }(); }   // cannot compile -> OK
        char test2Dy() { return { return this.  foo(); }(); }
        char test2Bz() { return { return typeof(super).foo(); }(); }
        char test2Cz() { return { return typeof(this). foo(); }(); }
      //char test2Dz();

        char test3Bx() { return (new class Object { char bar() { return B.foo(); } }).bar(); }
        char test3Cx() { return (new class Object { char bar() { return C.foo(); } }).bar(); }
        char test3Dx() { return (new class Object { char bar() { return   foo(); } }).bar(); }

        override char foo() { return 'C'; }
    }
    static class D : C
    {
        override char foo() { return 'D'; }
    }

    C c = new D();

    assert(c.test1Bx() == 'B');
    assert(c.test1Cx() == 'C');
    assert(c.test1Dx() == 'D');
    assert(c.test1By() == 'B');
    assert(c.test1Cy() == 'C');
    assert(c.test1Dy() == 'D');
    assert(c.test1Bz() == 'B'); // NG('D') -> OK
    assert(c.test1Cz() == 'C');
  //assert(c.test1Dz() == 'D');

    assert(c.test2Bx() == 'B'); // NG('D') -> OK
    assert(c.test2Cx() == 'C'); // NG('D') -> OK
    assert(c.test2Dx() == 'D');
    assert(c.test2By() == 'B');
    assert(c.test2Cy() == 'C');
    assert(c.test2Dy() == 'D');
    assert(c.test2Bz() == 'B'); // NG('D') -> OK
    assert(c.test2Cz() == 'C'); // NG('D') -> OK
  //assert(c.test2Dz() == 'D');

    assert(c.test3Bx() == 'B'); // NG('D') -> OK
    assert(c.test3Cx() == 'C'); // NG('D') -> OK
    assert(c.test3Dx() == 'D');
}

/**************************************/
// 9734

void test9734()
{
    class C {}
    class D : C
    {
        static bool test(C) { return true; }

        void foo()() if (is(typeof(test(super)))) {}
        void bar()() if (is(typeof(super) == C)) {}
    }
    void baz()() if (is(typeof(super))) {}

    auto d = new D();
    d.foo();
    d.bar();
    static assert(!__traits(compiles, baz()));
}

/**************************************/

int main(string[] argv)
{
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
    test61();
    test62();
    test63();
    test64();
    test65();
    test8809();
    test9734();

    printf("Success\n");
    return 0;
}


