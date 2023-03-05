/*
TEST_OUTPUT:
---
runnable/test8.d(261): Deprecation: identity comparison of static arrays implicitly coerces them to slices, which are compared by reference
---
*/

module testxxx8;

import core.vararg;

extern(C)
{
    int atoi(const char*);
    int printf(const char*, ...);
    size_t strlen(const char*);
    version(Windows)
    {
        int _snprintf(char*, size_t, const char*, ...);
        alias _snprintf snprintf;
    }
    else
        int snprintf(char*, size_t, const char*, ...);
}

/***********************************/

struct Foo1
{
        static int x = 3;
        int y = 4;
}

void test1()
{
    Foo1 f;

    assert(Foo1.x == 3);
    assert(f.x == 3);
    assert(f.y == 4);
}

/***********************************/

class Foo2
{
        static int x = 5;
        int y = 6;
}

void test2()
{
    Foo2 f = new Foo2();

    assert(Foo2.x == 5);
    assert(f.x == 5);
    assert(f.y == 6);
}


/***********************************/

struct Foo3
{
        static int bar() { return 3; }
        int y = 4;
}

void test3()
{
    Foo3 f;

    assert(Foo3.bar() == 3);
    assert(f.bar() == 3);
}

/***********************************/

class Foo4
{
        static int bar() { return 3; }
        int y = 4;
}

void test4()
{
    Foo4 f = new Foo4();

    assert(Foo4.bar() == 3);
    assert(f.bar() == 3);
}


/***********************************/

struct Foo5
{
        int bar() { return y + 3; }
        int y = 4;
}

void test5()
{
    Foo5 f;

    assert(f.bar() == 7);
}

/***********************************/

class Foo6
{
        int bar() { return y + 3; }
        final int abc() { return y + 8; }
        int y = 4;
}

class FooX6 : Foo6
{
        override int bar() { return y + 5; }
}

void test6()
{
    Foo6 f = new FooX6();

    assert(f.bar() == 9);
    assert(f.abc() == 12);
}


/***********************************/

void bar7(char[3] cad)
{
    assert(cad.length == 3);
    printf("cad[0] = %d\n", cad[0]);
    assert(cad[0] == 0xFF);
    assert(cad[1] == 1);
    assert(cad[2] == 0xFF);
}

void test7()
{
    char[3] foo;

    foo[1] = 1;
    bar7(foo);
}


/***********************************/

class gap8
{
    this(char[3] cad)
    {
        assert(cad[0] == 0xFF);
        assert(cad[1] == 1);
        assert(cad[2] == 0xFF);
    }
}

void test8()
{
    char[3] foo;
    gap8 g;

    foo[1] = 1;
    g = new gap8(foo);
}


/***********************************/

class Foo11
{
  public:
    int a = 47;

  protected:
    int b;

  private:
    int c;

    int bar()
    {
        return a + b + c;
    }
}

class Bar11 : Foo11
{
    int abc()
    {
        return a + b;
    }
}

void test11()
{
    Foo11 f = new Foo11();

    int i = f.a;
    assert(i == 47);
}

/***********************************/

class A12
{
    protected void foo() { }
}

class B12: A12
{
    override void foo() { super.foo(); }
}

void test12()
{
}

/***********************************/

alias void *HWND;

const HWND hWnd = cast(HWND)(null);

void test13()
{
}

/***********************************/

string bar14()
{
    return "f";
}

char foo14()
{
    return bar14()[0];
}

void test14()
{
    char f = foo14();
    assert(f == 'f');
}


/***********************************/

void test15()
{
        char[30] a;
        char[30] b;

        assert(a !is b);
}

/***********************************/

void test16()
{
    static int function() fp = &func16;
    int i = fp();
    assert(i == 648);
}

int func16()
{
    return 648;
}


/***********************************/

string returnSameString(string inputstr)
{
    return inputstr;
}

string passString()
{
    return returnSameString("First string" ~ "Concatenated with second");
}

string butThisWorks()
{
    string s = "Third string";
    s = s ~ "Concatenated with fourth";
    return returnSameString(s);
}

void test17()
{
    string s;

    s = passString();
    printf("passString() = %.*s\n", cast(int)s.length, s.ptr);
    assert(s == "First stringConcatenated with second");

    s = butThisWorks();
    printf("butThisWorks() = %.*s\n", cast(int)s.length, s.ptr);
    assert(s == "Third stringConcatenated with fourth");
}

/***********************************/



class A20
{
    private:
        static int a;

    public:
        int foo(B20 j) { return j.b; }
}

class B20
{
    private:
        static int b;

    public:
        int bar(A20 j) { return j.a; }
}

void test20()
{
}

/***********************************/

alias int* IP;

void test21()
{
    int i = 5;
    IP ip = cast(IP) &i;
    assert(*ip == 5);
}

/***********************************/

struct RECT
{
    int    left = 1;
    int    top = 2;
    int    right = 3;
    int    bottom = 4;
}

struct Rect
{
  RECT theRect;
}


void Test(Rect pos)
{
    //printf("left = %d\n", pos.theRect.left);
    assert(pos.theRect.left == 1);
    assert(pos.theRect.top == 2);
    assert(pos.theRect.right == 3);
    assert(pos.theRect.bottom == 4);
}

class Window
{
  Rect position;

  void createWindow()
  {
    Test(position);
  }
}

void test22()
{
    Window w = new Window();
    w.createWindow();
}

/***********************************/

struct Size
{
  int width;
  int height;
}

Size computeSize()
{
  Size foo;

  foo.width = 12;
  foo.height = 34;

  printf("Inside: %d,%d\n",foo.width,foo.height);

  return foo;
}


void test24()
{
  Size bar;
  bar = computeSize();

  printf("Outside: %d,%d\n",bar.width,bar.height);
  assert(bar.width == 12);
  assert(bar.height == 34);
}

/***********************************/

void test25()
{   int i = 5;

    while (i)
    {
        break;
    }
}

/***********************************/

int test26()
in
{
}
out (result)
{
}
do
{   int i = 5;

    while (i)
    {
        break;
    }
    return i;
}

/***********************************/

class A27
{
    int a;

    this()
    {
        a = 1;
    }
}

class B27 : A27
{
}

class C27 : B27
{
    this()
    {
        super();
    }

    this(int i)
    {
    }
}

void test27()
{
    A27 a = new A27();
    assert(a.a == 1);

    B27 b = new B27();
    assert(b.a == 1);

    C27 c = new C27();
    assert(c.a == 1);

    C27 c2 = new C27(2);
    assert(c2.a == 1);
}


/***********************************/

const char[1] sep = '/';

string testx28(string s, string t)
{
    return cast(string)(s ~ sep ~ t);
}

void test28()
{
    string r;

    r = testx28("ab", "cd");
    assert(r == "ab/cd");
}

/***********************************/

void test29()
{
}


/***********************************/

bool func30(int x, int y)
{
    bool b;
    b|=(x==y);
    return b;
}

void test30()
{
    bool b;

    b = func30(1,1);
    assert(b == true);
    b = func30(1,2);
    assert(b == false);
}

/***********************************/

int a31;

void test31()
{
    testxxx8.a31 = 3;
    assert(a31 == 3);
}

/***********************************/

void test32()
{
    string[] foo;
    int i;

    foo = new string[45];
    for (i = 0; i < 45; i++)
        foo[i] = "hello";
    for (i = 0; i < 45; i++)
        assert(foo[i] == "hello");
}


/***********************************/

void test33()
{
    string[] foo;
    int i = 45;

    foo = new string[i];
    for (i = 0; i < 45; i++)
        foo[i] = "hello";
    for (i = 0; i < 45; i++)
        assert(foo[i] == "hello");
}


/***********************************/

void test34()
{
    int[3][4] a;
    int[5][6] b = 16;
    int i, j;

    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            assert(a[i][j] == 0);

    for (i = 0; i < 6; i++)
        for (j = 0; j < 5; j++)
            assert(b[i][j] == 16);
}

/***********************************/
// https://issues.dlang.org/show_bug.cgi?id=19178

float[3][4] arr2f = 10;
Int3_4[1] arr3i = 20;
short[3][4][1][1] arr4s = 30;

enum Int3 : int[3] {
    a = [0, 1, 2],
}

enum Int3_4 : Int3[4] {
    b = Int3[4].init,
}

struct S35
{
    int[3][3] arr = [2, 1];
}

void test35()
{
    for (int i = 0; i < 4; i++)
    {
        for (int j = 0; j < 3; j++)
        {
            // printf("[%d %d]: %f %d %d\n", i, j, arr2f[i][j], arr3i[0][i][j], arr4s[0][0][i][j]);
            assert(arr2f[i][j] == 10);
            assert(arr3i[0][i][j] == 20);
            assert(arr4s[0][0][i][j] == 30);
        }
    }

    S35 t = S35.init;
    assert(t.arr[0] == [2, 2, 2]);
    assert(t.arr[1] == [1, 1, 1]);
    assert(t.arr[2] == [0, 0, 0]);
}
/***********************************/

string itoa(int i)
{
    char[32] buffer;
    snprintf(buffer.ptr, 32, "%d", i);
    return buffer[0 .. strlen(buffer.ptr)].idup;
}

string testa36(int i, int j, string a, string b, string c)
{
    string s =  "string 0;" ~ itoa(i) ~
                "string 1;" ~ itoa(j) ~
                "string 2;" ~ itoa(i) ~
                "string 3;";

//    string s = a ~ b ~ c;
    return s;
}

void test36()
{
    string s = testa36(26, 47, "a", "b", "c");

    printf("s = '%.*s'\n", cast(int)s.length, s.ptr);
    assert(s == "string 0;26string 1;47string 2;26string 3;");
}

/***********************************/

void test37()
{
    string[ulong] x;
    ulong v1 = 297321415603;
    ulong v2 = 331681153971;
    x[v1] = "aa";
    printf( "%llx %llx\n", v1, v2 );
    assert(!(v2 in x));
}


/***********************************/

void test38()
{
    int n = atoi("1");
    static char[8192 + 1] flags;
    long i, k;
    int count = 0;

    try
    {
       while (n--)
       {
          count = 0;

          for (i = 2; i <= 8192; i++)
             flags[cast(size_t)i] = 1;

          for (i = 2; i <= 8192; i++)
          {
             if (flags[cast(size_t)i])
             {
                for (k = i+i; k <= 8192; k += i)
                   flags[cast(size_t)k] = 0;

                count++;
             }
          }
       }

       printf("Count: %d\n", count);
        assert(count == 1028);
    }
    catch(Throwable)
    {
       printf("Exception: %lld\n", k);
        assert(0);
    }
}


/***********************************/

interface I39
{
}

class C39 : I39
{
    int x = 432;
}

void test39()
{
    C39 c = new C39;

    printf("%p %d\n", c, c.x);
    assert(c.x == 432);
    printf("%p\n", cast(I39) c);
    c = cast(C39) cast(I39) c;
    printf("%p\n", c);
    assert(c !is null);
}


/***********************************/

void test40()
{
       Object x;

       x = null;
       x = 0 ? x : null;
       x = 0 ? null : x;
}

/***********************************/

int foo42(const(char) *x, ...)
{
    va_list ap;

    va_start!(typeof(x))(ap, x);
    assert(ap != va_list.init);

    int i;
    i = va_arg!(typeof(i))(ap);
    assert(i == 3);

    long l;
    l = va_arg!(typeof(l))(ap);
    assert(l == 23);

    uint k;
    k = va_arg!(typeof(k))(ap);
    assert(k == 4);

    va_end(ap);

    return cast(int)(i + l + k);
}

void test42()
{
    int j;

    j = foo42("hello", 3, 23L, 4);
    printf("j = %d\n", j);
    assert(j == 30);
}

/***********************************/

int x44;

class A44 {
     this() scope { printf("A44 ctor\n"); x44 += 1; }
     ~this() scope { printf("A44 dtor\n"); x44 += 0x100; }
}
class B44 : A44 { }

void foo44() { scope B44 b = new B44; }

void test44()
{
     printf("foo44...\n");
     foo44();
     printf("...foo44\n");
     assert(x44 == 0x101);
}

/***********************************/

/*
import std.stdarg;
import std.utf;

int unFormat( bool delegate( out dchar ) getc,
        bool delegate( dchar ) ungetc,
        TypeInfo[] arguments,
        void* argptr )
{
    size_t  arg = 0;
    dchar[] fmt;

    if( arguments[arg] is typeid( string ) )
        fmt = toUTF32( va_arg!(string)( argptr ) );
    else if( arguments[arg] is typeid( wchar[] ) )
        fmt = toUTF32( va_arg!(wchar[])( argptr ) );
    else if( arguments[arg] is typeid( dchar[] ) )
        fmt = va_arg!(dchar[])( argptr );
    else
        return 0;
}
*/

void test45()
{
}

/***********************************/

int sreadf( ... )
{
    va_arg!(string)( _argptr );
    return 0;
}


void test46()
{
    printf( "hello world\n" );
}

/***********************************/

void test48()
{
  try{
  }finally{
    debug(p48) { }
  }
}

/***********************************/

void test49()
{
  int k = 1;
  if(k == 0)
    debug{printf("test");}
}

/***********************************/

void test50()
{       int x;

        if (x)
             version (none)
                 foo;
}

/***********************************/

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
    test11();
    test12();
    test13();
    test14();
    test15();
    test16();
    test17();
    test20();
    test21();
    test22();
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
    test42();
    test44();
    test45();
    test46();
    test48();
    test49();
    test50();

    printf("Success\n");
    return 0;
}
