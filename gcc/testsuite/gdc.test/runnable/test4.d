// PERMUTE_ARGS:
// REQUIRED_ARGS:

import core.exception;
import core.stdc.math;
import core.vararg;

extern(C)
{
    int atoi(const char*);
    int memcmp(const void*, const void*, size_t);
    int printf(const char*, ...);
}

int cmp(const(char)[] s1, const(char)[] s2)
{
    assert(s1.length == s2.length);

    return memcmp(s1.ptr, s2.ptr, s1.length);
}

/* ================================ */

void test1()
{
    int i;

    int[6] foo = 1;
    for (i = 0; i < foo.length; i++)
        assert(foo[i] == 1);

    int[10] bar;
    for (i = 0; i < bar.length; i++)
        assert(bar[i] == 0);

    foo[3] = 4;
    int[6] abc = foo;
    for (i = 0; i < abc.length; i++)
        assert(abc[i] == foo[i]);

    abc[2] = 27;
    foo[] = abc;
    for (i = 0; i < abc.length; i++)
        assert(abc[i] == foo[i]);
}

/* ================================ */

void test2()
{
    byte foo1[5];
    ubyte foo2[6];
    short foo3[7];
    ushort foo4[8];
    int foo5[9];
    uint foo6[10];
    long foo7[11];
    ulong foo8[12];
    float foo9[13];
    double foo10[14];
    real foo11[15];

    int i;

    printf("test2()\n");
    for (i = 0; i < foo1.length; i++)
        assert(foo1[i] == 0);
    for (i = 0; i < foo2.length; i++)
    {   printf("foo2[%d] = %d\n", i, foo2[i]);
        assert(foo2[i] == 0);
    }
    for (i = 0; i < foo3.length; i++)
        assert(foo3[i] == 0);
    for (i = 0; i < foo4.length; i++)
        assert(foo4[i] == 0);
    for (i = 0; i < foo5.length; i++)
    {
        printf("foo5[%d] = %d\n", i, foo5[i]);
        assert(foo5[i] == 0);
    }
    for (i = 0; i < foo6.length; i++)
        assert(foo6[i] == 0);
    for (i = 0; i < foo7.length; i++)
        assert(foo7[i] == 0);
    for (i = 0; i < foo8.length; i++)
        assert(foo8[i] == 0);
    for (i = 0; i < foo9.length; i++)
        assert(isnan(foo9[i]));
    for (i = 0; i < foo10.length; i++)
        assert(isnan(foo10[i]));
    for (i = 0; i < foo11.length; i++)
        assert(isnan(foo11[i]));
}

/* ================================ */

void test3()
{
    byte foo1[5] = 20;
    ubyte foo2[6] = 21;
    short foo3[7] = 22;
    ushort foo4[8] = 23;
    int foo5[9] = 24;
    uint foo6[10] = 25;
    long foo7[11] = 26;
    ulong foo8[12] = 27;
    float foo9[13] = 28;
    double foo10[14] = 29;
    real foo11[15] = 30;

    int i;

    for (i = 0; i < foo1.length; i++)
        assert(foo1[i] == 20);
    for (i = 0; i < foo2.length; i++)
        assert(foo2[i] == 21);
    for (i = 0; i < foo3.length; i++)
        assert(foo3[i] == 22);
    for (i = 0; i < foo4.length; i++)
        assert(foo4[i] == 23);
    for (i = 0; i < foo5.length; i++)
        assert(foo5[i] == 24);
    for (i = 0; i < foo6.length; i++)
        assert(foo6[i] == 25);
    for (i = 0; i < foo7.length; i++)
        assert(foo7[i] == 26);
    for (i = 0; i < foo8.length; i++)
        assert(foo8[i] == 27);
    for (i = 0; i < foo9.length; i++)
        assert(foo9[i] == 28);
    for (i = 0; i < foo10.length; i++)
        assert(foo10[i] == 29);
    for (i = 0; i < foo11.length; i++)
        assert(foo11[i] == 30);
}

/* ================================ */

struct a4
{
    string b = "string";
    int c;
}

void test4()
{
    a4 a;
    int i;

    assert(a.b.length == 6);
    i = cmp(a.b, "string");
    assert(i == 0);

    a4[3] c;
    int j;
    for (j = 0; j < 3; j++)
    {
        assert(c[j].b.length == 6);
        i = cmp(c[j].b, "string");
        assert(i == 0);
    }
}


/* ================================ */

float f5;
const char[4] x5 = "abcd";

struct a5
{
    string b = "string";
    int c;
}

a5[5] foo5;

void test5()
{
    printf("test5()\n");
    assert(isnan(f5));

    assert(cmp(x5, "abcd") == 0);

    int i;
    for (i = 0; i < 5; i++)
    {
        assert(foo5[i].c == 0);
        assert(cmp(foo5[i].b, "string") == 0);
    }
}

/* ================================ */

struct TRECT6
{
    int foo1 = 2;

    union {
      struct
      {
        int Left = 3, Top = 4, Right = 5, Bottom = 6;
      }
      struct
      {
        long TopLeft, BottomRight;
      }
    }

    int foo2 = 7;
}

void test6()
{
    TRECT6 t;

    assert(t.foo1   == 2);
    assert(t.Left   == 3);
    assert(t.Top    == 4);
    assert(t.Right  == 5);
    assert(t.Bottom == 6);
    assert(t.foo2   == 7);
    assert(&t.foo1 < &t.Left);
    assert(&t.Bottom < &t.foo2);

    assert(TRECT6.foo1.offsetof == 0);
    static if (long.alignof == 8)
    {
        assert(TRECT6.Left.offsetof == 8);
        assert(TRECT6.Top.offsetof == 12);
        assert(TRECT6.Right.offsetof == 16);
        assert(TRECT6.Bottom.offsetof == 20);
        assert(TRECT6.TopLeft.offsetof == 8);
        assert(TRECT6.BottomRight.offsetof == 16);
        assert(TRECT6.foo2.offsetof == 24);
    }
    else
    {
        assert(TRECT6.Left.offsetof == 4);
        assert(TRECT6.Top.offsetof == 8);
        assert(TRECT6.Right.offsetof == 12);
        assert(TRECT6.Bottom.offsetof == 16);
        assert(TRECT6.TopLeft.offsetof == 4);
        assert(TRECT6.BottomRight.offsetof == 12);
        assert(TRECT6.foo2.offsetof == 20);
    }
}

/* ================================ */

struct TestVectors
{
    string pattern;
    string input;
    string result;
    string format;
    string replace;
};

TestVectors tva[] =
[
  {  pattern:"(a)\\1",  input:"abaab",  result:"y",     format:"&",     replace:"aa" },
  {  pattern:"abc",     input:"abc",    result:"y",     format:"&",     replace:"abc" },
];

TestVectors tvs[2] =
[
  {  pattern:"(a)\\1",  input:"abaab",  result:"y",     format:"&",     replace:"aa" },
  {  pattern:"abc",     input:"abc",    result:"y",     format:"&",     replace:"abc" },
];

TestVectors* tvp =
[
  {  pattern:"(a)\\1",  input:"abaab",  result:"y",     format:"&",     replace:"aa" },
  {  pattern:"abc",     input:"abc",    result:"y",     format:"&",     replace:"abc" },
];

void test7()
{
    int i;

    //printf("start\n");
    //printf("%d\n", tva[0].pattern.length);
    //printf("%.*s\n", tva[0].pattern.length, tva[0].pattern.ptr);

    i = cmp(tva[0].pattern, "(a)\\1");
    assert(i == 0);
    i = cmp(tva[1].replace, "abc");
    assert(i == 0);

    i = cmp(tvs[0].pattern, "(a)\\1");
    assert(i == 0);
    i = cmp(tvs[1].replace, "abc");
    assert(i == 0);

    i = cmp(tvp[0].pattern, "(a)\\1");
    assert(i == 0);
    i = cmp(tvp[1].replace, "abc");
    assert(i == 0);

    //printf("finish\n");
}

/* ================================ */

const uint WSABASEERR            = 10000;
const uint WSAENOTCONN           = (WSABASEERR+57);

void test8()
{
 switch (10057)
 {
  case WSAENOTCONN:
   break;
  default:
   assert(0);
 }
}

/* ================================ */


alias T9* PPixel;
align(1) struct TAG
{
    int foo;
}
alias TAG T9;
alias TAG Pixel;


void func9(PPixel x)
{
}

void test9()
{
    Pixel p;
    func9(&p);
}

/* ================================ */


string[] colors10 = [ "red", "green", "blue" ];

void test10()
{
    printf("test10()\n");

    int i;

    i = cmp(colors10[0], "red");
    assert(i == 0);
}

/* ================================ */

const uint MAX_PATH1 = 260;
enum { MAX_PATH2 = 261 }

struct WIN32_FIND_DATA {
    char   cFileName1[MAX_PATH1];
    char   cFileName2[MAX_PATH2];
}

void test11()
{
}

/* ================================ */


interface IPersistent
{
 int store(Object);
 int retrieve(Object);
}

class Persistent: IPersistent
{
 int store(Object n) { return 1; }
 int retrieve(Object n) { return 2; }
}

void func12(IPersistent p)
{
    Object o = new Object();
    assert(p.store(o) == 1);
    assert(p.retrieve(o) == 2);
}

void test12()
{
    Persistent p = new Persistent();
    Object o = new Object();
    assert(p.store(o) == 1);
    assert(p.retrieve(o) == 2);

    func12(p);
}

/* ================================ */

class X13 { }

class A13
{
     X13 B(X13 x, out int i)
     {
        i = 666;
        return new X13;
     }

     X13 B(X13 x)
     {
        int j;
        return B(x, j);
     }
}

void test13()
{
    A13 a;
    X13 x;
    int i;

    a = new A13();
    x = a.B(x, i);
    assert(i == 666);
}

/* ================================ */

void foo14() { }

int testx14(int x)
{
    try
    {
        return 3;
    }
    finally
    {
        foo14();
    }
}

class bar
{
    int y;
    synchronized int sync(int x)
    {
        printf("in sync(%d) = %d\n", x, y + 3);
        return y + 3;
    }
}

void test14()
{
    auto b = new shared(bar)();
    int i;
    i = b.sync(4);
    printf("i = %d\n", i);
    assert(i == 3);

    assert(testx14(7) == 3);
}

/* ================================ */

/+
int foo15(int i)
{
    switch (i)
    {
        case 7:
        case 8:
            return i;
    }
    return 0;
}

void test15()
{
    int i = 0;
    try
    {
        foo15(3);
    }
    catch (SwitchError sw)
    {
        //printf("caught switch error\n");
        i = 1;
    }
    assert(i == 1);
}
+/

/* ================================ */


struct GUID {          // size is 16
    align(1):
        uint    Data1;
        ushort  Data2;
        ushort  Data3;
        ubyte   Data4[8];
}


GUID CLSID_Hello = { 0x30421140, 0, 0, [0xC0,0,0,0,0,0,0,0x46] };
GUID IID_IHello  = { 0x00421140, 0, 0, [0xC0,0,0,0,0,0,0,0x46] };

int testa()
{
    return CLSID_Hello == IID_IHello;
}

int testb()
{
    return CLSID_Hello != IID_IHello;
}

int testc()
{
    return CLSID_Hello == IID_IHello ? 5 : 3;
}

int testd()
{
    return CLSID_Hello != IID_IHello ? 7 : 9;
}

void test16()
{
    assert(testa() == 0);
    assert(testb() == 1);
    assert(testc() == 3);
    assert(testd() == 7);
}


/* ================================ */

void test17()
{
    creal z = 1. + 2.0i;

    real r = z.re;
    assert(r == 1.0);

    real i = z.im;
    assert(i == 2.0);

    assert(r.im == 0.0);
    assert(r.re == 1.0);

    assert(i.re == 2.0);
    assert(i.im == 0.0i);
}

/* ================================ */

private const uint crc_table[256] = [
    0x00000000u, 0x77073096u, 0xee0e612cu, 0x990951bau, 0x076dc419u,
    0x2d02ef8du
  ];

public const(uint)[] get_crc_table()
{
  return crc_table;
}

void test18()
{
    const(uint)[] c;

    c = get_crc_table();
    assert(c[3] == 0x990951bau);
}

/* ================================ */

int[0xffff] foo19;

void test19()
{
    int i;

    for (i = 0; i < 0xffff; i++)
        assert(foo19[i] == 0);
}

/* ================================ */


extern (Windows) int cfw(int x, int y)
{
    return x * 10 + y;
}

extern (C) int cfc(int x, int y)
{
    return x * 10 + y;
}

int cfd(int x, int y)
{
    return x * 10 + y;
}


extern (Windows) int function (int, int) fpw;
extern (C) int function (int, int) fpc;
int function (int, int) fpd;

void test20()
{
    printf("test20()\n");
    int i;

    fpw = &cfw;
    fpc = &cfc;
    fpd = &cfd;

//printf("test w\n");
    i = (*fpw)(1, 2);
    assert(i == 12);

//printf("test c\n");
    i = (*fpc)(3, 4);
    assert(i == 34);

//printf("test d\n");
    i = (*fpd)(7, 8);
    assert(i == 78);
}


/* ================================ */

void test21()
{
    ireal imag = 2.5i;
    printf ("test of imag*imag = %Lf\n",imag*imag);
    assert(imag * imag == -6.25);
}

/* ================================ */

void test22()
{
    creal z1 = 1. - 2.0i;
    ireal imag_part = z1.im/1i;
}


/* ================================ */

int def23(int x, int y)
{
    return x * y;
}

struct Foo23
{
    int a = 7;
    int b = 8;
    int c = 9;

    int abc()
    {
        def23(3, 4);
        a *= b;
        bar();
        return a;
    }

    int bar()
    {
        a *= 2;
        return a;
    }

    invariant()
    {
        assert(c == 9);
    }
}

void test23()
{
    Foo23 f;
    int i;

    assert(f.a == 7 && f.b == 8);
    i = f.abc();
    assert(i == 112);
}

/* ================================ */

struct Foo24
{
        int x, y;
        int[] z;
}

void test24()
{
    assert(Foo24.z.offsetof == 8);
}

/* ================================ */

void test27()
{
    static real[1] n = [ -1 ];
    //printf("%Le\n", n[0]);
    assert(n[0] == -1.0);
}

/* ================================ */

int x29;

class Foo29
{
    ~this()
    {
        x29 = bar();
    }

    int bar()
    {
        return 3;
    }
}


void test29()
{
    printf("test29()\n");

    Foo29 f = new Foo29();

    delete f;
    assert(x29 == 3);
}

/* ================================ */


struct GC30
{
    static ClassInfo gcLock;

    invariant()
    {
    }

    void *malloc()
    {   void *p;

        synchronized (gcLock)
        {
            p = test();
            if (!p)
                return null;
        }
        return p;
    }

    void *test() { return null; }
}

void test30()
{
    printf("test30()\n");
    GC30 gc;

    GC30.gcLock = Object.classinfo;
    assert(gc.malloc() == null);
}


/* ================================ */

void test31()
{
    char[14] foo;
    char[] bar;
    int i = 3;

    while (i--)
        bar = foo;
}

/* ================================ */



real foo32()
{
    return 4 % 1.0;
}

void test32()
{
    assert(foo32() == 0);
}

/* ================================ */

void test33()
{
    char[8] foo;

    foo[] = "12345678";
    assert(foo[7] == '8');
}


/* ================================ */

void foo34(int[] a)
{
    //printf("a.length = %d\n", a.length);
    assert(a.length == 5);
    assert(a[0] == 11);
    assert(a[1] == 22);
    assert(a[2] == 33);
    assert(a[3] == 44);
    assert(a[4] == 55);
}

void test34()
{
    printf("test34()\n");

    static int[5] x = [11,22,33,44,55];
    int[] y;
    int* z;

    foo34(x);

    y = x;
    foo34(y);

    z = x.ptr;
    foo34(z[0..5]);
}

/* ================================ */

class X35
{
    final synchronized void foo()
    {
        for(;;)
        {
            break;
        }
    }
}

void test35()
{
    auto x = new shared(X35);

    x.foo();
}


/* ================================ */

void test36()
{
    synchronized
    {
    }
}

void test36b() @nogc nothrow
{
    synchronized
    {
    }
}

/* ================================ */
int test37()
{
       string one = "1";
       debug printf("pre\n");
       int N = atoi(one.ptr);
       assert(N == 1);
       return 0;
}


/* ================================ */

void test39()
{
        char[] array;
        array.length=4;
        char letter = 'a';
        array[0..4]=letter;
        assert(array[0]=='a');
        assert(array[1]=='a');
        assert(array[2]=='a');
        assert(array[3]=='a');
}

/* ================================ */

int dummyJob;

int dummy()
{
        return ++dummyJob;
}

void bar40(){
        return cast(void)dummy();
}

int foo40()
{
        bar40();
        return dummyJob-1;
}

void test40()
{
    assert(foo40() == 0);
}

/* ================================ */

int status;

void check()
{
        assert(status==1);
        void main(int dummy){
                assert(status==3);
                status+=5;
        }
        status+=2;
        assert(status==3);
        main(2);
        assert(status==8);
        status+=7;
}

void test41()
{
        status++;
        assert(status==1);
        check();
        assert(status==15);
}

/* ================================ */

void test42()
{
    real[10] array;
    real[] copy = array.dup;
}

/* ================================ */

void test43()
{
    string s;

    s = __FILE__; printf("file = '%.*s'\n", s.length, s.ptr);
    printf("line = %d\n", __LINE__);
    s = __DATE__; printf("date = '%.*s'\n", s.length, s.ptr);
    s = __TIME__; printf("time = '%.*s'\n", s.length, s.ptr);
    s = __TIMESTAMP__; printf("timestamp = '%.*s'\n", s.length, s.ptr);
}

/* ================================ */

void test44()
{
    int[] a;
    int i;

    a.length = 6;
    a = a[0 .. $ - 1];
    assert(a.length == 5);
}

/* ================================ */

alias int MyInt;

void test45()
{
        MyInt test(string c="x"){
                return 2;
        }
        assert(test("abc")==2);
}

/* ================================ */

int status46;

class Check46
{
        void sum(byte[] b){
                status46++;
        }

        void add(byte b){
                assert(0);
        }

        alias sum write;
        alias add write;

        void test(){
                byte[] buffer;
                write(buffer);
        }
}


void test46()
{
        Check46 c = new Check46();
        status46=0;
        assert(status46==0);
        c.test();
        assert(status46==1);
}

/* ================================ */

int status47;

int foo47(int arg)
{
loop:
    while(1)
    {
        try
        {
            try
            {
                if (arg == 1)
                {
                    break loop;
                }
            }
            finally
            {
                assert(status47==0);
                status47+=2;
            }

            try
            {
                assert(0);
            }
            finally
            {
                assert(0);
            }
        }
        finally
        {
            assert(status47==2);
            status47+=3;
        }
        assert(0);
        return 0;
    }
    return -1;
}

void test47()
{
        assert(status47 == 0);
        assert(foo47(1) == -1);
        assert(status47 == 5);
}

/* ================================ */

int status48;

int foo48(int arg)
{

loop:
    while(1){
        try{
            try{
                if(arg == 1)
                {
                    break loop;
                }
            }finally{
                assert(status48==0);
                status48+=2;
            }
        }finally{
            assert(status48==2);
            status48+=3;
        }
        return 0;
    }
    return -1;
}

void test48()
{
    assert(status48 == 0);
    assert(foo48(1) == -1);
    assert(status48 == 5);
}

/* ================================ */

int getch49()
{
        return 0;
}

void writefln49(...)
{
}

class Cout{
        Cout set(int x){
                return this;
        }
        alias set opShl;
}

void test49()
{
        Cout cout = new Cout;
        cout << 5 << 4;
        writefln49,getch49;
}

/* ================================ */

struct S50{
        int i;
}

class C50{
        static S50 prop(){
                S50 s;
                return s;
        }

        static void prop(S50 s){
        }
}

void test50()
{
        C50 c = new C50();
        c.prop = true ? C50.prop : C50.prop;
        assert(c.prop.i == 0);
        c.prop.i = 7;
        assert(c.prop.i != 7);
}

/* ================================ */

void func1()
{
        static class foo {
                public int a;
        }
}

void test51()
{
        static class foo {
                public int b;
        }

        foo bar = new foo();
        bar.b = 255;
}

/* ================================ */

struct S52
{
        int i;
}

const int a52 = 3;
const S52 b52 = { a52 };
const S52 c52 = b52;

void test52()
{
    assert(c52.i == 3);
}

/* ================================ */

const int c53 = b53 + 1;
const int a53 = 1;
const int b53 = a53 + 1;

void test53()
{
        assert(a53==1);
        assert(b53==2);
        assert(c53==3);
}

/* ================================ */
void test54()
{
        int status=0;

        try
        {
                try
                {
                        status++;
                        assert(status==1);
                        throw new Exception("first");
                }
                finally
                {
                        printf("finally\n");
                        status++;
                        assert(status==2);
                        status++;
                        throw new Exception("second");
                }
        }
        catch(Exception e)
        {
                printf("catch %.*s\n", e.msg.length, e.msg.ptr);
                assert(e.msg == "first");
                assert(e.next.msg == "second");
        }
        printf("success54\n");
}

/* ================================ */

void foo55()
{
    try
    {
        Exception x = new Exception("second");
        printf("inner throw %p\n", x);
        throw x;
    }
    catch (Exception e)
    {
        printf("inner catch %p\n", e);
        printf("e.msg == %.*s\n", e.msg.length, e.msg.ptr);
        assert(e.msg == "second");
        //assert(e.msg == "first");
        //assert(e.next.msg == "second");
    }
}

void test55()
{
        int status=0;
        try{
                try{
                        status++;
                        assert(status==1);
                        Exception x = new Exception("first");
                        printf("outer throw %p\n", x);
                        throw x;
                }finally{
                        printf("finally\n");
                        status++;
                        assert(status==2);
                        status++;
                        foo55();
                        printf("finally2\n");
                }
        }catch(Exception e){
                printf("outer catch %p\n", e);
                assert(e.msg == "first");
                assert(status==3);
        }
        printf("success55\n");
}

/* ================================ */

void test56()
{
    assert('\&sup2;'==178);
    assert('\&sup3;'==179);
    assert('\&sup1;'==185);
    assert('\&frac14;'==188);
    assert('\&frac12;'==189);
    assert('\&frac34;'==190);
    assert('\&there4;'==8756);
}

/* ================================ */

void test57()
{
  version (D_Bits)
  {
    void displayb(char[] name, bit[] x)
    {
        writef("%-5s: ", name);
        foreach(bit b; x)
            writef("%d",b);
        writefln("");
    }

    bit[] a;
    bit[] b;

    a.length = 7;
    a[0] = 0;
    a[1] = 1;
    a[2] = 1;
    a[3] = 0;
    a[4] = 0;
    a[5] = 1;
    a[6] = 0;

    displayb("a", a);

    b ~= a;
    displayb("b1", b);

    b ~= a;
    displayb("b2", b);

    for (int i = 0; i < a.length; i++)
    {
        assert(b[i] == a[i]);
        assert(b[i+7] == a[i]);
    }

    b.length = 0;
    b ~= a;
    displayb("b3", b);

    b.length = b.length + 7;
    for(int i = 0; i < a.length; i++)
        b[i+7] = a[i];
    displayb("b4", b);

    for (int i = 0; i < a.length; i++)
    {
        assert(b[i] == a[i]);
        assert(b[i+7] == a[i]);
    }
  }
}

/* ================================ */

interface Foo58
{
}

interface Bar58 : Foo58
{
}

class Baz58 : Bar58
{
}

Object test58()
{
    Bar58 b = new Baz58;
    return cast(Object)b;
}

/* ================================ */

float x59;

void test59()
{
    return cast(void)(x59 = -x59);
}


/* ================================ */


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
    //test15();
    test16();
    test17();
    test18();
    test19();
    test20();
    test21();
    test22();
    test23();
    test24();
//    test26();
    test27();
//    test28();
    test29();
    test30();
    test31();
    test32();
    test33();
    test34();
    test35();
    test36();
    test37();
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

    printf("Success\n");
    return 0;
}
