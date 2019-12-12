// REQUIRED_ARGS: -unittest

import std.algorithm: cmp;

extern(C) int printf(const char*, ...);

/* ================================ */

class Foo
{
    int foo(int x) { return x + 3; }
}

class Bar : Foo
{
    override int foo(int y) { return y + 4; }
}

void test1()
{
    Bar e;

    assert(e is null);
    e = new Bar();
    assert(e.foo(5) == 9);
}

/* ================================ */

class Foo2
{
    int foo(int x)
    {
        return x + 3;
    }
}

class Bar2 : Foo2
{
    override int foo(int y)
    {
        assert(Foo2.foo(2) == 5);
        return y + 4;
    }
}

void test2()
{
    Bar2 e;

    assert(e is null);
    e = new Bar2();
    assert(e.foo(5) == 9);
    assert(e.Foo2.foo(10) == 13);
}

/* ================================ */

void test3()
{
    debug printf("debug\n");
    debug(1) printf("debug(1)\n");
    debug(2) printf("debug(2)\n");
    debug(3) printf("debug(3)\n");
    debug(bar) printf("debug(bar)\n");
    debug(10) assert(0);

    debug(1)
    {
        int d1 = 3;

        printf("debug(1) { }\n");
    }
    debug(2)
    {
        printf("debug(2): d1 = %d\n", d1);
    }
}

/* ================================ */

int x1;
int x2;

class Foo4
{
    static  this() { x1 = 3; printf("Foo4 ctor()\n"); }
    static ~this() { x1 = 4; printf("Foo4 dtor()\n"); }
}

static  this() { x2 = 5; printf("ctor()\n"); }
static ~this() { x2 = 6; printf("dtor()\n"); }

void test4()
{
    printf("x1 = %d, x2 = %d\n", x1, x2);
    assert(x1 == 3);
    assert(x2 == 5);
}

/* ================================ */

void test5()
{
    version (D_Bits)
    {
    printf("test5()\n");
    static uint foo;
    static uint x = 3;
    static uint len = 32;

    bool[] bools;

    bools = (cast(bool *)&foo)[0..len];
    bools[6] = true;
    assert(foo == (1 << 6));
    }
}

/* ================================ */

int[] test6_1(int[] a)
{
    a.length = 6;
    return a;
}

void test6()
{
    printf("test6()\n");
    int b[3];
    int a[];

    b[0] = 0;
    b[1] = 1;
    b[2] = 2;
    assert(b.length == 3);
    a = test6_1(b);
    a[2] = 2;
    assert(a.length == 6);
}

/* ================================ */

class OutBuffer7
{
    char data[];
    uint offset;

    void write(const(char) *p, uint nbytes)
    {
        data[offset .. offset + nbytes] = (cast(char *)p)[0 .. nbytes];
    }
}


void test7()
{
    printf("test7()\n");
    int i;
    OutBuffer7 ob = new OutBuffer7;

    ob.data = new char[10];
    printf("ob.data.length = %d\n", ob.data.length);
    assert(ob.data.length == 10);
    for (i = 0; i < 10; i++)
        assert(ob.data[i] == char.init);

printf("test7.1()\n");
    ob.data[] = '-';
printf("test7.2()\n");
    printf("ob.data[] = '%.*s'\n", ob.data.length, ob.data.ptr);
    for (i = 0; i < 10; i++)
        assert(ob.data[i] == '-');

    ob.offset = 3;
    ob.write("foo", 3);
    printf("ob.data.length = %d\n", ob.data.length);
    printf("ob.data[] = '%.*s'\n", ob.data.length, ob.data.ptr);
    for (i = 0; i < 10; i++)
    {
        if (i < 3 || i >= 6)
            assert(ob.data[i] == '-');
    }
    assert(ob.data[3] == 'f');
    assert(ob.data[4] == 'o');
    assert(ob.data[5] == 'o');
}

/* ================================ */

class A8
{
    enum { bar = 8, baz }
    int foo;
}

void test8()
{
    printf("test8()\n");
    A8 a;
    a = new A8();
    a.foo = A8.bar;
    assert(a.foo == 8);
}

/* ================================ */


int z9;

unittest
{
    printf("module unittest 9\n");
    z9 = 3;
}


void test9()
{
    assert(z9 == 3);
}

/* ================================ */

void test10()
{
    printf("test10()\n");
    const int i = 8000;
    assert(i == 8000);
    static int j = 78;
    assert(j == 78);
}

/* ================================ */

Object test11_a()
{
    return null;
}

void test11()
{
    assert(test11_a() is null);
}

/* ================================ */

class A12 { }
class B12 { }

int testx(A12 a) { return 1; }

int testx(B12 b) { return 2; }

void test12()
{
    A12 a = new A12();
    B12 b = new B12();

    assert(testx(a) == 1);
    assert(testx(b) == 2);
}

/* ================================ */

char[] tolower13(ref char[] s)
{
    int i;

    for (i = 0; i < s.length; i++)
    {
        char c = s[i];
        if ('A' <= c && c <= 'Z')
            s[i] = cast(char)(c + (cast(char)'a' - 'A'));
    }
    return s;
}

void test13()
{
    char[] s1 = "FoL".dup;
    char[] s2;

    s1 = s1.dup;
    s2 = tolower13(s1);
    assert(cmp(s2, "fol") == 0);
    assert(s2 == s1);
}

/* ================================ */

alias ABC14* LPABC14;
class ABC14 { }

alias DEF14* LPDEF14;
DEF14[3] foo;
struct DEF14 { int x; }

void test14()
{
    assert(foo.sizeof == int.sizeof * 3);
}

/* ================================ */

class bools15
{
    bool a = true, b = true, c = true;
    void dump()
    {
        printf("%d %d %d\n", a, b, c);
    }
}

void test15()
{
     bools15 k = new bools15;
     k.a = true; k.dump();
     k.b = true; k.dump();
     k.c = true; k.dump();
     assert(k.a == true);
     assert(k.b == true);
     assert(k.c == true);
}


/* ================================ */

align(4) struct foo16
{
    short s;
    int i;
}

void test16()
{
    assert(foo16.sizeof == 8);
}

/* ================================ */

enum Color { red, blue, green };
int[Color.max+1] colors1 = [ Color.blue:6, Color.green:2, Color.red:5 ];


enum { red, blue, green };
int[3] colors2 = [ blue:6, green:2, red:5 ];

void test17()
{
    assert(colors1.length == 3);
    assert(colors1[0] == 5);
    assert(colors1[1] == 6);
    assert(colors1[2] == 2);
    assert(colors2[0] == 5);
    assert(colors2[1] == 6);
    assert(colors2[2] == 2);
}

/* ================================ */


alias void* HANDLE18;

HANDLE18 testx18()
{
    return null;
}

void test18()
{
    assert(testx18() is null);
}

/* ================================ */

class Test19 { struct { int a, b, c; } }

void test19()
{
    Test19 t = new Test19();

    t.a = 3;
    assert(t.a == 3);
}

/* ================================ */

bool tested20;

struct S20
{
    unittest
    {
        assert(!tested20);
        tested20 = true;
    }
}

void test20()
{
    assert(tested20);
}

/* ================================ */
// 7848

@safe pure nothrow void func7848() {}

@safe pure nothrow unittest
{
    func7848();
}

/* ================================ */
// 8128

int flag8128 = 0;

interface I8128
{
    unittest
    {
        printf("utest, flag8128 = %d\n", flag8128);
        flag8128 = 1;
    }
}

void test8128()
{
    printf("main, flag8128 = %d\n", flag8128);
    assert(flag8128 == 1);
}

/* ================================ */

class C8635{
        int x;
        this(int x)
        {
                this.x = x;
        }
}
void test8635()
{
        assert(new C8635(2).x==2);
        assert(new C8635(3).x==3);
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
    test15();
    test16();
    test17();
    test18();
    test19();
    test20();
    test8128();
    test8635();
    printf("Success\n");
    return 0;
}
