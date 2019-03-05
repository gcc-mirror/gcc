// PERMUTE_ARGS:

extern(C) int printf(const char*, ...);

/***********************************/

enum : float
{
    E1a,
    E1b,
    E1c
}

void test1()
{
    assert(E1a == 0.0f);
    assert(E1b == 1.0f);
    assert(E1c == 2.0f);
}

/***********************************/

enum : string
{
    E2a = "foo",
    E2b = "bar",
    E2c = "abc"
}

void test2()
{
    assert(E2a == "foo");
    assert(E2b == "bar");
    assert(E2c == "abc");
}

/***********************************/

enum E3 : string
{
    E3a = "foo",
    E3b = "bar",
    E3c = "abc"
}

void test3()
{
    printf("%.*s\n", E3.E3a.length, E3.E3a.ptr);

    assert(E3.E3a == "foo");
    assert(E3.E3b == "bar");
    assert(E3.E3c == "abc");
}

/***********************************/

enum E4 : char
{
    Tvoid     = 'v',
    Tbool     = 'b',
}

void test4()
{
    E4 m;
}

/***********************************/

enum E5 : byte
{
    e1,
    e2
}

void test5()
{
    E5 m;
}

/***********************************/

enum : ubyte
{
    REend,
    REchar,
    REichar,
    REdchar,
    REidchar,
    REanychar,
}

void foo6(ubyte) { }
void foo6(int) { assert(0); }

void test6()
{
    foo6(REchar);
}

/***********************************/

enum
{
        foo7 = 1,
        long bar7 = 2,
        abc7,
}

enum x7 = 3;

void test7()
{
    assert(x7 == 3);
    assert(is(typeof(foo7) == int));
    assert(is(typeof(bar7) == long));
    assert(is(typeof(abc7) == long));
    assert(abc7 == 3L);
}

/***********************************/

enum E8 : real { a, b }

/***********************************/

struct S7379
{

    enum ENUM
    {
        M1,
        M2,
        M3
    }
    alias ENUM this;
}

class C7379
{
    this(S7379 test)
    {
    }

    this(string test)
    {
        this(S7379());
    }
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

    printf("Success\n");
    return 0;
}
