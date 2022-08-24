/* REQUIRED_ARGS: -preview=bitfields
 */

struct S
{
    int a:2, b:4;
}

static assert(S.sizeof == 4);

void test1()
{
    S s;
    s.a = 3;
    assert(s.a == -1);

    s.b = 4;
    assert(s.b == 4);
}

/******************************************/

struct S2
{
    uint a:2, b:4;
}

S2 foo()
{
    S2 s = { 7, 8 };     // test struct literal expressions
    return s;
}

void test2()
{
    S2 s = foo();

    assert(s.a == 3);
    assert(s.b == 8);
}

/******************************************/

struct S3
{
    int i1;
    uint a:2, b:4, c:6;
    int i2;
}

static assert(S3.sizeof == 12);

S3 s3 = { 63, 7, 8 };

void test3()
{
    assert(s3.i1 == 63);
    assert(s3.a == 3);
    assert(s3.b == 8);
    assert(s3.c == 0);
    assert(s3.i2 == 0);
}

/******************************************/

struct S4
{
    int i1;
    uint a:2, b:31;
}

static assert(S4.sizeof == 12);

S4 s4 = { 63, 7, 8 };

void test4()
{
    assert(s4.i1 == 63);
    assert(s4.a == 3);
    assert(s4.b == 8);
}

/******************************************/

struct S5
{
    int i1;
    uint a:2, :0, b:5;
}

static assert(S5.sizeof == 12);

S5 s5 = { 63, 7, 8 };

void test5()
{
    assert(s5.i1 == 63);
    assert(s5.a == 3);
    assert(s5.b == 8);
}

/******************************************/

// https://issues.dlang.org/show_bug.cgi?id=22710

struct S6
{
    uint a:2, b:2;
}

int boo6()
{
    S s;
    s.a = 3;
    s.b = 1;
    s.a += 2;
    return s.a;
}

void test6()
{
    //printf("res: %d\n", test());
    assert(boo6() == 1);
}

/******************************************/

// https://issues.dlang.org/show_bug.cgi?id=22710

struct S7
{
    uint a:2, b:2;
    int c:2, d:2;
}

int test7u()
{
    S7 s;
    s.a = 7;
    s.b = 1;
    s.a += 2;
    return s.a;
}

int test7s()
{
    S7 s;
    s.c = 7;
    s.d = 1;
    s.c += 4;
    return s.c;
}

int test7s2()
{
    S7 s;
    s.c = 7;
    s.d = 2;
    s.c += 4;
    return s.d;
}

void test7()
{
    //printf("uns: %d\n", test7u());
    assert(test7u() == 1);
    //printf("sig: %d\n", test7s());
    assert(test7s() == -1);
    assert(test7s2() == -2);
}

static assert(test7u() ==  1);
static assert(test7s() == -1);
static assert(test7s2() == -2);

/******************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();

    return 0;
}
