// PERMUTE_ARGS:

// Test memset style array assignments.

extern(C) int printf(const char*, ...);

/**************************************/

char[] b1;

char[] func1(int dim)
{
    char[] a = new char[dim];

    b1 = a;
    return a;
}

void test1()
{
    printf("test1()\n");
    int i;
    int j;

    char[1] a1;
    a1[] = 'x';
    assert(a1[0] == 'x');

    char[2] a2;
    a2[] = 'x';
    for (i = 0; i < a2.length; i++)
        assert(a2[i] == 'x');

    char[3] a3;
    a3[] = 'x';
    for (i = 0; i < a3.length; i++)
        assert(a3[i] == 'x');

    char[4] a4;
    a4[] = 'x';
    for (i = 0; i < a4.length; i++)
        assert(a4[i] == 'x');

    char[8] a8;
    a8[] = 'x';
    for (i = 0; i < a8.length; i++)
        assert(a8[i] == 'x');

    char[27] a27;
    a27[] = 'x';
    for (i = 0; i < a27.length; i++)
        assert(a27[i] == 'x');

    func1(33)[] = 'y';
    for (i = 0; i < b1.length; i++)
        assert(b1[i] == 'y');


    char[23] a23 = 'x';
    i = 16;
    j = 18;
    a23[i++..++j] = 'c';
    printf("i = %d, j = %d\n", i, j);
    assert(i == 17);
    assert(j == 19);
    assert(a23[15] == 'x');
    assert(a23[16] == 'c');
    assert(a23[17] == 'c');
    assert(a23[18] == 'c');
    assert(a23[19] == 'x');
    assert(a23[20] == 'x');
}

/**************************************/

short[] b2;

short[] func2(int dim)
{
    short[] a = new short[dim];

    b2 = a;
    return a;
}

void test2()
{
    printf("test2()\n");
    int i;
    int j;

    short[1] a1;
    a1[] = 0x1234;
    assert(a1[0] == 0x1234);

    short[2] a2;
    a2[] = 0x1234;
    for (i = 0; i < a2.length; i++)
        assert(a2[i] == 0x1234);

    short[3] a3;
    a3[] = 0x1234;
    for (i = 0; i < a3.length; i++)
        assert(a3[i] == 0x1234);

    short[4] a4;
    a4[] = 0x1234;
    for (i = 0; i < a4.length; i++)
        assert(a4[i] == 0x1234);

    short[8] a8;
    a8[] = 0x1234;
    for (i = 0; i < a8.length; i++)
        assert(a8[i] == 0x1234);

    short[27] a27;
    a27[] = 0x1234;
    for (i = 0; i < a27.length; i++)
        assert(a27[i] == 0x1234);

    func2(33)[] = 0x5678;
    for (i = 0; i < b2.length; i++)
        assert(b2[i] == 0x5678);


    short[23] a23 = 0x1234;
    i = 16;
    j = 18;
    a23[i++..++j] = 0x4554;
    printf("i = %d, j = %d\n", i, j);
    assert(i == 17);
    assert(j == 19);
    assert(a23[15] == 0x1234);
    assert(a23[16] == 0x4554);
    assert(a23[17] == 0x4554);
    assert(a23[18] == 0x4554);
    assert(a23[19] == 0x1234);
    assert(a23[20] == 0x1234);
}

/**************************************/

int[] b3;

int[] func3(int dim)
{
    int[] a = new int[dim];

    b3 = a;
    return a;
}

void test3()
{
    printf("test3()\n");
    int i;
    int j;

    int[1] a1;
    a1[] = 0x12345678;
    assert(a1[0] == 0x12345678);

    int[2] a2;
    a2[] = 0x12345678;
    for (i = 0; i < a2.length; i++)
        assert(a2[i] == 0x12345678);

    int[3] a3;
    a3[] = 0x12345678;
    for (i = 0; i < a3.length; i++)
        assert(a3[i] == 0x12345678);

    int[4] a4;
    a4[] = 0x12345678;
    for (i = 0; i < a4.length; i++)
        assert(a4[i] == 0x12345678);

    int[8] a8;
    a8[] = 0x12345678;
    for (i = 0; i < a8.length; i++)
        assert(a8[i] == 0x12345678);

    int[27] a27;
    a27[] = 0x12345678;
    for (i = 0; i < a27.length; i++)
        assert(a27[i] == 0x12345678);

    func3(33)[] = 0x56781234;
    for (i = 0; i < b3.length; i++)
        assert(b3[i] == 0x56781234);


    int[23] a23 = 0x12345678;
    i = 16;
    j = 18;
    a23[i++..++j] = 0x45546776;
    printf("i = %d, j = %d\n", i, j);
    assert(i == 17);
    assert(j == 19);
    assert(a23[15] == 0x12345678);
    assert(a23[16] == 0x45546776);
    assert(a23[17] == 0x45546776);
    assert(a23[18] == 0x45546776);
    assert(a23[19] == 0x12345678);
    assert(a23[20] == 0x12345678);
}

/**************************************/

long[] b4;

long[] func4(int dim)
{
    long[] a = new long[dim];

    b4 = a;
    return a;
}

void test4()
{
    printf("test4()\n");
    int i;
    int j;

    long[1] a1;
    a1[] = 0x123456789ABCDEF0;
    assert(a1[0] == 0x123456789ABCDEF0);

    long[2] a2;
    a2[] = 0x123456789ABCDEF0;
    for (i = 0; i < a2.length; i++)
        assert(a2[i] == 0x123456789ABCDEF0);

    long[3] a3;
    a3[] = 0x123456789ABCDEF0;
    for (i = 0; i < a3.length; i++)
        assert(a3[i] == 0x123456789ABCDEF0);

    long[4] a4;
    a4[] = 0x123456789ABCDEF0;
    for (i = 0; i < a4.length; i++)
        assert(a4[i] == 0x123456789ABCDEF0);

    long[8] a8;
    a8[] = 0x123456789ABCDEF0;
    for (i = 0; i < a8.length; i++)
        assert(a8[i] == 0x123456789ABCDEF0);

    long[27] a27;
    a27[] = 0x123456789ABCDEF0;
    for (i = 0; i < a27.length; i++)
        assert(a27[i] == 0x123456789ABCDEF0);

    func4(33)[] = 0x5678889911223344;
    for (i = 0; i < b4.length; i++)
        assert(b4[i] == 0x5678889911223344);


    long[23] a23 = 0x123456789ABCDEF0;
    i = 16;
    j = 18;
    a23[i++..++j] = 0x567888991122334B;
    printf("i = %d, j = %d\n", i, j);
    assert(i == 17);
    assert(j == 19);
    assert(a23[15] == 0x123456789ABCDEF0);
    assert(a23[16] == 0x567888991122334B);
    assert(a23[17] == 0x567888991122334B);
    assert(a23[18] == 0x567888991122334B);
    assert(a23[19] == 0x123456789ABCDEF0);
    assert(a23[20] == 0x123456789ABCDEF0);
}

/**************************************/

real[] b5;

real[] func5(int dim)
{
    real[] a = new real[dim];

    b5 = a;
    return a;
}

void test5()
{
    printf("test5()\n");
    int i;
    int j;

    real[1] a1;
    a1[] = 31234;
    assert(a1[0] == 31234);

    real[2] a2;
    a2[] = 31234;
    for (i = 0; i < a2.length; i++)
        assert(a2[i] == 31234);

    real[3] a3;
    a3[] = 31234;
    for (i = 0; i < a3.length; i++)
        assert(a3[i] == 31234);

    real[4] a4;
    a4[] = 31234;
    for (i = 0; i < a4.length; i++)
        assert(a4[i] == 31234);

    real[8] a8;
    a8[] = 31234;
    for (i = 0; i < a8.length; i++)
        assert(a8[i] == 31234);

    real[27] a27;
    a27[] = 31234;
    for (i = 0; i < a27.length; i++)
        assert(a27[i] == 31234);

    func5(33)[] = 35678;
    for (i = 0; i < b5.length; i++)
        assert(b5[i] == 35678);


    real[23] a23 = 31234;
    i = 16;
    j = 18;
    a23[i++..++j] = 34554e+4;
    printf("i = %d, j = %d\n", i, j);
    assert(i == 17);
    assert(j == 19);
    assert(a23[15] == 31234);
    assert(a23[16] == 34554e+4);
    assert(a23[17] == 34554e+4);
    assert(a23[18] == 34554e+4);
    assert(a23[19] == 31234);
    assert(a23[20] == 31234);
}

/**************************************/

struct ABC
{
    int a,b,c,d;
}

ABC abc1 = { a:1, b:2, c:3, d:4 };
ABC abc2 = { a:7, b:6, c:8, d:9 };
ABC abc3 = { a:5, b:10, c:11, d:12 };

ABC[] b6;

ABC[] func6(int dim)
{
    ABC[] a = new ABC[dim];

    b6 = a;
    return a;
}

void test6()
{
    printf("test6()\n");
    int i;
    int j;

    ABC[1] a1;
    a1[] = abc1;
    assert(a1[0] == abc1);

    ABC[2] a2;
    a2[] = abc1;
    for (i = 0; i < a2.length; i++)
        assert(a2[i] == abc1);

    ABC[3] a3;
    a3[] = abc1;
    for (i = 0; i < a3.length; i++)
        assert(a3[i] == abc1);

    ABC[4] a4;
    a4[] = abc1;
    for (i = 0; i < a4.length; i++)
        assert(a4[i] == abc1);

    ABC[8] a8;
    a8[] = abc1;
    for (i = 0; i < a8.length; i++)
        assert(a8[i] == abc1);

    ABC[27] a27;
    a27[] = abc1;
    for (i = 0; i < a27.length; i++)
        assert(a27[i] == abc1);

    func6(33)[] = abc2;
    for (i = 0; i < b6.length; i++)
        assert(b6[i] == abc2);


    ABC[23] a23 = abc1;
    i = 16;
    j = 18;
    a23[i++..++j] = abc3;
    printf("i = %d, j = %d\n", i, j);
    assert(i == 17);
    assert(j == 19);
    assert(a23[15] == abc1);
    assert(a23[16] == abc3);
    assert(a23[17] == abc3);
    assert(a23[18] == abc3);
    assert(a23[19] == abc1);
    assert(a23[20] == abc1);
}

/**************************************/

bool[] b7;

bool[] func7(int dim)
{
    bool[] a = new bool[dim];

    b7 = a;
    return a;
}

void test7()
{
    printf("test7()\n");
    int i;
    int j;

    bool[1] a1;
    a1[] = 1;
    assert(a1[0] == 1);

    bool[2] a2;
    a2[] = 1;
    for (i = 0; i < a2.length; i++)
        assert(a2[i] == 1);

    bool[3] a3;
    a3[] = 1;
    for (i = 0; i < a3.length; i++)
        assert(a3[i] == 1);

    bool[4] a4;
    a4[] = 1;
    for (i = 0; i < a4.length; i++)
        assert(a4[i] == 1);

    bool[8] a8;
    a8[] = 1;
    for (i = 0; i < a8.length; i++)
        assert(a8[i] == 1);

    bool[27] a27;
    a27[] = 1;
    for (i = 0; i < a27.length; i++)
        assert(a27[i] == 1);

    func7(33)[] = 1;
    assert(b7.length == 33);
    for (i = 0; i < b7.length; i++)
        assert(b7[i] == 1);

    func7(33)[3..6] = 1;
    //printf("b7.ptr = %p, b7.length = %d\n", b7.ptr, b7.length);
    assert(b7.length == 33);
    for (i = 0; i < b7.length; i++)
    {
        //printf("b7[%d] = %d\n", i, b7[i]);
        if (i >= 3 && i < 6)
            assert(b7[i] == 1);
        else
            assert(b7[i] == 0);
    }

    bool[23] a23 = 1;
    i = 16;
    j = 18;
    a23[i++..++j] = 0;
    printf("i = %d, j = %d\n", i, j);
    assert(i == 17);
    assert(j == 19);
    assert(a23[15] == 1);
    assert(a23[16] == 0);
    assert(a23[17] == 0);
    assert(a23[18] == 0);
    assert(a23[19] == 1);
    assert(a23[20] == 1);

}

/**************************************/

bool[] slice8(bool[] b)
{
    return b[8..16];
}

void test8()
{
    bool[16] b;
    bool[] b2;
    b[9] = true;
    b2 = slice8(b);
    assert(b2.length == 8);
    assert(b2[1] == true);
    b[9] = false;
    assert(b2[1] == false);
}


/**************************************/

bool[] slice9(bool[] b, int i, int j)
{
    return b[i..j];
}

void test9()
{
    bool[17] b;
    bool[] b2;
    b[9] = true;
    b[16] = true;
    b2 = slice9(b,8,17);
    assert(b2.length == 9);
    assert(b2[1] == true);
    assert(b2[8] == true);
    b[9] = false;
    b[16] = false;
    assert(b2[1] == false);
    assert(b2[8] == false);
}


/**************************************/

bool* foo10(bool[] b, int i)
{
    return &b[i];
}

void test10()
{
    bool[17] b;
    bool* pb;

    b[8] = true;
    b[9] = true;
    pb = foo10(b, 8);
    assert(*pb == true);
    b[8] = false;
    assert(*pb == false);
}

/**************************************/

bool* foo11(bool[] b, int i)
{
    return &b[i];
}

void test11()
{
    bool[17] b;
    bool* pb;

    b[9] = true;
    b[10] = true;
    pb = foo11(b, 8);
    assert(pb[1] == true);
    b[9] = false;
    assert(pb[1] == false);
}


/**************************************/

bool* foo12(bool[] b, int i)
{
    return &b[i];
}

void test12()
{
    bool[17] b;
    bool* pb;

    b[9] = true;
    b[10] = true;
    pb = foo12(b, 0);
    pb = pb + 8;
    assert(pb[1] == true);
    b[9] = false;
    assert(pb[1] == false);
}

/**************************************/

bool* foo13(bool* b, int i)
{
    return &b[i];
}

void test13()
{
    bool[17] b;
    bool* pb;

    b[9] = true;
    b[10] = true;
    pb = foo13(b.ptr, 8);
    assert(pb[1] == true);
    b[9] = false;
    assert(pb[1] == false);
}

/**************************************/

void test14()
{
    bool b = true;
    assert(b);
    b = !b;
    assert(!b);
}

/**************************************/

void test15()
{
   bool b = 1;
   bool *pb = &b;
   *pb = false;
   assert(!b);
}


/**************************************/

void foo16(bool[] b1, bool[] b2)
{
    b1[0..3] = b2[8..11];
}

void test16()
{
    static bool[16] b1 = [1,1,0,1,0,0,0,0];
    static bool[16] b2 = [0,0,0,0,0,0,0,0, 0,0,1,0,1,1,1,1];

    foo16(b1, b2);
    assert(b1[0] == false);
    assert(b1[1] == false);
    assert(b1[2] == true);
    assert(b1[3] == true);
    assert(b1[4] == false);
    assert(b1[5] == false);
    assert(b1[6] == false);
    assert(b1[7] == false);
    assert(b1[8] == false);
}

/**************************************/

void test17()
{
  bool bi = true;
  byte by;

  by=bi;
  assert(by == 1);
}


/**************************************/

void test18()
{
    bool b = 1;
    bool c = 1;
    b |= cast(bool)(3 + c);
    assert(b == true);
}


/**************************************/

void test19()
{
        int i;
        for (i = 0; i < 10; i++){
                version(dummy) i=22;
        }
        assert(i==10);

        char[][] args;
        foreach(char[] p; args){
                version(dummy) int i;
        }
}

/**************************************/

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

    printf("Success\n");
    return 0;
}
