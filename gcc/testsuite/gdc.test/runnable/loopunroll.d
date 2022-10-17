/*
PERMUTE_ARGS: -O
RUN_OUTPUT:
---
0
45
45
45
45
45
45
45
10
45
0
5
45
45
---
 */

import core.stdc.stdio;

version (all)
{

/************************************/

int func1(int[] data)
{
        int j;
        for (int i = 0; i < 10; i++) {
                data[i*10] = i;
                j = data[0] * 10;
        }
        return j;
}

void test1()
{
    int[100] data = [1,7,6,3,8,9,7,2,2,4,
                     1,7,6,3,8,9,7,2,2,4,
                     1,7,6,3,8,9,7,2,2,4,
                     1,7,6,3,8,9,7,2,2,4,
                     1,7,6,3,8,9,7,2,2,4,
                     1,7,6,3,8,9,7,2,2,4,
                     1,7,6,3,8,9,7,2,2,4,
                     1,7,6,3,8,9,7,2,2,4,
                     1,7,6,3,8,9,7,2,2,4,
                     1,7,6,3,8,9,7,2,2,4,
                    ];
    int i = func1(data[]);
    if (i != 0)
        assert(0);
    printf("%d\n", i);
}

/************************************/

void test2()
{
    int result = 0;
    for (int i = 0; i < 10; ++i)
        result += i;
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test3()
{
    int result = 0;
    for (int i = 0; i < 10; i++)
        result += i;
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test4()
{
    int result = 0;
    for (int i = 0; i < 10; i += 1)
        result += i;
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test5()
{
    int result = 0;
    for (int i = 0; i < 10; i -= -1)
        result += i;
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test6()
{
    int result = 0;
    for (uint i = 0; i < 10; i++)
        result += i;
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test7()
{
    int result = 0;
    for (long i = 0; i < 10; i++)
        result += i;
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test8()
{
    int result = 0;
    for (ulong i = 0; i < 10; i++)
        result += i;
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test9()
{
    int result = 0;
    for (ulong i = 0; i < 5; i++)
        result += i;
    printf("%d\n", result);
    if (result != 10)
        assert(0);
}

/************************************/

void test10()
{
    __gshared int i;
    int result = 0;
    for (i = 0; i < 10; i++)
        result += i;
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test11()
{
    int result = 0;
    for (int i = 0; i < 10; i += 10)
        result += i;
    printf("%d\n", result);
    if (result != 0)
        assert(0);
}

/************************************/

void test12()
{
    int result = 0;
    for (int i = 0; i < 10; i += 5)
        result += i;
    printf("%d\n", result);
    if (result != 5)
        assert(0);
}

/************************************/

void test13()
{
    int result = 0;
    int i;
    int* p = &i;

    int foo() { return *p; }

    for (i = 0; i < 10; ++i)
    {
        if (foo() != i)
            assert(0);
        result += i;
    }
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test14()
{
    int result = 0;
    int i;

    int foo() { return i; }

    for (i = 0; i < 10; ++i)
    {
        if (foo() != i)
            assert(0);
        result += i;
    }
    printf("%d\n", result);
    if (result != 45)
        assert(0);
}

/************************************/

void test15()
{
    int result = 0;
    int i;

    try
    {
        for (i = 0; i < 10; ++i)
        {
            if (i == 1)
                throw new Exception("hello");
            result += i;
        }
        assert(0);
    }
    catch (Exception e)
    {
        assert(i == 1);
    }
}

/************************************/

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
    return 0;
}

}
else
{

void main()
{
    int result = 0;
    int i;

    try
    {
        for (i = 0; i < 10; ++i)
        {
            if (i == 1)
                throw new Exception("hello");
            result += i;
        }
        assert(0);
    }
    catch (Exception e)
    {
        assert(i == 1);
    }
}

}
