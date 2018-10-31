/*
TEST_OUTPUT:
---
fail_compilation/fail163.d(11): Error: cannot implicitly convert expression `q` of type `const(char)[]` to `char[]`
---
*/
void test1()
{
    char[] p;
    const(char)[] q;
    p = q;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail163.d(24): Error: cannot implicitly convert expression `p` of type `const(int***)` to `const(int)***`
---
*/
void test2()
{
    const int*** p;
    const(int)*** cp;
    cp = p;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail163.d(37): Error: cannot modify const expression p
---
*/
void test3()
{
    const(uint***) p;
    const(int)*** cp;
    p = cp;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail163.d(50): Error: cannot implicitly convert expression `cp` of type `const(int)***[]` to `const(uint***)[]`
---
*/
void test4()
{
    const(uint***)[] p;
    const(int)***[] cp;
    p = cp;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail163.d(63): Error: cannot modify const expression *p
---
*/
void test5()
{
    int x;
    const(int)* p = &x;
    *p = 3;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail163.d(76): Error: cannot implicitly convert expression `& x` of type `int*` to `immutable(int)*`
fail_compilation/fail163.d(77): Error: cannot modify immutable expression *p
---
*/
void test6()
{
    int x;
    immutable(int)* p = &x;
    *p = 3;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail163.d(89): Error: cannot implicitly convert expression `& x` of type `const(int)*` to `int*`
---
*/
void test7()
{
    const(int) x = 3;
    int* p = &x;
}
