/*
TEST_OUTPUT:
---
fail_compilation/diag9148.d(19): Error: `pure` function `diag9148.test9148a.foo` cannot access mutable static data `g`
fail_compilation/diag9148.d(23): Error: `pure` function `diag9148.test9148a.bar` cannot access mutable static data `g`
fail_compilation/diag9148.d(24): Error: `immutable` function `diag9148.test9148a.bar` cannot access mutable data `x`
fail_compilation/diag9148.d(31): Error: `pure` function `diag9148.test9148a.S.foo` cannot access mutable static data `g`
fail_compilation/diag9148.d(35): Error: `pure` function `diag9148.test9148a.S.bar` cannot access mutable static data `g`
fail_compilation/diag9148.d(36): Error: `immutable` function `diag9148.test9148a.S.bar` cannot access mutable data `x`
---
*/
void test9148a() pure
{
    static int g;
    int x;

    void foo() /+pure+/
    {
        g++;
    }
    void bar() immutable /+pure+/
    {
        g++;
        x++;
    }

    struct S
    {
        void foo() /+pure+/
        {
            g++;
        }
        void bar() immutable /+pure+/
        {
            g++;
            x++;
        }
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/diag9148.d(54): Error: `static` function `diag9148.test9148b.foo` cannot access variable `x` in frame of function `diag9148.test9148b`
fail_compilation/diag9148.d(51):        `x` declared here
---
*/

void test9148b()
{
    int x;
    static void foo() pure
    {
        int y = x;
    }
}
