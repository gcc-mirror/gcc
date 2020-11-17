/*
TEST_OUTPUT:
---
fail_compilation/fail18970.d(22): Error: no property `y` for type `fail18970.S`
fail_compilation/fail18970.d(29): Error: no property `yyy` for type `fail18970.S2`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18970

struct S
{
    auto opDispatch(string name)(int)
    {
        alias T = typeof(x);
        static assert(!is(T.U));
        return 0;
    }
}
void f()
{
    S().y(1);
}

struct S2
{
    this(int)
    {
        this.yyy;
    }

    auto opDispatch(string name)()
    {
        alias T = typeof(x);
        static if(is(T.U)) {}
    }
}
