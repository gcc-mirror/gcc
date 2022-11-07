/*
TEST_OUTPUT:
---
fail_compilation/fail18970.d(24): Error: no property `y` for `S()` of type `fail18970.S`
fail_compilation/fail18970.d(24):        potentially malformed `opDispatch`. Use an explicit instantiation to get a better error message
fail_compilation/fail18970.d(31): Error: no property `yyy` for `this` of type `fail18970.S2`
fail_compilation/fail18970.d(31):        potentially malformed `opDispatch`. Use an explicit instantiation to get a better error message
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
