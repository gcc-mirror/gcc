/*
TEST_OUTPUT:
---
fail_compilation/fail18970.d(26): Error: no property `y` for `S()` of type `fail18970.S`
fail_compilation/fail18970.d(19): Error: undefined identifier `x`
fail_compilation/fail18970.d(26): Error: template instance `fail18970.S.opDispatch!"y"` error instantiating
fail_compilation/fail18970.d(33): Error: no property `yyy` for `this` of type `fail18970.S2`
fail_compilation/fail18970.d(38): Error: undefined identifier `x`
fail_compilation/fail18970.d(33): Error: template instance `fail18970.S2.opDispatch!"yyy"` error instantiating
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
