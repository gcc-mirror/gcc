/*
TEST_OUTPUT:
---
fail_compilation/diag12829.d(15): Error: function `diag12829.test1` is `@nogc` yet allocates closure for `test1()` with the GC
fail_compilation/diag12829.d(18):        delegate `diag12829.test1.__lambda2` closes over variable `x`
fail_compilation/diag12829.d(17):        `x` declared here
fail_compilation/diag12829.d(22):        function `diag12829.test1.bar` closes over variable `x`
fail_compilation/diag12829.d(17):        `x` declared here
fail_compilation/diag12829.d(29): Error: function `diag12829.test2` is `@nogc` yet allocates closure for `test2()` with the GC
fail_compilation/diag12829.d(34):        function `diag12829.test2.S.foo` closes over variable `x`
fail_compilation/diag12829.d(31):        `x` declared here
---
*/

auto test1() @nogc
{
    int x;
    void delegate() @nogc foo = () {
        int y = x;
    };

    void bar()
    {
        int y = x;
    }
    auto dg = &bar;
}

auto test2() @nogc
{
    int x;
    struct S
    {
        void foo()
        {
            int y = x;
        }
    }
    return S();
}
