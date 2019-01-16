/*
TEST_OUTPUT:
---
fail_compilation/diag12829.d(12): Error: function diag12829.test1 is @nogc yet allocates closures with the GC
fail_compilation/diag12829.d(15):        diag12829.test1.__lambda1 closes over variable x at fail_compilation/diag12829.d(14)
fail_compilation/diag12829.d(19):        diag12829.test1.bar closes over variable x at fail_compilation/diag12829.d(14)
fail_compilation/diag12829.d(26): Error: function diag12829.test2 is @nogc yet allocates closures with the GC
fail_compilation/diag12829.d(31):        diag12829.test2.S.foo closes over variable x at fail_compilation/diag12829.d(28)
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
