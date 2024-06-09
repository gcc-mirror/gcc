

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=9361

/*
TEST_OUTPUT:
---
fail_compilation/ice6538.d(23): Error: template instance `Sym!(super)` expression `super` is not a valid template value argument
fail_compilation/ice6538.d(28): Error: template `foo` is not callable using argument types `!()()`
fail_compilation/ice6538.d(23):        Candidate is: `foo()()`
---
*/

template Sym(alias A)
{
    enum Sym = true;
}

class C {}
class D : C
{
    void foo()() if (Sym!(super)) {}
}
void test9361b()
{
    auto d = new D();
    d.foo();
}
