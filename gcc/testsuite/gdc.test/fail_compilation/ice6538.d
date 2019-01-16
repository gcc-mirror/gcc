

/**************************************/
// 9361

/*
TEST_OUTPUT:
---
fail_compilation/ice6538.d(23): Error: expression super is not a valid template value argument
fail_compilation/ice6538.d(28): Error: template ice6538.D.foo cannot deduce function from argument types !()(), candidates are:
fail_compilation/ice6538.d(23):        ice6538.D.foo()() if (Sym!(super))
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

