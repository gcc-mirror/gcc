// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/test15785.d(17): Deprecation: imports.test15785.Base.foo is not visible from module test15785
fail_compilation/test15785.d(17): Error: class test15785.Derived member `foo` is not accessible
fail_compilation/test15785.d(18): Deprecation: imports.test15785.Base.bar is not visible from module test15785
fail_compilation/test15785.d(18): Error: class test15785.Derived member `bar` is not accessible
---
*/
import imports.test15785;

class Derived : Base
{
    void test()
    {
        super.foo();
        bar();
    }
}
