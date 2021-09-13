// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/test15785.d(16): Error: no property `foo` for type `imports.test15785.Base`, did you mean `imports.test15785.Base.foo`?
fail_compilation/test15785.d(17): Error: undefined identifier `bar`
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
