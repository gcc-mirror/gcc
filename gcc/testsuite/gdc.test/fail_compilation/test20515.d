// REQUIRED_ARGS: -verror-style=gnu
/*
TEST_OUTPUT:
---
fail_compilation/test20515.d:16: Deprecation: function `test20515.foo` is deprecated
fail_compilation/test20515.d:17: Error: undefined identifier `bar`
---
*/

module test20515;

deprecated void foo() { }

void test()
{
    foo;
    bar;
}
