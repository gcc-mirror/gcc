/*
REQUIRED_ARGS: -verrors=context
TEST_OUTPUT:
---
fail_compilation/fail_pretty_errors.d(20): Error: undefined identifier `a`
    a = 1;
    ^
fail_compilation/fail_pretty_errors.d-mixin-25(25): Error: undefined identifier `b`
fail_compilation/fail_pretty_errors.d(30): Error: cannot implicitly convert expression `5` of type `int` to `string`
    string x = 5;
               ^
fail_compilation/fail_pretty_errors.d(35): Error: mixin `fail_pretty_errors.testMixin2.mixinTemplate!()` error instantiating
    mixin mixinTemplate;
    ^
---
*/

void foo()
{
    a = 1;
}

void testMixin1()
{
    mixin("b = 1;");
}

mixin template mixinTemplate()
{
    string x = 5;
}

void testMixin2()
{
    mixin mixinTemplate;
}
