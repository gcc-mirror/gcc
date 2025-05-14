/*
REQUIRED_ARGS: -verrors=context
TEST_OUTPUT:
---
fail_compilation/fail_pretty_errors.d(29): Error: undefined identifier `a`
    a = 1;
    ^
fail_compilation/fail_pretty_errors.d-mixin-34(34): Error: undefined identifier `b`
b = 1;
^
fail_compilation/fail_pretty_errors.d(39): Error: cannot implicitly convert expression `5` of type `int` to `string`
    string x = 5;
               ^
fail_compilation/fail_pretty_errors.d(44): Error: mixin `fail_pretty_errors.testMixin2.mixinTemplate!()` error instantiating
    mixin mixinTemplate;
    ^
fail_compilation/fail_pretty_errors.d(50): Error: invalid array operation `"" + ""` (possible missing [])
    auto x = ""+"";
             ^
fail_compilation/fail_pretty_errors.d(50):        did you mean to concatenate (`"" ~ ""`) instead ?
fail_compilation/fail_pretty_errors.d(53): Error: cannot implicitly convert expression `1111` of type `int` to `byte`
        byte ɑ =    1111;
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

void f()
{
    // check supplemental error doesn't show context
    auto x = ""+"";

    // Check correct spacing with the presence of unicode characters and tabs
	 	byte ɑ = 	1111;
}
