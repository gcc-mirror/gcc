/*
TEST_OUTPUT:
----
fail_compilation/test17868b.d(10): Error: function `test17868b.foo` must be `extern(C)` for `pragma(crt_constructor)`
fail_compilation/test17868b.d(14): Error: function `test17868b.bar` must be `extern(C)` for `pragma(crt_constructor)`
fail_compilation/test17868b.d(9): Error: pragma `crt_constructor` can only apply to a single declaration
----
 */
pragma(crt_constructor):
void foo()
{
}

void bar()
{
}
