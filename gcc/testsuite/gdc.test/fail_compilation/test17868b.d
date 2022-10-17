/*
TEST_OUTPUT:
----
fail_compilation/test17868b.d(9): Error: pragma `crt_constructor` can only apply to a single declaration
fail_compilation/test17868b.d(14): Error: function `test17868b.bar` must return `void` for `pragma(crt_constructor)`
fail_compilation/test17868b.d(18): Error: function `test17868b.baz` must be `extern(C)` for `pragma(crt_constructor)` when taking parameters
----
 */
pragma(crt_constructor):
void foo()
{
}

extern(C) int bar()
{
}

void baz(int argc, char** argv)
{
}

extern(C) void bazC(int, char**)
{
}
