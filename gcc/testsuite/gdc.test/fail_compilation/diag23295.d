/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/diag23295.d(21): Error: scope variable `x` assigned to non-scope parameter `y` calling `foo`
fail_compilation/diag23295.d(32):        which is assigned to non-scope parameter `z`
fail_compilation/diag23295.d(34):        which is not `scope` because of `f = & z`
fail_compilation/diag23295.d(24): Error: scope variable `ex` assigned to non-scope parameter `e` calling `thro`
fail_compilation/diag23295.d(39):        which is not `scope` because of `throw e`
---
*/

// explain why scope inference failed
// https://issues.dlang.org/show_bug.cgi?id=23295

@safe:

void main()
{
    scope int* x;
    foo(x, null);

    scope Exception ex;
    thro(ex);
}

auto foo(int* y, int** w)
{
    fooImpl(y, null);
}

auto fooImpl(int* z, int** w)
{
    auto f = &z;
}

auto thro(Exception e)
{
    throw e;
}
