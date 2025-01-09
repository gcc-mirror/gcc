// REQUIRED_ARGS: -preview=dip1000
/*
TEST_OUTPUT:
---
fail_compilation/fail20108.d(15): Error: assigning address of variable `y` to `x` with longer lifetime is not allowed in a `@safe` function
fail_compilation/fail20108.d(16): Error: scope parameter `x` may not be returned
fail_compilation/fail20108.d(23): Error: assigning address of variable `y` to `x` with longer lifetime is not allowed in a `@safe` function
fail_compilation/fail20108.d(24): Error: returning scope variable `x` is not allowed in a `@safe` function
---
*/

@safe auto test(scope int* x)
{
    int y = 69;
    x = &y; //bad
    return x;
}

@safe auto test2()
{
    scope int* x;
    int y = 69;
    x = &y; //bad
    return x;
}

void main()
{
    auto y = test(null);
    auto z = test2();
}
