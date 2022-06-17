/*
TEST_OUTPUT:
---
fail_compilation/safe_gshared.d(13): Error: `@safe` function `f` cannot access `__gshared` data `x`
fail_compilation/safe_gshared.d(14): Error: `@safe` function `f` cannot access `__gshared` data `x`
---
*/

__gshared int x;

@safe int f()
{
    x++;
    return x;
}
