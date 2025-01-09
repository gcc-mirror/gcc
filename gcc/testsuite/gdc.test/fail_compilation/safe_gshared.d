/*
TEST_OUTPUT:
---
fail_compilation/safe_gshared.d(13): Error: accessing `__gshared` data `x` is not allowed in a `@safe` function
fail_compilation/safe_gshared.d(14): Error: accessing `__gshared` data `x` is not allowed in a `@safe` function
---
*/

__gshared int x;

@safe int f()
{
    x++;
    return x;
}
