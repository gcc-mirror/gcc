/*
TEST_OUTPUT:
---
fail_compilation/safe_pointer_index.d(11): Error: `@safe` function `f` cannot index pointer `x`
---
*/

@safe void f(int* x)
{
    int y = x[0]; // allowed, same as *x
    int z = x[1];
}
