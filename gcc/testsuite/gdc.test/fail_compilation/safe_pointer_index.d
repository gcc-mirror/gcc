/*
TEST_OUTPUT:
---
fail_compilation/safe_pointer_index.d(11): Error: indexing pointer `x` is not allowed in a `@safe` function
---
*/

@safe void f(int* x)
{
    int y = x[0]; // allowed, same as *x
    int z = x[1];
}
