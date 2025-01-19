/*
TEST_OUTPUT:
---
fail_compilation/diag10359.d(10): Error: pointer slicing is not allowed in a `@safe` function
---
*/

void foo(int* p) @safe
{
    auto a = p[0 .. 10];
}
