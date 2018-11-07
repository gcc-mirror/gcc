/*
TEST_OUTPUT:
---
fail_compilation/diag10359.d(10): Error: pointer slicing not allowed in safe functions
---
*/

void foo(int* p) @safe
{
    auto a = p[0 .. 10];
}
