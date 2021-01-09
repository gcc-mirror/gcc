/*
TEST_OUTPUT:
---
fail_compilation/fail6497.d(12): Error: cannot take address of local `n` in `@safe` function `main`
fail_compilation/fail6497.d(12): Error: cannot take address of local `n` in `@safe` function `main`
---
*/

void main() @safe
{
    int n;
    auto b = &(0 ? n : n);
}
