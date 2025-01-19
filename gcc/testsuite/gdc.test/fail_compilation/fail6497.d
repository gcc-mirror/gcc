/*
TEST_OUTPUT:
---
fail_compilation/fail6497.d(12): Error: taking the address of stack-allocated local variable `n` is not allowed in a `@safe` function
fail_compilation/fail6497.d(12): Error: taking the address of stack-allocated local variable `n` is not allowed in a `@safe` function
---
*/

void main() @safe
{
    int n;
    auto b = &(0 ? n : n);
}
