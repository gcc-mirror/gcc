/*
TEST_OUTPUT:
---
fail_compilation/fail6497.d(13): Error: taking the address of stack-allocated local variable `n` is not allowed in a `@safe` function
fail_compilation/fail6497.d(13): Error: taking the address of stack-allocated local variable `n` is not allowed in a `@safe` function
fail_compilation/fail6497.d(19): Error: taking the address of local variable `i` is not allowed in a `@safe` function
---
*/

void main() @safe
{
    int n;
    auto b = &(0 ? n : n);
}

void f() @safe
{
    ref i = *new int;
    auto b = &i;
}
