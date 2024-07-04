/*
TEST_OUTPUT:
---
fail_compilation/fail10299.d(11): Error: cannot take address of expression `foo!string` because it is not an lvalue
---
*/

template foo(T)
{
}
auto fp = &foo!string;    // ICE
