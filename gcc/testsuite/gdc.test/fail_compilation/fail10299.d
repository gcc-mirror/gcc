/*
TEST_OUTPUT:
---
fail_compilation/fail10299.d(11): Error: foo!string is not an lvalue
---
*/

template foo(T)
{
}
auto fp = &foo!string;    // ICE
