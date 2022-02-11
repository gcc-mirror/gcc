/*
TEST_OUTPUT:
---
fail_compilation/cppmangle.d(11): Error: expected valid identifier for C++ namespace but got ``
fail_compilation/cppmangle.d(15): Error: expected valid identifier for C++ namespace but got `0num`
fail_compilation/cppmangle.d(19): Error: compile time string constant (or tuple) expected, not `2`
fail_compilation/cppmangle.d(23): Error: expected valid identifier for C++ namespace but got `invalid@namespace`
---
*/

extern(C++, "")
{
}

extern(C++, "0num")
{
}

extern(C++, 1+1)
{
}

extern(C++, "invalid@namespace")
{
}
