/*
TEST_OUTPUT:
---
fail_compilation/cppmangle.d(10): Error: invalid zero length C++ namespace
fail_compilation/cppmangle.d(14): Error: expected valid identifer for C++ namespace but got `0num`
fail_compilation/cppmangle.d(18): Error: string expected following `,` for C++ namespace, not `)`
---
*/

extern(C++, "")
{
}

extern(C++, "0num")
{
}

extern(C++, "std", )
{
}
