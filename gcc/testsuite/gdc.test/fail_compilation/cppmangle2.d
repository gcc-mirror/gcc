/*
TEST_OUTPUT:
---
fail_compilation/cppmangle2.d(9): Error: namespace `cppmangle2.ns` conflicts with variable `cppmangle2.ns` at fail_compilation/cppmangle2.d(8)
---
*/

enum ns = "ns";
extern(C++, ns)
{
}
