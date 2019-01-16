/*
TEST_OUTPUT:
---
fail_compilation/fail183.d(10): Error: redundant attribute `const`
fail_compilation/fail183.d(10): Error: redundant attribute `scope`
fail_compilation/fail183.d(11): Error: redundant attribute `in`
---
*/

void f(in final const scope int x) {}
void g(final const scope in int x) {}
