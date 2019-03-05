/*
TEST_OUTPUT:
---
fail_compilation/diag9880.d(9): Error: template instance diag9880.foo!string does not match template declaration foo(T)(int) if (is(T == int))
---
*/

void foo(T)(int) if (is(T == int)) {}
void main() { alias f = foo!string; }
