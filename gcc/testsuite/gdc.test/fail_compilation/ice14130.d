/*
TEST_OUTPUT:
---
fail_compilation/ice14130.d(10): Error: undefined identifier `Undef`
fail_compilation/ice14130.d(14): Error: template ice14130.foo cannot deduce function from argument types !()(int), candidates are:
fail_compilation/ice14130.d(10):        ice14130.foo(R, F = Undef)(R r, F s = 0)
---
*/

F foo(R, F = Undef)(R r, F s = 0) {}

void main()
{
    0.foo;
}
