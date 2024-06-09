/*
TEST_OUTPUT:
---
fail_compilation/ice14130.d(10): Error: undefined identifier `Undef`
fail_compilation/ice14130.d(14): Error: template `foo` is not callable using argument types `!()(int)`
fail_compilation/ice14130.d(10):        Candidate is: `foo(R, F = Undef)(R r, F s = 0)`
---
*/

F foo(R, F = Undef)(R r, F s = 0) {}

void main()
{
    0.foo;
}
