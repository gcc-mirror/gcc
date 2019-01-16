/*
TEST_OUTPUT:
---
fail_compilation/ice13459.d(12): Error: undefined identifier `B`
fail_compilation/ice13459.d(18): Error: none of the overloads of 'opSlice' are callable using argument types (int, int), candidates are:
fail_compilation/ice13459.d(11):        ice13459.A.opSlice()
---
*/
struct A
{
    auto opSlice() {}
    auto opSlice() { return B; }
}

void main()
{
    auto df = A();
    foreach (fi; df[0..0]) {}
}
