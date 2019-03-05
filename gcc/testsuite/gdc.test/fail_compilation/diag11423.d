/*
TEST_OUTPUT:
---
fail_compilation/diag11423.d(9): Error: undefined identifier `Foo`
---
*/
void main()
{
    auto foo = new shared Foo();
}
