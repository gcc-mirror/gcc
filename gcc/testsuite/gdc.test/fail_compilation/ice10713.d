/*
TEST_OUTPUT:
---
fail_compilation/ice10713.d(10): Error: no property `nonExistingField` for type `ice10713.S`
---
*/

struct S
{
    void f(typeof(this.nonExistingField) a) {}
}
