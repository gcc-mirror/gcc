/*
TEST_OUTPUT:
---
fail_compilation/ice10713.d(10): Error: no property 'nonExistingField' for type 'S'
---
*/

struct S
{
    void f(typeof(this.nonExistingField) a) {}
}
