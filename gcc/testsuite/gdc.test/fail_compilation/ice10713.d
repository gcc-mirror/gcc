/*
TEST_OUTPUT:
---
fail_compilation/ice10713.d(11): Error: no property `nonExistingField` for type `ice10713.S`
fail_compilation/ice10713.d(9):        struct `S` defined here
---
*/

struct S
{
    void f(typeof(this.nonExistingField) a) {}
}
