/*
TEST_OUTPUT:
---
fail_compilation/imports/a15816.d(3): Error: anonymous classes not allowed
---
*/

class A
{
    import imports.a15816;
}
