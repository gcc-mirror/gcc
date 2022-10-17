/*
TEST_OUTPUT:
---
fail_compilation/fail328.d(13): Error: `@safe` function `fail328.foo` cannot call `@system` function `fail328.bar`
fail_compilation/fail328.d(9):        `fail328.bar` is declared here
---
*/

void bar();

@safe void foo()
{
    bar();
}
