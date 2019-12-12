/*
TEST_OUTPUT:
---
fail_compilation/diag6539.d(21): Error: overloadset diag6539.Rectangle is used as a type
---
*/

mixin template foo()
{
    struct Rectangle(T) {}
}

mixin template bar()
{
    bool Rectangle(bool, int, int, int, int) {}
}

mixin foo;
mixin bar;

void test(Rectangle rect)
{
}
