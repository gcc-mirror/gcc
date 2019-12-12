/*
TEST_OUTPUT:
---
fail_compilation/fail328.d(12): Error: @safe function 'fail328.foo' cannot call @system function 'fail328.bar'
---
*/

void bar();

@safe void foo()
{
    bar();
}
