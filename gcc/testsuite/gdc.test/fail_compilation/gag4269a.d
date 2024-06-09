// REQUIRED_ARGS: -c -o-
/*
TEST_OUTPUT:
---
fail_compilation/gag4269a.d(12): Error: undefined identifier `B`
---
*/

static if(is(typeof(A4269.sizeof))) {}
class A4269
{
    void foo(B b);
}
