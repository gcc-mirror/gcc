
// REQUIRED_ARGS: -dip25

/*
TEST_OUTPUT:
---
fail_compilation/retref2.d(21): Error: function retref2.D.foo does not override any function, did you mean to override 'retref2.C.foo'?
fail_compilation/retref2.d(22): Error: function retref2.D.bar does not override any function, did you mean to override 'retref2.C.bar'?
---
*/


class C
{
    ref int foo(ref int);
    ref int bar();
}

class D : C
{
    override ref int foo(return ref int);
    override ref int bar() return;
}
