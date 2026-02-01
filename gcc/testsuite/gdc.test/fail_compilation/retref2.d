/*
TEST_OUTPUT:
---
fail_compilation/retref2.d(20): Error: function `ref int retref2.D.foo(return ref int)` does not override any function
fail_compilation/retref2.d(14):        did you mean to override `ref int retref2.C.foo(ref int)`?
fail_compilation/retref2.d(21): Error: function `ref int retref2.D.bar() scope return` does not override any function
fail_compilation/retref2.d(15):        did you mean to override `ref int retref2.C.bar()`?
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
