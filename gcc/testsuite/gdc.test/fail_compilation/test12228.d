/*
TEST_OUTPUT:
---
fail_compilation/test12228.d(12): Error: undefined identifier `this`, did you mean `typeof(this)`?
fail_compilation/test12228.d(18): Error: undefined identifier `super`, did you mean `typeof(super)`?
fail_compilation/test12228.d(19): Error: undefined identifier `super`, did you mean `typeof(super)`?
---
*/

class C
{
    shared(this) x;
}

class D : C
{
    alias x = typeof(super).x;
    shared(super) a;
    super b;
}
