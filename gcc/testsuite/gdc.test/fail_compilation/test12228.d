/*
TEST_OUTPUT:
---
fail_compilation/test12228.d(13): Error: undefined identifier `this`, did you mean `typeof(this)`?
fail_compilation/test12228.d(18): Error: no property `x` for type `object.Object`
fail_compilation/test12228.d(19): Error: undefined identifier `super`, did you mean `typeof(super)`?
fail_compilation/test12228.d(20): Error: undefined identifier `super`, did you mean `typeof(super)`?
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
