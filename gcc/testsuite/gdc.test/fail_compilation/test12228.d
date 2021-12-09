/*
TEST_OUTPUT:
---
fail_compilation/test12228.d(13): Error: Using `this` as a type is obsolete. Use `typeof(this)` instead
fail_compilation/test12228.d(18): Error: no property `x` for type `object.Object`
fail_compilation/test12228.d(19): Error: Using `super` as a type is obsolete. Use `typeof(super)` instead
fail_compilation/test12228.d(20): Error: Using `super` as a type is obsolete. Use `typeof(super)` instead
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
