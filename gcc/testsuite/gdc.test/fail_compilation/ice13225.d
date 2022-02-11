/*
TEST_OUTPUT:
---
fail_compilation/ice13225.d(12): Error: mixin `ice13225.S.M!(function (S _param_0) pure nothrow @nogc @safe => 0)` does not match template declaration `M(T)`
fail_compilation/ice13225.d(16): Error: undefined identifier `undefined`
---
*/
mixin template M(T) {}

struct S
{
    mixin M!((typeof(this)) => 0);
}
struct T
{
    mixin M!(() => undefined);
}
