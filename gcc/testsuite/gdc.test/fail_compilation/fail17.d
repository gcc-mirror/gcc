/*
TEST_OUTPUT:
---
fail_compilation/fail17.d(11): Error: undefined identifier `B`
fail_compilation/fail17.d(11): Error: mixin `fail17.A!int.A.B!(T, A!T)` is not defined
fail_compilation/fail17.d(14): Error: template instance `fail17.A!int` error instantiating
---
*/
struct A(T)
{
    mixin B!(T, A!(T));
}

A!(int) x;


