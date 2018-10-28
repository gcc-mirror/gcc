/*
TEST_OUTPUT:
---
fail_compilation/fail154.d(18): Error: template instance X!(MYP!int) does not match template declaration X(T : Policy!T, alias Policy)
---
*/

class X(T:Policy!(T), alias Policy)
{
    mixin Policy!(T);
}

template MYP(T)
{
    void foo(T);
}

X!(MYP!(int)) x;
