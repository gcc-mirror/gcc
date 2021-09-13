/*
TEST_OUTPUT:
---
fail_compilation/fail8032.d(19): Error: function `fail8032.B.f` cannot determine overridden function
---
*/
mixin template T()
{
    void f() { }
}

class A {
    mixin T;
    mixin T;
}

class B : A
{
    override void f() { }
    // raises "cannot determine overridden function" error.
}

void main(){}
