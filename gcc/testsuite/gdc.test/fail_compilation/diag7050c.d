/*
TEST_OUTPUT:
---
fail_compilation/diag7050c.d(13): Error: @safe destructor 'diag7050c.B.~this' cannot call @system destructor 'diag7050c.A.~this'
---
*/

struct A
{
    ~this(){}
}

@safe struct B
{
    A a;
}

@safe void f()
{
    auto x = B.init;
}
