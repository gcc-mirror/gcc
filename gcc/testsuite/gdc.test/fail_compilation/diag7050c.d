/*
TEST_OUTPUT:
---
fail_compilation/diag7050c.d(14): Error: `@safe` destructor `diag7050c.B.~this` cannot call `@system` destructor `diag7050c.A.~this`
fail_compilation/diag7050c.d(11):        `diag7050c.A.~this` is declared here
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
