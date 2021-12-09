/*
TEST_OUTPUT:
---
fail_compilation/diag7050a.d(15): Error: `@safe` function `diag7050a.foo` cannot call `@system` constructor `diag7050a.Foo.this`
fail_compilation/diag7050a.d(11):        `diag7050a.Foo.this` is declared here
---
*/

struct Foo
{
    this (int a) {}
}
@safe void foo()
{
    auto f = Foo(3);
}
