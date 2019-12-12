/*
TEST_OUTPUT:
---
fail_compilation/fail310.d(10): Error: undefined identifier `Foo`, did you mean function `foo`?
fail_compilation/fail310.d(14): Error: template instance fail310.foo!(1, 2) error instantiating
fail_compilation/fail310.d(14):        while evaluating: `static assert(foo!(1, 2)())`
---
*/

Foo foo(A...)()
{
}

static assert(foo!(1, 2)());
