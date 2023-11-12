/*
TEST_OUTPUT:
---
fail_compilation/fail212.d(10): Error: function `fail212.baz` without `this` cannot be `const`
fail_compilation/fail212.d(10):        did you mean to use `const(int)` as the return type?
fail_compilation/fail212.d(18): Error: function `fail212.S.bar` without `this` cannot be `const`
---
*/

const int baz();

struct S
{
    void foo() const
    {
    }

    static void bar() const
    {
    }
}
