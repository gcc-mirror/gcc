/*
TEST_OUTPUT:
---
fail_compilation/fail212.d(14): Error: function `fail212.S.bar` without `this` cannot be `const`
---
*/

struct S
{
    void foo() const
    {
    }

    static void bar() const
    {
    }
}
