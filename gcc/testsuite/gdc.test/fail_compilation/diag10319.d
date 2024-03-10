/*
TEST_OUTPUT:
---
fail_compilation/diag10319.d(33): Error: `pure` function `D main` cannot call impure function `diag10319.foo`
fail_compilation/diag10319.d(33): Error: `@safe` function `D main` cannot call `@system` function `diag10319.foo`
fail_compilation/diag10319.d(22):        `diag10319.foo` is declared here
fail_compilation/diag10319.d(34): Error: `pure` function `D main` cannot call impure function `diag10319.bar!int.bar`
fail_compilation/diag10319.d(26):        which wasn't inferred `pure` because of:
fail_compilation/diag10319.d(26):        `pure` function `diag10319.bar!int.bar` cannot access mutable static data `g`
fail_compilation/diag10319.d(34): Error: `@safe` function `D main` cannot call `@system` function `diag10319.bar!int.bar`
fail_compilation/diag10319.d(27):        which wasn't inferred `@safe` because of:
fail_compilation/diag10319.d(27):        cannot take address of local `x` in `@safe` function `bar`
fail_compilation/diag10319.d(24):        `diag10319.bar!int.bar` is declared here
fail_compilation/diag10319.d(33): Error: function `diag10319.foo` is not `nothrow`
fail_compilation/diag10319.d(34): Error: function `diag10319.bar!int.bar` is not `nothrow`
fail_compilation/diag10319.d(28):        which wasn't inferred `nothrow` because of:
fail_compilation/diag10319.d(28):        `object.Exception` is thrown but not caught
fail_compilation/diag10319.d(31): Error: function `D main` may throw but is marked as `nothrow`
---
*/

void foo() {}

void bar(T)()
{
    static int g; g = 10;       // impure
    int x; auto p = &x;         // system
    throw new Exception("");    // may throw
}

@safe pure nothrow void main()  // L23
{
    foo();      // L25
    bar!int();  // L26
}
