/*
TEST_OUTPUT:
---
fail_compilation/diag10319.d(29): Error: `pure` function `D main` cannot call impure function `diag10319.foo`
fail_compilation/diag10319.d(29): Error: `@safe` function `D main` cannot call `@system` function `diag10319.foo`
fail_compilation/diag10319.d(18):        `diag10319.foo` is declared here
fail_compilation/diag10319.d(30): Error: `pure` function `D main` cannot call impure function `diag10319.bar!int.bar`
fail_compilation/diag10319.d(30): Error: `@safe` function `D main` cannot call `@system` function `diag10319.bar!int.bar`
fail_compilation/diag10319.d(23):        which was inferred `@system` because of:
fail_compilation/diag10319.d(23):        cannot take address of local `x` in `@safe` function `bar`
fail_compilation/diag10319.d(20):        `diag10319.bar!int.bar` is declared here
fail_compilation/diag10319.d(29): Error: function `diag10319.foo` is not `nothrow`
fail_compilation/diag10319.d(30): Error: function `diag10319.bar!int.bar` is not `nothrow`
fail_compilation/diag10319.d(27): Error: function `D main` may throw but is marked as `nothrow`
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
