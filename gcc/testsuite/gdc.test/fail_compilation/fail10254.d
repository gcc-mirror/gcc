/*
TEST_OUTPUT:
---
fail_compilation/fail10254.d(20): Error: `pure` function `fail10254.foo` cannot call impure constructor `fail10254.C.this`
fail_compilation/fail10254.d(20): Error: `@safe` function `fail10254.foo` cannot call `@system` constructor `fail10254.C.this`
fail_compilation/fail10254.d(15):        `fail10254.C.this` is declared here
fail_compilation/fail10254.d(21): Error: `pure` function `fail10254.foo` cannot call impure constructor `fail10254.S.this`
fail_compilation/fail10254.d(21): Error: `@safe` function `fail10254.foo` cannot call `@system` constructor `fail10254.S.this`
fail_compilation/fail10254.d(16):        `fail10254.S.this` is declared here
---
*/

int a;

class C { this() { a = 2; } }
struct S { this(int) { a = 2; } }

void foo() pure @safe
{
    auto c = new C; // This line should be a compilation error.
    auto s = new S(1);
}
