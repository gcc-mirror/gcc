/*
TEST_OUTPUT:
---
fail_compilation/fail10254.d(18): Error: pure function 'fail10254.foo' cannot call impure constructor 'fail10254.C.this'
fail_compilation/fail10254.d(18): Error: @safe function 'fail10254.foo' cannot call @system constructor 'fail10254.C.this'
fail_compilation/fail10254.d(19): Error: pure function 'fail10254.foo' cannot call impure constructor 'fail10254.S.this'
fail_compilation/fail10254.d(19): Error: @safe function 'fail10254.foo' cannot call @system constructor 'fail10254.S.this'
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
