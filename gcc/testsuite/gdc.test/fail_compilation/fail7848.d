// REQUIRED_ARGS: -unittest

/*
TEST_OUTPUT:
---
fail_compilation/fail7848.d(27): Error: `pure` function `fail7848.C.__unittest_L25_C30` cannot call impure function `fail7848.func`
fail_compilation/fail7848.d(27): Error: `@safe` function `fail7848.C.__unittest_L25_C30` cannot call `@system` function `fail7848.func`
fail_compilation/fail7848.d(21):        `fail7848.func` is declared here
fail_compilation/fail7848.d(27): Error: `@nogc` function `fail7848.C.__unittest_L25_C30` cannot call non-@nogc function `fail7848.func`
fail_compilation/fail7848.d(27): Error: function `fail7848.func` is not `nothrow`
fail_compilation/fail7848.d(25): Error: `nothrow` function `fail7848.C.__unittest_L25_C30` may throw
fail_compilation/fail7848.d(32): Error: `pure` function `fail7848.C.__invariant1` cannot call impure function `fail7848.func`
fail_compilation/fail7848.d(32): Error: `@safe` function `fail7848.C.__invariant1` cannot call `@system` function `fail7848.func`
fail_compilation/fail7848.d(21):        `fail7848.func` is declared here
fail_compilation/fail7848.d(32): Error: `@nogc` function `fail7848.C.__invariant1` cannot call non-@nogc function `fail7848.func`
fail_compilation/fail7848.d(32): Error: function `fail7848.func` is not `nothrow`
fail_compilation/fail7848.d(30): Error: `nothrow` function `fail7848.C.__invariant1` may throw
---
*/

void func() {}

class C
{
    @safe pure nothrow @nogc unittest
    {
        func();
    }

    @safe pure nothrow @nogc invariant
    {
        func();
    }
}
