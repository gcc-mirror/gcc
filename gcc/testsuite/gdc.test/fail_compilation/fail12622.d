/*
TEST_OUTPUT:
---
fail_compilation/fail12622.d(26): Error: `pure` function `fail12622.foo` cannot call impure function pointer `fp`
fail_compilation/fail12622.d(26): Error: `@nogc` function `fail12622.foo` cannot call non-@nogc function pointer `fp`
fail_compilation/fail12622.d(26): Error: `@safe` function `fail12622.foo` cannot call `@system` function pointer `fp`
fail_compilation/fail12622.d(28): Error: `pure` function `fail12622.foo` cannot call impure function pointer `fp`
fail_compilation/fail12622.d(28): Error: `@nogc` function `fail12622.foo` cannot call non-@nogc function pointer `fp`
fail_compilation/fail12622.d(28): Error: `@safe` function `fail12622.foo` cannot call `@system` function pointer `fp`
fail_compilation/fail12622.d(30): Error: `pure` function `fail12622.foo` cannot call impure function `fail12622.bar`
fail_compilation/fail12622.d(30): Error: `@safe` function `fail12622.foo` cannot call `@system` function `fail12622.bar`
fail_compilation/fail12622.d(20):        `fail12622.bar` is declared here
fail_compilation/fail12622.d(30): Error: `@nogc` function `fail12622.foo` cannot call non-@nogc function `fail12622.bar`
---
*/
// Note that, today nothrow violation errors are accidentally hidden.



void bar();

pure nothrow @nogc @safe void foo()
{
    auto fp = &bar;

    (*fp)();

    fp();

    bar();
}
