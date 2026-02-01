/*
TEST_OUTPUT:
---
fail_compilation/test20489.d(18): Error: function `pure nothrow @nogc @safe int test20489.D.f(int delegate(int) pure nothrow @nogc @safe body)` does not override any function
fail_compilation/test20489.d(14):        did you mean to override `pure nothrow @nogc @safe int test20489.B.f(scope int delegate(int) pure nothrow @nogc @safe)`?
fail_compilation/test20489.d(18):        parameter 1 is missing `scope`
---
*/

// Test case for https://github.com/dlang/dmd/issues/20489
// Improved error message on override mismatches

class B {
    pure nothrow @nogc @safe int f(scope int delegate(int) pure nothrow @nogc @safe) { return 0; }
}

class D : B {
    override pure nothrow @nogc @safe int f(int delegate(int) pure nothrow @nogc @safe body) { return 0; }
}
