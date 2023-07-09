/*
TEST_OUTPUT:
---
fail_compilation/var_func_attr.d(19): Error: cannot implicitly convert expression `__lambda8` of type `void function() nothrow @nogc @safe` to `void function() pure`
---
*/

// Test the effect of function attributes on variables
// See:
//   https://issues.dlang.org/show_bug.cgi?id=7432
//   https://github.com/dlang/dmd/pull/14199
// Usually it's a no-op, but the attribute can apply to the function/delegate type of the variable
// The current behavior is weird, so this is a test of the current behavior, not necessarily the desired behavior

// No-op
pure int x;

// Applies to function type (existing code in dmd and Phobos relies on this)
pure void function() pf = () {
    static int g;
    g++;
};

// Function attributes currently don't apply to inferred types (somewhat surprisingly)
nothrow nf = () {
	throw new Exception("");
};

// Neither do they apply to indirections
alias F = void function();

pure F pf2 = () {
    static int g;
    g++;
};
