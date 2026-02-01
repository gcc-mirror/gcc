/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/scope_infer_diagnostic.d(34): Error: `@safe` function `scope_infer_diagnostic.outer` cannot call `@system` function `scope_infer_diagnostic.inner!(void delegate(const(char)[]) pure nothrow @nogc @safe).inner`
fail_compilation/scope_infer_diagnostic.d(28):        and assigning scope variable `w` to non-scope parameter `w` calling `callee` makes it fail to infer `@safe`
fail_compilation/scope_infer_diagnostic.d(21):        `w` is not `scope` because of `globalPtr = & w`
fail_compilation/scope_infer_diagnostic.d(25):        `scope_infer_diagnostic.inner!(void delegate(const(char)[]) pure nothrow @nogc @safe).inner` is declared here
---
*/

// Test that scope violation error messages show WHY the callee's parameter
// is not scope when going through the @safe inference chain.
// This is similar to how @nogc violations show the full inference chain.

void* globalPtr;

void callee(Writer)(Writer w) @trusted
{
    // This escapes w, preventing it from being inferred as scope
    globalPtr = cast(void*)&w;
    w("x");
}

void inner(Writer)(scope Writer w)
{
    // Scope violation: passing scope w to non-scope parameter
    callee(w);
}

void outer() @safe
{
    int x;
    inner((const(char)[] s) { x++; });
}
