/*
REQUIRED_ARGS: -preview=in -preview=dip1000
TEST_OUTPUT:
----
fail_compilation/previewin.d(4): Error: function `takeFunction` is not callable using argument types `(void function(real x) pure nothrow @nogc @safe)`
fail_compilation/previewin.d(4):        cannot pass argument `__lambda1` of type `void function(real x) pure nothrow @nogc @safe` to parameter `void function(in real) f`
fail_compilation/previewin.d(11):        `previewin.takeFunction(void function(in real) f)` declared here
fail_compilation/previewin.d(5): Error: function `takeFunction` is not callable using argument types `(void function(scope const(real) x) pure nothrow @nogc @safe)`
fail_compilation/previewin.d(5):        cannot pass argument `__lambda2` of type `void function(scope const(real) x) pure nothrow @nogc @safe` to parameter `void function(in real) f`
fail_compilation/previewin.d(11):        `previewin.takeFunction(void function(in real) f)` declared here
fail_compilation/previewin.d(6): Error: function `takeFunction` is not callable using argument types `(void function(ref scope const(real) x) pure nothrow @nogc @safe)`
fail_compilation/previewin.d(6):        cannot pass argument `__lambda3` of type `void function(ref scope const(real) x) pure nothrow @nogc @safe` to parameter `void function(in real) f`
fail_compilation/previewin.d(11):        `previewin.takeFunction(void function(in real) f)` declared here
fail_compilation/previewin.d(15): Error: scope variable `arg` assigned to global variable `myGlobal`
fail_compilation/previewin.d(16): Error: scope variable `arg` assigned to global variable `myGlobal`
fail_compilation/previewin.d(17): Error: scope parameter `arg` may not be returned
fail_compilation/previewin.d(18): Error: scope variable `arg` assigned to `ref` variable `escape` with longer lifetime
fail_compilation/previewin.d(22): Error: returning `arg` escapes a reference to parameter `arg`
fail_compilation/previewin.d(22):        perhaps annotate the parameter with `return`
----
 */

#line 1
void main ()
{
    // No covariance without explicit `in`
    takeFunction((real x) {});
    takeFunction((const scope real x) {});
    takeFunction((const scope ref real x) {});

    tryEscape("Hello World"); // Yes by `tryEscape` is NG
}

void takeFunction(void function(in real) f);

// Make sure things cannot be escaped (`scope` is applied)
const(char)[] myGlobal;
void tryEscape(in char[] arg) @safe { myGlobal = arg; }
void tryEscape2(scope const char[] arg) @safe { myGlobal = arg; }
const(char)[] tryEscape3(in char[] arg) @safe { return arg; }
void tryEscape4(in char[] arg, ref const(char)[] escape) @safe { escape = arg; }
// Okay: value type
ulong[8] tryEscape5(in ulong[8] arg) @safe { return arg; }
// NG: Ref
ref const(ulong[8]) tryEscape6(in ulong[8] arg) @safe { return arg; }
