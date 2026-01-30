/*
REQUIRED_ARGS: -preview=in -preview=dip1000
TEST_OUTPUT:
----
fail_compilation/previewin3.d(2): Error: function `foo` is not callable using argument types `(int)`
fail_compilation/previewin3.d(2):        cannot pass argument `42` of type `int` to parameter `in WithDtor`
fail_compilation/previewin3.d(8):        `previewin3.foo(in WithDtor)` declared here
----
 */

#line 1
void rvalueErrorMsg () {
    foo(42);
}

// Add a dtor to ensure things are passed by ref
struct WithDtor { ~this() @safe pure nothrow @nogc {} }

void foo(in WithDtor) {}
