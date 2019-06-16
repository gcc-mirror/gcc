// REQUIRED_ARGS: -de
/* TEST_OUTPUT:
---
fail_compilation/b19691.d(13): Error: forward reference to template `this`
fail_compilation/b19691.d(19): Deprecation: constructor `b19691.S2.this` all parameters have default arguments, but structs cannot have default constructors.
---
*/
// https://issues.dlang.org/show_bug.cgi?id=19691
module b19691;

struct S1 {
    this(T...)(T) {
        S2("");
    }
}

struct S2 {
    this(string) {}
    this(S1 s = null) {}
}
