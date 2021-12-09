// REQUIRED_ARGS: -de
/* TEST_OUTPUT:
---
fail_compilation/b19691.d(12): Error: forward reference to template `this`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=19691
module b19691;

struct S1 {
    this(T...)(T) {
        S2(42, "");
    }
}

struct S2 {
    this(int a, string) {}
    this(int a, S1 s = null) {}
}
