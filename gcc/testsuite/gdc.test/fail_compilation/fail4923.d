/*
TEST_OUTPUT:
---
fail_compilation/fail4923.d(5): Error: immutable variable `bar` initialization is not allowed in `static this`
fail_compilation/fail4923.d(5):        Use `shared static this` instead.
fail_compilation/fail4923.d(6): Deprecation: const variable `baz` initialization is not allowed in `static this`
fail_compilation/fail4923.d(6):        Use `shared static this` instead.
---
*/
#line 1
immutable int bar;
const int baz; // https://issues.dlang.org/show_bug.cgi?id=24056
static this()
{
    bar = 42;
    baz = 43;
}
