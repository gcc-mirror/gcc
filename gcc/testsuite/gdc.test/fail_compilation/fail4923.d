/*
TEST_OUTPUT:
---
fail_compilation/fail4923.d(4): Error: immutable variable `bar` initialization is not allowed in `static this`
fail_compilation/fail4923.d(4):        Use `shared static this` instead.
---
*/
#line 1
immutable int bar;
static this()
{
    bar = 42;
}
