/*
TEST_OUTPUT:
---
fail_compilation/fail19913.d(11): Error: no property `b` for `a` of type `int`
fail_compilation/fail19913.d(11): Error: mixin `fail19913.S.b!()` is not defined
---
*/

struct S
{
    mixin a.b;
    enum { a }
}
