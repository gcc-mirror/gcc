/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/test20655.d(29): Deprecation: `@safe` function `g` calling `f1`
fail_compilation/test20655.d(24):        which wouldn't be `@safe` because of:
fail_compilation/test20655.d(24):        accessing overlapped field `U.s` with pointers
fail_compilation/test20655.d(30): Deprecation: `@safe` function `g` calling `f2`
fail_compilation/test20655.d(25):        which wouldn't be `@safe` because of:
fail_compilation/test20655.d(25):        accessing overlapped field `U.s` with pointers
fail_compilation/test20655.d(31): Deprecation: `@safe` function `g` calling `f3`
fail_compilation/test20655.d(28):        which wouldn't be `@safe` because of:
fail_compilation/test20655.d(28):        accessing overlapped field `U.s` with pointers
---
*/

union U
{
    string s;
    int x;
}
U u;

auto f1() { auto s = u.s; } /* Should be inferred as @system. */
void f2()() { auto s = u.s; } /* ditto */
void g() @safe
{
    void f3() { auto s = u.s; } /* ditto */
    f1(); /* Should be rejected with error "cannot call @system function". */
    f2(); /* ditto */
    f3(); /* ditto */
}
