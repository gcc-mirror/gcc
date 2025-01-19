/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/test20655.d(26): Deprecation: `@safe` function `g` calling `f1`
fail_compilation/test20655.d(21):        and accessing overlapped field `U.s` with pointers makes it fail to infer `@safe`
fail_compilation/test20655.d(27): Deprecation: `@safe` function `g` calling `f2`
fail_compilation/test20655.d(22):        and accessing overlapped field `U.s` with pointers makes it fail to infer `@safe`
fail_compilation/test20655.d(28): Deprecation: `@safe` function `g` calling `f3`
fail_compilation/test20655.d(25):        and accessing overlapped field `U.s` with pointers makes it fail to infer `@safe`
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
