/*
TEST_OUTPUT:
---
fail_compilation/fail17502.d(13): Error: function `fail17502.Foo.foo` `void` functions have no result
fail_compilation/fail17502.d(13): Error: undefined identifier `res`
fail_compilation/fail17502.d(17): Error: function `fail17502.Foo.bar` `void` functions have no result
fail_compilation/fail17502.d(17): Error: undefined identifier `res`
---
*/
class Foo
{
    void foo()
    out (res) { assert(res > 5); }
    do {}

    auto bar()
    out (res) { assert (res > 5); }
    do { return; }
}
