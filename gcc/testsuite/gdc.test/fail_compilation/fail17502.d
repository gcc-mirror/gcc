/*
TEST_OUTPUT:
---
fail_compilation/fail17502.d(12): Error: function fail17502.Foo.foo void functions have no result
fail_compilation/fail17502.d(13): Error: cannot have parameter of type const(void)
fail_compilation/fail17502.d(16): Error: function fail17502.Foo.bar void functions have no result
fail_compilation/fail17502.d(17): Error: cannot have parameter of type const(void)
---
*/
class Foo
{
    void foo()
    out (res) { assert(res > 5); }
    body {}

    auto bar()
    out (res) { assert (res > 5); }
    body { return; }
}
