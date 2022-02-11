/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test17959.d(18): Error: scope variable `this` assigned to non-scope `this.escape`
fail_compilation/test17959.d(19): Error: scope variable `this` assigned to non-scope `this.f`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17959

class Foo
{
    void delegate () @safe escape;
    Foo f;

    void escfoo() @safe scope
    {
        this.escape = &this.escfoo;
        f = this;
    }
}
