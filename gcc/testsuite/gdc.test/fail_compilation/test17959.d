/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test17959.d(18): Error: assigning scope variable `this` to non-scope `this.escape` is not allowed in a `@safe` function
fail_compilation/test17959.d(19): Error: assigning scope variable `this` to non-scope `this.f` is not allowed in a `@safe` function
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
