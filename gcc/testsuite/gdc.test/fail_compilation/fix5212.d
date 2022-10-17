/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fix5212.d(14): Error: scope variable `args_` assigned to non-scope `this.args`
---
*/


// https://issues.dlang.org/show_bug.cgi?id=5212

class Foo {
    int[] args;
    @safe this(int[] args_...) {
        args = args_;
    }
}
