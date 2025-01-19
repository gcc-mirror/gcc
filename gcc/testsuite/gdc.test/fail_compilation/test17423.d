/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test17423.d(27): Error: assigning reference to local `this` to non-scope parameter `dlg` calling `opApply` is not allowed in a `@safe` function
fail_compilation/test17423.d(16):        which is not `scope` because of `this.myDlg = dlg`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17423

struct Bar
{
    int delegate(int) @safe myDlg;

    auto opApply(int delegate(int) @safe dlg) @safe {
        myDlg = dlg;
        return 0;
    }
}

struct Foo
{
    Bar o;
    int i = 3;

    this(int x) @safe {
        foreach(_; o) { i = 0; }
        i = x;
    }
}
