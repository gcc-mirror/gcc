// https://issues.dlang.org/show_bug.cgi?id=13435

/*
TEST_OUTPUT:
---
fail_compilation/fail13435.d(22): Error: cannot implicitly convert expression `d` of type `int[]` to `S!int`
fail_compilation/fail13435.d(22):        `this._a = d` is the first assignment of `this._a` therefore it represents its initialization
fail_compilation/fail13435.d(22):        `opAssign` methods are not used for initialization, but for subsequent assignments
---
*/

struct S(T)
{
    void opAssign(T[] arg) {}
}

class B
{
    this(int[] d)
    {
        S!int c;
        _a = d; // Error: cannot implicitly convert expression (d) of type int[] to S!int
        c = d; // compiles OK
    }

    S!int _a;
}
