/* REQUIRED_ARGS: -dip25
   TEST_OUTPUT:
---
fail_compilation/test16228.d(22): Error: function type 'return int()' has 'return' but does not return any indirections
fail_compilation/test16228.d(23): Error: function test16228.S.foo static member has no 'this' to which 'return' can apply
---
*/



// https://issues.dlang.org/show_bug.cgi?id=16228

int* wrap ( return ref int input )
{
    return &input;
}

struct S
{
    int x;

    int foo() return { return 3; }
    static ref int foo() return { return x; }
}
