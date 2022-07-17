/* REQUIRED_ARGS: -preview=fixImmutableConv
TEST_OUTPUT:
---
fail_compilation/test15660.d(20): Error: cannot implicitly convert expression `f(v)` of type `int[]` to `immutable(int[])`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=15660

int[] f(ref void[] m) pure
{
    auto result = new int[5];
    m = result;
    return result;
}

void main()
{
    void[] v;
    immutable x = f(v);
}
