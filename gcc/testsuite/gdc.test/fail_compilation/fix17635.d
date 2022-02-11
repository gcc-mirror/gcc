/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fix17635.d(22): Error: cannot implicitly convert expression `f(& p)` of type `immutable(int)**` to `immutable(int**)`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=17635
// https://issues.dlang.org/show_bug.cgi?id=15660

alias T = immutable int;

T** f(const T** input) pure
{
    T** output;
    return output;
}

void main()
{
    T i;
    T* p = &i;
    immutable T** r = f(&p);
}
