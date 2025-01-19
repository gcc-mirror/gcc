/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail22138.d(107): Error: returning scope variable `e` is not allowed in a `@safe` function
---
 */

// https://issues.dlang.org/show_bug.cgi?id=22138

#line 100

@safe
int* test()
{
    int*[] a;
    foreach (scope e; a)
    {
        return e;
    }
    return null;
}
