// REQUIRED_ARGS: -dip1000

/*
TEST_OUTPUT:
---
fail_compilation/fail22366.d(13): Error: scope variable `__aaval2` assigned to non-scope `aa[0]`
---
*/

int* fun(scope int* x) @safe
{
    int*[int] aa;
    aa[0] = x; // should give an error
    return aa[0];
}
