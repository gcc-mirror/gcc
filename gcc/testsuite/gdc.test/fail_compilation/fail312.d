/*
TEST_OUTPUT:
---
fail_compilation/fail312.d(13): Error: incompatible types for `(a[]) == (b)`: `int[]` and `short`
fail_compilation/fail312.d(14): Error: incompatible types for `(a[]) <= (b)`: `int[]` and `short`
---
*/

void main()
{
    int[1] a = 1;
    short b = 1;
    assert(a[] == b);
    assert(a[] <= b);
}
