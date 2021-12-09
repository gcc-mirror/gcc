/*
TEST_OUTPUT:
---
fail_compilation/fail20618.d(13): Error: in slice `a[1 .. 12]`, upper bound is greater than array length `10`
fail_compilation/fail20618.d(14): Error: in slice `a[4 .. 3]`, lower bound is greater than upper bound
fail_compilation/fail20618.d(15): Error: in slice `a[0 .. 11]`, upper bound is greater than array length `10`
---
*/

void main()
{
    int[10] a;
    auto b = a[1..12];
    auto c = a[4..3];
    auto d = a[0..$ + 1];
}
