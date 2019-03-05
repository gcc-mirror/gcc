/*
TEST_OUTPUT:
---
fail_compilation/aacmp10381.d(12): Error: > is not defined for associative arrays
---
*/

bool test10381()
{
    int[int] aa1 = [0: 1];
    int[int] aa2 = [0: 1];
    return aa1 > aa2;
}
