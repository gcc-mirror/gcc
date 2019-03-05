/*
TEST_OUTPUT:
---
fail_compilation/fail306.d(11): Error: cannot perform array operations on `void[]` arrays
---
*/

void bar()
{
    void [] x;
    x[] = -x[];
}
