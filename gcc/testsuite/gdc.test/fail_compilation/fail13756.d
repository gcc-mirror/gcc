/*
TEST_OUTPUT:
---
fail_compilation/fail13756.d(11): Error: `foreach`: index must be type `const(int)`, not `int`
---
*/

void maiin()
{
    int[int] aa = [1:2];
    foreach (ref int k, v; aa)
    {
    }
}
