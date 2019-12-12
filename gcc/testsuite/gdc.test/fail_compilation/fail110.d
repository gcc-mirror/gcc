/*
TEST_OUTPUT:
---
fail_compilation/fail110.d(16): Error: variable i is shadowing variable fail110.main.i
fail_compilation/fail110.d(17): Error: variable i is shadowing variable fail110.main.i
fail_compilation/fail110.d(18): Error: variable i is shadowing variable fail110.main.i
---
*/

// Issue 297 - Shadowing declarations allowed in foreach type lists

void main()
{
    int i;
    int[] a;
    foreach (i; a) {}
    foreach (size_t i, n; a) {}
    for (int i;;) {}
}
