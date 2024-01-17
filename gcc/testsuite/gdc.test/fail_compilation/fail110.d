/*
TEST_OUTPUT:
---
fail_compilation/fail110.d(19): Error: variable `i` is shadowing variable `fail110.main.i`
fail_compilation/fail110.d(17):        declared here
fail_compilation/fail110.d(20): Error: variable `i` is shadowing variable `fail110.main.i`
fail_compilation/fail110.d(17):        declared here
fail_compilation/fail110.d(21): Error: variable `i` is shadowing variable `fail110.main.i`
fail_compilation/fail110.d(17):        declared here
---
*/

// https://issues.dlang.org/show_bug.cgi?id=297
// Shadowing declarations allowed in foreach type lists
void main()
{
    int i;
    int[] a;
    foreach (i; a) {}
    foreach (size_t i, n; a) {}
    for (int i;;) {}
}
