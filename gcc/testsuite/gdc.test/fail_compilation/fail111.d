/*
TEST_OUTPUT:
---
fail_compilation/fail111.d(12): Error: can't have array of int(int)
---
*/

// Issue 289 - Compiler allows (and crashes on) dynamic arrays of typedefs of "immediate"-function types

alias int ft(int);

ft[] x;  // is allowed

void test()
{
    x.length = 2;  // crashes DMD
}
