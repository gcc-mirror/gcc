/*
TEST_OUTPUT:
---
fail_compilation/fail18.d(14): Error: upper and lower bounds are needed to slice a pointer
---
*/

// 7/25
// Internal error: ..\ztc\cgcod.c 1464

void main ()
{
    int x = 3;
    int[] a = (&x)[];
}
