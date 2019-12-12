/*
TEST_OUTPUT:
---
fail_compilation/fail18.d(14): Error: need upper and lower bound to slice pointer
---
*/

// 7/25
// Internal error: ..\ztc\cgcod.c 1464

void main ()
{
    int x = 3;
    int[] a = (&x)[];
}
