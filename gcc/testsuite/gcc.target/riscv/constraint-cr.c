/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-final { check-function-bodies "**" "" } } */

void foo(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int m0, int m1) {
/*
** foo:
**   ...
**   addi\s*t0,\s*(a[0-5]|s[0-1]),\s*(a[0-5]|s[0-1])
**   ...
*/
    __asm__ volatile("addi t0, %0, %0" : : "cr" (m0) : "memory");
}
