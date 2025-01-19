/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-additional-options "-std=gnu99" } */

void foo(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int m0, int m1) {
/*
** foo:
**   ...
**   addi t1, (a[0246]|s[02468]|t[02]), 1
**   ...
*/
    __asm__ volatile("addi t1, %0, 1" : : "R" (a1) : "memory");
}
void foo2(int a0, long long a1a2) {
/*
** foo2:
**   ...
**   addi t1, (a[0246]|s[02468]|t[02]), 1
**   ...
*/
    __asm__ volatile("addi t1, %0, 1" : : "R" (a1a2) : "memory");
}
