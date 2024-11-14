/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-final { check-function-bodies "**" "" } } */


void foo() {
/*
** foo:
**   ...
**   addi\s*t0,\s*9,\s*4
**   ...
*/
  register int s1 __asm__ ("s1");
  register int tp __asm__ ("tp");
  __asm__ volatile("addi t0, %N0, %N1" : : "r" (s1), "r" (tp) : "memory");
}
