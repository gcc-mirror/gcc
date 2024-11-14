/* { dg-do compile } */
/* { dg-options "-march=rv64if -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-final { check-function-bodies "**" "" } } */

void foo() {
/*
** foo:
**   ...
**   fadd.s\s*ft0,\s*8,\s*9
**   ...
*/
  register float fs0 __asm__ ("fs0");
  register float fs1 __asm__ ("fs1");
  __asm__ volatile("fadd.s ft0, %N0, %N1" : : "f" (fs0), "f" (fs1) : "memory");
}
