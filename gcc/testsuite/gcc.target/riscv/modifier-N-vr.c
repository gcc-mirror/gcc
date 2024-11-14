/* { dg-do compile } */
/* { dg-options "-march=rv64gv -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma riscv intrinsic "vector"

void foo() {
/*
** foo:
**   ...
**   vadd.vv\s*v0,\s*1,\s*2
**   ...
*/
  register vint32m1_t v1 __asm__ ("v1");
  register vint32m1_t v2 __asm__ ("v2");
  __asm__ volatile("vadd.vv v0, %N0, %N1" : : "vr" (v1), "vr" (v2) : "memory");
}
