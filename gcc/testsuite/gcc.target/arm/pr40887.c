/* { dg-do compile } */
/* { dg-skip-if "need at least armv5" { *-*-* } { "-march=armv[234]*" } { "" } } */
/* { dg-skip-if "FDPIC does not support armv5te" { arm*-*-uclinuxfdpiceabi } "*" "" } */
/* { dg-require-effective-target arm_arch_v5te_ok } */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_arch_v5te } */
/* { dg-final { scan-assembler "blx" } } */

int (*indirect_func)(int x);

int indirect_call()
{
  return indirect_func(20) + indirect_func (40);
}
