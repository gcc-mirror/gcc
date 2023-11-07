/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v5te_arm_ok } */
/* { dg-skip-if "FDPIC does not support tailcall optimization" { arm*-*-uclinuxfdpiceabi } "*" "" } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v5te_arm } */
/* { dg-final { scan-assembler "bx" } } */
/* { dg-final { scan-assembler-not "blx" } } */

int lcal (int) __attribute__ ((long_call));

int
dec (int a)
{
  return lcal (a);
}
