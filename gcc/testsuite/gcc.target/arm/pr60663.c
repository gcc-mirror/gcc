/* PR rtl-optimization/60663 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v7a } */

int
foo (void)
{
  unsigned i, j;
  asm ("%0 %1" : "=r" (i), "=r" (j));
  return i;
}
