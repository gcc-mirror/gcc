/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7em_hard_ok } */
/* { dg-skip-if "-mslow-flash-data and -mword-relocations incompatible" { *-*-* } { "-mword-relocations" } } */
/* { dg-options "-O2 -mslow-flash-data" } */
/* { dg-add-options arm_arch_v7em_hard } */

double __attribute__ ((target ("fpu=fpv5-sp-d16")))
foo (void)
{
  return 1.0f;
}

/* { dg-final { scan-assembler-not "#1\\.0e\\+0" } } */
