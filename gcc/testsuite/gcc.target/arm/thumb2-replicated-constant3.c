/* Ensure negated/inverted replicated constant immediates work.  */
/* { dg-options "-mthumb -O2" } */
/* { dg-require-effective-target arm_thumb2_ok } */

int
foo1 (int a)
{
  return a | 0xffffff00;
}

/* { dg-final { scan-assembler "orn.*#255" } } */

int
foo2 (int a)
{
  return a & 0xffeeffee;
}

/* { dg-final { scan-assembler "bic.*#1114129" } } */

int
foo3 (int a)
{
  return a & 0xaaaaaa00;
}

/* { dg-final { scan-assembler "and.*#-1431655766" } } */
/* { dg-final { scan-assembler "bic.*#170" } } */
