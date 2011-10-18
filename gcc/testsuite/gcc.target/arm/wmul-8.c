/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long
foo (long long a, int *b, int *c)
{
  return a + (long long)*b * *c;
}

/* { dg-final { scan-assembler "smlal" } } */
