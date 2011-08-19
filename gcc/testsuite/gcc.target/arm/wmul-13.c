/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long
foo (int *a, int *b)
{
  return *a + (long long)*b * 10;
}

/* { dg-final { scan-assembler "smlal" } } */
