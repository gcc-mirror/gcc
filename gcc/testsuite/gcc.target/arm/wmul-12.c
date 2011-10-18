/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long
foo (int *b, int *c)
{
  long long tmp = (long long)*b * *c;
  return 10 + tmp;
}

/* { dg-final { scan-assembler "smlal" } } */
