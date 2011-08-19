/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long
foo (int *b, int *c)
{
  int tmp = *b * *c;
  return 10 + (long long)tmp;
}

/* { dg-final { scan-assembler "smlal" } } */
