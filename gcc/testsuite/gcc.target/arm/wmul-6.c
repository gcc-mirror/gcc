/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long
foo (long long a, unsigned char *b, signed char *c)
{
  return a + (long long)*b * (long long)*c;
}

/* { dg-final { scan-assembler "smlalbb" } } */
