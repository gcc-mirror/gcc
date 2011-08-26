/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long
foo (int *b)
{
  return 10 * (long long)*b;
}

/* { dg-final { scan-assembler "smull" } } */
