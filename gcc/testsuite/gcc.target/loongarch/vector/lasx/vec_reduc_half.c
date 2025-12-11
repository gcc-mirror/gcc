/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mlasx" } */

double
foo_1 (double *a, double *b)
{
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3];
}

/* { dg-final { scan-assembler-times "xvpermi.d" 1} } */
