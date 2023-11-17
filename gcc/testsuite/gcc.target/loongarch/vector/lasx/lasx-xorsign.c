/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mlasx" } */
/* { dg-final { scan-assembler "xvand\\.v" } } */
/* { dg-final { scan-assembler "xvxor\\.v" } } */
/* { dg-final { scan-assembler-not "xvfmul" } } */

double
my_xorsign (double *restrict a, double *restrict b, double *restrict c, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i] * __builtin_copysign (1.0d, c[i]);
}

float
my_xorsignf (float *restrict a, float *restrict b, float *restrict c, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i] * __builtin_copysignf (1.0f, c[i]);
}
