/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp-simd -ftree-vectorize" } */

/* Check that the "omp simd array" variables holding the per-lane partial
   sums are aligned at run time, to 256 bytes (or 128 bytes for a 32-lane
   float array).  */

double
f (double *a, double *b, int n)
{
  double sum = 0;
#pragma omp simd reduction(+:sum)
  for (int i = 0; i < n; i++)
    sum += a[i] * b[i];
  return sum;
}

float
g (float *a, float *b, int n)
{
  float sum = 0;
#pragma omp simd reduction(+:sum)
  for (int i = 0; i < n; i++)
    sum += a[i] * b[i];
  return sum;
}

/* { dg-final { scan-assembler-times {_and_b32\t[^\n]*, -(128|256), } 2 } } */
