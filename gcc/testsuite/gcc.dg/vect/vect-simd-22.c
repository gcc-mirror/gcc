/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx512vl -mno-avx512bw --param vect-partial-vector-usage=2" { target { avx512vl } } } */

#pragma omp declare simd simdlen(2)
float f(double);

void g(float *d, double *a, int n)
{
#pragma omp simd
  for (int i = 0; i < n; i++)
    d[i] = f(a[i]);

}
