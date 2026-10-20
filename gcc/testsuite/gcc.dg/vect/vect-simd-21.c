/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx512vl -mavx512bw -mprefer-vector-width=512" { target { avx512bw && avx512vl } } } */

#pragma omp declare simd simdlen(32)
signed char f(signed char) __attribute__((const));

void g(signed char *d, signed char *a, int *m, int n)
{
#pragma omp simd
  for (int i = 0; i < n; i++)
    if (m[i])
      d[i] = f(a[i]);
}
