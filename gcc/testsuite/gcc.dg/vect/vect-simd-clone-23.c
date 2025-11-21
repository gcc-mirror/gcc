/* { dg-do compile } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx512bw" { target avx512bw } } */

#pragma omp declare simd simdlen(32) inbranch
int __attribute__((const)) baz (int x);

short a[1024];

void __attribute__((noipa))
foo (int n, int * __restrict b)
{
  for (int i = 0; i < n; ++i)
    if (a[i])
      b[i] = baz (b[i]);
}
