/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

int array[N] __attribute__((aligned (32)));

#pragma omp declare simd simdlen(4) notinbranch aligned(a:16) uniform(a) linear(b)
#pragma omp declare simd simdlen(4) notinbranch aligned(a:32) uniform(a) linear(b)
#ifdef __aarch64__
#pragma omp declare simd simdlen(2) notinbranch aligned(a:16) uniform(a) linear(b)
#pragma omp declare simd simdlen(2) notinbranch aligned(a:32) uniform(a) linear(b)
#else
#pragma omp declare simd simdlen(8) notinbranch aligned(a:16) uniform(a) linear(b)
#pragma omp declare simd simdlen(8) notinbranch aligned(a:32) uniform(a) linear(b)
#endif
__attribute__((noinline)) void
foo (int *a, int b, int c)
/* { dg-warning {unsupported simdlen 8 \(amdgcn\)} "" { target amdgcn*-*-* } .-1 } */
/* { dg-warning {unsupported simdlen 4 \(amdgcn\)} "" { target amdgcn*-*-* } .-2 } */
{
  a[b] = c;
}

__attribute__((noinline, noclone)) void
bar ()
{
  int i;
#pragma omp simd
  for (i = 0; i < N; ++i)
    foo (array, i, i * array[i]);
}

__attribute__((noinline, noclone)) void
baz ()
{
  int i;
  for (i = 0; i < N; i++)
    array[i] = 5 * (i & 7);
}

int
main ()
{
  int i;
  check_vect ();
  baz ();
  bar ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (array[i] != 5 * (i & 7) * i)
      abort ();
  return 0;
}
