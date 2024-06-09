/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

int a[N], b[N];
long int c[N];
unsigned char d[N];

#ifdef __aarch64__
#pragma omp declare simd simdlen(2) notinbranch
#else
#pragma omp declare simd simdlen(8) notinbranch
#endif
__attribute__((noinline)) int
foo (long int a, int b, int c)
/* { dg-warning {unsupported simdlen 8 \(amdgcn\)} "" { target amdgcn*-*-* } .-1 } */
{
  return a + b + c;
}

#ifdef __aarch64__
#pragma omp declare simd simdlen(2) notinbranch
#else
#pragma omp declare simd simdlen(8) notinbranch
#endif
__attribute__((noinline)) long int
bar (int a, int b, long int c)
/* { dg-warning {unsupported simdlen 8 \(amdgcn\)} "" { target amdgcn*-*-* } .-1 } */
{
  return a + b + c;
}

__attribute__((noinline)) void
fn1 (void)
{
  int i;
  #pragma omp simd
  for (i = 0; i < N; i++)
    a[i] = foo (c[i], a[i], b[i]) + 6;
  #pragma omp simd
  for (i = 0; i < N; i++)
    c[i] = bar (a[i], b[i], c[i]) * 2;
}

__attribute__((noinline)) void
fn2 (void)
{
  int i;
  #pragma omp simd
  for (i = 0; i < N; i++)
    {
      a[i] = foo (c[i], a[i], b[i]) + 6;
      d[i]++;
    }
  #pragma omp simd
  for (i = 0; i < N; i++)
    {
      c[i] = bar (a[i], b[i], c[i]) * 2;
      d[i] /= 2;
    }
}

__attribute__((noinline)) void
fn3 (void)
{
  int i;
  for (i = 0; i < N; i++)
    {
      a[i] = i * 2;
      b[i] = 17 + (i % 37);
      c[i] = (i & 63);
      d[i] = 16 + i;
    }
}

int
main ()
{
  int i;
  check_vect ();
  fn3 ();
  fn1 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (a[i] != i * 2 + 23 + (i % 37) + (i & 63)
	|| b[i] != 17 + (i % 37)
	|| c[i] != i * 4 + 80 + 4 * (i % 37) + 4 * (i & 63))
      abort ();
  fn3 ();
  fn2 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (a[i] != i * 2 + 23 + (i % 37) + (i & 63)
	|| b[i] != 17 + (i % 37)
	|| c[i] != i * 4 + 80 + 4 * (i % 37) + 4 * (i & 63)
	|| d[i] != ((unsigned char) (17 + i)) / 2)
      abort ();
  return 0;
}
