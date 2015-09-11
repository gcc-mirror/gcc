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

#pragma omp declare simd notinbranch
__attribute__((noinline)) static int
foo (long int a, int b, int c)
{
  return a + b + c;
}

#pragma omp declare simd notinbranch
__attribute__((noinline)) static long int
bar (int a, int b, long int c)
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
  for (i = 0; i < N; i++)
    if (a[i] != i * 2 + 23 + (i % 37) + (i & 63)
	|| b[i] != 17 + (i % 37)
	|| c[i] != i * 4 + 80 + 4 * (i % 37) + 4 * (i & 63))
      abort ();
  fn3 ();
  fn2 ();
  for (i = 0; i < N; i++)
    if (a[i] != i * 2 + 23 + (i % 37) + (i & 63)
	|| b[i] != 17 + (i % 37)
	|| c[i] != i * 4 + 80 + 4 * (i % 37) + 4 * (i & 63)
	|| d[i] != ((unsigned char) (17 + i)) / 2)
      abort ();
  return 0;
}

