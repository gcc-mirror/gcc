/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#ifndef N
#define N 1024
#endif

int a[N];
long long int b[N];
short c[N];

#pragma omp declare simd
#pragma omp declare simd uniform(b) linear(c:3)
__attribute__((noinline)) short
foo (int a, long long int b, short c)
{
  return a + b + c;
}

__attribute__((noinline, noclone)) void
bar (int x)
{
  int i;
  if (x == 0)
    {
    #pragma omp simd
      for (i = 0; i < N; i++)
	c[i] = foo (a[i], b[i], c[i]);
    }
  else
    {
    #pragma omp simd
      for (i = 0; i < N; i++)
	c[i] = foo (a[i], x, i * 3);
    }
}

__attribute__((noinline, noclone)) void
baz (void)
{
  int i;
  for (i = 0; i < N; i++)
    {
      a[i] = 2 * i;
      b[i] = -7 * i + 6;
      c[i] = (i & 31) << 4;
    }
}

int
main ()
{
  int i;
  check_vect ();
  baz ();
  bar (0);
  for (i = 0; i < N; i++)
    if (a[i] != 2 * i || b[i] != 6 - 7 * i
	|| c[i] != 6 - 5 * i + ((i & 31) << 4))
      abort ();
    else
      a[i] = c[i];
  bar (17);
  for (i = 0; i < N; i++)
    if (a[i] != 6 - 5 * i + ((i & 31) << 4)
	|| b[i] != 6 - 7 * i
	|| c[i] != 23 - 2 * i + ((i & 31) << 4))
      abort ();
  return 0;
}

