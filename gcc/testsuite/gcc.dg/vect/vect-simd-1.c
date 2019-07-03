/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#define N 1024
int a[N];
int x;

__attribute__((noipa)) int
bar (void)
{
  return x;
}

__attribute__((noipa)) void
foo (void)
{
  #pragma omp simd if (bar ())
  for (int i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

__attribute__((noipa)) void
baz (void)
{
  int c = 0;
  #pragma omp simd if (c)
  for (int i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

__attribute__((noipa)) void
qux (void)
{
  int c = 1;
  #pragma omp simd if (c)
  for (int i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

int
main ()
{
  check_vect ();
  foo ();
  for (int i = 0; i < N; ++i)
    if (a[i] != 1)
      abort ();
  x = 1;
  foo ();
  for (int i = 0; i < N; ++i)
    if (a[i] != 2)
      abort ();
  baz ();
  for (int i = 0; i < N; ++i)
    if (a[i] != 3)
      abort ();
  qux ();
  for (int i = 0; i < N; ++i)
    if (a[i] != 4)
      abort ();
  return 0;
}
