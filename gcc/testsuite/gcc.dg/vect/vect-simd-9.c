/* { dg-require-effective-target size32plus } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 2 "vect" { target i?86-*-* x86_64-*-* } } } */

#ifndef main
#include "tree-vect.h"
#endif

int r, a[1024], b[1024];

#pragma omp declare reduction (foo: int: omp_out += omp_in) initializer (omp_priv = 0)

__attribute__((noipa)) void
foo (int *a, int *b)
{
  #pragma omp simd reduction (inscan, foo:r)
  for (int i = 0; i < 1024; i++)
    {
      r += a[i];
      #pragma omp scan inclusive(r)
      b[i] = r;
    }
}

__attribute__((noipa)) int
bar (void)
{
  int s = 0;
  #pragma omp simd reduction (inscan, foo:s)
  for (int i = 0; i < 1024; i++)
    {
      s += 2 * a[i];
      #pragma omp scan inclusive(s)
      b[i] = s;
    }
  return s;
}

__attribute__((noipa)) void
baz (int *a, int *b)
{
  #pragma omp simd reduction (inscan, foo:r) if (simd: 0)
  for (int i = 0; i < 1024; i++)
    {
      r += a[i];
      #pragma omp scan inclusive(r)
      b[i] = r;
    }
}

__attribute__((noipa)) int
qux (void)
{
  int s = 0;
  #pragma omp simd reduction (inscan, foo:s) simdlen (1)
  for (int i = 0; i < 1024; i++)
    {
      s += 2 * a[i];
      #pragma omp scan inclusive(s)
      b[i] = s;
    }
  return s;
}

int
main ()
{
  int s = 0;
#ifndef main
  check_vect ();
#endif
  for (int i = 0; i < 1024; ++i)
    {
      a[i] = i;
      b[i] = -1;
      asm ("" : "+g" (i));
    }
  foo (a, b);
  if (r != 1024 * 1023 / 2)
    abort ();
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    {
      s += i;
      if (b[i] != s)
	abort ();
      else
	b[i] = 25;
    }
  if (bar () != 1024 * 1023)
    abort ();
  s = 0;
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    {
      s += 2 * i;
      if (b[i] != s)
	abort ();
      else
	b[i] = -1;
    }
  r = 0;
  baz (a, b);
  if (r != 1024 * 1023 / 2)
    abort ();
  s = 0;
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    {
      s += i;
      if (b[i] != s)
	abort ();
      else
	b[i] = -25;
    }
  if (qux () != 1024 * 1023)
    abort ();
  s = 0;
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    {
      s += 2 * i;
      if (b[i] != s)
	abort ();
    }
  return 0;
}
