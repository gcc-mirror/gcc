// { dg-require-effective-target size32plus }
// { dg-additional-options "-fopenmp-simd" }
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 2 "vect" { target i?86-*-* x86_64-*-* } } }

#include "../../gcc.dg/vect/tree-vect.h"

int r, a[1024], b[1024], q;

#pragma omp declare reduction (foo: int: omp_out += omp_in) initializer (omp_priv = 0)

__attribute__((noipa)) void
foo (int *a, int *b, int &r)
{
  #pragma omp simd reduction (inscan, foo:r)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = r;
      #pragma omp scan exclusive(r)
      r += a[i];
    }
}

__attribute__((noipa)) int
bar (void)
{
  int &s = q;
  q = 0;
  #pragma omp simd reduction (inscan, foo:s)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = s;
      #pragma omp scan exclusive(s)
      s += 2 * a[i];
    }
  return s;
}

__attribute__((noipa)) void
baz (int *a, int *b, int &r)
{
  #pragma omp simd reduction (inscan, foo:r) if (simd: 0)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = r;
      #pragma omp scan exclusive(r)
      r += a[i];
    }
}

__attribute__((noipa)) int
qux (void)
{
  int &s = q;
  q = 0;
  #pragma omp simd reduction (inscan, foo:s) simdlen (1)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = s;
      #pragma omp scan exclusive(s)
      s += 2 * a[i];
    }
  return s;
}

int
main ()
{
  int s = 0;
  check_vect ();
  for (int i = 0; i < 1024; ++i)
    {
      a[i] = i;
      b[i] = -1;
      asm ("" : "+g" (i));
    }
  foo (a, b, r);
  if (r != 1024 * 1023 / 2)
    abort ();
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      else
	b[i] = 25;
      s += i;
    }
  if (bar () != 1024 * 1023)
    abort ();
  s = 0;
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      else
	b[i] = -1;
      s += 2 * i;
    }
  r = 0;
  baz (a, b, r);
  if (r != 1024 * 1023 / 2)
    abort ();
  s = 0;
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      else
	b[i] = -25;
      s += i;
    }
  if (qux () != 1024 * 1023)
    abort ();
  s = 0;
#pragma GCC novector
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      s += 2 * i;
    }
  return 0;
}
