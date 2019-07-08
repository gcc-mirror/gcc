/* { dg-require-effective-target size32plus } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 2 "vect" { target i?86-*-* x86_64-*-* } } } */

#ifndef main
#include "tree-vect.h"
#endif

#ifdef __FAST_MATH__
#define FLT_MIN_VALUE (-__FLT_MAX__)
#else
#define FLT_MIN_VALUE (-__builtin_inff ())
#endif

float r = 1.0f, a[1024], b[1024];

__attribute__((noipa)) void
foo (float *a, float *b)
{
  #pragma omp simd reduction (inscan, *:r)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = r;
      #pragma omp scan exclusive(r)
      r *= a[i];
    }
}

__attribute__((noipa)) float
bar (void)
{
  float s = FLT_MIN_VALUE;
  #pragma omp simd reduction (inscan, max:s)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = s;
      #pragma omp scan exclusive(s)
      s = s > a[i] ? s : a[i];
    }
  return s;
}

int
main ()
{
  float s = 1.0f;
#ifndef main
  check_vect ();
#endif
  for (int i = 0; i < 1024; ++i)
    {
      if (i < 80)
	a[i] = (i & 1) ? 0.25f : 0.5f;
      else if (i < 200)
	a[i] = (i % 3) == 0 ? 2.0f : (i % 3) == 1 ? 4.0f : 1.0f;
      else if (i < 280)
	a[i] = (i & 1) ? 0.25f : 0.5f;
      else if (i < 380)
	a[i] = (i % 3) == 0 ? 2.0f : (i % 3) == 1 ? 4.0f : 1.0f;
      else
	switch (i % 6)
	  {
	  case 0: a[i] = 0.25f; break;
	  case 1: a[i] = 2.0f; break;
	  case 2: a[i] = -1.0f; break;
	  case 3: a[i] = -4.0f; break;
	  case 4: a[i] = 0.5f; break;
	  case 5: a[i] = 1.0f; break;
	  default: a[i] = 0.0f; break;
	  }
      b[i] = -19.0f;
      asm ("" : "+g" (i));
    }
  foo (a, b);
  if (r * 16384.0f != 0.125f)
    abort ();
  float m = -175.25f;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      else
	b[i] = -231.75f;
      s *= a[i];
      a[i] = m - ((i % 3) == 1 ? 2.0f : (i % 3) == 2 ? 4.0f : 0.0f);
      m += 0.75f;
    }
  if (bar () != 592.0f)
    abort ();
  s = FLT_MIN_VALUE;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      if (s < a[i])
	s = a[i];
    }
  return 0;
}
