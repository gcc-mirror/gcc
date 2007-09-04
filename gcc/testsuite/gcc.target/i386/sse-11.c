/* PR rtl-optimization/21239 */
/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#include "sse2-check.h"

#include <emmintrin.h>

void
foo (unsigned int x, double *y, const double *z)
{
  __m128d tmp;
  while (x)
    {
      tmp = _mm_load_sd (z);
      _mm_store_sd (y, tmp);
      --x; ++z; ++y;
    }
}

void
bar (unsigned int x, float *y, const float *z)
{
  __m128 tmp;
  unsigned int i;
  for (i = 0; i < x; ++i)
    {
      tmp = (__m128) { *z, 0, 0, 0 };
      *y = __builtin_ia32_vec_ext_v4sf (tmp, 0);
      ++z; ++y;
    }
  for (i = 0; i < x; ++i)
    {
      tmp = (__m128) { 0, *z, 0, 0 };
      *y = __builtin_ia32_vec_ext_v4sf (tmp, 1);
      ++z; ++y;
    }
  for (i = 0; i < x; ++i)
    {
      tmp = (__m128) { 0, 0, *z, 0 };
      *y = __builtin_ia32_vec_ext_v4sf (tmp, 2);
      ++z; ++y;
    }
  for (i = 0; i < x; ++i)
    {
      tmp = (__m128) { 0, 0, 0, *z };
      *y = __builtin_ia32_vec_ext_v4sf (tmp, 3);
      ++z; ++y;
    }
}

static void
sse2_test (void)
{
  unsigned int i;
  double a[16], b[16];
  float c[16], d[16];
  for (i = 0; i < 16; ++i)
    {
      a[i] = 1;
      b[i] = 2;
      c[i] = 3;
      d[i] = 4;
    }
  foo (16, a, b);
  bar (4, c, d);
  for (i = 0; i < 16; ++i)
    {
      if (a[i] != 2)
	abort ();
      if (c[i] != 4)
	abort ();
    }
}
