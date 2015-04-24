/* { dg-do compile } */
/* { dg-options "-march=corei7 -O2" } */

#include <emmintrin.h>

double a[8];

__m128d load_1 ()
{
  __m128d res;
  res = _mm_load_sd (&a[1]);
  res = _mm_loadh_pd (res, &a[2]);
  return res;
}

__m128d load_2 (double *a)
{
  __m128d res;
  res = _mm_load_sd (&a[1]);
  res = _mm_loadh_pd (res, &a[2]);
  return res;
}

/* { dg-final { scan-assembler-times "movup" 2 } } */
