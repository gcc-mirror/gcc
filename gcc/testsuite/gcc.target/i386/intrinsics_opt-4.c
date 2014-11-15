/* { dg-do compile } */
/* { dg-options "-O -ffast-math -msse2" } */

#include <emmintrin.h>

__m128d f(__m128d x, __m128d y, __m128d z){
  y = _mm_add_pd (x, y);
  y = _mm_add_pd (z, y);
  return _mm_sub_pd (y, x);
}

/* { dg-final { scan-assembler-not "subpd" } } */
