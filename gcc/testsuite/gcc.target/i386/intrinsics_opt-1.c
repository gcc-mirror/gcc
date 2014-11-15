/* { dg-do compile } */
/* { dg-options "-O2 -mfma" } */

#include <emmintrin.h>

__m128d myfma(__m128d x, __m128d y, __m128d z){
  __m128d m = _mm_mul_pd (x, y);
  return _mm_add_pd (m, z);
}

/* { dg-final { scan-assembler "vfmadd" } } */
