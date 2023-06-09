/* { dg-do compile } */
/* { dg-options "-mavx2 -O2 -funsigned-char" } */
/* { dg-final { scan-assembler-times "vpblendvb" 2 } } */

#include <immintrin.h>
__m128i do_stuff_128(__m128i X0, __m128i X1, __m128i X2) {
  __m128i Result = _mm_blendv_epi8(X0, X1, X2);
  return Result;
}

__m256i do_stuff_256(__m256i X0, __m256i X1, __m256i X2) {
  __m256i Result = _mm256_blendv_epi8(X0, X1, X2);
  return Result;
}
