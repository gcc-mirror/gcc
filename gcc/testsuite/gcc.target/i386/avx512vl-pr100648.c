/* PR target/100648.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -masm=att" } */
/* { dg-final { scan-assembler-times "\tvpblendvb\t" 2 } } */
/* { dg-final { scan-assembler-not "\tvpcmpeq" } } */
/* { dg-final { scan-assembler-not "\tvpandn" } } */
#include <x86intrin.h>

__m256i
f1 (__m256i a, __m256i b, __m256i mask)
{
  return _mm256_blendv_epi8(a, b, 
    _mm256_andnot_si256(mask, _mm256_set1_epi8(255)));
}

__m128i
f2 (__m128i a, __m128i b, __m128i mask)
{
  return _mm_blendv_epi8(a, b, 
    _mm_andnot_si128(mask, _mm_set1_epi8(255)));
}
