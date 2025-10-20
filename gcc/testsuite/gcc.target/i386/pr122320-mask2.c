/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-not "vpcmp" } } */

#include <immintrin.h>

__mmask8 dumpy_eq (__m128i vx){
  return _mm_cmp_epi64_mask (vx, vx, 0);
}

__mmask8 dumpy_lt (__m128i vx)
{
  return _mm_cmp_epi64_mask (vx, vx, 1);
}

__mmask8 dumpy_le (__m128i vx){
  return _mm_cmp_epi64_mask (vx, vx, 2);
}

__mmask8 dumpy_ne (__m128i vx)
{
  return _mm_cmp_epi64_mask (vx, vx, 4);
}

__mmask8 dumpy_nlt (__m128i vx)
{
  return _mm_cmp_epi64_mask (vx, vx, 5);
}

__mmask8 dumpy_nle (__m128i vx){
  return _mm_cmp_epi64_mask (vx, vx, 6);
}
