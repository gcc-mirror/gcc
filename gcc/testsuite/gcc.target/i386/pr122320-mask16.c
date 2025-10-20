/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-not "vpcmp" } } */

#include <immintrin.h>

__mmask16 dumpy_eq (__m512i vx){
  return _mm512_cmp_epi32_mask (vx, vx, 0);
}

__mmask16 dumpy_lt (__m512i vx)
{
  return _mm512_cmp_epi32_mask (vx, vx, 1);
}

__mmask16 dumpy_le (__m512i vx){
  return _mm512_cmp_epi32_mask (vx, vx, 2);
}

__mmask16 dumpy_ne (__m512i vx)
{
  return _mm512_cmp_epi32_mask (vx, vx, 4);
}

__mmask16 dumpy_nlt (__m512i vx)
{
  return _mm512_cmp_epi32_mask (vx, vx, 5);
}

__mmask16 dumpy_nle (__m512i vx){
  return _mm512_cmp_epi32_mask (vx, vx, 6);
}
