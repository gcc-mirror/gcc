/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-final { scan-assembler-times "vpternlog" 4 } } */
/* { dg-final { scan-assembler-times "\\\{1to4\\\}" 4 } } */
#include<immintrin.h>
extern long long C;
__m256d
copysign2_pd(__m256d from, __m256d to) {
  __m256i a = _mm256_castpd_si256(from);
  __m256d avx_signbit = _mm256_castsi256_pd(_mm256_slli_epi64(_mm256_cmpeq_epi64(a, a), 63));
  /* (avx_signbit & from) | (~avx_signbit & to)  */
  return _mm256_or_pd(_mm256_and_pd(avx_signbit, from), _mm256_andnot_pd(avx_signbit, to));
}

__m256i
mask_pternlog (__m256i A, __m256i B, __mmask8 U)
{
  return _mm256_mask_ternarylogic_epi64 (A, U, B, _mm256_set1_epi64x (C) ,202);
}

__m256i
maskz_pternlog (__m256i A, __m256i B, __mmask8 U)
{
  return _mm256_maskz_ternarylogic_epi64 (U, A, B, _mm256_set1_epi64x (C) ,202);
}

__m256i
none_pternlog (__m256i A, __m256i B)
{
  return _mm256_ternarylogic_epi64 (A, B, _mm256_set1_epi64x (C) ,202);
}
