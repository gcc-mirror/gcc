/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -mtune=generic -mtune-ctrl=dest_false_dep_for_glc -O2" } */

#include <immintrin.h>

extern __m512i i1;
extern __m256i i2;
extern __m128i i3;
extern __m512d d1, d11;
extern __m256d d2;
extern __m128d d3, d33;
extern __m512 f1, f11;
extern __m256 f2;
extern __m128 f3, f33;

__mmask32 m32;
__mmask16 m16;
__mmask8 m8;

void mullo_test (void)
{
  i1 = _mm512_mullo_epi64 (i1, i1);
  i1 = _mm512_mask_mullo_epi64 (i1, m8, i1, i1);
  i1 = _mm512_maskz_mullo_epi64 (m8, i1, i1);
  i2 = _mm256_mullo_epi64 (i2, i2);
  i2 = _mm256_mask_mullo_epi64 (i2, m8, i2, i2);
  i2 = _mm256_maskz_mullo_epi64 (m8, i2, i2);
  i3 = _mm_mullo_epi64 (i3, i3);
  i3 = _mm_mask_mullo_epi64 (i3, m8, i3, i3);
  i3 = _mm_maskz_mullo_epi64 (m8, i3, i3);
}

void range_test (void)
{
  d1 = _mm512_range_pd (d1, d11, 15);
  d11 = _mm512_range_round_pd (d11, d1, 15, 8);
  d1 = _mm512_mask_range_pd (d1, m8, d11, d11, 15);
  d11 = _mm512_mask_range_round_pd (d11, m8, d1, d1, 15, 8);
  d1 = _mm512_maskz_range_pd (m8, d11, d11, 15);
  d11 = _mm512_maskz_range_round_pd (m8, d1, d1, 15, 8);
  d2 = _mm256_range_pd (d2, d2, 15);
  d2 = _mm256_mask_range_pd (d2, m8, d2, d2, 15);
  d2 = _mm256_maskz_range_pd (m8, d2, d2, 15);
  d3 = _mm_range_pd (d3, d3, 15);
  d3 = _mm_mask_range_pd (d3, m8, d3, d3, 15);
  d3 = _mm_maskz_range_pd (m8, d3, d3, 15);
  d33 = _mm_range_sd (d33, d33, 15);
  d33 = _mm_mask_range_sd (d33, m8, d33, d33, 15);
  d33 = _mm_maskz_range_sd (m8, d33, d33, 15);

  f1 = _mm512_range_ps (f1, f11, 15);
  f11 = _mm512_range_round_ps (f11, f1, 15, 8);
  f1 = _mm512_mask_range_ps (f1, m16, f11, f11, 15);
  f11 = _mm512_mask_range_round_ps (f11, m16, f1, f1, 15, 8);
  f1 = _mm512_maskz_range_ps (m16, f11, f11, 15);
  f11 = _mm512_maskz_range_round_ps (m16, f1, f1, 15, 8);
  f2 = _mm256_range_ps (f2, f2, 15);
  f2 = _mm256_mask_range_ps (f2, m8, f2, f2, 15);
  f2 = _mm256_maskz_range_ps (m8, f2, f2, 15);
  f3 = _mm_range_ps (f3, f3, 15);
  f3 = _mm_mask_range_ps (f3, m8, f3, f3, 15);
  f3 = _mm_maskz_range_ps (m8, f3, f3, 15);
  f33 = _mm_range_ss (f33, f33, 15);
  f33 = _mm_mask_range_ss (f33, m8, f33, f33, 15);
  f33 = _mm_maskz_range_ss (m8, f33, f33, 15);
}

/* { dg-final { scan-assembler-times "vxorps" 26 } } */
/* { dg-final { scan-assembler-times "vpmullq" 9 } } */
/* { dg-final { scan-assembler-times "vrangepd" 12 } } */
/* { dg-final { scan-assembler-times "vrangesd" 3 } } */
/* { dg-final { scan-assembler-times "vrangeps" 12 } } */
/* { dg-final { scan-assembler-times "vrangess" 3 } } */
