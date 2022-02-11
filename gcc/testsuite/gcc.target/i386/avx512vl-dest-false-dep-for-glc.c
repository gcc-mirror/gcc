/* { dg-do compile } */
/* { dg-options "-mavx512f -mtune=generic -mavx512vl -mtune-ctrl=dest_false_dep_for_glc -O2" } */


#include <immintrin.h>

extern __m256i i1, i2, i3;
extern __m256d d1, d11, *pd1;
extern __m128d d2, *pd2;
extern __m256 f1, *pf1;
extern __m128 f2, *pf2;

__mmask16 m16;
__mmask8 m8;

void vperm_test (void)
{
  d1 = _mm256_permutex_pd (d1, 12);
  d1 = _mm256_mask_permutex_pd (d1, m8, d1, 12);
  d1 = _mm256_maskz_permutex_pd (m8, d1, 12);
  d11 = _mm256_permutexvar_pd (i1, d11);
  d11 = _mm256_mask_permutexvar_pd (d11, m8, i1, d11);
  d11 = _mm256_maskz_permutexvar_pd (m8, i1, d11);

  f1 = _mm256_permutexvar_ps (i1, f1);
  f1 = _mm256_mask_permutexvar_ps (f1, m8, i1, f1);
  f1 = _mm256_maskz_permutexvar_ps (m8, i1, f1);

  i1 = _mm256_permutexvar_epi64 (i1, i1);
  i1 = _mm256_mask_permutexvar_epi64 (i1, m8, i1, i1);
  i1 = _mm256_maskz_permutexvar_epi64 (m8, i1, i1);
  i1 = _mm256_permutex_epi64 (i1, 12);
  i1 = _mm256_mask_permutex_epi64 (i1, m8, i1, 12);
  i1 = _mm256_maskz_permutex_epi64 (m8, i1, 12);

  i2 = _mm256_permutexvar_epi32 (i2, i2);
  i2 = _mm256_mask_permutexvar_epi32 (i2, m8, i2, i2);
  i3 = _mm256_maskz_permutexvar_epi32 (m8, i3, i3);
} 

void getmant_test (void)
{
  d1 = _mm256_getmant_pd (*pd1, _MM_MANT_NORM_p75_1p5,
			  _MM_MANT_SIGN_src);
  d1 = _mm256_mask_getmant_pd (d1, m8, *pd1, _MM_MANT_NORM_p75_1p5,
			       _MM_MANT_SIGN_src);
  d1 = _mm256_maskz_getmant_pd (m8, *pd1, _MM_MANT_NORM_p75_1p5,
				_MM_MANT_SIGN_src);
  d2 = _mm_getmant_pd (*pd2, _MM_MANT_NORM_p75_1p5,
		       _MM_MANT_SIGN_src);
  d2 = _mm_mask_getmant_pd (d2, m8, *pd2, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
  d2 = _mm_maskz_getmant_pd (m8, *pd2, _MM_MANT_NORM_p75_1p5,
			     _MM_MANT_SIGN_src);
  f1 = _mm256_getmant_ps (*pf1, _MM_MANT_NORM_p75_1p5,
			  _MM_MANT_SIGN_src);
  f1 = _mm256_mask_getmant_ps (f1, m8, *pf1, _MM_MANT_NORM_p75_1p5,
			       _MM_MANT_SIGN_src);
  f1 = _mm256_maskz_getmant_ps (m8, *pf1, _MM_MANT_NORM_p75_1p5,
				_MM_MANT_SIGN_src);
  f2 = _mm_getmant_ps (*pf2, _MM_MANT_NORM_p75_1p5,
		       _MM_MANT_SIGN_src);
  f2 = _mm_mask_getmant_ps (f2, m8, *pf2, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
  f2 = _mm_maskz_getmant_ps (m8, *pf2, _MM_MANT_NORM_p75_1p5,
			     _MM_MANT_SIGN_src);
}

/* { dg-final { scan-assembler-times "vxorps" 19 } } */
/* { dg-final { scan-assembler-times "vpermpd" 6 } } */
/* { dg-final { scan-assembler-times "vpermps" 3 } } */
/* { dg-final { scan-assembler-times "vpermq" 6 } } */
/* { dg-final { scan-assembler-times "vpermd" 3 } } */
/* { dg-final { scan-assembler-times "vgetmantpd" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantps" 6 } } */

