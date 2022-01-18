/* { dg-do compile } */
/* { dg-options "-mavx512f -mtune=generic -mtune-ctrl=dest_false_dep_for_glc -O2" } */

#include <immintrin.h>

extern __m512i i1, i2, i3;
extern __m512d d1, d11, *pd1;
extern __m128d d2;
extern __m512 f1, *pf1;
extern __m128 f2;
volatile __m512d *pd11;

__mmask16 m16;
__mmask8 m8;

void vperm_test (void)
{
  d1 = _mm512_permutex_pd (d1, 12);
  d1 = _mm512_mask_permutex_pd (d1, m8, d1, 13);
  d1 = _mm512_maskz_permutex_pd (m8, d1, 14);
  d11 = _mm512_permutexvar_pd (i1, d11);
  d11 = _mm512_mask_permutexvar_pd (d11, m8, i2, d11);
  d11 = _mm512_maskz_permutexvar_pd (m8, i3, d11);

  f1 = _mm512_permutexvar_ps (i1, f1);
  f1 = _mm512_mask_permutexvar_ps (f1, m16, i1, f1);
  f1 = _mm512_maskz_permutexvar_ps (m16, i1, f1);

  i3 = _mm512_permutexvar_epi64 (i3, i3);
  i3 = _mm512_mask_permutexvar_epi64 (i3, m8, i1, i1);
  i3 = _mm512_maskz_permutexvar_epi64 (m8, i3, i1);
  i1 = _mm512_permutex_epi64 (i3, 12);
  i1 = _mm512_mask_permutex_epi64 (i1, m8, i1, 12);
  i1 = _mm512_maskz_permutex_epi64 (m8, i1, 12);

  i2 = _mm512_permutexvar_epi32 (i2, i2);
  i2 = _mm512_mask_permutexvar_epi32 (i2, m16, i2, i2);
  i3 = _mm512_maskz_permutexvar_epi32 (m16, i3, i3);
} 

void getmant_test (void)
{
  d1 = _mm512_getmant_pd (*pd1, _MM_MANT_NORM_p75_1p5,
			  _MM_MANT_SIGN_src);
  d1 = _mm512_getmant_round_pd (*pd11, _MM_MANT_NORM_p75_1p5,
				_MM_MANT_SIGN_src, 8);
  d1 = _mm512_mask_getmant_pd (d1, m8, *pd1, _MM_MANT_NORM_p75_1p5,
			       _MM_MANT_SIGN_src);
  d1 = _mm512_mask_getmant_round_pd (d1, m8, *pd1, _MM_MANT_NORM_p75_1p5,
				     _MM_MANT_SIGN_src, 8);
  d1 = _mm512_maskz_getmant_pd (m8, *pd1, _MM_MANT_NORM_p75_1p5,
				_MM_MANT_SIGN_src);
  d1 = _mm512_maskz_getmant_round_pd (m8, *pd1, _MM_MANT_NORM_p75_1p5,
				      _MM_MANT_SIGN_src, 8);
  f1 = _mm512_getmant_ps (*pf1, _MM_MANT_NORM_p75_1p5,
			  _MM_MANT_SIGN_src);
  f1 = _mm512_getmant_round_ps (*pf1, _MM_MANT_NORM_p75_1p5,
				_MM_MANT_SIGN_src, 8);
  f1 = _mm512_mask_getmant_ps (f1, m16, *pf1, _MM_MANT_NORM_p75_1p5,
			       _MM_MANT_SIGN_src);
  f1 = _mm512_mask_getmant_round_ps (f1, m16, *pf1, _MM_MANT_NORM_p75_1p5,
				     _MM_MANT_SIGN_src, 8);
  f1 = _mm512_maskz_getmant_ps (m16, *pf1, _MM_MANT_NORM_p75_1p5,
				_MM_MANT_SIGN_src);
  f1 = _mm512_maskz_getmant_round_ps (m16, *pf1, _MM_MANT_NORM_p75_1p5,
				      _MM_MANT_SIGN_src, 8);

  d2 = _mm_getmant_sd (d2, d2, _MM_MANT_NORM_p75_1p5,
		       _MM_MANT_SIGN_src);
  d2 = _mm_getmant_round_sd (d2, d2, _MM_MANT_NORM_p75_1p5,
			     _MM_MANT_SIGN_src, 8);
  d2 = _mm_mask_getmant_sd (d2, m8, d2, d2, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
  d2 = _mm_mask_getmant_round_sd (d2, m8, d2, d2, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src, 8);
  d2 = _mm_maskz_getmant_sd (m8, d2, d2, _MM_MANT_NORM_p75_1p5,
			     _MM_MANT_SIGN_src);
  d2 = _mm_maskz_getmant_round_sd (m8, d2, d2, _MM_MANT_NORM_p75_1p5,
				   _MM_MANT_SIGN_src, 8);
  f2 = _mm_getmant_ss (f2, f2, _MM_MANT_NORM_p75_1p5,
		       _MM_MANT_SIGN_src);
  f2 = _mm_getmant_round_ss (f2, f2, _MM_MANT_NORM_p75_1p5,
			     _MM_MANT_SIGN_src, 8);
  f2 = _mm_mask_getmant_ss (f2, m8, f2, f2, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
  f2 = _mm_mask_getmant_round_ss (f2, m8, f2, f2, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src, 8);
  f2 = _mm_maskz_getmant_ss (m8, f2, f2, _MM_MANT_NORM_p75_1p5,
			     _MM_MANT_SIGN_src);
  f2 = _mm_maskz_getmant_round_ss (m8, f2, f2, _MM_MANT_NORM_p75_1p5,
				   _MM_MANT_SIGN_src, 8);

}

/* { dg-final { scan-assembler-times "vxorps" 22 } } */
/* { dg-final { scan-assembler-times "vpermd" 3 } } */
/* { dg-final { scan-assembler-times "vpermq" 6 } } */
/* { dg-final { scan-assembler-times "vpermps" 3 } } */
/* { dg-final { scan-assembler-times "vpermpd" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantpd" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantps" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantsd" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantss" 6 } } */
