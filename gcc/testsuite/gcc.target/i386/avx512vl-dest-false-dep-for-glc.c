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

__m256d
foo1 (__m256d d2, __m256d d1)
{
  return _mm256_permutex_pd (d1, 12);
}

__m256d
foo2 (__m256d d2, __m256d d1)
{
  return _mm256_mask_permutex_pd (d1, m8, d1, 13);
}

__m256d
foo3 (__m256d d2, __m256d d1)
{
  return _mm256_maskz_permutex_pd (m8, d1, 14);
}

__m256d
foo4 (__m256d d2, __m256d d11, __m256i i1)
{
  return _mm256_permutexvar_pd (i1, d11);
}

__m256d
foo5 (__m256d d2, __m256d d11, __m256i i2)
{
  return _mm256_mask_permutexvar_pd (d11, m8, i2, d11);
}

__m256d
foo6 (__m256d d2, __m256d d11, __m256i i3)
{
  return _mm256_maskz_permutexvar_pd (m8, i3, d11);
}

__m256i
ioo1 (__m256i d2, __m256i d1)
{
  return _mm256_permutex_epi64 (d1, 12);
}

__m256i
ioo2 (__m256i d2, __m256i d1)
{
  return _mm256_mask_permutex_epi64 (d1, m8, d1, 13);
}

__m256i
ioo3 (__m256i d2, __m256i d1)
{
  return _mm256_maskz_permutex_epi64 (m8, d1, 14);
}

__m256i
ioo4 (__m256i d2, __m256i d11, __m256i i1)
{
  return _mm256_permutexvar_epi64 (i1, d11);
}

__m256i
ioo5 (__m256i d2, __m256i d11, __m256i i2)
{
  return _mm256_mask_permutexvar_epi64 (d11, m8, i2, d11);
}

__m256i
ioo6 (__m256i d2, __m256i d11, __m256i i3)
{
  return _mm256_maskz_permutexvar_epi64 (m8, i3, d11);
}

__m256
koo1 (__m256 f2, __m256i i1, __m256 f1)
{
  return _mm256_permutexvar_ps (i1, f1);
}

__m256
koo2 (__m256 f2, __m256i i1, __m256 f1)
{
  return _mm256_mask_permutexvar_ps (f1, m8, i1, f1);
}

__m256
koo3 (__m256 f2, __m256i i1, __m256 f1)
{
  return _mm256_maskz_permutexvar_ps (m8, i1, f1);
}

__m256i
hoo1 (__m256i f2, __m256i i1, __m256i f1)
{
  return _mm256_permutexvar_epi32 (i1, f1);
}

__m256i
hoo2 (__m256i f2, __m256i i1, __m256i f1)
{
  return _mm256_mask_permutexvar_epi32 (f1, m8, i1, f1);
}

__m256i
hoo3 (__m256i f2, __m256i i1, __m256i f1)
{
  return _mm256_maskz_permutexvar_epi32 (m8, i1, f1);
}

__m256d
moo1 (__m256d d2, __m256d* d1)
{
  return _mm256_getmant_pd (*d1, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}

__m256d
moo3 (__m256d d2, __m256d d1, __m256d* d3)
{

  return _mm256_mask_getmant_pd (d1, m8, *d3, _MM_MANT_NORM_p75_1p5,
				 _MM_MANT_SIGN_src);
}

__m256d
moo5 (__m256d d2, __m256d* d1)
{
  return _mm256_maskz_getmant_pd (m8, *d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src);
}

__m128d
moo2 (__m128d d2, __m128d* d1)
{
  return _mm_getmant_pd (*d1, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}

__m128d
moo4 (__m128d d2, __m128d d1, __m128d* d3)
{

  return _mm_mask_getmant_pd (d1, m8, *d3, _MM_MANT_NORM_p75_1p5,
				 _MM_MANT_SIGN_src);
}

__m128d
moo6 (__m128d d2, __m128d* d1)
{
  return _mm_maskz_getmant_pd (m8, *d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src);
}

__m256
noo1 (__m256 d2, __m256* d1)
{
  return _mm256_getmant_ps (*d1, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}

__m256
noo3 (__m256 d2, __m256 d1, __m256* d3)
{

  return _mm256_mask_getmant_ps (d1, m8, *d3, _MM_MANT_NORM_p75_1p5,
				 _MM_MANT_SIGN_src);
}

__m256
noo5 (__m256 d2, __m256* d1)
{
  return _mm256_maskz_getmant_ps (m8, *d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src);
}

__m128
noo2 (__m128 d2, __m128* d1)
{
  return _mm_getmant_ps (*d1, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}

__m128
noo4 (__m128 d2, __m128 d1, __m128* d3)
{

  return _mm_mask_getmant_ps (d1, m8, *d3, _MM_MANT_NORM_p75_1p5,
				 _MM_MANT_SIGN_src);
}

__m128
noo6 (__m128 d2, __m128* d1)
{
  return _mm_maskz_getmant_ps (m8, *d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src);
}

/* { dg-final { scan-assembler-times "vxorps" 20 } } */
/* { dg-final { scan-assembler-times "vpermpd" 6 } } */
/* { dg-final { scan-assembler-times "vpermps" 3 } } */
/* { dg-final { scan-assembler-times "vpermq" 6 } } */
/* { dg-final { scan-assembler-times "vpermd" 3 } } */
/* { dg-final { scan-assembler-times "vgetmantpd" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantps" 6 } } */

