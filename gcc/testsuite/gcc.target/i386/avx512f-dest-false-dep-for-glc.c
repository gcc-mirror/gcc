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

__m512d
foo1 (__m512d d2, __m512d d1)
{
  return _mm512_permutex_pd (d1, 12);
}

__m512d
foo2 (__m512d d2, __m512d d1)
{
  return _mm512_mask_permutex_pd (d1, m8, d1, 13);
}

__m512d
foo3 (__m512d d2, __m512d d1)
{
  return _mm512_maskz_permutex_pd (m8, d1, 14);
}

__m512d
foo4 (__m512d d2, __m512d d11, __m512i i1)
{
  return _mm512_permutexvar_pd (i1, d11);
}

__m512d
foo5 (__m512d d2, __m512d d11, __m512i i2)
{
  return _mm512_mask_permutexvar_pd (d11, m8, i2, d11);
}

__m512d
foo6 (__m512d d2, __m512d d11, __m512i i3)
{
  return _mm512_maskz_permutexvar_pd (m8, i3, d11);
}

__m512i
ioo1 (__m512i d2, __m512i d1)
{
  return _mm512_permutex_epi64 (d1, 12);
}

__m512i
ioo2 (__m512i d2, __m512i d1)
{
  return _mm512_mask_permutex_epi64 (d1, m8, d1, 13);
}

__m512i
ioo3 (__m512i d2, __m512i d1)
{
  return _mm512_maskz_permutex_epi64 (m8, d1, 14);
}

__m512i
ioo4 (__m512i d2, __m512i d11, __m512i i1)
{
  return _mm512_permutexvar_epi64 (i1, d11);
}

__m512i
ioo5 (__m512i d2, __m512i d11, __m512i i2)
{
  return _mm512_mask_permutexvar_epi64 (d11, m8, i2, d11);
}

__m512i
ioo6 (__m512i d2, __m512i d11, __m512i i3)
{
  return _mm512_maskz_permutexvar_epi64 (m8, i3, d11);
}

__m512
koo1 (__m512 f2, __m512i i1, __m512 f1)
{
  return _mm512_permutexvar_ps (i1, f1);
}

__m512
koo2 (__m512 f2, __m512i i1, __m512 f1)
{
  return _mm512_mask_permutexvar_ps (f1, m16, i1, f1);
}

__m512
koo3 (__m512 f2, __m512i i1, __m512 f1)
{
  return _mm512_maskz_permutexvar_ps (m16, i1, f1);
}

__m512i
hoo1 (__m512i f2, __m512i i1, __m512i f1)
{
  return _mm512_permutexvar_epi32 (i1, f1);
}

__m512i
hoo2 (__m512i f2, __m512i i1, __m512i f1)
{
  return _mm512_mask_permutexvar_epi32 (f1, m16, i1, f1);
}

__m512i
hoo3 (__m512i f2, __m512i i1, __m512i f1)
{
  return _mm512_maskz_permutexvar_epi32 (m16, i1, f1);
}

__m512d
moo1 (__m512d d2, __m512d* d1)
{
  return _mm512_getmant_pd (*d1, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}

__m512d
moo2 (__m512d d2, __m512d* d1)
{
  return _mm512_getmant_round_pd (*d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src, 8);
}

__m512d
moo3 (__m512d d2, __m512d d1, __m512d* d3)
{

  return _mm512_mask_getmant_pd (d1, m8, *d3, _MM_MANT_NORM_p75_1p5,
				 _MM_MANT_SIGN_src);
}

__m512d
moo4 (__m512d d2, __m512d d1, __m512d* d3)
{
  return _mm512_mask_getmant_round_pd (d1, m8, *d3, _MM_MANT_NORM_p75_1p5,
				       _MM_MANT_SIGN_src, 8);
}

__m512d
moo5 (__m512d d2, __m512d* d1)
{
  return _mm512_maskz_getmant_pd (m8, *d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src);
}

__m512d
moo6 (__m512d d2, __m512d* d1, __m512d d3)
{
  return _mm512_maskz_getmant_round_pd (m8, *d1, _MM_MANT_NORM_p75_1p5,
					_MM_MANT_SIGN_src, 8);
}

__m512
noo1 (__m512 d2, __m512* d1)
{
  return _mm512_getmant_ps (*d1, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}

__m512
noo2 (__m512 d2, __m512* d1)
{
  return _mm512_getmant_round_ps (*d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src, 8);
}

__m512
noo3 (__m512 d2, __m512 d1, __m512* d3)
{

  return _mm512_mask_getmant_ps (d1, m16, *d3, _MM_MANT_NORM_p75_1p5,
				 _MM_MANT_SIGN_src);
}

__m512
noo4 (__m512 d2, __m512 d1, __m512* d3)
{
  return _mm512_mask_getmant_round_ps (d1, m16, *d3, _MM_MANT_NORM_p75_1p5,
				       _MM_MANT_SIGN_src, 8);
}

__m512
noo5 (__m512 d2, __m512* d1)
{
  return _mm512_maskz_getmant_ps (m16, *d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src);
}

__m512
noo6 (__m512 d2, __m512* d1, __m512 d3)
{
  return _mm512_maskz_getmant_round_ps (m16, *d1, _MM_MANT_NORM_p75_1p5,
					_MM_MANT_SIGN_src, 8);
}


__m128d
ooo1 (__m128d d2, __m128d d1)
{
  return _mm_getmant_sd (d1, d1, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}

__m128d
ooo2 (__m128d d2, __m128d d1)
{
  return _mm_getmant_round_sd (d1, d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src, 8);
}

__m128d
ooo3 (__m128d d2, __m128d d1, __m128d d3)
{

  return _mm_mask_getmant_sd (d1, m8, d3, d1,  _MM_MANT_NORM_p75_1p5,
				 _MM_MANT_SIGN_src);
}

__m128d
ooo4 (__m128d d2, __m128d d1, __m128d d3)
{
  return _mm_mask_getmant_round_sd (d1, m8, d3, d1, _MM_MANT_NORM_p75_1p5,
				       _MM_MANT_SIGN_src, 8);
}

__m128d
ooo5 (__m128d d2, __m128d d1)
{
  return _mm_maskz_getmant_sd (m8, d1, d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src);
}

__m128d
ooo6 (__m128d d2, __m128d d1, __m128d d3)
{
  return _mm_maskz_getmant_round_sd (m8, d1, d3, _MM_MANT_NORM_p75_1p5,
					_MM_MANT_SIGN_src, 8);
}

__m128
poo1 (__m128 d2, __m128 d1)
{
  return _mm_getmant_ss (d1, d1, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}

__m128
poo2 (__m128 d2, __m128 d1)
{
  return _mm_getmant_round_ss (d1, d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src, 8);
}

__m128
poo3 (__m128 d2, __m128 d1, __m128 d3)
{

  return _mm_mask_getmant_ss (d1, m8, d3, d1, _MM_MANT_NORM_p75_1p5,
				 _MM_MANT_SIGN_src);
}

__m128
poo4 (__m128 d2, __m128 d1, __m128 d3)
{
  return _mm_mask_getmant_round_ss (d1, m8, d3, d1, _MM_MANT_NORM_p75_1p5,
				       _MM_MANT_SIGN_src, 8);
}

__m128
poo5 (__m128 d2, __m128 d1)
{
  return _mm_maskz_getmant_ss (m8, d1, d1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src);
}

__m128
poo6 (__m128 d2, __m128 d1, __m128 d3)
{
  return _mm_maskz_getmant_round_ss (m8, d1, d3, _MM_MANT_NORM_p75_1p5,
					_MM_MANT_SIGN_src, 8);
}

/* { dg-final { scan-assembler-times "vxorps" 24 } } */
/* { dg-final { scan-assembler-times "vpermd" 3 } } */
/* { dg-final { scan-assembler-times "vpermq" 6 } } */
/* { dg-final { scan-assembler-times "vpermps" 3 } } */
/* { dg-final { scan-assembler-times "vpermpd" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantpd" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantps" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantsd" 6 } } */
/* { dg-final { scan-assembler-times "vgetmantss" 6 } } */
