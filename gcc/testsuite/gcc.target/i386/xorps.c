/* { dg-do compile } */
/* { dg-options "-Os -msse2" } */
/* { dg-require-effective-target sse2 } */

typedef float __m128 __attribute__ ((vector_size (16)));

static __inline __m128
_mm_mul_ps (__m128 __A, __m128 __B)
{
  return __builtin_ia32_mulps (__A, __B);
}

static __inline __m128
_mm_sub_ps (__m128 __A, __m128 __B)
{
  return  __builtin_ia32_subps (__A, __B);
}

__m128 POW_FUNC (__m128 x, __m128 y)
{
    __m128 xmm0 = x, xmm1 = y, xmm2;

    xmm0 = __builtin_ia32_xorps (xmm1, xmm1);

    xmm0 = _mm_mul_ps (xmm0, xmm1);

    xmm0 = _mm_sub_ps (xmm0, xmm1);

    xmm0 = _mm_mul_ps (xmm0, xmm1);

    return xmm0;
}
