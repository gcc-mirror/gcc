/* { dg-do compile } */
/* { dg-options "-O3 -msse" } */
#include <xmmintrin.h>
static const __m128 v_sign = {-.0f, -.0f, -.0f, -.0f};
static const __m128 v_half = {0.5f, 0.5f, 0.5f, 0.5f};
static const __m128 v_one  = {1.0f, 1.0f, 1.0f, 1.0f};
static inline __m128 insn_ABS (__m128 a)
{
  return _mm_andnot_ps (v_sign, a);
}
__m128 voodoo (__m128 a)
{
  __m128 x = insn_ABS (a), y = _mm_rsqrt_ps (x);
  y = _mm_add_ps (_mm_mul_ps (_mm_sub_ps (_mm_setzero_ps(), _mm_sub_ps (_mm_mul_ps (x, _mm_add_ps (_mm_mul_ps (y, y), _mm_setzero_ps())), v_one)), _mm_add_ps (_mm_mul_ps (y, v_half), _mm_setzero_ps())), y);
  return y;
}
