/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse -msse2" } */

/* Test that the intrinsics compile with optimization.  These were not
   tested in i386-sse-[12].c because these builtins require immediate
   operands.  */

#include <xmmintrin.h>

__m128
test_shuf (void)
{
  __m128 a = _mm_set1_ps (1.0);
  __m128 b = _mm_set1_ps (2.0);
  return _mm_shuffle_ps (a, b, _MM_SHUFFLE (0,1,2,3));
}

__m64
test_ins_ext (__m64 a)
{
  return _mm_insert_pi16 (a, _mm_extract_pi16 (a, 0), 3);
}

__m64
test_shuf2 (__m64 a)
{
  return _mm_shuffle_pi16 (a, 0xA5);
}

void
test_prefetch (char *p)
{
  _mm_prefetch (p, _MM_HINT_T0);
  _mm_prefetch (p+4, _MM_HINT_T1);
  _mm_prefetch (p+8, _MM_HINT_T2);
  _mm_prefetch (p+12, _MM_HINT_NTA);
}

__m128i
test__slli_si128 (__m128i a)
{
  return _mm_slli_si128 (a, 3);
}

__m128i
test__srli_si128 (__m128i a)
{
  return _mm_srli_si128 (a, 3);
}
