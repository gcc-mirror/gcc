/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse --no-warning" } */

typedef double __m128d __attribute__((__vector_size__(16), __may_alias__));
typedef double __m256d __attribute__((__vector_size__(32), __may_alias__));

typedef float __m128 __attribute__((__vector_size__(16), __may_alias__));
typedef float __m256 __attribute__((__vector_size__(32), __may_alias__));

typedef char __m128i __attribute__((__vector_size__(16), __may_alias__));
typedef char __m256i __attribute__((__vector_size__(32), __may_alias__));

__m128d sse4_1_blendvpd (__m128d a, __m128d b, __m128d c) __attribute__((__target__("avx2")));

__m128d
generic_blendvpd (__m128d a, __m128d b, __m128d c) /* { dg-error "SSE register return with SSE disabled" "" { target { ! ia32 } } }  */
{
  return __builtin_ia32_blendvpd (a, b, c); /* { dg-error "needs isa option -msse4.1" "" { target ia32 } } */
}

__m128
generic_blendvps (__m128 a, __m128 b, __m128 c)
{
  return __builtin_ia32_blendvps (a, b, c); /* { dg-error "needs isa option -msse4.1" "" { target ia32 } } */
}

__m128i
generic_pblendvb (__m128i a, __m128i b, __m128i c)
{
  return __builtin_ia32_pblendvb128 (a, b, c);/* { dg-error "needs isa option -msse4.1" "" { target ia32 } } */
}

__m256i
generic_pblendvb256 (__m256i a, __m256i b, __m256i c)
{
  return __builtin_ia32_pblendvb256 (a, b, c);/* { dg-error "needs isa option -mavx2" "" { target ia32 } } */
}

__m256d
generic_blendvpd256 (__m256d a, __m256d b, __m256d c)
{
  return __builtin_ia32_blendvpd256 (a, b, c);/* { dg-error "needs isa option -mavx" "" { target ia32 } } */
}

__m256
generic_blendvps256 (__m256 a, __m256 b, __m256 c)
{
  return __builtin_ia32_blendvps256 (a, b, c);/* { dg-error "needs isa option -mavx" "" { target ia32 } } */
}
