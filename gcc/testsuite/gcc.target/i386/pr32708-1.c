/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

typedef long long __v2di __attribute__ ((__vector_size__ (16)));
typedef long long __m128i __attribute__ ((__vector_size__ (16)));

static __inline __m128i __attribute__((__always_inline__))
_mm_set_epi64x (long long __q1, long long __q0)
{
  return __extension__ (__m128i)(__v2di){ __q0, __q1 };
}

__m128i long2vector(long long __i)
{
  return _mm_set_epi64x (0, __i);
}

/* { dg-final { scan-assembler-not "movq2dq" } } */
