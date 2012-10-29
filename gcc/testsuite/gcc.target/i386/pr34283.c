/* { dg-do compile } */
/* { dg-options "-O2 -msse4" } */

typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
typedef long long __v2di __attribute__ ((__vector_size__ (16)));

__m128i _mm_set_epi64x (long long __q1, long long __q0)
{
  return __extension__ (__m128i)(__v2di){ __q0, __q1 };
}

/* { dg-final { scan-assembler-not "movdqa" } } */
