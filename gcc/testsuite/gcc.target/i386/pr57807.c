/* { dg-do assemble } */
/* { dg-options "-msse2 -masm=intel" } */
/* { dg-require-effective-target sse2 } */
/* { dg-require-effective-target masm_intel } */

typedef double __v2df __attribute__((__vector_size__(16)));
typedef double __m128d __attribute__((__vector_size__(16), __may_alias__));

__m128d _mm_unpacklo_pd(__m128d __A, __m128d __B) {
  return (__m128d)__builtin_ia32_unpcklpd((__v2df)__A, (__v2df)__B);
}
