/* { dg-do compile } */
/* { dg-options "-msse2 -O2" } */

/* We used to ICE because of a bogous pattern.  */

typedef double __v2df __attribute__ ((__vector_size__ (16)));
typedef __v2df __m128d;
static __inline __m128d __attribute__((__always_inline__)) _mm_set1_pd (double __F) {
  return __extension__ (__m128d){__F, __F};
}
static __inline __m128d __attribute__((__always_inline__)) _mm_move_sd (__m128d __A, __m128d __B) {
  return (__m128d) __builtin_ia32_movsd ((__v2df)__A, (__v2df)__B);
}
void g(__m128d b);
__m128d cross(__m128d tmp9)
{
  __m128d t1 = _mm_set1_pd(1.0);
  __m128d tmp10 = _mm_move_sd(t1, tmp9);
  return tmp10;
}
