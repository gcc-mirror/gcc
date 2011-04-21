/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#include <emmintrin.h>

typedef long long T __attribute__((may_alias));
struct S { __m128i d; };

__m128i
foo (long long *x, struct S *y, __m128i *z)
{
  struct S s = *y;
  ((T *) &s.d)[0] = *x;
  return _mm_cmpeq_epi16 (s.d, *z);
}
