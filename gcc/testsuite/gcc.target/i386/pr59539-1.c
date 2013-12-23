/* PR target/59539 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

#include <immintrin.h>

int
foo (void *p1, void *p2)
{
  __m128i d1 = _mm_loadu_si128 ((__m128i *) p1);
  __m128i d2 = _mm_loadu_si128 ((__m128i *) p2);
  __m128i result = _mm_cmpeq_epi16 (d1, d2);
  return _mm_movemask_epi8 (result);
}

/* { dg-final { scan-assembler-times "vmovdqu" 1 } } */
