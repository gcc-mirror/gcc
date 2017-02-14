/* PR target/59539 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=haswell" } */

#include <immintrin.h>

int
foo (void *p1, void *p2)
{
  __m256i d1 = _mm256_loadu_si256 ((__m256i *) p1);
  __m256i d2 = _mm256_loadu_si256 ((__m256i *) p2);
  __m256i result = _mm256_cmpeq_epi16 (d1, d2);
  return _mm256_movemask_epi8 (result);
}

/* { dg-final { scan-assembler-times "vmovdqu" 1 } } */
