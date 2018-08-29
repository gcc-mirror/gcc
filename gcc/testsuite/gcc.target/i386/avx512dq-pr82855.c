/* PR target/82855 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512dq" } */
/* { dg-final { scan-assembler {\mktestb\M} } } */

#include <immintrin.h>

int
foo (const __m256i *ptr)
{
  __m256i v = _mm256_loadu_si256 (ptr);
  __mmask8 m = _mm256_cmpeq_epi32_mask (v, _mm256_setzero_si256 ());
  return 0 == m;
}
