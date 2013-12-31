/* { dg-do compile } */
/* { dg-options "-O2 -msha" } */
/* { dg-final { scan-assembler "sha1nexte\[ \\t\]+\[^\n\]*%xmm\[0-9\]" } } */

#include <immintrin.h>

volatile __m128i x;

void extern
sha_test (void)
{
  x = _mm_sha1nexte_epu32 (x, x);
}
