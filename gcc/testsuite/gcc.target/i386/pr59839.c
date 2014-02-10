/* PR target/59839 */
/* { dg-do compile } */
/* { dg-options "-O0 -mavx2" } */

#include <x86intrin.h>

void
test (const float *x)
{
  __m256i i = _mm256_set1_epi32 (1);
  __m256 d = _mm256_i32gather_ps (x, i, 1);
}
