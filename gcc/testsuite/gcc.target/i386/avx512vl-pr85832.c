/* PR target/85832 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mno-avx512bw -masm=att" } */
/* { dg-final { scan-assembler-times {\mvptestnmd\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvptestnmq\M} 2 } } */

#include <x86intrin.h>

int
f1 (__m256i x)
{
  return _mm256_cmpeq_epi32_mask (x, _mm256_setzero_si256 ());
}

int
f2 (__m256i x)
{
  return _mm256_cmpeq_epi64_mask (x, _mm256_setzero_si256 ());
}

int
f3 (__m128i x)
{
  return _mm_cmpeq_epi32_mask (x, _mm_setzero_si128 ());
}

int
f4 (__m128i x)
{
  return _mm_cmpeq_epi64_mask (x, _mm_setzero_si128 ());
}
