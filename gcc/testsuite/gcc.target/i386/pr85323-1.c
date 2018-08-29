/* PR target/85323 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return x_\[0-9]*.D.;" 3 "optimized" } } */

#include <x86intrin.h>

__m128i
foo (__m128i x)
{
  x = _mm_sll_epi64 (x, _mm_set1_epi32 (0));
  x = _mm_sll_epi32 (x, _mm_set1_epi32 (0));
  x = _mm_sll_epi16 (x, _mm_set1_epi32 (0));
  x = _mm_srl_epi64 (x, _mm_set1_epi32 (0));
  x = _mm_srl_epi32 (x, _mm_set1_epi32 (0));
  x = _mm_srl_epi16 (x, _mm_set1_epi32 (0));
  x = _mm_sra_epi64 (x, _mm_set1_epi32 (0));
  x = _mm_sra_epi32 (x, _mm_set1_epi32 (0));
  x = _mm_sra_epi16 (x, _mm_set1_epi32 (0));
  x = _mm_slli_epi64 (x, 0);
  x = _mm_slli_epi32 (x, 0);
  x = _mm_slli_epi16 (x, 0);
  x = _mm_srli_epi64 (x, 0);
  x = _mm_srli_epi32 (x, 0);
  x = _mm_srli_epi16 (x, 0);
  x = _mm_srai_epi64 (x, 0);
  x = _mm_srai_epi32 (x, 0);
  x = _mm_srai_epi16 (x, 0);
  return x;
}

__m256i
bar (__m256i x)
{
  x = _mm256_sll_epi64 (x, _mm_set1_epi32 (0));
  x = _mm256_sll_epi32 (x, _mm_set1_epi32 (0));
  x = _mm256_sll_epi16 (x, _mm_set1_epi32 (0));
  x = _mm256_srl_epi64 (x, _mm_set1_epi32 (0));
  x = _mm256_srl_epi32 (x, _mm_set1_epi32 (0));
  x = _mm256_srl_epi16 (x, _mm_set1_epi32 (0));
  x = _mm256_sra_epi64 (x, _mm_set1_epi32 (0));
  x = _mm256_sra_epi32 (x, _mm_set1_epi32 (0));
  x = _mm256_sra_epi16 (x, _mm_set1_epi32 (0));
  x = _mm256_slli_epi64 (x, 0);
  x = _mm256_slli_epi32 (x, 0);
  x = _mm256_slli_epi16 (x, 0);
  x = _mm256_srli_epi64 (x, 0);
  x = _mm256_srli_epi32 (x, 0);
  x = _mm256_srli_epi16 (x, 0);
  x = _mm256_srai_epi64 (x, 0);
  x = _mm256_srai_epi32 (x, 0);
  x = _mm256_srai_epi16 (x, 0);
  return x;
}

__m512i
baz (__m512i x)
{
  x = _mm512_sll_epi64 (x, _mm_set1_epi32 (0));
  x = _mm512_sll_epi32 (x, _mm_set1_epi32 (0));
  x = _mm512_sll_epi16 (x, _mm_set1_epi32 (0));
  x = _mm512_srl_epi64 (x, _mm_set1_epi32 (0));
  x = _mm512_srl_epi32 (x, _mm_set1_epi32 (0));
  x = _mm512_srl_epi16 (x, _mm_set1_epi32 (0));
  x = _mm512_sra_epi64 (x, _mm_set1_epi32 (0));
  x = _mm512_sra_epi32 (x, _mm_set1_epi32 (0));
  x = _mm512_sra_epi16 (x, _mm_set1_epi32 (0));
  x = _mm512_slli_epi64 (x, 0);
  x = _mm512_slli_epi32 (x, 0);
  x = _mm512_slli_epi16 (x, 0);
  x = _mm512_srli_epi64 (x, 0);
  x = _mm512_srli_epi32 (x, 0);
  x = _mm512_srli_epi16 (x, 0);
  x = _mm512_srai_epi64 (x, 0);
  x = _mm512_srai_epi32 (x, 0);
  x = _mm512_srai_epi16 (x, 0);
  return x;
}
