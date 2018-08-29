/* PR target/85323 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "= \{ 0, 0 \};" 12 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= \{ 0, 0, 0, 0 \};" 12 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= \{ 0, 0, 0, 0, 0, 0, 0, 0 \};" 12 "optimized" } } */

#include <x86intrin.h>

void
foo (__m128i x[12])
{
  x[0] = _mm_sll_epi64 (x[0], _mm_set1_epi64x (64));
  x[1] = _mm_sll_epi32 (x[1], _mm_set1_epi64x (32));
  x[2] = _mm_sll_epi16 (x[2], _mm_set1_epi64x (16));
  x[3] = _mm_srl_epi64 (x[3], _mm_set1_epi64x (65));
  x[4] = _mm_srl_epi32 (x[4], _mm_set1_epi64x (33));
  x[5] = _mm_srl_epi16 (x[5], _mm_set1_epi64x (17));
  x[6] = _mm_slli_epi64 (x[6], 66);
  x[7] = _mm_slli_epi32 (x[7], 34);
  x[8] = _mm_slli_epi16 (x[8], 18);
  x[9] = _mm_srli_epi64 (x[9], 67);
  x[10] = _mm_srli_epi32 (x[10], 35);
  x[11] = _mm_srli_epi16 (x[11], 19);
}

void
bar (__m256i x[12])
{
  x[0] = _mm256_sll_epi64 (x[0], _mm_set1_epi64x (64));
  x[1] = _mm256_sll_epi32 (x[1], _mm_set1_epi64x (32));
  x[2] = _mm256_sll_epi16 (x[2], _mm_set1_epi64x (16));
  x[3] = _mm256_srl_epi64 (x[3], _mm_set1_epi64x (65));
  x[4] = _mm256_srl_epi32 (x[4], _mm_set1_epi64x (33));
  x[5] = _mm256_srl_epi16 (x[5], _mm_set1_epi64x (17));
  x[6] = _mm256_slli_epi64 (x[6], 66);
  x[7] = _mm256_slli_epi32 (x[7], 34);
  x[8] = _mm256_slli_epi16 (x[8], 18);
  x[9] = _mm256_srli_epi64 (x[9], 67);
  x[10] = _mm256_srli_epi32 (x[10], 35);
  x[11] = _mm256_srli_epi16 (x[11], 19);
}

void
baz (__m512i x[12])
{
  x[0] = _mm512_sll_epi64 (x[0], _mm_set1_epi64x (64));
  x[1] = _mm512_sll_epi32 (x[1], _mm_set1_epi64x (32));
  x[2] = _mm512_sll_epi16 (x[2], _mm_set1_epi64x (16));
  x[3] = _mm512_srl_epi64 (x[3], _mm_set1_epi64x (65));
  x[4] = _mm512_srl_epi32 (x[4], _mm_set1_epi64x (33));
  x[5] = _mm512_srl_epi16 (x[5], _mm_set1_epi64x (17));
  x[6] = _mm512_slli_epi64 (x[6], 66);
  x[7] = _mm512_slli_epi32 (x[7], 34);
  x[8] = _mm512_slli_epi16 (x[8], 18);
  x[9] = _mm512_srli_epi64 (x[9], 67);
  x[10] = _mm512_srli_epi32 (x[10], 35);
  x[11] = _mm512_srli_epi16 (x[11], 19);
}
