/* PR target/85323 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "= \{ 0, 0 \};" 6 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= \{ 0, 0, 0, 0 \};" 6 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= \{ 0, 0, 0, 0, 0, 0, 0, 0 \};" 6 "optimized" } } */

#include <x86intrin.h>

void
foo (__m128i x[6])
{
  x[0] = _mm_sllv_epi64 (x[0], _mm_set_epi64x (64, 65));
  x[1] = _mm_sllv_epi32 (x[1], _mm_set_epi32 (32, 33, 34, 32));
  x[2] = _mm_sllv_epi16 (x[2], _mm_set_epi16 (16, 18, -16, -1, 19, 16, 18, 20));
  x[3] = _mm_srlv_epi64 (x[3], _mm_set_epi64x (65, -1));
  x[4] = _mm_srlv_epi32 (x[4], _mm_set_epi32 (33, 32, 39, -5));
  x[5] = _mm_srlv_epi16 (x[5], _mm_set1_epi16 (17));
}

void
bar (__m256i x[6])
{
  x[0] = _mm256_sllv_epi64 (x[0], _mm256_set_epi64x (64, 65, -2, 66));
  x[1] = _mm256_sllv_epi32 (x[1], _mm256_set_epi32 (32, 32, 39, -4, -32, 98, 2048, 32));
  x[2] = _mm256_sllv_epi16 (x[2], _mm256_set_epi16 (16, 32, 64, 128, 16, 16, 32, -5,
						    -1, -2, -3, 16, 17, 18, 19, 200));
  x[3] = _mm256_srlv_epi64 (x[3], _mm256_set_epi64x (65, 64, -5, 64));
  x[4] = _mm256_srlv_epi32 (x[4], _mm256_set_epi32 (33, 49, 2048, 32, -1, 32, 37, 16384));
  x[5] = _mm256_srlv_epi16 (x[5], _mm256_set1_epi16 (17));
}

void
baz (__m512i x[6])
{
  x[0] = _mm512_sllv_epi64 (x[0], _mm512_set_epi64 (64, 64, 69, -1, 2048, 64, 16348, -64));
  x[1] = _mm512_sllv_epi32 (x[1], _mm512_set_epi32 (32, 33, 34, 35, 36, -32, -33, -34,
						    -1, -2, -3, -4, -5, -6, 32, 2048));
  x[2] = _mm512_sllv_epi16 (x[2], _mm512_set_epi16 (16, 32, 64, 128, 16, 16, 32, -5,
						    -1, -2, -3, 16, 17, 18, 19, 200,
						    16, 19, 2048, 16, -2, -8, -19, 16,
						    -1, -2, -3, -4, -5, -6, -7, -8));
  x[3] = _mm512_srlv_epi64 (x[3], _mm512_set_epi64 (65, 64, 69, 68, 64, 79, 2048, -1));
  x[4] = _mm512_srlv_epi32 (x[4], _mm512_set_epi32 (32, 33, 34, 35, 36, -32, -33, -34,
						    -1, -2, -3, -4, -5, -6, 32, 2048));
  x[5] = _mm512_srlv_epi16 (x[5], _mm512_set1_epi16 (17));
}
