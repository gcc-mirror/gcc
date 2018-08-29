/* PR target/85323 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return x_\[0-9]*.D.;" 3 "optimized" } } */

#include <x86intrin.h>

__m128i
foo (__m128i x)
{
  x = _mm_sllv_epi64 (x, _mm_set1_epi32 (0));
  x = _mm_sllv_epi32 (x, _mm_set1_epi32 (0));
  x = _mm_sllv_epi16 (x, _mm_set1_epi32 (0));
  x = _mm_srlv_epi64 (x, _mm_set1_epi32 (0));
  x = _mm_srlv_epi32 (x, _mm_set1_epi32 (0));
  x = _mm_srlv_epi16 (x, _mm_set1_epi32 (0));
  x = _mm_srav_epi64 (x, _mm_set1_epi32 (0));
  x = _mm_srav_epi32 (x, _mm_set1_epi32 (0));
  x = _mm_srav_epi16 (x, _mm_set1_epi32 (0));
  return x;
}

__m256i
bar (__m256i x)
{
  x = _mm256_sllv_epi64 (x, _mm256_set1_epi32 (0));
  x = _mm256_sllv_epi32 (x, _mm256_set1_epi32 (0));
  x = _mm256_sllv_epi16 (x, _mm256_set1_epi32 (0));
  x = _mm256_srlv_epi64 (x, _mm256_set1_epi32 (0));
  x = _mm256_srlv_epi32 (x, _mm256_set1_epi32 (0));
  x = _mm256_srlv_epi16 (x, _mm256_set1_epi32 (0));
  x = _mm256_srav_epi64 (x, _mm256_set1_epi32 (0));
  x = _mm256_srav_epi32 (x, _mm256_set1_epi32 (0));
  x = _mm256_srav_epi16 (x, _mm256_set1_epi32 (0));
  return x;
}

__m512i
baz (__m512i x)
{
  x = _mm512_sllv_epi64 (x, _mm512_setzero_epi32 ());
  x = _mm512_sllv_epi32 (x, _mm512_setzero_epi32 ());
  x = _mm512_sllv_epi16 (x, _mm512_setzero_epi32 ());
  x = _mm512_srlv_epi64 (x, _mm512_setzero_epi32 ());
  x = _mm512_srlv_epi32 (x, _mm512_setzero_epi32 ());
  x = _mm512_srlv_epi16 (x, _mm512_setzero_epi32 ());
  x = _mm512_srav_epi64 (x, _mm512_setzero_epi32 ());
  x = _mm512_srav_epi32 (x, _mm512_setzero_epi32 ());
  x = _mm512_srav_epi16 (x, _mm512_setzero_epi32 ());
  return x;
}
