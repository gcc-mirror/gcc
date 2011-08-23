/* { dg-do compile } */
/* { dg-options "-O0 -mavx2" } */

#include <x86intrin.h>

__m128i i1, i2, i3, i4;
__m256i l1, l2, l3, l4;
__m256d e1, e2, e3, e4;

void
test8bit (void)
{
  l1 = _mm256_mpsadbw_epu8 (l2, l3, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_alignr_epi8 (l2, l3, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_blend_epi32 (i1, i1, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_blend_epi32 (l2, l3, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_blend_epi16(l2, l3, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_permute2x128_si256 (l2, l3, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  e1 = _mm256_permute4x64_pd (e2, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_permute4x64_epi64 (l2, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_shuffle_epi32 (l2, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_shufflehi_epi16 (l2, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_shufflelo_epi16 (l2, 256);  /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_slli_si256 (l2, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_srli_si256 (l2, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
}

void
test1bit (void)
{
  i1 = _mm256_extracti128_si256 (l1, 2);  /* { dg-error "the last argument must be an 1-bit immediate" } */
  l1 = _mm256_inserti128_si256 (l1, i2, 2);  /* { dg-error "the last argument must be an 1-bit immediate" } */
}
