/* PR target/79932 */
/* { dg-do compile } */
/* { dg-options "-O0 -mavx512vl" } */

#include <x86intrin.h>

__m256i a, b;
__m128i c, d;
__mmask32 e, f[64];

void
foo (void)
{
  f[0] = _mm256_cmpge_epi32_mask (a, b);
  f[1] = _mm256_cmpge_epi64_mask (a, b);
  f[2] = _mm256_cmpge_epu32_mask (a, b);
  f[3] = _mm256_cmpge_epu64_mask (a, b);
  f[4] = _mm256_cmple_epi32_mask (a, b);
  f[5] = _mm256_cmple_epi64_mask (a, b);
  f[6] = _mm256_cmple_epu32_mask (a, b);
  f[7] = _mm256_cmple_epu64_mask (a, b);
  f[8] = _mm256_cmplt_epi32_mask (a, b);
  f[9] = _mm256_cmplt_epi64_mask (a, b);
  f[10] = _mm256_cmplt_epu32_mask (a, b);
  f[11] = _mm256_cmplt_epu64_mask (a, b);
  f[12] = _mm256_cmpneq_epi32_mask (a, b);
  f[13] = _mm256_cmpneq_epi64_mask (a, b);
  f[14] = _mm256_cmpneq_epu32_mask (a, b);
  f[15] = _mm256_cmpneq_epu64_mask (a, b);
  f[16] = _mm256_mask_cmpge_epi32_mask (e, a, b);
  f[17] = _mm256_mask_cmpge_epi64_mask (e, a, b);
  f[18] = _mm256_mask_cmpge_epu32_mask (e, a, b);
  f[19] = _mm256_mask_cmpge_epu64_mask (e, a, b);
  f[20] = _mm256_mask_cmple_epi32_mask (e, a, b);
  f[21] = _mm256_mask_cmple_epi64_mask (e, a, b);
  f[22] = _mm256_mask_cmple_epu32_mask (e, a, b);
  f[23] = _mm256_mask_cmple_epu64_mask (e, a, b);
  f[24] = _mm256_mask_cmplt_epi32_mask (e, a, b);
  f[25] = _mm256_mask_cmplt_epi64_mask (e, a, b);
  f[26] = _mm256_mask_cmplt_epu32_mask (e, a, b);
  f[27] = _mm256_mask_cmplt_epu64_mask (e, a, b);
  f[28] = _mm256_mask_cmpneq_epi32_mask (e, a, b);
  f[29] = _mm256_mask_cmpneq_epi64_mask (e, a, b);
  f[30] = _mm256_mask_cmpneq_epu32_mask (e, a, b);
  f[31] = _mm256_mask_cmpneq_epu64_mask (e, a, b);
  f[32] = _mm_cmpge_epi32_mask (c, d);
  f[33] = _mm_cmpge_epi64_mask (c, d);
  f[34] = _mm_cmpge_epu32_mask (c, d);
  f[35] = _mm_cmpge_epu64_mask (c, d);
  f[36] = _mm_cmple_epi32_mask (c, d);
  f[37] = _mm_cmple_epi64_mask (c, d);
  f[38] = _mm_cmple_epu32_mask (c, d);
  f[39] = _mm_cmple_epu64_mask (c, d);
  f[40] = _mm_cmplt_epi32_mask (c, d);
  f[41] = _mm_cmplt_epi64_mask (c, d);
  f[42] = _mm_cmplt_epu32_mask (c, d);
  f[43] = _mm_cmplt_epu64_mask (c, d);
  f[44] = _mm_cmpneq_epi32_mask (c, d);
  f[45] = _mm_cmpneq_epi64_mask (c, d);
  f[46] = _mm_cmpneq_epu32_mask (c, d);
  f[47] = _mm_cmpneq_epu64_mask (c, d);
  f[48] = _mm_mask_cmpge_epi32_mask (e, c, d);
  f[49] = _mm_mask_cmpge_epi64_mask (e, c, d);
  f[50] = _mm_mask_cmpge_epu32_mask (e, c, d);
  f[51] = _mm_mask_cmpge_epu64_mask (e, c, d);
  f[52] = _mm_mask_cmple_epi32_mask (e, c, d);
  f[53] = _mm_mask_cmple_epi64_mask (e, c, d);
  f[54] = _mm_mask_cmple_epu32_mask (e, c, d);
  f[55] = _mm_mask_cmple_epu64_mask (e, c, d);
  f[56] = _mm_mask_cmplt_epi32_mask (e, c, d);
  f[57] = _mm_mask_cmplt_epi64_mask (e, c, d);
  f[58] = _mm_mask_cmplt_epu32_mask (e, c, d);
  f[59] = _mm_mask_cmplt_epu64_mask (e, c, d);
  f[60] = _mm_mask_cmpneq_epi32_mask (e, c, d);
  f[61] = _mm_mask_cmpneq_epi64_mask (e, c, d);
  f[62] = _mm_mask_cmpneq_epu32_mask (e, c, d);
  f[63] = _mm_mask_cmpneq_epu64_mask (e, c, d);
}
