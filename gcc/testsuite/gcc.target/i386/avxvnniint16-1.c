/* { dg-do compile } */
/* { dg-options "-mavxvnniint16 -O2" } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */


#include <immintrin.h>

volatile __m256i x,y,z;
volatile __m128i x_,y_,z_;
volatile __mmask8 m;

void extern
avxvnniint16_test (void)
{
  x = _mm256_dpwusd_avx_epi32 (x, y, z);
  x_ = _mm_dpwusd_avx_epi32 (x_, y_, z_);

  x = _mm256_dpwusds_avx_epi32 (x, y, z);
  x_ = _mm_dpwusds_avx_epi32 (x_, y_, z_);

  x = _mm256_dpwsud_avx_epi32 (x, y, z);
  x_ = _mm_dpwsud_avx_epi32 (x_, y_, z_);

  x = _mm256_dpwsuds_avx_epi32 (x, y, z);
  x_ = _mm_dpwsuds_avx_epi32 (x_, y_, z_);

  x = _mm256_dpwuud_avx_epi32 (x, y, z);
  x_ = _mm_dpwuud_avx_epi32 (x_, y_, z_);

  x = _mm256_dpwuuds_avx_epi32 (x, y, z);
  x_ = _mm_dpwuuds_avx_epi32 (x_, y_, z_);

  x = _mm256_dpwusd_epi32 (x, y, z);
  x_ = _mm_dpwusd_epi32 (x_, y_, z_);

  x = _mm256_dpwusds_epi32 (x, y, z);
  x_ = _mm_dpwusds_epi32 (x_, y_, z_);

  x = _mm256_dpwsud_epi32 (x, y, z);
  x_ = _mm_dpwsud_epi32 (x_, y_, z_);

  x = _mm256_dpwsuds_epi32 (x, y, z);
  x_ = _mm_dpwsuds_epi32 (x_, y_, z_);

  x = _mm256_dpwuud_epi32 (x, y, z);
  x_ = _mm_dpwuud_epi32 (x_, y_, z_);

  x = _mm256_dpwuuds_epi32 (x, y, z);
  x_ = _mm_dpwuuds_epi32 (x_, y_, z_);
}
