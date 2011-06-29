/* PR target/49411 */
/* { dg-do assemble } */
/* { dg-options "-O0 -mf16c -maes -mpclmul" } */
/* { dg-require-effective-target f16c } */
/* { dg-require-effective-target vaes } */
/* { dg-require-effective-target vpclmul } */

#include <x86intrin.h>

__m128i i1, i2, i3, i4;
__m128 a1, a2, a3, a4;
__m128d d1, d2, d3, d4;
__m256i l1, l2, l3, l4;
__m256 b1, b2, b3, b4;
__m256d e1, e2, e3, e4;
__m64 m1, m2, m3, m4;
int k1, k2, k3, k4;
float f1, f2, f3, f4;

void
test8bit (void)
{
  i1 = _mm_cmpistrm (i2, i3, 255);
  k1 = _mm_cmpistri (i2, i3, 255);
  k1 = _mm_cmpistra (i2, i3, 255);
  k1 = _mm_cmpistrc (i2, i3, 255);
  k1 = _mm_cmpistro (i2, i3, 255);
  k1 = _mm_cmpistrs (i2, i3, 255);
  k1 = _mm_cmpistrz (i2, i3, 255);
  i1 = _mm_cmpestrm (i2, k2, i3, k3, 255);
  k1 = _mm_cmpestri (i2, k2, i3, k3, 255);
  k1 = _mm_cmpestra (i2, k2, i3, k3, 255);
  k1 = _mm_cmpestrc (i2, k2, i3, k3, 255);
  k1 = _mm_cmpestro (i2, k2, i3, k3, 255);
  k1 = _mm_cmpestrs (i2, k2, i3, k3, 255);
  k1 = _mm_cmpestrz (i2, k2, i3, k3, 255);
  b1 = _mm256_blend_ps (b2, b3, 255);
  k1 = _cvtss_sh (f1, 255);
  i1 = _mm256_cvtps_ph (b2, 255);
  b1 = _mm256_dp_ps (b2, b3, 255);
  e1 = _mm256_permute2f128_pd (e2, e3, 255);
  b1 = _mm256_permute2f128_ps (b2, b3, 255);
  l1 = _mm256_permute2f128_si256 (l2, l3, 255);
  b1 = _mm256_permute_ps (b2, 255);
  i1 = _mm_aeskeygenassist_si128 (i2, 255);
  i1 = _mm_blend_epi16 (i2, i3, 255);
  i1 = _mm_clmulepi64_si128 (i2, i3, 255);
  i1 = _mm_cvtps_ph (a1, 255);
  d1 = _mm_dp_pd (d2, d3, 255);
  a1 = _mm_dp_ps (a2, a3, 255);
  a1 = _mm_insert_ps (a2, a3, 255);
  i1 = _mm_mpsadbw_epu8 (i2, i3, 255);
  a1 = _mm_permute_ps (a2, 255);
  i1 = _mm_slli_si128 (i2, 255);
  i1 = _mm_srli_si128 (i2, 255);
}

void
test5bit (void)
{
  d1 = _mm_cmp_sd (d2, d3, 31);
  a1 = _mm_cmp_ss (a2, a3, 31);
  d1 = _mm_cmp_pd (d2, d3, 31);
  a1 = _mm_cmp_ps (a2, a3, 31);
  e1 = _mm256_cmp_pd (e2, e3, 31);
  b1 = _mm256_cmp_ps (b2, b3, 31);
}

void
test4bit (void)
{
  d1 = _mm_round_pd (d2, 15);
  d1 = _mm_round_sd (d2, d3, 15);
  a1 = _mm_round_ps (a2, 15);
  a1 = _mm_round_ss (a2, a2, 15);
  a1 = _mm_blend_ps (a2, a3, 15);
  e1 = _mm256_blend_pd (e2, e3, 15);
  e1 = _mm256_round_pd (e2, 15);
  b1 = _mm256_round_ps (b2, 15);
}

void
test2bit (void)
{
  d1 = _mm_blend_pd (d2, d3, 3);
}

void
test1bit (void)
{
  d1 = _mm256_extractf128_pd (e2, 1);
  a1 = _mm256_extractf128_ps (b2, 1);
  i1 = _mm256_extractf128_si256 (l2, 1);
  e1 = _mm256_insertf128_pd (e2, d1, 1);
  b1 = _mm256_insertf128_ps (b2, a1, 1);
  l1 = _mm256_insertf128_si256 (l2, i1, 1);
}
