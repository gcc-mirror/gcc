/* PR target/49411 */
/* { dg-do compile } */
/* { dg-options "-O0 -mf16c -maes -mpclmul" } */

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
  i1 = _mm_cmpistrm (i2, i3, -10);	  /* { dg-error "the third argument must be an 8-bit immediate" } */
  k1 = _mm_cmpistri (i2, i3, -10);	  /* { dg-error "the third argument must be an 8-bit immediate" } */
  k1 = _mm_cmpistra (i2, i3, -10);	  /* { dg-error "the third argument must be an 8-bit immediate" } */
  k1 = _mm_cmpistrc (i2, i3, -10);	  /* { dg-error "the third argument must be an 8-bit immediate" } */
  k1 = _mm_cmpistro (i2, i3, -10);	  /* { dg-error "the third argument must be an 8-bit immediate" } */
  k1 = _mm_cmpistrs (i2, i3, -10);	  /* { dg-error "the third argument must be an 8-bit immediate" } */
  k1 = _mm_cmpistrz (i2, i3, -10);	  /* { dg-error "the third argument must be an 8-bit immediate" } */
  i1 = _mm_cmpestrm (i2, k2, i3, k3, -10);/* { dg-error "the fifth argument must be an 8-bit immediate" } */
  k1 = _mm_cmpestri (i2, k2, i3, k3, -10);/* { dg-error "the fifth argument must be an 8-bit immediate" } */
  k1 = _mm_cmpestra (i2, k2, i3, k3, -10);/* { dg-error "the fifth argument must be an 8-bit immediate" } */
  k1 = _mm_cmpestrc (i2, k2, i3, k3, -10);/* { dg-error "the fifth argument must be an 8-bit immediate" } */
  k1 = _mm_cmpestro (i2, k2, i3, k3, -10);/* { dg-error "the fifth argument must be an 8-bit immediate" } */
  k1 = _mm_cmpestrs (i2, k2, i3, k3, -10);/* { dg-error "the fifth argument must be an 8-bit immediate" } */
  k1 = _mm_cmpestrz (i2, k2, i3, k3, -10);/* { dg-error "the fifth argument must be an 8-bit immediate" } */
  b1 = _mm256_blend_ps (b2, b3, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
  k1 = _cvtss_sh (f1, -10);		  /* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm256_cvtps_ph (b2, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
  b1 = _mm256_dp_ps (b2, b3, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
  e1 = _mm256_permute2f128_pd (e2, e3, -10);/* { dg-error "the last argument must be an 8-bit immediate" } */
  b1 = _mm256_permute2f128_ps (b2, b3, -10);/* { dg-error "the last argument must be an 8-bit immediate" } */
  l1 = _mm256_permute2f128_si256 (l2, l3, -10);/* { dg-error "the last argument must be an 8-bit immediate" } */
  b1 = _mm256_permute_ps (b2, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_aeskeygenassist_si128 (i2, -10);/* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_blend_epi16 (i2, i3, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_clmulepi64_si128 (i2, i3, -10);/* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_cvtps_ph (a1, -10);		  /* { dg-error "the last argument must be an 8-bit immediate" } */
  d1 = _mm_dp_pd (d2, d3, -10);		  /* { dg-error "the last argument must be an 8-bit immediate" } */
  a1 = _mm_dp_ps (a2, a3, -10);		  /* { dg-error "the last argument must be an 8-bit immediate" } */
  a1 = _mm_insert_ps (a2, a3, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_mpsadbw_epu8 (i2, i3, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
  a1 = _mm_permute_ps (a2, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_slli_si128 (i2, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_srli_si128 (i2, -10);	  /* { dg-error "the last argument must be an 8-bit immediate" } */
}

void
test5bit (void)
{
  d1 = _mm_cmp_sd (d2, d3, -7);		  /* { dg-error "the last argument must be a 5-bit immediate" } */
  a1 = _mm_cmp_ss (a2, a3, -7);		  /* { dg-error "the last argument must be a 5-bit immediate" } */
  d1 = _mm_cmp_pd (d2, d3, -7);		  /* { dg-error "the last argument must be a 5-bit immediate" } */
  a1 = _mm_cmp_ps (a2, a3, -7);		  /* { dg-error "the last argument must be a 5-bit immediate" } */
  e1 = _mm256_cmp_pd (e2, e3, -7);	  /* { dg-error "the last argument must be a 5-bit immediate" } */
  b1 = _mm256_cmp_ps (b2, b3, -7);	  /* { dg-error "the last argument must be a 5-bit immediate" } */
}

void
test4bit (void)
{
  d1 = _mm_round_pd (d2, -7);		  /* { dg-error "the last argument must be a 4-bit immediate" } */
  d1 = _mm_round_sd (d2, d3, -7);	  /* { dg-error "the last argument must be a 4-bit immediate" } */
  a1 = _mm_round_ps (a2, -7);		  /* { dg-error "the last argument must be a 4-bit immediate" } */
  a1 = _mm_round_ss (a2, a2, -7);	  /* { dg-error "the last argument must be a 4-bit immediate" } */
  a1 = _mm_blend_ps (a2, a3, -7);	  /* { dg-error "the last argument must be a 4-bit immediate" } */
  e1 = _mm256_blend_pd (e2, e3, -7);	  /* { dg-error "the last argument must be a 4-bit immediate" } */
  e1 = _mm256_round_pd (e2, -7);	  /* { dg-error "the last argument must be a 4-bit immediate" } */
  b1 = _mm256_round_ps (b2, -7);	  /* { dg-error "the last argument must be a 4-bit immediate" } */
}

void
test2bit (void)
{
  d1 = _mm_blend_pd (d2, d3, -1);	  /* { dg-error "the last argument must be a 2-bit immediate" } */
}

void
test1bit (void)
{
  d1 = _mm256_extractf128_pd (e2, -1);	  /* { dg-error "the last argument must be a 1-bit immediate" } */
  a1 = _mm256_extractf128_ps (b2, -1);	  /* { dg-error "the last argument must be a 1-bit immediate" } */
  i1 = _mm256_extractf128_si256 (l2, -1); /* { dg-error "the last argument must be a 1-bit immediate" } */
  e1 = _mm256_insertf128_pd (e2, d1, -1); /* { dg-error "the last argument must be a 1-bit immediate" } */
  b1 = _mm256_insertf128_ps (b2, a1, -1); /* { dg-error "the last argument must be a 1-bit immediate" } */
  l1 = _mm256_insertf128_si256 (l2, i1, -1);/* { dg-error "the last argument must be a 1-bit immediate" } */
}
