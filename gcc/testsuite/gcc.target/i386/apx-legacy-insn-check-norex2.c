/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -mapxf -m64 -DDTYPE32" } */

#include <immintrin.h>

typedef unsigned int u32;
typedef unsigned long long u64;

#ifndef DTYPE32
#define DTYPE32
#endif

#ifdef DTYPE32
typedef u32 DTYPE;
#endif

__attribute__((target("xsave,fxsr")))
void legacy_test ()
{
  register DTYPE* val __asm__("r16");
  _xsave64 (val, 1);
  _xrstor64 (val, 1);
  _fxsave64 (val);
  _fxrstor64 (val);
}

/* { dg-final { scan-assembler-not "xsave64\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "xrstor64\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "fxsave64\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "fxrstor64\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */

#ifdef DTYPE
#undef DTYPE
#define DTYPE u64
#endif

typedef union
{
  __m128i xi[8];
  __m128 xf[8];
  __m128d xd[8];
  __m256i yi[4];
  __m256 yf[4];
  __m256d yd[4];
  DTYPE a[16];
} tmp_u;

__attribute__((target("sse4.2,aes")))
void sse_test ()
{
  register tmp_u *tdst __asm__("%r16");
  register tmp_u *src1 __asm__("%r17");
  register tmp_u *src2 __asm__("%r18");

  src1->xi[0] = _mm_minpos_epu16 (src1->xi[1]);
  src1->a[2] = _mm_testc_si128 (src1->xi[3], src2->xi[4]);
  src1->xf[3] = _mm_round_ss (src1->xf[5], src2->xf[6],
			      _MM_FROUND_CUR_DIRECTION);
  src1->xf[4] = _mm_round_ps (src1->xf[7], _MM_FROUND_CUR_DIRECTION);
  src1->xd[0] = _mm_round_sd (src1->xd[2], src2->xd[3],
			      _MM_FROUND_CUR_DIRECTION);
  src1->xd[1] = _mm_round_pd (src1->xd[4], _MM_FROUND_CUR_DIRECTION);

  src1->xi[0] = _mm_hadd_epi16 (tdst->xi[2], src2->xi[3]);
  src1->xi[1] = _mm_hadd_epi32 (tdst->xi[0], src2->xi[1]);
  tdst->xi[2] = _mm_hadds_epi16 (src1->xi[4], src2->xi[5]);
  tdst->xi[3] = _mm_hsub_epi16 (src1->xi[6], src2->xi[7]);
  tdst->xi[4] = _mm_hsub_epi32 (src1->xi[0], src2->xi[1]);
  tdst->xi[5] = _mm_hsubs_epi16 (src1->xi[2], src2->xi[3]);

  src1->xi[6] = _mm_cmpeq_epi64 (tdst->xi[4], src2->xi[5]);
  src1->xi[7] = _mm_cmpgt_epi64 (tdst->xi[6], src2->xi[7]);

  tdst->xf[0] = _mm_dp_ps (src1->xf[0], src2->xf[1], 0xbf);
  tdst->xd[1] = _mm_dp_pd (src1->xd[2], src2->xd[3], 0xae);

  tdst->xi[2] = _mm_mpsadbw_epu8 (src1->xi[4], src2->xi[5], 0xc1);

  tdst->xi[3] = _mm_blend_epi16 (src1->xi[6], src2->xi[7], 0xc);
  tdst->xi[4] = _mm_blendv_epi8 (src1->xi[0], src2->xi[1], tdst->xi[2]);
  tdst->xf[5] = _mm_blend_ps (src1->xf[3], src2->xf[4], 0x4);
  tdst->xf[6] = _mm_blendv_ps (src1->xf[5], src2->xf[6], tdst->xf[7]);
  tdst->xd[7] = _mm_blend_pd (tdst->xd[0], src1->xd[1], 0x1);
  tdst->xd[0] = _mm_blendv_pd (src1->xd[2], src2->xd[3], tdst->xd[4]);

  tdst->xi[1] = _mm_sign_epi8 (src1->xi[5], src2->xi[6]);
  tdst->xi[2] = _mm_sign_epi16 (src1->xi[7], src2->xi[0]);
  tdst->xi[3] = _mm_sign_epi32 (src1->xi[1], src2->xi[2]);

  tdst->a[2] = _mm_cmpestri (src1->xi[3], 16, src2->xi[4], 16, 0x0c);
  tdst->xi[4] = _mm_cmpestrm (src1->xi[3], 16, src2->xi[4], 16, 0x20);
  tdst->a[5] = _mm_cmpistri (src1->xi[5], src2->xi[6], 0x30);
  tdst->xi[6] = _mm_cmpistrm (src1->xi[5], src2->xi[6], 0x40);

  tdst->xi[7] = _mm_aesimc_si128 (src1->xi[7]);
  tdst->xi[0] = _mm_aeskeygenassist_si128 (src1->xi[1], 0x1b);
}

__attribute__((target("avx2,aes")))
void vex_test ()
{

  register tmp_u *tdst __asm__("%r16");
  register tmp_u *src1 __asm__("%r17");
  register tmp_u *src2 __asm__("%r18");
 
  src1->xi[0] = _mm_minpos_epu16 (src1->xi[1]);
  src1->a[2] = _mm256_testc_si256 (src1->yi[2], src2->yi[3]);
  src1->xf[3] = _mm_round_ss (src1->xf[5], src2->xf[6],
			      _MM_FROUND_CUR_DIRECTION);
  src1->yf[4] = _mm256_round_ps (src1->yf[2], _MM_FROUND_CUR_DIRECTION);
  src1->xd[0] = _mm_round_sd (src1->xd[2], src2->xd[3],
			      _MM_FROUND_CUR_DIRECTION);
  src1->yd[1] = _mm256_round_pd (src1->yd[3], _MM_FROUND_CUR_DIRECTION);
 
  src1->yi[1] = _mm256_hadd_epi16 (tdst->yi[2], src2->yi[3]);
  src1->yi[2] = _mm256_hadd_epi32 (tdst->yi[0], src2->yi[1]);
  tdst->yi[3] = _mm256_hadds_epi16 (src1->yi[1], src2->yi[2]);
  tdst->yi[0] = _mm256_hsub_epi16 (src1->yi[3], src2->yi[0]);
  tdst->yi[1] = _mm256_hsub_epi32 (src1->yi[0], src2->yi[1]);
  tdst->yi[2] = _mm256_hsubs_epi16 (src1->yi[2], src2->yi[3]);

  src1->yi[2] = _mm256_cmpeq_epi64 (tdst->yi[1], src2->yi[2]);
  src1->yi[1] = _mm256_cmpgt_epi64 (tdst->yi[3], src2->yi[0]);

  tdst->yf[2] = _mm256_dp_ps (src1->yf[0], src2->yf[1], 0xbf);

  tdst->yi[3] = _mm256_mpsadbw_epu8 (src1->yi[1], src2->yi[1], 0xc1);

  tdst->yi[0] = _mm256_blend_epi16 (src1->yi[1], src2->yi[2], 0xc);
  tdst->yi[1] = _mm256_blendv_epi8 (src1->yi[1], src2->yi[2], tdst->yi[0]);
  tdst->yf[2] = _mm256_blend_ps (src1->yf[0], src2->yf[1], 0x4);
  tdst->yf[3] = _mm256_blendv_ps (src1->yf[2], src2->yf[3], tdst->yf[1]);
  tdst->yd[3] = _mm256_blend_pd (tdst->yd[1], src1->yd[0], 0x1);
  tdst->yd[1] = _mm256_blendv_pd (src1->yd[2], src2->yd[3], tdst->yd[2]);

  tdst->yi[2] = _mm256_sign_epi8 (src1->yi[0], src2->yi[1]);
  tdst->yi[3] = _mm256_sign_epi16 (src1->yi[2], src2->yi[3]);
  tdst->yi[0] = _mm256_sign_epi32 (src1->yi[0], src2->yi[1]);

  tdst->a[2] = _mm_cmpestri (src1->xi[3], 16, src2->xi[4], 16, 0x0c);
  tdst->xi[4] = _mm_cmpestrm (src1->xi[3], 16, src2->xi[4], 16, 0x20);
  tdst->a[5] = _mm_cmpistri (src1->xi[5], src2->xi[6], 0x30);
  tdst->xi[6] = _mm_cmpistrm (src1->xi[5], src2->xi[6], 0x40);

  tdst->xi[7] = _mm_aesimc_si128 (src1->xi[7]);
  tdst->xi[0] = _mm_aeskeygenassist_si128 (src1->xi[1], 0x1b);
}

/* { dg-final { scan-assembler-not "v?pcmpeqq\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?pcmpgtq\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?phaddw\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?phaddd\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?phaddsw\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?phsubw\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?phsubd\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?phsubsw\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?dpps\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?dppd\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?psadbw\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?pblendw\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?pblendvb\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?blendps\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?blendvps\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?blendpd\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?blendvpd\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?psignb\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?psignw\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?psignd\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?phminposuw\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?ptest\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?roundss\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?roundsd\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?roundps\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?roundpd\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?pcmpestri\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?pcmpistri\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?pcmpestrm\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?pcmpistrm\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?aesimc\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "v?aeskeygenassist\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
