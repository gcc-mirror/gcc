/* { dg-do compile } */
/* { dg-options "-O0 -Werror-implicit-function-declaration -march=k8 -msse4a -m3dnow -mavx -mavx2 -mfma4 -mxop -maes -mpclmul -mpopcnt -mabm -mlzcnt -mbmi -mbmi2 -mtbm -mlwp -mfsgsbase -mrdrnd -mf16c -mfma -mrtm -mrdseed -mprfchw -madx -mfxsr -mxsaveopt -mavx512f -mavx512er -mavx512cd -mavx512pf -msha -mprefetchwt1 -mxsavec -mxsaves -mclflushopt -mavx512dq -mavx512bw -mavx512vl -mavx512ifma -mavx512vbmi -mavx5124fmaps -mavx5124vnniw -mavx512vpopcntdq -mclwb -mmwaitx -mclzero -mpku -msgx -mrdpid" } */
/* { dg-add-options bind_pic_locally } */

#include <mm_malloc.h>

/* Test that the intrinsics compile without optimization.  All of them are
   defined as inline functions in {,x,e,p,t,s,w,a,b,i}mmintrin.h, mm3dnow.h,
   fma4intrin.h, xopintrin.h, abmintrin.h, bmiintrin.h, tbmintrin.h, 
   lwpintrin.h, fmaintrin.h and mm_malloc.h that reference the proper 
   builtin functions.

   Defining away "extern" and "__inline" results in all of them being compiled
   as proper functions.  */

#define extern
#define __inline

#include <x86intrin.h>

#define _CONCAT(x,y) x ## y

#define test_0(func, type, imm)						\
  type _CONCAT(_,func) (int const I)					\
  { return func (imm); }

#define test_1(func, type, op1_type, imm)				\
  type _CONCAT(_,func) (op1_type A, int const I)			\
  { return func (A, imm); }

#define test_1x(func, type, op1_type, imm1, imm2)			\
  type _CONCAT(_,func) (op1_type A, int const I, int const L)		\
  { return func (A, imm1, imm2); }

#define test_1y(func, type, op1_type, imm1, imm2, imm3)			\
  type _CONCAT(_,func) (op1_type A, int const I, int const L, int const R)\
  { return func (A, imm1, imm2, imm3); }

#define test_2(func, type, op1_type, op2_type, imm)			\
  type _CONCAT(_,func) (op1_type A, op2_type B, int const I)		\
  { return func (A, B, imm); }

#define test_2x(func, type, op1_type, op2_type, imm1, imm2)		\
  type _CONCAT(_,func) (op1_type A, op2_type B, int const I, int const L) \
  { return func (A, B, imm1, imm2); }

#define test_2y(func, type, op1_type, op2_type, imm1, imm2, imm3)	 \
  type _CONCAT(_,func) (op1_type A, op2_type B, int const I, int const L,\
			int const R)					 \
  { return func (A, B, imm1, imm2, imm3); }

#define test_2vx(func, op1_type, op2_type, imm1, imm2)     \
  void _CONCAT(_,func) (op1_type A, op2_type B, int const I, int const L) \
  { func (A, B, imm1, imm2); }

#define test_3(func, type, op1_type, op2_type, op3_type, imm)		\
  type _CONCAT(_,func) (op1_type A, op2_type B,				\
			op3_type C, int const I)			\
  { return func (A, B, C, imm); }

#define test_3x(func, type, op1_type, op2_type, op3_type, imm1, imm2)		\
  type _CONCAT(_,func) (op1_type A, op2_type B,				\
			op3_type C, int const I, int const L)			\
  { return func (A, B, C, imm1, imm2); }

#define test_3y(func, type, op1_type, op2_type, op3_type, imm1, imm2, imm3)	\
  type _CONCAT(_,func) (op1_type A, op2_type B,				\
			op3_type C, int const I, int const L, int const R)	\
  { return func (A, B, C, imm1, imm2, imm3); }

#define test_3v(func, op1_type, op2_type, op3_type, imm)		\
  int _CONCAT(_,func) (op1_type A, op2_type B,				\
		       op3_type C, int const I)				\
  { func (A, B, C, imm); }

#define test_3vx(func, op1_type, op2_type, op3_type, imm1, imm2)   \
  void _CONCAT(_,func) (op1_type A, op2_type B,			   \
		       op3_type C, int const I, int const L)       \
  { func (A, B, C, imm1, imm2); }

#define test_4(func, type, op1_type, op2_type, op3_type, op4_type, imm)	\
  type _CONCAT(_,func) (op1_type A, op2_type B,				\
			op3_type C, op4_type D, int const I)		\
  { return func (A, B, C, D, imm); }

#define test_4x(func, type, op1_type, op2_type, op3_type, op4_type, imm1, imm2)	\
  type _CONCAT(_,func) (op1_type A, op2_type B,				\
			op3_type C, op4_type D, int const I, int const L)		\
  { return func (A, B, C, D, imm1, imm2); }

#define test_4y(func, type, op1_type, op2_type, op3_type, op4_type, imm1, imm2, imm3)	\
  type _CONCAT(_,func) (op1_type A, op2_type B,	op3_type C,		\
			op4_type D, int const I, int const L, int const R)		\
  { return func (A, B, C, D, imm1, imm2, imm3); }

#define test_4v(func, op1_type, op2_type, op3_type, op4_type, imm)	\
  int _CONCAT(_,func) (op1_type A, op2_type B,				\
		       op3_type C, op4_type D, int const I)		\
  { func (A, B, C, D, imm); }


/* Following intrinsics require immediate arguments.  They
   are defined as macros for non-optimized compilations. */

/* ammintrin.h */
test_1x (_mm_extracti_si64, __m128i, __m128i, 1, 1)
test_2x (_mm_inserti_si64, __m128i, __m128i, __m128i, 1, 1)

/* immintrin.h */
test_2 (_mm256_blend_pd, __m256d, __m256d, __m256d, 1)
test_2 (_mm256_blend_ps, __m256, __m256, __m256, 1)
test_2 (_mm256_dp_ps, __m256, __m256, __m256, 1)
test_2 (_mm256_shuffle_pd, __m256d, __m256d, __m256d, 1)
test_2 (_mm256_shuffle_ps, __m256, __m256, __m256, 1)
test_2 (_mm_cmp_sd, __m128d, __m128d, __m128d, 1)
test_2 (_mm_cmp_ss, __m128, __m128, __m128, 1)
test_2 (_mm_cmp_pd, __m128d, __m128d, __m128d, 1)
test_2 (_mm_cmp_ps, __m128, __m128, __m128, 1)
test_2 (_mm256_cmp_pd, __m256d, __m256d, __m256d, 1)
test_2 (_mm256_cmp_ps, __m256, __m256, __m256, 1)
test_1 (_mm256_extractf128_pd, __m128d, __m256d, 1)
test_1 (_mm256_extractf128_ps, __m128, __m256, 1)
test_1 (_mm256_extractf128_si256, __m128i, __m256i, 1)
test_1 (_mm256_extract_epi8, int, __m256i, 20)
test_1 (_mm256_extract_epi16, int, __m256i, 13)
test_1 (_mm256_extract_epi32, int, __m256i, 6)
#ifdef __x86_64__
test_1 (_mm256_extract_epi64, long long, __m256i, 2)
#endif
test_1 (_mm_permute_pd, __m128d, __m128d, 1)
test_1 (_mm256_permute_pd, __m256d, __m256d, 1)
test_1 (_mm_permute_ps, __m128, __m128, 1)
test_1 (_mm256_permute_ps, __m256, __m256, 1)
test_2 (_mm256_permute2f128_pd, __m256d, __m256d, __m256d, 1)
test_2 (_mm256_permute2f128_ps, __m256, __m256, __m256, 1)
test_2 (_mm256_permute2f128_si256, __m256i, __m256i, __m256i, 1)
test_2 (_mm256_insertf128_pd, __m256d, __m256d, __m128d, 1)
test_2 (_mm256_insertf128_ps, __m256, __m256, __m128, 1)
test_2 (_mm256_insertf128_si256, __m256i, __m256i, __m128i, 1)
test_2 (_mm256_insert_epi8, __m256i, __m256i, int, 30)
test_2 (_mm256_insert_epi16, __m256i, __m256i, int, 7)
test_2 (_mm256_insert_epi32, __m256i, __m256i, int, 3)
#ifdef __x86_64__
test_2 (_mm256_insert_epi64, __m256i, __m256i, long long, 1)
#endif
test_1 (_mm256_round_pd, __m256d, __m256d, 9)
test_1 (_mm256_round_ps, __m256, __m256, 9)
test_1 (_cvtss_sh, unsigned short, float, 1)
test_1 (_mm_cvtps_ph, __m128i, __m128, 1)
test_1 (_mm256_cvtps_ph, __m128i, __m256, 1)
test_0 (_xabort, void, 1)
test_1 (_mm512_cvt_roundepi32_ps, __m512, __m512i, 9)
test_1 (_mm512_cvt_roundepu32_ps, __m512, __m512i, 9)
test_1 (_mm512_cvt_roundpd_epi32, __m256i, __m512d, 9)
test_1 (_mm512_cvt_roundpd_epu32, __m256i, __m512d, 9)
test_1 (_mm512_cvt_roundpd_ps, __m256, __m512d, 9)
test_1 (_mm512_cvt_roundph_ps, __m512, __m256i, 8)
test_1 (_mm512_cvt_roundps_epi32, __m512i, __m512, 9)
test_1 (_mm512_cvt_roundps_epu32, __m512i, __m512, 9)
test_1 (_mm512_cvt_roundps_pd, __m512d, __m256, 8)
test_1 (_mm512_cvtps_ph, __m256i, __m512, 1)
test_1 (_mm512_cvtt_roundpd_epi32, __m256i, __m512d, 8)
test_1 (_mm512_cvtt_roundpd_epu32, __m256i, __m512d, 8)
test_1 (_mm512_cvtt_roundps_epi32, __m512i, __m512, 8)
test_1 (_mm512_cvtt_roundps_epu32, __m512i, __m512, 8)
test_1 (_mm512_extractf32x4_ps, __m128, __m512, 1)
test_1 (_mm512_extractf64x4_pd, __m256d, __m512d, 1)
test_1 (_mm512_extracti32x4_epi32, __m128i, __m512i, 1)
test_1 (_mm512_extracti64x4_epi64, __m256i, __m512i, 1)
test_1 (_mm512_getexp_round_pd, __m512d, __m512d, 8)
test_1 (_mm512_getexp_round_ps, __m512, __m512, 8)
test_1y (_mm512_getmant_round_pd, __m512d, __m512d, 1, 1, 8)
test_1y (_mm512_getmant_round_ps, __m512, __m512, 1, 1, 8)
test_1 (_mm512_permute_pd, __m512d, __m512d, 1)
test_1 (_mm512_permute_ps, __m512, __m512, 1)
test_1 (_mm512_permutex_epi64, __m512i, __m512i, 1)
test_1 (_mm512_permutex_pd, __m512d, __m512d, 1)
test_1 (_mm512_rol_epi32, __m512i, __m512i, 1)
test_1 (_mm512_rol_epi64, __m512i, __m512i, 1)
test_1 (_mm512_ror_epi32, __m512i, __m512i, 1)
test_1 (_mm512_ror_epi64, __m512i, __m512i, 1)
test_1 (_mm512_shuffle_epi32, __m512i, __m512i, 1)
test_1 (_mm512_slli_epi32, __m512i, __m512i, 1)
test_1 (_mm512_slli_epi64, __m512i, __m512i, 1)
test_1 (_mm512_sqrt_round_pd, __m512d, __m512d, 9)
test_1 (_mm512_sqrt_round_ps, __m512, __m512, 9)
test_1 (_mm512_srai_epi32, __m512i, __m512i, 1)
test_1 (_mm512_srai_epi64, __m512i, __m512i, 1)
test_1 (_mm512_srli_epi32, __m512i, __m512i, 1)
test_1 (_mm512_srli_epi64, __m512i, __m512i, 1)
test_1 (_mm_cvt_roundsd_i32, int, __m128d, 9)
test_1 (_mm_cvt_roundsd_u32, unsigned, __m128d, 9)
test_1 (_mm_cvt_roundss_i32, int, __m128, 9)
test_1 (_mm_cvt_roundss_u32, unsigned, __m128, 9)
test_1 (_mm_cvtt_roundsd_i32, int, __m128d, 8)
test_1 (_mm_cvtt_roundsd_u32, unsigned, __m128d, 8)
test_1 (_mm_cvtt_roundss_i32, int, __m128, 8)
test_1 (_mm_cvtt_roundss_u32, unsigned, __m128, 8)
test_1x (_mm512_getmant_pd, __m512d, __m512d, 1, 1)
test_1x (_mm512_getmant_ps, __m512, __m512, 1, 1)
test_1x (_mm512_roundscale_round_pd, __m512d, __m512d, 1, 8)
test_1x (_mm512_roundscale_round_ps, __m512, __m512, 1, 8)
test_1x (_mm_cvt_roundi32_ss, __m128, __m128, 1, 9)
test_2 (_mm512_add_round_pd, __m512d, __m512d, __m512d, 9)
test_2 (_mm512_add_round_ps, __m512, __m512, __m512, 9)
test_2 (_mm512_alignr_epi32, __m512i, __m512i, __m512i, 1)
test_2 (_mm512_alignr_epi64, __m512i, __m512i, __m512i, 1)
test_2 (_mm512_cmp_epi32_mask, __mmask16, __m512i, __m512i, 1)
test_2 (_mm512_cmp_epi64_mask, __mmask8, __m512i, __m512i, 1)
test_2 (_mm512_cmp_epu32_mask, __mmask16, __m512i, __m512i, 1)
test_2 (_mm512_cmp_epu64_mask, __mmask8, __m512i, __m512i, 1)
test_2 (_mm512_cmp_pd_mask, __mmask8, __m512d, __m512d, 1)
test_2 (_mm512_cmp_ps_mask, __mmask16, __m512, __m512, 1)
test_2 (_mm512_div_round_pd, __m512d, __m512d, __m512d, 9)
test_2 (_mm512_div_round_ps, __m512, __m512, __m512, 9)
test_2 (_mm512_i32gather_epi32, __m512i, __m512i, void const *, 1)
test_2 (_mm512_i32gather_epi64, __m512i, __m256i, void const *, 1)
test_2 (_mm512_i32gather_pd, __m512d, __m256i, void const *, 1)
test_2 (_mm512_i32gather_ps, __m512, __m512i, void const *, 1)
test_2 (_mm512_i64gather_epi32, __m256i, __m512i, void const *, 1)
test_2 (_mm512_i64gather_epi64, __m512i, __m512i, void const *, 1)
test_2 (_mm512_i64gather_pd, __m512d, __m512i, void const *, 1)
test_2 (_mm512_i64gather_ps, __m256, __m512i, void const *, 1)
test_2 (_mm512_insertf32x4, __m512, __m512, __m128, 1)
test_2 (_mm512_insertf64x4, __m512d, __m512d, __m256d, 1)
test_2 (_mm512_inserti32x4, __m512i, __m512i, __m128i, 1)
test_2 (_mm512_inserti64x4, __m512i, __m512i, __m256i, 1)
test_2 (_mm512_maskz_cvt_roundepi32_ps, __m512, __mmask16, __m512i, 9)
test_2 (_mm512_maskz_cvt_roundepu32_ps, __m512, __mmask16, __m512i, 9)
test_2 (_mm512_maskz_cvt_roundpd_epi32, __m256i, __mmask8, __m512d, 9)
test_2 (_mm512_maskz_cvt_roundpd_epu32, __m256i, __mmask8, __m512d, 9)
test_2 (_mm512_maskz_cvt_roundpd_ps, __m256, __mmask8, __m512d, 9)
test_2 (_mm512_maskz_cvt_roundph_ps, __m512, __mmask16, __m256i, 8)
test_2 (_mm512_maskz_cvt_roundps_epi32, __m512i, __mmask16, __m512, 9)
test_2 (_mm512_maskz_cvt_roundps_epu32, __m512i, __mmask16, __m512, 9)
test_2 (_mm512_maskz_cvt_roundps_pd, __m512d, __mmask8, __m256, 8)
test_2 (_mm512_maskz_cvtps_ph, __m256i, __mmask16, __m512, 1)
test_2 (_mm512_maskz_cvtt_roundpd_epi32, __m256i, __mmask8, __m512d, 8)
test_2 (_mm512_maskz_cvtt_roundpd_epu32, __m256i, __mmask8, __m512d, 8)
test_2 (_mm512_maskz_cvtt_roundps_epi32, __m512i, __mmask16, __m512, 8)
test_2 (_mm512_maskz_cvtt_roundps_epu32, __m512i, __mmask16, __m512, 8)
test_2 (_mm512_maskz_extractf32x4_ps, __m128, __mmask8, __m512, 1)
test_2 (_mm512_maskz_extractf64x4_pd, __m256d, __mmask8, __m512d, 1)
test_2 (_mm512_maskz_extracti32x4_epi32, __m128i, __mmask8, __m512i, 1)
test_2 (_mm512_maskz_extracti64x4_epi64, __m256i, __mmask8, __m512i, 1)
test_2 (_mm512_maskz_getexp_round_pd, __m512d, __mmask8, __m512d, 8)
test_2 (_mm512_maskz_getexp_round_ps, __m512, __mmask16, __m512, 8)
test_2y (_mm512_maskz_getmant_round_pd, __m512d, __mmask8, __m512d, 1, 1, 8)
test_2y (_mm512_maskz_getmant_round_ps, __m512, __mmask16, __m512, 1, 1, 8)
test_2 (_mm512_maskz_permute_pd, __m512d, __mmask8, __m512d, 1)
test_2 (_mm512_maskz_permute_ps, __m512, __mmask16, __m512, 1)
test_2 (_mm512_maskz_permutex_epi64, __m512i, __mmask8, __m512i, 1)
test_2 (_mm512_maskz_permutex_pd, __m512d, __mmask8, __m512d, 1)
test_2 (_mm512_maskz_rol_epi32, __m512i, __mmask16, __m512i, 1)
test_2 (_mm512_maskz_rol_epi64, __m512i, __mmask8, __m512i, 1)
test_2 (_mm512_maskz_ror_epi32, __m512i, __mmask16, __m512i, 1)
test_2 (_mm512_maskz_ror_epi64, __m512i, __mmask8, __m512i, 1)
test_2 (_mm512_maskz_shuffle_epi32, __m512i, __mmask16, __m512i, 1)
test_2 (_mm512_maskz_slli_epi32, __m512i, __mmask16, __m512i, 1)
test_2 (_mm512_maskz_slli_epi64, __m512i, __mmask8, __m512i, 1)
test_2 (_mm512_maskz_sqrt_round_pd, __m512d, __mmask8, __m512d, 9)
test_2 (_mm512_maskz_sqrt_round_ps, __m512, __mmask16, __m512, 9)
test_2 (_mm512_maskz_srai_epi32, __m512i, __mmask16, __m512i, 1)
test_2 (_mm512_maskz_srai_epi64, __m512i, __mmask8, __m512i, 1)
test_2 (_mm512_maskz_srli_epi32, __m512i, __mmask16, __m512i, 1)
test_2 (_mm512_maskz_srli_epi64, __m512i, __mmask8, __m512i, 1)
test_2 (_mm512_max_round_pd, __m512d, __m512d, __m512d, 8)
test_2 (_mm512_max_round_ps, __m512, __m512, __m512, 8)
test_2 (_mm512_min_round_pd, __m512d, __m512d, __m512d, 8)
test_2 (_mm512_min_round_ps, __m512, __m512, __m512, 8)
test_2 (_mm512_mul_round_pd, __m512d, __m512d, __m512d, 9)
test_2 (_mm512_mul_round_ps, __m512, __m512, __m512, 9)
test_2 (_mm512_scalef_round_pd, __m512d, __m512d, __m512d, 9)
test_2 (_mm512_scalef_round_ps, __m512, __m512, __m512, 9)
test_2 (_mm512_shuffle_f32x4, __m512, __m512, __m512, 1)
test_2 (_mm512_shuffle_f64x2, __m512d, __m512d, __m512d, 1)
test_2 (_mm512_shuffle_i32x4, __m512i, __m512i, __m512i, 1)
test_2 (_mm512_shuffle_i64x2, __m512i, __m512i, __m512i, 1)
test_2 (_mm512_shuffle_pd, __m512d, __m512d, __m512d, 1)
test_2 (_mm512_shuffle_ps, __m512, __m512, __m512, 1)
test_2 (_mm512_sub_round_pd, __m512d, __m512d, __m512d, 9)
test_2 (_mm512_sub_round_ps, __m512, __m512, __m512, 9)
test_2 (_mm_add_round_sd, __m128d, __m128d, __m128d, 9)
test_2 (_mm_add_round_ss, __m128, __m128, __m128, 9)
test_2 (_mm_cmp_sd_mask, __mmask8, __m128d, __m128d, 1)
test_2 (_mm_cmp_ss_mask, __mmask8, __m128, __m128, 1)
#ifdef __x86_64__
test_2 (_mm_cvt_roundi64_sd, __m128d, __m128d, long long, 9)
test_2 (_mm_cvt_roundi64_ss, __m128, __m128, long long, 9)
#endif
test_2 (_mm_cvt_roundsd_ss, __m128, __m128, __m128d, 9)
test_2 (_mm_cvt_roundss_sd, __m128d, __m128d, __m128, 8)
test_2 (_mm_cvt_roundu32_ss, __m128, __m128, unsigned, 9)
#ifdef __x86_64__
test_2 (_mm_cvt_roundu64_sd, __m128d, __m128d, unsigned long long, 9)
test_2 (_mm_cvt_roundu64_ss, __m128, __m128, unsigned long long, 9)
#endif
test_2 (_mm_div_round_sd, __m128d, __m128d, __m128d, 9)
test_2 (_mm_div_round_ss, __m128, __m128, __m128, 9)
test_2 (_mm_getexp_round_sd, __m128d, __m128d, __m128d, 8)
test_2 (_mm_getexp_round_ss, __m128, __m128, __m128, 8)
test_2y (_mm_getmant_round_sd, __m128d, __m128d, __m128d, 1, 1, 8)
test_2y (_mm_getmant_round_ss, __m128, __m128, __m128, 1, 1, 8)
test_2 (_mm_mul_round_sd, __m128d, __m128d, __m128d, 9)
test_2 (_mm_mul_round_ss, __m128, __m128, __m128, 9)
test_2 (_mm_scalef_round_sd, __m128d, __m128d, __m128d, 9)
test_2 (_mm_scalef_round_ss, __m128, __m128, __m128, 9)
test_2 (_mm_sqrt_round_sd, __m128d, __m128d, __m128d, 9)
test_2 (_mm_sqrt_round_ss, __m128, __m128, __m128, 9)
test_2 (_mm_sub_round_sd, __m128d, __m128d, __m128d, 9)
test_2 (_mm_sub_round_ss, __m128, __m128, __m128, 9)
test_2x (_mm512_cmp_round_pd_mask, __mmask8, __m512d, __m512d, 1, 8)
test_2x (_mm512_cmp_round_ps_mask, __mmask16, __m512, __m512, 1, 8)
test_2x (_mm512_maskz_roundscale_round_pd, __m512d, __mmask8, __m512d, 1, 8)
test_2x (_mm512_maskz_roundscale_round_ps, __m512, __mmask16, __m512, 1, 8)
test_2x (_mm_cmp_round_sd_mask, __mmask8, __m128d, __m128d, 1, 8)
test_2x (_mm_cmp_round_ss_mask, __mmask8, __m128, __m128, 1, 8)
test_2x (_mm_comi_round_sd, int, __m128d, __m128d, 1, 8)
test_2x (_mm_comi_round_ss, int, __m128, __m128, 1, 8)
test_2x (_mm_roundscale_round_sd, __m128d, __m128d, __m128d, 1, 8)
test_2x (_mm_roundscale_round_ss, __m128, __m128, __m128, 1, 8)
test_3 (_mm512_fmadd_round_pd, __m512d, __m512d, __m512d, __m512d, 9)
test_3 (_mm512_fmadd_round_ps, __m512, __m512, __m512, __m512, 9)
test_3 (_mm512_fmaddsub_round_pd, __m512d, __m512d, __m512d, __m512d, 9)
test_3 (_mm512_fmaddsub_round_ps, __m512, __m512, __m512, __m512, 9)
test_3 (_mm512_fmsub_round_pd, __m512d, __m512d, __m512d, __m512d, 9)
test_3 (_mm512_fmsub_round_ps, __m512, __m512, __m512, __m512, 9)
test_3 (_mm512_fmsubadd_round_pd, __m512d, __m512d, __m512d, __m512d, 9)
test_3 (_mm512_fmsubadd_round_ps, __m512, __m512, __m512, __m512, 9)
test_3 (_mm512_fnmadd_round_pd, __m512d, __m512d, __m512d, __m512d, 9)
test_3 (_mm512_fnmadd_round_ps, __m512, __m512, __m512, __m512, 9)
test_3 (_mm512_fnmsub_round_pd, __m512d, __m512d, __m512d, __m512d, 9)
test_3 (_mm512_fnmsub_round_ps, __m512, __m512, __m512, __m512, 9)
test_3 (_mm512_mask_cmp_epi32_mask, __mmask16, __mmask16, __m512i, __m512i, 1)
test_3 (_mm512_mask_cmp_epi64_mask, __mmask8, __mmask8, __m512i, __m512i, 1)
test_3 (_mm512_mask_cmp_epu32_mask, __mmask16, __mmask16, __m512i, __m512i, 1)
test_3 (_mm512_mask_cmp_epu64_mask, __mmask8, __mmask8, __m512i, __m512i, 1)
test_3 (_mm512_mask_cmp_pd_mask, __mmask8, __mmask8, __m512d, __m512d, 1)
test_3 (_mm512_mask_cmp_ps_mask, __mmask16, __mmask16, __m512, __m512, 1)
test_3 (_mm512_mask_cvt_roundepi32_ps, __m512, __m512, __mmask16, __m512i, 9)
test_3 (_mm512_mask_cvt_roundepu32_ps, __m512, __m512, __mmask16, __m512i, 9)
test_3 (_mm512_mask_cvt_roundpd_epi32, __m256i, __m256i, __mmask8, __m512d, 9)
test_3 (_mm512_mask_cvt_roundpd_epu32, __m256i, __m256i, __mmask8, __m512d, 9)
test_3 (_mm512_mask_cvt_roundpd_ps, __m256, __m256, __mmask8, __m512d, 9)
test_3 (_mm512_mask_cvt_roundph_ps, __m512, __m512, __mmask16, __m256i, 8)
test_3 (_mm512_mask_cvt_roundps_epi32, __m512i, __m512i, __mmask16, __m512, 9)
test_3 (_mm512_mask_cvt_roundps_epu32, __m512i, __m512i, __mmask16, __m512, 9)
test_3 (_mm512_mask_cvt_roundps_pd, __m512d, __m512d, __mmask8, __m256, 8)
test_3 (_mm512_mask_cvtps_ph, __m256i, __m256i, __mmask16, __m512, 1)
test_3 (_mm512_mask_cvtt_roundpd_epi32, __m256i, __m256i, __mmask8, __m512d, 8)
test_3 (_mm512_mask_cvtt_roundpd_epu32, __m256i, __m256i, __mmask8, __m512d, 8)
test_3 (_mm512_mask_cvtt_roundps_epi32, __m512i, __m512i, __mmask16, __m512, 8)
test_3 (_mm512_mask_cvtt_roundps_epu32, __m512i, __m512i, __mmask16, __m512, 8)
test_3 (_mm512_mask_extractf32x4_ps, __m128, __m128, __mmask8, __m512, 1)
test_3 (_mm512_mask_extractf64x4_pd, __m256d, __m256d, __mmask8, __m512d, 1)
test_3 (_mm512_mask_extracti32x4_epi32, __m128i, __m128i, __mmask8, __m512i, 1)
test_3 (_mm512_mask_extracti64x4_epi64, __m256i, __m256i, __mmask8, __m512i, 1)
test_3 (_mm512_mask_getexp_round_pd, __m512d, __m512d, __mmask8, __m512d, 8)
test_3 (_mm512_mask_getexp_round_ps, __m512, __m512, __mmask16, __m512, 8)
test_3y (_mm512_mask_getmant_round_pd, __m512d, __m512d, __mmask8, __m512d, 1, 1, 8)
test_3y (_mm512_mask_getmant_round_ps, __m512, __m512, __mmask16, __m512, 1, 1, 8)
test_3 (_mm512_mask_permute_pd, __m512d, __m512d, __mmask8, __m512d, 1)
test_3 (_mm512_mask_permute_ps, __m512, __m512, __mmask16, __m512, 1)
test_3 (_mm512_mask_permutex_epi64, __m512i, __m512i, __mmask8, __m512i, 1)
test_3 (_mm512_mask_permutex_pd, __m512d, __m512d, __mmask8, __m512d, 1)
test_3 (_mm512_mask_rol_epi32, __m512i, __m512i, __mmask16, __m512i, 1)
test_3 (_mm512_mask_rol_epi64, __m512i, __m512i, __mmask8, __m512i, 1)
test_3 (_mm512_mask_ror_epi32, __m512i, __m512i, __mmask16, __m512i, 1)
test_3 (_mm512_mask_ror_epi64, __m512i, __m512i, __mmask8, __m512i, 1)
test_3 (_mm512_mask_shuffle_epi32, __m512i, __m512i, __mmask16, __m512i, 1)
test_3 (_mm512_mask_slli_epi32, __m512i, __m512i, __mmask16, __m512i, 1)
test_3 (_mm512_mask_slli_epi64, __m512i, __m512i, __mmask8, __m512i, 1)
test_3 (_mm512_mask_sqrt_round_pd, __m512d, __m512d, __mmask8, __m512d, 9)
test_3 (_mm512_mask_sqrt_round_ps, __m512, __m512, __mmask16, __m512, 9)
test_3 (_mm512_mask_srai_epi32, __m512i, __m512i, __mmask16, __m512i, 1)
test_3 (_mm512_mask_srai_epi64, __m512i, __m512i, __mmask8, __m512i, 1)
test_3 (_mm512_mask_srli_epi32, __m512i, __m512i, __mmask16, __m512i, 1)
test_3 (_mm512_mask_srli_epi64, __m512i, __m512i, __mmask8, __m512i, 1)
test_3 (_mm512_maskz_add_round_pd, __m512d, __mmask8, __m512d, __m512d, 9)
test_3 (_mm512_maskz_add_round_ps, __m512, __mmask16, __m512, __m512, 9)
test_3 (_mm512_maskz_alignr_epi32, __m512i, __mmask16, __m512i, __m512i, 1)
test_3 (_mm512_maskz_alignr_epi64, __m512i, __mmask8, __m512i, __m512i, 1)
test_3 (_mm512_maskz_div_round_pd, __m512d, __mmask8, __m512d, __m512d, 9)
test_3 (_mm512_maskz_div_round_ps, __m512, __mmask16, __m512, __m512, 9)
test_3 (_mm512_maskz_insertf32x4, __m512, __mmask16, __m512, __m128, 1)
test_3 (_mm512_maskz_insertf64x4, __m512d, __mmask8, __m512d, __m256d, 1)
test_3 (_mm512_maskz_inserti32x4, __m512i, __mmask16, __m512i, __m128i, 1)
test_3 (_mm512_maskz_inserti64x4, __m512i, __mmask8, __m512i, __m256i, 1)
test_3 (_mm512_maskz_max_round_pd, __m512d, __mmask8, __m512d, __m512d, 8)
test_3 (_mm512_maskz_max_round_ps, __m512, __mmask16, __m512, __m512, 8)
test_3 (_mm512_maskz_min_round_pd, __m512d, __mmask8, __m512d, __m512d, 8)
test_3 (_mm512_maskz_min_round_ps, __m512, __mmask16, __m512, __m512, 8)
test_3 (_mm512_maskz_mul_round_pd, __m512d, __mmask8, __m512d, __m512d, 9)
test_3 (_mm512_maskz_mul_round_ps, __m512, __mmask16, __m512, __m512, 9)
test_3 (_mm512_maskz_scalef_round_pd, __m512d, __mmask8, __m512d, __m512d, 9)
test_3 (_mm512_maskz_scalef_round_ps, __m512, __mmask16, __m512, __m512, 9)
test_3 (_mm512_maskz_shuffle_f32x4, __m512, __mmask16, __m512, __m512, 1)
test_3 (_mm512_maskz_shuffle_f64x2, __m512d, __mmask8, __m512d, __m512d, 1)
test_3 (_mm512_maskz_shuffle_i32x4, __m512i, __mmask16, __m512i, __m512i, 1)
test_3 (_mm512_maskz_shuffle_i64x2, __m512i, __mmask8, __m512i, __m512i, 1)
test_3 (_mm512_maskz_shuffle_pd, __m512d, __mmask8, __m512d, __m512d, 1)
test_3 (_mm512_maskz_shuffle_ps, __m512, __mmask16, __m512, __m512, 1)
test_3 (_mm512_maskz_sub_round_pd, __m512d, __mmask8, __m512d, __m512d, 9)
test_3 (_mm512_maskz_sub_round_ps, __m512, __mmask16, __m512, __m512, 9)
test_3 (_mm512_ternarylogic_epi32, __m512i, __m512i, __m512i, __m512i, 1)
test_3 (_mm512_ternarylogic_epi64, __m512i, __m512i, __m512i, __m512i, 1)
test_3 (_mm_fmadd_round_sd, __m128d, __m128d, __m128d, __m128d, 9)
test_3 (_mm_fmadd_round_ss, __m128, __m128, __m128, __m128, 9)
test_3 (_mm_fmsub_round_sd, __m128d, __m128d, __m128d, __m128d, 9)
test_3 (_mm_fmsub_round_ss, __m128, __m128, __m128, __m128, 9)
test_3 (_mm_fnmadd_round_sd, __m128d, __m128d, __m128d, __m128d, 9)
test_3 (_mm_fnmadd_round_ss, __m128, __m128, __m128, __m128, 9)
test_3 (_mm_fnmsub_round_sd, __m128d, __m128d, __m128d, __m128d, 9)
test_3 (_mm_fnmsub_round_ss, __m128, __m128, __m128, __m128, 9)
test_3 (_mm_mask_cmp_sd_mask, __mmask8, __mmask8, __m128d, __m128d, 1)
test_3 (_mm_mask_cmp_ss_mask, __mmask8, __mmask8, __m128, __m128, 1)
test_3v (_mm512_i32scatter_epi32, void *, __m512i, __m512i, 1)
test_3v (_mm512_i32scatter_epi64, void *, __m256i, __m512i, 1)
test_3v (_mm512_i32scatter_pd, void *, __m256i, __m512d, 1)
test_3v (_mm512_i32scatter_ps, void *, __m512i, __m512, 1)
test_3v (_mm512_i64scatter_epi32, void *, __m512i, __m256i, 1)
test_3v (_mm512_i64scatter_epi64, void *, __m512i, __m512i, 1)
test_3v (_mm512_i64scatter_pd, void *, __m512i, __m512d, 1)
test_3v (_mm512_i64scatter_ps, void *, __m512i, __m256, 1)
test_3x (_mm512_mask_roundscale_round_pd, __m512d, __m512d, __mmask8, __m512d, 1, 8)
test_3x (_mm512_mask_roundscale_round_ps, __m512, __m512, __mmask16, __m512, 1, 8)
test_3x (_mm_fixupimm_round_sd, __m128d, __m128d, __m128d, __m128i, 1, 8)
test_3x (_mm_fixupimm_round_ss, __m128, __m128, __m128, __m128i, 1, 8)
test_3x (_mm_mask_cmp_round_sd_mask, __mmask8, __mmask8, __m128d, __m128d, 1, 8)
test_3x (_mm_mask_cmp_round_ss_mask, __mmask8, __mmask8, __m128, __m128, 1, 8)
test_4 (_mm512_mask3_fmadd_round_pd, __m512d, __m512d, __m512d, __m512d, __mmask8, 9)
test_4 (_mm512_mask3_fmadd_round_ps, __m512, __m512, __m512, __m512, __mmask16, 9)
test_4 (_mm512_mask3_fmaddsub_round_pd, __m512d, __m512d, __m512d, __m512d, __mmask8, 9)
test_4 (_mm512_mask3_fmaddsub_round_ps, __m512, __m512, __m512, __m512, __mmask16, 9)
test_4 (_mm512_mask3_fmsub_round_pd, __m512d, __m512d, __m512d, __m512d, __mmask8, 9)
test_4 (_mm512_mask3_fmsub_round_ps, __m512, __m512, __m512, __m512, __mmask16, 9)
test_4 (_mm512_mask3_fmsubadd_round_pd, __m512d, __m512d, __m512d, __m512d, __mmask8, 9)
test_4 (_mm512_mask3_fmsubadd_round_ps, __m512, __m512, __m512, __m512, __mmask16, 9)
test_4 (_mm512_mask3_fnmadd_round_pd, __m512d, __m512d, __m512d, __m512d, __mmask8, 9)
test_4 (_mm512_mask3_fnmadd_round_ps, __m512, __m512, __m512, __m512, __mmask16, 9)
test_4 (_mm512_mask3_fnmsub_round_pd, __m512d, __m512d, __m512d, __m512d, __mmask8, 9)
test_4 (_mm512_mask3_fnmsub_round_ps, __m512, __m512, __m512, __m512, __mmask16, 9)
test_4 (_mm512_mask_add_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_add_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_alignr_epi32, __m512i, __m512i, __mmask16, __m512i, __m512i, 1)
test_4 (_mm512_mask_alignr_epi64, __m512i, __m512i, __mmask8, __m512i, __m512i, 1)
test_4 (_mm512_mask_div_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_div_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_fmadd_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_fmadd_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_fmaddsub_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_fmaddsub_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_fmsub_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_fmsub_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_fmsubadd_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_fmsubadd_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_fnmadd_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_fnmadd_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_fnmsub_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_fnmsub_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_i32gather_epi32, __m512i, __m512i, __mmask16, __m512i, void const *, 1)
test_4 (_mm512_mask_i32gather_epi64, __m512i, __m512i, __mmask8, __m256i, void const *, 1)
test_4 (_mm512_mask_i32gather_pd, __m512d, __m512d, __mmask8, __m256i, void const *, 1)
test_4 (_mm512_mask_i32gather_ps, __m512, __m512, __mmask16, __m512i, void const *, 1)
test_4 (_mm512_mask_i64gather_epi32, __m256i, __m256i, __mmask8, __m512i, void const *, 1)
test_4 (_mm512_mask_i64gather_epi64, __m512i, __m512i, __mmask8, __m512i, void const *, 1)
test_4 (_mm512_mask_i64gather_pd, __m512d, __m512d, __mmask8, __m512i, void const *, 1)
test_4 (_mm512_mask_i64gather_ps, __m256, __m256, __mmask8, __m512i, void const *, 1)
test_4 (_mm512_mask_insertf32x4, __m512, __m512, __mmask16, __m512, __m128, 1)
test_4 (_mm512_mask_insertf64x4, __m512d, __m512d, __mmask8, __m512d, __m256d, 1)
test_4 (_mm512_mask_inserti32x4, __m512i, __m512i, __mmask16, __m512i, __m128i, 1)
test_4 (_mm512_mask_inserti64x4, __m512i, __m512i, __mmask8, __m512i, __m256i, 1)
test_4 (_mm512_mask_max_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 8)
test_4 (_mm512_mask_max_round_ps, __m512, __m512, __mmask16, __m512, __m512, 8)
test_4 (_mm512_mask_min_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 8)
test_4 (_mm512_mask_min_round_ps, __m512, __m512, __mmask16, __m512, __m512, 8)
test_4 (_mm512_mask_mul_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_mul_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_scalef_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_scalef_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_shuffle_f32x4, __m512, __m512, __mmask16, __m512, __m512, 1)
test_4 (_mm512_mask_shuffle_f64x2, __m512d, __m512d, __mmask8, __m512d, __m512d, 1)
test_4 (_mm512_mask_shuffle_i32x4, __m512i, __m512i, __mmask16, __m512i, __m512i, 1)
test_4 (_mm512_mask_shuffle_i64x2, __m512i, __m512i, __mmask8, __m512i, __m512i, 1)
test_4 (_mm512_mask_shuffle_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 1)
test_4 (_mm512_mask_shuffle_ps, __m512, __m512, __mmask16, __m512, __m512, 1)
test_4 (_mm512_mask_sub_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512d, 9)
test_4 (_mm512_mask_sub_round_ps, __m512, __m512, __mmask16, __m512, __m512, 9)
test_4 (_mm512_mask_ternarylogic_epi32, __m512i, __m512i, __mmask16, __m512i, __m512i, 1)
test_4 (_mm512_mask_ternarylogic_epi64, __m512i, __m512i, __mmask8, __m512i, __m512i, 1)
test_4 (_mm512_maskz_fmadd_round_pd, __m512d, __mmask8, __m512d, __m512d, __m512d, 9)
test_4 (_mm512_maskz_fmadd_round_ps, __m512, __mmask16, __m512, __m512, __m512, 9)
test_4 (_mm512_maskz_fmaddsub_round_pd, __m512d, __mmask8, __m512d, __m512d, __m512d, 9)
test_4 (_mm512_maskz_fmaddsub_round_ps, __m512, __mmask16, __m512, __m512, __m512, 9)
test_4 (_mm512_maskz_fmsub_round_pd, __m512d, __mmask8, __m512d, __m512d, __m512d, 9)
test_4 (_mm512_maskz_fmsub_round_ps, __m512, __mmask16, __m512, __m512, __m512, 9)
test_4 (_mm512_maskz_fmsubadd_round_pd, __m512d, __mmask8, __m512d, __m512d, __m512d, 9)
test_4 (_mm512_maskz_fmsubadd_round_ps, __m512, __mmask16, __m512, __m512, __m512, 9)
test_4 (_mm512_maskz_fnmadd_round_pd, __m512d, __mmask8, __m512d, __m512d, __m512d, 9)
test_4 (_mm512_maskz_fnmadd_round_ps, __m512, __mmask16, __m512, __m512, __m512, 9)
test_4 (_mm512_maskz_fnmsub_round_pd, __m512d, __mmask8, __m512d, __m512d, __m512d, 9)
test_4 (_mm512_maskz_fnmsub_round_ps, __m512, __mmask16, __m512, __m512, __m512, 9)
test_4 (_mm512_maskz_ternarylogic_epi32, __m512i, __mmask16, __m512i, __m512i, __m512i, 1)
test_4 (_mm512_maskz_ternarylogic_epi64, __m512i, __mmask8, __m512i, __m512i, __m512i, 1)
test_4v (_mm512_mask_i32scatter_epi32, void *, __mmask16, __m512i, __m512i, 1)
test_4v (_mm512_mask_i32scatter_epi64, void *, __mmask8, __m256i, __m512i, 1)
test_4v (_mm512_mask_i32scatter_pd, void *, __mmask8, __m256i, __m512d, 1)
test_4v (_mm512_mask_i32scatter_ps, void *, __mmask16, __m512i, __m512, 1)
test_4v (_mm512_mask_i64scatter_epi32, void *, __mmask8, __m512i, __m256i, 1)
test_4v (_mm512_mask_i64scatter_epi64, void *, __mmask8, __m512i, __m512i, 1)
test_4v (_mm512_mask_i64scatter_pd, void *, __mmask8, __m512i, __m512d, 1)
test_4v (_mm512_mask_i64scatter_ps, void *, __mmask8, __m512i, __m256, 1)
test_4x (_mm512_mask_fixupimm_round_pd, __m512d, __m512d, __mmask8, __m512d, __m512i, 1, 8)
test_4x (_mm512_mask_fixupimm_round_ps, __m512, __m512, __mmask16, __m512, __m512i, 1, 8)
test_4x (_mm512_maskz_fixupimm_round_pd, __m512d, __mmask8, __m512d, __m512d, __m512i, 1, 8)
test_4x (_mm512_maskz_fixupimm_round_ps, __m512, __mmask16, __m512, __m512, __m512i, 1, 8)
test_4x (_mm_mask_fixupimm_round_sd, __m128d, __m128d, __mmask8, __m128d, __m128i, 1, 8)
test_4x (_mm_mask_fixupimm_round_ss, __m128, __m128, __mmask8, __m128, __m128i, 1, 8)
test_4x (_mm_maskz_fixupimm_round_sd, __m128d, __mmask8, __m128d, __m128d, __m128i, 1, 8)
test_4x (_mm_maskz_fixupimm_round_ss, __m128, __mmask8, __m128, __m128, __m128i, 1, 8)

/* avx512pfintrin.h */
test_2vx (_mm512_prefetch_i32gather_ps, __m512i, void const *, 1, _MM_HINT_T0)
test_2vx (_mm512_prefetch_i32scatter_ps, void const *, __m512i, 1, _MM_HINT_T0)
test_2vx (_mm512_prefetch_i64gather_ps, __m512i, void const *, 1, _MM_HINT_T0)
test_2vx (_mm512_prefetch_i64scatter_ps, void const *, __m512i, 1, _MM_HINT_T0)
test_2vx (_mm512_prefetch_i32gather_pd, __m256i, void const *, 1, _MM_HINT_T0)
test_2vx (_mm512_prefetch_i32scatter_pd, void const *, __m256i, 1, _MM_HINT_T0)
test_2vx (_mm512_prefetch_i64gather_pd, __m512i, void const *, 1, _MM_HINT_T0)
test_2vx (_mm512_prefetch_i64scatter_pd, void const *, __m512i, 1, _MM_HINT_T0)
test_3vx (_mm512_mask_prefetch_i32gather_ps, __m512i, __mmask16, void const *, 1, _MM_HINT_T0)
test_3vx (_mm512_mask_prefetch_i32scatter_ps, void const *, __mmask16, __m512i, 1, _MM_HINT_T0)
test_3vx (_mm512_mask_prefetch_i64gather_ps, __m512i, __mmask8, void const *, 1, _MM_HINT_T0)
test_3vx (_mm512_mask_prefetch_i64scatter_ps, void const *, __mmask8, __m512i, 1, _MM_HINT_T0)
test_3vx (_mm512_mask_prefetch_i32gather_pd, __m256i, __mmask8, void const *, 1, _MM_HINT_T0)
test_3vx (_mm512_mask_prefetch_i32scatter_pd, void const *, __mmask8, __m256i, 1, _MM_HINT_T0)
test_3vx (_mm512_mask_prefetch_i64gather_pd, __m512i, __mmask8, void const *, 1, _MM_HINT_T0)
test_3vx (_mm512_mask_prefetch_i64scatter_pd, void const *, __mmask8, __m512i, 1, _MM_HINT_T0)

/* avx512erintrin.h */
test_1 (_mm512_exp2a23_round_pd, __m512d, __m512d, 8)
test_1 (_mm512_exp2a23_round_ps, __m512, __m512, 8)
test_1 (_mm512_rcp28_round_pd, __m512d, __m512d, 8)
test_1 (_mm512_rcp28_round_ps, __m512, __m512, 8)
test_1 (_mm512_rsqrt28_round_pd, __m512d, __m512d, 8)
test_1 (_mm512_rsqrt28_round_ps, __m512, __m512, 8)
test_2 (_mm512_maskz_exp2a23_round_pd, __m512d, __mmask8, __m512d, 8)
test_2 (_mm512_maskz_exp2a23_round_ps, __m512, __mmask16, __m512, 8)
test_2 (_mm512_maskz_rcp28_round_pd, __m512d, __mmask8, __m512d, 8)
test_2 (_mm512_maskz_rcp28_round_ps, __m512, __mmask16, __m512, 8)
test_2 (_mm512_maskz_rsqrt28_round_pd, __m512d, __mmask8, __m512d, 8)
test_2 (_mm512_maskz_rsqrt28_round_ps, __m512, __mmask16, __m512, 8)
test_3 (_mm512_mask_exp2a23_round_pd, __m512d, __m512d, __mmask8, __m512d, 8)
test_3 (_mm512_mask_exp2a23_round_ps, __m512, __m512, __mmask16, __m512, 8)
test_3 (_mm512_mask_rcp28_round_pd, __m512d, __m512d, __mmask8, __m512d, 8)
test_3 (_mm512_mask_rcp28_round_ps, __m512, __m512, __mmask16, __m512, 8)
test_3 (_mm512_mask_rsqrt28_round_pd, __m512d, __m512d, __mmask8, __m512d, 8)
test_3 (_mm512_mask_rsqrt28_round_ps, __m512, __m512, __mmask16, __m512, 8)

/* shaintrin.h */
test_2 (_mm_sha1rnds4_epu32, __m128i, __m128i, __m128i, 1)

/* wmmintrin.h */
test_1 (_mm_aeskeygenassist_si128, __m128i, __m128i, 1)
test_2 (_mm_clmulepi64_si128, __m128i, __m128i, __m128i, 1)

/* smmintrin.h */
test_1 (_mm_round_pd, __m128d, __m128d, 9)
test_1 (_mm_round_ps, __m128, __m128, 9)
test_2 (_mm_round_sd, __m128d, __m128d, __m128d, 9)
test_2 (_mm_round_ss, __m128, __m128, __m128, 9)

test_2 (_mm_blend_epi16, __m128i, __m128i, __m128i, 1)
test_2 (_mm_blend_ps, __m128, __m128, __m128, 1)
test_2 (_mm_blend_pd, __m128d, __m128d, __m128d, 1)
test_2 (_mm_dp_ps, __m128, __m128, __m128, 1)
test_2 (_mm_dp_pd, __m128d, __m128d, __m128d, 1)
test_2 (_mm_insert_ps, __m128, __m128, __m128, 1)
test_1 (_mm_extract_ps, int, __m128, 1)
test_2 (_mm_insert_epi8, __m128i, __m128i, int, 1)
test_2 (_mm_insert_epi32, __m128i, __m128i, int, 1)
#ifdef __x86_64__
test_2 (_mm_insert_epi64, __m128i, __m128i, long long, 1)
#endif
test_1 (_mm_extract_epi8, int, __m128i, 1)
test_1 (_mm_extract_epi32, int, __m128i, 1)
#ifdef __x86_64__
test_1 (_mm_extract_epi64, long long, __m128i, 1)
#endif
test_2 (_mm_mpsadbw_epu8, __m128i, __m128i, __m128i, 1)
test_2 (_mm_cmpistrm, __m128i, __m128i, __m128i, 1)
test_2 (_mm_cmpistri, int, __m128i, __m128i, 1)
test_4 (_mm_cmpestrm, __m128i, __m128i, int, __m128i, int, 1)
test_4 (_mm_cmpestri, int, __m128i, int, __m128i, int, 1)
test_2 (_mm_cmpistra, int, __m128i, __m128i, 1)
test_2 (_mm_cmpistrc, int, __m128i, __m128i, 1)
test_2 (_mm_cmpistro, int, __m128i, __m128i, 1)
test_2 (_mm_cmpistrs, int, __m128i, __m128i, 1)
test_2 (_mm_cmpistrz, int, __m128i, __m128i, 1)
test_4 (_mm_cmpestra, int, __m128i, int, __m128i, int, 1)
test_4 (_mm_cmpestrc, int, __m128i, int, __m128i, int, 1)
test_4 (_mm_cmpestro, int, __m128i, int, __m128i, int, 1)
test_4 (_mm_cmpestrs, int, __m128i, int, __m128i, int, 1)
test_4 (_mm_cmpestrz, int, __m128i, int, __m128i, int, 1)

/* tmmintrin.h */
test_2 (_mm_alignr_epi8, __m128i, __m128i, __m128i, 1)
test_2 (_mm_alignr_pi8, __m64, __m64, __m64, 1)

/* emmintrin.h */
test_2 (_mm_shuffle_pd, __m128d, __m128d, __m128d, 1)
test_1 (_mm_bsrli_si128, __m128i, __m128i, 1)
test_1 (_mm_bslli_si128, __m128i, __m128i, 1)
test_1 (_mm_srli_si128, __m128i, __m128i, 1)
test_1 (_mm_slli_si128, __m128i, __m128i, 1)
test_1 (_mm_extract_epi16, int, __m128i, 1)
test_2 (_mm_insert_epi16, __m128i, __m128i, int, 1)
test_1 (_mm_shufflehi_epi16, __m128i, __m128i, 1)
test_1 (_mm_shufflelo_epi16, __m128i, __m128i, 1)
test_1 (_mm_shuffle_epi32, __m128i, __m128i, 1)

/* xmmintrin.h */
test_2 (_mm_shuffle_ps, __m128, __m128, __m128, 1)
test_1 (_mm_extract_pi16, int, __m64, 1)
test_1 (_m_pextrw, int, __m64, 1)
test_2 (_mm_insert_pi16, __m64, __m64, int, 1)
test_2 (_m_pinsrw, __m64, __m64, int, 1)
test_1 (_mm_shuffle_pi16, __m64, __m64, 1)
test_1 (_m_pshufw, __m64, __m64, 1)
test_1 (_mm_prefetch, void, void *, _MM_HINT_NTA)

/* xopintrin.h */
test_1 ( _mm_roti_epi8, __m128i, __m128i, 1)
test_1 ( _mm_roti_epi16, __m128i, __m128i, 1)
test_1 ( _mm_roti_epi32, __m128i, __m128i, 1)
test_1 ( _mm_roti_epi64, __m128i, __m128i, 1)
test_3 (_mm_permute2_pd, __m128d, __m128d, __m128d, __m128d, 1)
test_3 (_mm256_permute2_pd, __m256d, __m256d, __m256d, __m256d, 1)
test_3 (_mm_permute2_ps, __m128, __m128, __m128, __m128, 1)
test_3 (_mm256_permute2_ps, __m256, __m256, __m256, __m256, 1)

/* lwpintrin.h */
test_2 ( __lwpval32, void, unsigned int, unsigned int, 1)
test_2 ( __lwpins32, unsigned char, unsigned int, unsigned int, 1)
#ifdef __x86_64__
test_2 ( __lwpval64, void, unsigned long long, unsigned int, 1)
test_2 ( __lwpins64, unsigned char, unsigned long long, unsigned int, 1)
#endif

/* tbmintrin.h */
test_1 ( __bextri_u32, unsigned int, unsigned int, 1)
#ifdef __x86_64__
test_1 ( __bextri_u64, unsigned long long, unsigned long long, 1)
#endif
