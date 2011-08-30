/* Same as sse-14, except converted to use #pragma GCC option.  */
/* { dg-do compile } */
/* { dg-options "-O0 -Werror-implicit-function-declaration -march=k8" } */

#include <mm_malloc.h>

/* Test that the intrinsics compile with optimization.  All of them
   are defined as inline functions in {,x,e,p,t,s,w,a,b,i}mmintrin.h,
   mm3dnow.h, fma4intrin.h, xopintrin.h, abmintrin.h, bmiintrin.h,
   tbmintrin.h, lwpintrin.h, popcntintrin.h, fmaintrin.h and mm_malloc.h 
   that reference the proper builtin functions.

   Defining away "extern" and "__inline" results in all of them being
   compiled as proper functions.  */

#define extern
#define __inline

#define _CONCAT(x,y) x ## y

#define test_1(func, type, op1_type, imm)				\
  type _CONCAT(_,func) (op1_type A, int const I)			\
  { return func (A, imm); }

#define test_1x(func, type, op1_type, imm1, imm2)			\
  type _CONCAT(_,func) (op1_type A, int const I, int const L)		\
  { return func (A, imm1, imm2); }

#define test_2(func, type, op1_type, op2_type, imm)			\
  type _CONCAT(_,func) (op1_type A, op2_type B, int const I)		\
  { return func (A, B, imm); }

#define test_2x(func, type, op1_type, op2_type, imm1, imm2)		\
  type _CONCAT(_,func) (op1_type A, op2_type B, int const I, int const L) \
  { return func (A, B, imm1, imm2); }

#define test_3(func, type, op1_type, op2_type, op3_type, imm)		\
  type _CONCAT(_,func) (op1_type A, op2_type B,				\
			op3_type C, int const I)			\
  { return func (A, B, C, imm); }

#define test_4(func, type, op1_type, op2_type, op3_type, op4_type, imm)	\
  type _CONCAT(_,func) (op1_type A, op2_type B,				\
			op3_type C, op4_type D, int const I)		\
  { return func (A, B, C, D, imm); }


#ifndef DIFFERENT_PRAGMAS
#pragma GCC target ("sse4a,3dnow,avx,avx2,fma4,xop,aes,pclmul,popcnt,abm,lzcnt,bmi,bmi2,tbm,lwp,fsgsbase,rdrnd,f16c")
#endif

/* Following intrinsics require immediate arguments.  They
   are defined as macros for non-optimized compilations. */

/* mmintrin.h (MMX).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("mmx")
#endif
#include <mmintrin.h>

/* mm3dnow.h (3DNOW).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("3dnow")
#endif
#include <mm3dnow.h>

/* xmmintrin.h (SSE).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("sse")
#endif
#include <xmmintrin.h>
test_2 (_mm_shuffle_ps, __m128, __m128, __m128, 1)
test_1 (_mm_extract_pi16, int, __m64, 1)
test_1 (_m_pextrw, int, __m64, 1)
test_2 (_mm_insert_pi16, __m64, __m64, int, 1)
test_2 (_m_pinsrw, __m64, __m64, int, 1)
test_1 (_mm_shuffle_pi16, __m64, __m64, 1)
test_1 (_m_pshufw, __m64, __m64, 1)
test_1 (_mm_prefetch, void, void *, _MM_HINT_NTA)

/* emmintrin.h (SSE2).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("sse2")
#endif
#include <emmintrin.h>
test_2 (_mm_shuffle_pd, __m128d, __m128d, __m128d, 1)
test_1 (_mm_srli_si128, __m128i, __m128i, 1)
test_1 (_mm_slli_si128, __m128i, __m128i, 1)
test_1 (_mm_extract_epi16, int, __m128i, 1)
test_2 (_mm_insert_epi16, __m128i, __m128i, int, 1)
test_1 (_mm_shufflehi_epi16, __m128i, __m128i, 1)
test_1 (_mm_shufflelo_epi16, __m128i, __m128i, 1)
test_1 (_mm_shuffle_epi32, __m128i, __m128i, 1)

/* pmmintrin.h (SSE3).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("sse3")
#endif
#include <pmmintrin.h>

/* tmmintrin.h (SSSE3).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("ssse3")
#endif
#include <tmmintrin.h>
test_2 (_mm_alignr_epi8, __m128i, __m128i, __m128i, 1)
test_2 (_mm_alignr_pi8, __m64, __m64, __m64, 1)

/* ammintrin.h (SSE4A).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("sse4a")
#endif
#include <ammintrin.h>
test_1x (_mm_extracti_si64, __m128i, __m128i, 1, 1)
test_2x (_mm_inserti_si64, __m128i, __m128i, __m128i, 1, 1)

/* Note, nmmintrin.h includes smmintrin.h, and smmintrin.h
   checks for the #ifdef.  So just set the option to SSE4.2.  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("sse4.2")
#endif
#include <nmmintrin.h>
/* smmintrin.h (SSE4.2).  */
test_1 (_mm_round_pd, __m128d, __m128d, 1)
test_1 (_mm_round_ps, __m128, __m128, 1)
test_2 (_mm_round_sd, __m128d, __m128d, __m128d, 1)
test_2 (_mm_round_ss, __m128, __m128, __m128, 1)

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

/* immintrin.h (AVX/AVX2/RDRND/FSGSBASE/F16C) */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("avx,avx2,rdrnd,fsgsbase,f16c")
#endif
#include <immintrin.h>
test_1 (_cvtss_sh, unsigned short, float, 1)
test_1 (_mm_cvtps_ph, __m128i, __m128, 1)
test_1 (_mm256_cvtps_ph, __m128i, __m256, 1)

/* avxintrin.h */
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
test_1 (_mm256_round_pd, __m256d, __m256d, 1)
test_1 (_mm256_round_ps, __m256, __m256, 1)

/* avx2intrin.h */
test_2 ( _mm256_mpsadbw_epu8, __m256i, __m256i, __m256i, 1)
test_2 ( _mm256_alignr_epi8, __m256i, __m256i, __m256i, 1)
test_2 ( _mm256_blend_epi16, __m256i, __m256i, __m256i, 1)
test_1 ( _mm256_shuffle_epi32, __m256i, __m256i, 1)
test_1 ( _mm256_shufflehi_epi16, __m256i, __m256i, 1)
test_1 ( _mm256_shufflelo_epi16, __m256i, __m256i, 1)
test_1 ( _mm256_slli_si256, __m256i, __m256i, 8)
test_1 ( _mm256_srli_si256, __m256i, __m256i, 8)
test_2 ( _mm_blend_epi32, __m128i, __m128i, __m128i, 1)
test_2 ( _mm256_blend_epi32, __m256i, __m256i, __m256, 1)
test_1 ( _mm256_permute4x64_pd, __m256d, __m256d, 1)
test_1 ( _mm256_permute4x64_epi64, __m256i, __m256i, 1)
test_2 ( _mm256_permute2x128_si256, __m256i, __m256i, __m256i, 1)
test_1 ( _mm256_extracti128_si256, __m128i, __m256i, 1)
test_2 ( _mm256_inserti128_si256, __m256i, __m256i, __m128i, 1)
test_2 ( _mm_i32gather_pd, __m128d, double const *, __m128i, 1)
test_2 ( _mm256_i32gather_pd, __m256d, double const *, __m128i, 1)
test_2 ( _mm_i64gather_pd, __m128d, double const *, __m128i, 1)
test_2 ( _mm256_i64gather_pd, __m256d, double const *, __m256i, 1)
test_2 ( _mm_i32gather_ps, __m128, float const *, __m128i, 1)
test_2 ( _mm256_i32gather_ps, __m256, float const *, __m256i, 1)
test_2 ( _mm_i64gather_ps, __m128, float const *, __m128i, 1)
test_2 ( _mm256_i64gather_ps, __m128, float const *, __m256i, 1)
test_2 ( _mm_i32gather_epi64, __m128i, long long int const *, __m128i, 1)
test_2 ( _mm256_i32gather_epi64, __m256i, long long int const *, __m128i, 1)
test_2 ( _mm_i64gather_epi64, __m128i, long long int const *, __m128i, 1)
test_2 ( _mm256_i64gather_epi64,  __m256i, long long int const *, __m256i, 1)
test_2 ( _mm_i32gather_epi32, __m128i, int const *, __m128i, 1)
test_2 ( _mm256_i32gather_epi32, __m256i, int const *, __m256i, 1)
test_2 ( _mm_i64gather_epi32, __m128i, int const *, __m128i, 1)
test_2 ( _mm256_i64gather_epi32, __m128i, int const *, __m256i, 1)

/* wmmintrin.h (AES/PCLMUL).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("aes,pclmul")
#endif
#include <wmmintrin.h>
test_1 (_mm_aeskeygenassist_si128, __m128i, __m128i, 1)
test_2 (_mm_clmulepi64_si128, __m128i, __m128i, __m128i, 1)

/* popcnintrin.h (POPCNT).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("popcnt")
#endif
#include <popcntintrin.h>

/* x86intrin.h (FMA4/XOP/LWP/BMI/BMI2/TBM/LZCNT/FMA). */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("fma4,xop,lwp,bmi,bmi2,tbm,lzcnt,fma")
#endif
#include <x86intrin.h>
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
