/* PR target/82483 */
/* { dg-do compile } */
/* { dg-options "-mssse3 -mno-mmx -Wno-psabi" } */
/* { dg-error "needs isa option" "" { target *-*-* } 0 } */

#include <x86intrin.h>

void f1 (__m64 x, __m64 y, char *z) { _mm_maskmove_si64 (x, y, z); }
int f2 (__m64 x) { return _mm_extract_pi16 (x, 1); }
__m64 f3 (__m64 x, int y) { return _mm_insert_pi16 (x, y, 1); }
__m64 f4 (__m128 x) { return _mm_cvtps_pi32 (x); }
__m64 f5 (__m128 x) { return _mm_cvttps_pi32 (x); }
__m128 f6 (__m128 x, __m64 y) { return _mm_cvtpi32_ps (x, y); }
__m64 f7 (__m64 x, __m64 y) { return _mm_avg_pu8 (x, y); }
__m64 f8 (__m64 x, __m64 y) { return _mm_avg_pu16 (x, y); }
__m64 f9 (__m64 x, __m64 y) { return _mm_mulhi_pu16 (x, y); }
__m64 f10 (__m64 x, __m64 y) { return _mm_max_pu8 (x, y); }
__m64 f11 (__m64 x, __m64 y) { return _mm_max_pi16 (x, y); }
__m64 f12 (__m64 x, __m64 y) { return _mm_min_pu8 (x, y); }
__m64 f13 (__m64 x, __m64 y) { return _mm_min_pi16 (x, y); }
__m64 f14 (__m64 x, __m64 y) { return _mm_sad_pu8 (x, y); }
int f15 (__m64 x) { return _mm_movemask_pi8 (x); }
__m64 f16 (__m64 x) { return _mm_shuffle_pi16 (x, 1); }
__m64 f17 (__m128d x) { return _mm_cvtpd_pi32 (x); }
__m64 f18 (__m128d x) { return _mm_cvttpd_pi32 (x); }
__m128d f19 (__m64 x) { return _mm_cvtpi32_pd (x); }
__m64 f20 (__m64 x, __m64 y) { return _mm_mul_su32 (x, y); }
__m64 f21 (__m64 x) { return _mm_abs_pi8 (x); }
__m64 f22 (__m64 x) { return _mm_abs_pi16 (x); }
__m64 f23 (__m64 x) { return _mm_abs_pi32 (x); }
__m64 f24 (__m64 x, __m64 y) { return _mm_hadd_pi16 (x, y); }
__m64 f25 (__m64 x, __m64 y) { return _mm_hadd_pi32 (x, y); }
__m64 f26 (__m64 x, __m64 y) { return _mm_hadds_pi16 (x, y); }
__m64 f27 (__m64 x, __m64 y) { return _mm_hsub_pi16 (x, y); }
__m64 f28 (__m64 x, __m64 y) { return _mm_hsub_pi32 (x, y); }
__m64 f29 (__m64 x, __m64 y) { return _mm_hsubs_pi16 (x, y); }
__m64 f30 (__m64 x, __m64 y) { return _mm_maddubs_pi16 (x, y); }
__m64 f31 (__m64 x, __m64 y) { return _mm_mulhrs_pi16 (x, y); }
__m64 f32 (__m64 x, __m64 y) { return _mm_shuffle_pi8 (x, y); }
__m64 f33 (__m64 x, __m64 y) { return _mm_sign_pi8 (x, y); }
__m64 f34 (__m64 x, __m64 y) { return _mm_sign_pi16 (x, y); }
__m64 f35 (__m64 x, __m64 y) { return _mm_sign_pi32 (x, y); }
void f36 (__m64 *x, __m64 y) { _mm_stream_pi (x, y); }
__m64 f37 (__m64 x, __m64 y) { return _mm_alignr_pi8 (x, y, 3); }
