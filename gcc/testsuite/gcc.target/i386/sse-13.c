/* { dg-do compile } */
/* { dg-options "-O2 -Werror-implicit-function-declaration -march=k8 -msse4a -m3dnow -mavx -mavx2 -mfma4 -mxop -maes -mpclmul -mpopcnt -mabm -mlzcnt -mbmi -mbmi2 -mtbm -mlwp -mfsgsbase -mrdrnd -mf16c -mfma -mrtm -mrdseed -mprfchw -madx -mfxsr -mxsaveopt" } */

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

/* Following intrinsics require immediate arguments. */

/* ammintrin.h */
#define __builtin_ia32_extrqi(X, I, L)  __builtin_ia32_extrqi(X, 1, 1)
#define __builtin_ia32_insertqi(X, Y, I, L) __builtin_ia32_insertqi(X, Y, 1, 1)

/* immintrin.h */
#define __builtin_ia32_blendpd256(X, Y, M) __builtin_ia32_blendpd256(X, Y, 1)
#define __builtin_ia32_blendps256(X, Y, M) __builtin_ia32_blendps256(X, Y, 1)
#define __builtin_ia32_dpps256(X, Y, M) __builtin_ia32_dpps256(X, Y, 1)
#define __builtin_ia32_shufpd256(X, Y, M) __builtin_ia32_shufpd256(X, Y, 1)
#define __builtin_ia32_shufps256(X, Y, M) __builtin_ia32_shufps256(X, Y, 1)
#define __builtin_ia32_cmpsd(X, Y, O) __builtin_ia32_cmpsd(X, Y, 1)
#define __builtin_ia32_cmpss(X, Y, O) __builtin_ia32_cmpss(X, Y, 1)
#define __builtin_ia32_cmppd(X, Y, O) __builtin_ia32_cmppd(X, Y, 1)
#define __builtin_ia32_cmpps(X, Y, O) __builtin_ia32_cmpps(X, Y, 1)
#define __builtin_ia32_cmppd256(X, Y, O) __builtin_ia32_cmppd256(X, Y, 1)
#define __builtin_ia32_cmpps256(X, Y, O) __builtin_ia32_cmpps256(X, Y, 1)
#define __builtin_ia32_vextractf128_pd256(X, N) __builtin_ia32_vextractf128_pd256(X, 1)
#define __builtin_ia32_vextractf128_ps256(X, N) __builtin_ia32_vextractf128_ps256(X, 1)
#define __builtin_ia32_vextractf128_si256(X, N) __builtin_ia32_vextractf128_si256(X, 1)
#define __builtin_ia32_vpermilpd(X, N) __builtin_ia32_vpermilpd(X, 1)
#define __builtin_ia32_vpermilpd256(X, N) __builtin_ia32_vpermilpd256(X, 1)
#define __builtin_ia32_vpermilps(X, N) __builtin_ia32_vpermilps(X, 1)
#define __builtin_ia32_vpermilps256(X, N) __builtin_ia32_vpermilps256(X, 1)
#define __builtin_ia32_vpermil2pd(X, Y, C, I) __builtin_ia32_vpermil2pd(X, Y, C, 1)
#define __builtin_ia32_vpermil2pd256(X, Y, C, I) __builtin_ia32_vpermil2pd256(X, Y, C, 1)
#define __builtin_ia32_vpermil2ps(X, Y, C, I) __builtin_ia32_vpermil2ps(X, Y, C, 1)
#define __builtin_ia32_vpermil2ps256(X, Y, C, I) __builtin_ia32_vpermil2ps256(X, Y, C, 1)
#define __builtin_ia32_vperm2f128_pd256(X, Y, C) __builtin_ia32_vperm2f128_pd256(X, Y, 1)
#define __builtin_ia32_vperm2f128_ps256(X, Y, C) __builtin_ia32_vperm2f128_ps256(X, Y, 1)
#define __builtin_ia32_vperm2f128_si256(X, Y, C) __builtin_ia32_vperm2f128_si256(X, Y, 1)
#define __builtin_ia32_vinsertf128_pd256(X, Y, C) __builtin_ia32_vinsertf128_pd256(X, Y, 1)
#define __builtin_ia32_vinsertf128_ps256(X, Y, C) __builtin_ia32_vinsertf128_ps256(X, Y, 1)
#define __builtin_ia32_vinsertf128_si256(X, Y, C) __builtin_ia32_vinsertf128_si256(X, Y, 1)
#define __builtin_ia32_roundpd256(V, M) __builtin_ia32_roundpd256(V, 1)
#define __builtin_ia32_roundps256(V, M) __builtin_ia32_roundps256(V, 1)
#define __builtin_ia32_vcvtps2ph(A, I) __builtin_ia32_vcvtps2ph(A, 1)
#define __builtin_ia32_vcvtps2ph256(A, I) __builtin_ia32_vcvtps2ph256(A, 1)

/* wmmintrin.h */
#define __builtin_ia32_aeskeygenassist128(X, C) __builtin_ia32_aeskeygenassist128(X, 1)
#define __builtin_ia32_pclmulqdq128(X, Y, I) __builtin_ia32_pclmulqdq128(X, Y, 1)

/* smmintrin.h */
#define __builtin_ia32_roundpd(V, M) __builtin_ia32_roundpd(V, 1)
#define __builtin_ia32_roundsd(D, V, M) __builtin_ia32_roundsd(D, V, 1)
#define __builtin_ia32_roundps(V, M) __builtin_ia32_roundps(V, 1)
#define __builtin_ia32_roundss(D, V, M) __builtin_ia32_roundss(D, V, 1)

#define __builtin_ia32_pblendw128(X, Y, M) __builtin_ia32_pblendw128 (X, Y, 1)
#define __builtin_ia32_blendps(X, Y, M) __builtin_ia32_blendps(X, Y, 1)
#define __builtin_ia32_blendpd(X, Y, M) __builtin_ia32_blendpd(X, Y, 1)
#define __builtin_ia32_dpps(X, Y, M) __builtin_ia32_dpps(X, Y, 1)
#define __builtin_ia32_dppd(X, Y, M) __builtin_ia32_dppd(X, Y, 1)
#define __builtin_ia32_insertps128(D, S, N) __builtin_ia32_insertps128(D, S, 1)
#define __builtin_ia32_vec_ext_v4sf(X, N) __builtin_ia32_vec_ext_v4sf(X, 1)
#define __builtin_ia32_vec_set_v16qi(D, S, N) __builtin_ia32_vec_set_v16qi(D, S, 1)
#define __builtin_ia32_vec_set_v4si(D, S, N) __builtin_ia32_vec_set_v4si(D, S, 1)
#define __builtin_ia32_vec_set_v2di(D, S, N) __builtin_ia32_vec_set_v2di(D, S, 1)
#define __builtin_ia32_vec_ext_v16qi(X, N) __builtin_ia32_vec_ext_v16qi(X, 1)
#define __builtin_ia32_vec_ext_v4si(X, N) __builtin_ia32_vec_ext_v4si(X, 1)
#define __builtin_ia32_vec_ext_v2di(X, N) __builtin_ia32_vec_ext_v2di(X, 1)
#define __builtin_ia32_mpsadbw128(X, Y, M) __builtin_ia32_mpsadbw128(X, Y, 1)
#define __builtin_ia32_pcmpistrm128(X, Y, M) \
  __builtin_ia32_pcmpistrm128(X, Y, 1)
#define __builtin_ia32_pcmpistri128(X, Y, M) \
  __builtin_ia32_pcmpistri128(X, Y, 1)
#define __builtin_ia32_pcmpestrm128(X, LX, Y, LY, M) \
  __builtin_ia32_pcmpestrm128(X, LX, Y, LY, 1)
#define __builtin_ia32_pcmpestri128(X, LX, Y, LY, M) \
  __builtin_ia32_pcmpestri128(X, LX, Y, LY, 1)
#define __builtin_ia32_pcmpistria128(X, Y, M) \
  __builtin_ia32_pcmpistria128(X, Y, 1)
#define __builtin_ia32_pcmpistric128(X, Y, M) \
  __builtin_ia32_pcmpistric128(X, Y, 1)
#define __builtin_ia32_pcmpistrio128(X, Y, M) \
  __builtin_ia32_pcmpistrio128(X, Y, 1)
#define __builtin_ia32_pcmpistris128(X, Y, M) \
  __builtin_ia32_pcmpistris128(X, Y, 1)
#define __builtin_ia32_pcmpistriz128(X, Y, M) \
  __builtin_ia32_pcmpistriz128(X, Y, 1)
#define __builtin_ia32_pcmpestria128(X, LX, Y, LY, M) \
  __builtin_ia32_pcmpestria128(X, LX, Y, LY, 1)
#define __builtin_ia32_pcmpestric128(X, LX, Y, LY, M) \
  __builtin_ia32_pcmpestric128(X, LX, Y, LY, 1)
#define __builtin_ia32_pcmpestrio128(X, LX, Y, LY, M) \
  __builtin_ia32_pcmpestrio128(X, LX, Y, LY, 1)
#define __builtin_ia32_pcmpestris128(X, LX, Y, LY, M) \
  __builtin_ia32_pcmpestris128(X, LX, Y, LY, 1)
#define __builtin_ia32_pcmpestriz128(X, LX, Y, LY, M) \
  __builtin_ia32_pcmpestriz128(X, LX, Y, LY, 1)

/* tmmintrin.h */
#define __builtin_ia32_palignr128(X, Y, N) __builtin_ia32_palignr128(X, Y, 8)
#define __builtin_ia32_palignr(X, Y, N) __builtin_ia32_palignr(X, Y, 8)

/* emmintrin.h */
#define __builtin_ia32_psrldqi128(A, B) __builtin_ia32_psrldqi128(A, 8)
#define __builtin_ia32_pslldqi128(A, B) __builtin_ia32_pslldqi128(A, 8)
#define __builtin_ia32_pshufhw(A, N) __builtin_ia32_pshufhw(A, 0)
#define __builtin_ia32_pshuflw(A, N) __builtin_ia32_pshuflw(A, 0)
#define __builtin_ia32_pshufd(A, N) __builtin_ia32_pshufd(A, 0)
#define __builtin_ia32_vec_set_v8hi(A, D, N) \
  __builtin_ia32_vec_set_v8hi(A, D, 0)
#define __builtin_ia32_vec_ext_v8hi(A, N) __builtin_ia32_vec_ext_v8hi(A, 0)
#define __builtin_ia32_shufpd(A, B, N) __builtin_ia32_shufpd(A, B, 0)

/* xmmintrin.h */
#define __builtin_prefetch(P, A, I) __builtin_prefetch(P, A, _MM_HINT_NTA)
#define __builtin_ia32_pshufw(A, N) __builtin_ia32_pshufw(A, 0)
#define __builtin_ia32_vec_set_v4hi(A, D, N) \
  __builtin_ia32_vec_set_v4hi(A, D, 0)
#define __builtin_ia32_vec_ext_v4hi(A, N) __builtin_ia32_vec_ext_v4hi(A, 0)
#define __builtin_ia32_shufps(A, B, N) __builtin_ia32_shufps(A, B, 0)

/* xopintrin.h */
#define __builtin_ia32_vprotbi(A, N) __builtin_ia32_vprotbi (A,1)
#define __builtin_ia32_vprotwi(A, N) __builtin_ia32_vprotwi (A,1)
#define __builtin_ia32_vprotdi(A, N) __builtin_ia32_vprotdi (A,1)
#define __builtin_ia32_vprotqi(A, N) __builtin_ia32_vprotqi (A,1)

/* lwpintrin.h */
#define __builtin_ia32_lwpval32(D2, D1, F) __builtin_ia32_lwpval32 (D2, D1, 1)
#define __builtin_ia32_lwpval64(D2, D1, F) __builtin_ia32_lwpval64 (D2, D1, 1)
#define __builtin_ia32_lwpins32(D2, D1, F) __builtin_ia32_lwpins32 (D2, D1, 1)
#define __builtin_ia32_lwpins64(D2, D1, F) __builtin_ia32_lwpins64 (D2, D1, 1)

/* tbmintrin.h */
#define __builtin_ia32_bextri_u32(X, Y) __builtin_ia32_bextri_u32 (X, 1)
#define __builtin_ia32_bextri_u64(X, Y) __builtin_ia32_bextri_u64 (X, 1)

/* avx2intrin.h */
#define __builtin_ia32_mpsadbw256(X, Y, Z) __builtin_ia32_mpsadbw256 (X, Y, 1)
#define __builtin_ia32_palignr256(X, Y, Z) __builtin_ia32_palignr256 (X, Y, 8)
#define __builtin_ia32_pblendw256(X, Y, Z) __builtin_ia32_pblendw256 (X, Y, 1)
#define __builtin_ia32_pshufd256(X, Y) __builtin_ia32_pshufd256(X, 1)
#define __builtin_ia32_pshufhw256(X, Y) __builtin_ia32_pshufhw256(X, 1)
#define __builtin_ia32_pshuflw256(X, Y) __builtin_ia32_pshuflw256(X, 1)
#define __builtin_ia32_pslldqi256(X, Y) __builtin_ia32_pslldqi256(X, 8)
#define __builtin_ia32_psrldqi256(X, Y) __builtin_ia32_psrldqi256(X, 8)
#define __builtin_ia32_pblendd128(X, Y, Z) __builtin_ia32_pblendd128(X, Y, 1)
#define __builtin_ia32_pblendd256(X, Y, Z) __builtin_ia32_pblendd256(X, Y, 1)
#define __builtin_ia32_permdf256(X, Y) __builtin_ia32_permdf256(X, 1)
#define __builtin_ia32_permdi256(X, Y) __builtin_ia32_permdi256(X, 1)
#define __builtin_ia32_permti256(X, Y, Z) __builtin_ia32_permti256(X, Y, 1)
#define __builtin_ia32_extract128i256(X, Y) __builtin_ia32_extract128i256(X, 1)
#define __builtin_ia32_insert128i256(X, Y, Z) __builtin_ia32_insert128i256(X, Y, 1)
#define __builtin_ia32_gathersiv2df(X, Y, Z, K, M) __builtin_ia32_gathersiv2df(X, Y, Z, K, 1)
#define __builtin_ia32_gathersiv4df(X, Y, Z, K, M) __builtin_ia32_gathersiv4df(X, Y, Z, K, 1)
#define __builtin_ia32_gatherdiv2df(X, Y, Z, K, M) __builtin_ia32_gatherdiv2df(X, Y, Z, K, 1)
#define __builtin_ia32_gatherdiv4df(X, Y, Z, K, M) __builtin_ia32_gatherdiv4df(X, Y, Z, K, 1)
#define __builtin_ia32_gathersiv4sf(X, Y, Z, K, M) __builtin_ia32_gathersiv4sf(X, Y, Z, K, 1)
#define __builtin_ia32_gathersiv8sf(X, Y, Z, K, M) __builtin_ia32_gathersiv8sf(X, Y, Z, K, 1)
#define __builtin_ia32_gatherdiv4sf(X, Y, Z, K, M) __builtin_ia32_gatherdiv4sf(X, Y, Z, K, 1)
#define __builtin_ia32_gatherdiv4sf256(X, Y, Z, K, M) __builtin_ia32_gatherdiv4sf256(X, Y, Z, K, 1)
#define __builtin_ia32_gathersiv2di(X, Y, Z, K, M) __builtin_ia32_gathersiv2di(X, Y, Z, K, 1)
#define __builtin_ia32_gathersiv4di(X, Y, Z, K, M) __builtin_ia32_gathersiv4di(X, Y, Z, K, 1)
#define __builtin_ia32_gatherdiv2di(X, Y, Z, K, M) __builtin_ia32_gatherdiv2di(X, Y, Z, K, 1)
#define __builtin_ia32_gatherdiv4di(X, Y, Z, K, M) __builtin_ia32_gatherdiv4di(X, Y, Z, K, 1)
#define __builtin_ia32_gathersiv4si(X, Y, Z, K, M) __builtin_ia32_gathersiv4si(X, Y, Z, K, 1)
#define __builtin_ia32_gathersiv8si(X, Y, Z, K, M) __builtin_ia32_gathersiv8si(X, Y, Z, K, 1)
#define __builtin_ia32_gatherdiv4si(X, Y, Z, K, M) __builtin_ia32_gatherdiv4si(X, Y, Z, K, 1)
#define __builtin_ia32_gatherdiv4si256(X, Y, Z, K, M) __builtin_ia32_gatherdiv4si256(X, Y, Z, K, 1)

/* rtmintrin.h */
#define __builtin_ia32_xabort (N) __builtin_ia32_xabort (1)
