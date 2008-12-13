/* { dg-do compile } */
/* { dg-options "-O2 -Werror-implicit-function-declaration -march=k8 -m3dnow -mavx -msse5 -maes -mpclmul" } */

#include <mm_malloc.h>

/* Test that the intrinsics compile with optimization.  All of them are
   defined as inline functions in {,x,e,p,t,s,w,a,b,i}mmintrin.h and mm3dnow.h
   that reference the proper builtin functions.  Defining away "extern" and
   "__inline" results in all of them being compiled as proper functions.  */

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

/* wmmintrin.h */
#define __builtin_ia32_aeskeygenassist128(X, C) __builtin_ia32_aeskeygenassist128(X, 1)
#define __builtin_ia32_pclmulqdq128(X, Y, I) __builtin_ia32_pclmulqdq128(X, Y, 1)

/* mmintrin-common.h */
#define __builtin_ia32_roundpd(V, M) __builtin_ia32_roundpd(V, 1)
#define __builtin_ia32_roundsd(D, V, M) __builtin_ia32_roundsd(D, V, 1)
#define __builtin_ia32_roundps(V, M) __builtin_ia32_roundps(V, 1)
#define __builtin_ia32_roundss(D, V, M) __builtin_ia32_roundss(D, V, 1)

/* smmintrin.h */
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

/* bmmintrin.h */
#define __builtin_ia32_protbi(A, B) __builtin_ia32_protbi(A,1)
#define __builtin_ia32_protwi(A, B) __builtin_ia32_protwi(A,1)
#define __builtin_ia32_protdi(A, B) __builtin_ia32_protdi(A,1)
#define __builtin_ia32_protqi(A, B) __builtin_ia32_protqi(A,1)

#include <x86intrin.h>
