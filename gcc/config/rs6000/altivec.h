/* PowerPC AltiVec include file.
   Copyright (C) 2002-2016 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez (aldyh@redhat.com).
   Rewritten by Paolo Bonzini (bonzini@gnu.org).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Implemented to conform to the specification included in the AltiVec
   Technology Programming Interface Manual (ALTIVECPIM/D 6/1999 Rev 0).  */

#ifndef _ALTIVEC_H
#define _ALTIVEC_H 1

#if !defined(__VEC__) || !defined(__ALTIVEC__)
#error Use the "-maltivec" flag to enable PowerPC AltiVec support
#endif

/* If __APPLE_ALTIVEC__ is defined, the compiler supports 'vector',
   'pixel' and 'bool' as context-sensitive AltiVec keywords (in 
   non-AltiVec contexts, they revert to their original meanings,
   if any), so we do not need to define them as macros.  */

#if !defined(__APPLE_ALTIVEC__)
/* You are allowed to undef these for C++ compatibility.  */
#define vector __vector
#define pixel __pixel
#define bool __bool
#endif

/* Condition register codes for AltiVec predicates. */

#define __CR6_EQ		0
#define __CR6_EQ_REV		1
#define __CR6_LT		2
#define __CR6_LT_REV		3

/* Synonyms.  */
#define vec_vaddcuw vec_addc
#define vec_vand vec_and
#define vec_vandc vec_andc
#define vec_vrfip vec_ceil
#define vec_vcmpbfp vec_cmpb
#define vec_vcmpgefp vec_cmpge
#define vec_vctsxs vec_cts
#define vec_vctuxs vec_ctu
#define vec_vexptefp vec_expte
#define vec_vrfim vec_floor
#define vec_lvx vec_ld
#define vec_lvxl vec_ldl
#define vec_vlogefp vec_loge
#define vec_vmaddfp vec_madd
#define vec_vmhaddshs vec_madds
#define vec_vmladduhm vec_mladd
#define vec_vmhraddshs vec_mradds
#define vec_vnmsubfp vec_nmsub
#define vec_vnor vec_nor
#define vec_vor vec_or
#define vec_vpkpx vec_packpx
#define vec_vperm vec_perm
#define vec_vrefp vec_re
#define vec_vrfin vec_round
#define vec_vrsqrtefp vec_rsqrte
#define vec_vsel vec_sel
#define vec_vsldoi vec_sld
#define vec_vsl vec_sll
#define vec_vslo vec_slo
#define vec_vspltisb vec_splat_s8
#define vec_vspltish vec_splat_s16
#define vec_vspltisw vec_splat_s32
#define vec_vsr vec_srl
#define vec_vsro vec_sro
#define vec_stvx vec_st
#define vec_stvxl vec_stl
#define vec_vsubcuw vec_subc
#define vec_vsum2sws vec_sum2s
#define vec_vsumsws vec_sums
#define vec_vrfiz vec_trunc
#define vec_vxor vec_xor

/* Functions that are resolved by the backend to one of the
   typed builtins.  */
#define vec_vaddfp __builtin_vec_vaddfp
#define vec_addc __builtin_vec_addc
#define vec_adde __builtin_vec_adde
#define vec_addec __builtin_vec_addec
#define vec_vaddsws __builtin_vec_vaddsws
#define vec_vaddshs __builtin_vec_vaddshs
#define vec_vaddsbs __builtin_vec_vaddsbs
#define vec_vavgsw __builtin_vec_vavgsw
#define vec_vavguw __builtin_vec_vavguw
#define vec_vavgsh __builtin_vec_vavgsh
#define vec_vavguh __builtin_vec_vavguh
#define vec_vavgsb __builtin_vec_vavgsb
#define vec_vavgub __builtin_vec_vavgub
#define vec_ceil __builtin_vec_ceil
#define vec_cmpb __builtin_vec_cmpb
#define vec_vcmpeqfp __builtin_vec_vcmpeqfp
#define vec_cmpge __builtin_vec_cmpge
#define vec_vcmpgtfp __builtin_vec_vcmpgtfp
#define vec_vcmpgtsw __builtin_vec_vcmpgtsw
#define vec_vcmpgtuw __builtin_vec_vcmpgtuw
#define vec_vcmpgtsh __builtin_vec_vcmpgtsh
#define vec_vcmpgtuh __builtin_vec_vcmpgtuh
#define vec_vcmpgtsb __builtin_vec_vcmpgtsb
#define vec_vcmpgtub __builtin_vec_vcmpgtub
#define vec_vcfsx __builtin_vec_vcfsx
#define vec_vcfux __builtin_vec_vcfux
#define vec_cts __builtin_vec_cts
#define vec_ctu __builtin_vec_ctu
#define vec_cpsgn __builtin_vec_copysign
#define vec_double __builtin_vec_double
#define vec_expte __builtin_vec_expte
#define vec_floor __builtin_vec_floor
#define vec_loge __builtin_vec_loge
#define vec_madd __builtin_vec_madd
#define vec_madds __builtin_vec_madds
#define vec_mtvscr __builtin_vec_mtvscr
#define vec_vmaxfp __builtin_vec_vmaxfp
#define vec_vmaxsw __builtin_vec_vmaxsw
#define vec_vmaxsh __builtin_vec_vmaxsh
#define vec_vmaxsb __builtin_vec_vmaxsb
#define vec_vminfp __builtin_vec_vminfp
#define vec_vminsw __builtin_vec_vminsw
#define vec_vminsh __builtin_vec_vminsh
#define vec_vminsb __builtin_vec_vminsb
#define vec_mradds __builtin_vec_mradds
#define vec_vmsumshm __builtin_vec_vmsumshm
#define vec_vmsumuhm __builtin_vec_vmsumuhm
#define vec_vmsummbm __builtin_vec_vmsummbm
#define vec_vmsumubm __builtin_vec_vmsumubm
#define vec_vmsumshs __builtin_vec_vmsumshs
#define vec_vmsumuhs __builtin_vec_vmsumuhs
#define vec_vmulesb __builtin_vec_vmulesb
#define vec_vmulesh __builtin_vec_vmulesh
#define vec_vmuleuh __builtin_vec_vmuleuh
#define vec_vmuleub __builtin_vec_vmuleub
#define vec_vmulosh __builtin_vec_vmulosh
#define vec_vmulouh __builtin_vec_vmulouh
#define vec_vmulosb __builtin_vec_vmulosb
#define vec_vmuloub __builtin_vec_vmuloub
#define vec_nmsub __builtin_vec_nmsub
#define vec_packpx __builtin_vec_packpx
#define vec_vpkswss __builtin_vec_vpkswss
#define vec_vpkuwus __builtin_vec_vpkuwus
#define vec_vpkshss __builtin_vec_vpkshss
#define vec_vpkuhus __builtin_vec_vpkuhus
#define vec_vpkswus __builtin_vec_vpkswus
#define vec_vpkshus __builtin_vec_vpkshus
#define vec_re __builtin_vec_re
#define vec_round __builtin_vec_round
#define vec_recipdiv __builtin_vec_recipdiv
#define vec_rsqrt __builtin_vec_rsqrt
#define vec_rsqrte __builtin_vec_rsqrte
#define vec_vsubfp __builtin_vec_vsubfp
#define vec_subc __builtin_vec_subc
#define vec_vsubsws __builtin_vec_vsubsws
#define vec_vsubshs __builtin_vec_vsubshs
#define vec_vsubsbs __builtin_vec_vsubsbs
#define vec_sum4s __builtin_vec_sum4s
#define vec_vsum4shs __builtin_vec_vsum4shs
#define vec_vsum4sbs __builtin_vec_vsum4sbs
#define vec_vsum4ubs __builtin_vec_vsum4ubs
#define vec_sum2s __builtin_vec_sum2s
#define vec_sums __builtin_vec_sums
#define vec_trunc __builtin_vec_trunc
#define vec_vupkhpx __builtin_vec_vupkhpx
#define vec_vupkhsh __builtin_vec_vupkhsh
#define vec_vupkhsb __builtin_vec_vupkhsb
#define vec_vupklpx __builtin_vec_vupklpx
#define vec_vupklsh __builtin_vec_vupklsh
#define vec_vupklsb __builtin_vec_vupklsb
#define vec_abs __builtin_vec_abs
#define vec_abss __builtin_vec_abss
#define vec_add __builtin_vec_add
#define vec_adds __builtin_vec_adds
#define vec_and __builtin_vec_and
#define vec_andc __builtin_vec_andc
#define vec_avg __builtin_vec_avg
#define vec_cmpeq __builtin_vec_cmpeq
#define vec_cmpgt __builtin_vec_cmpgt
#define vec_ctf __builtin_vec_ctf
#define vec_dst __builtin_vec_dst
#define vec_dstst __builtin_vec_dstst
#define vec_dststt __builtin_vec_dststt
#define vec_dstt __builtin_vec_dstt
#define vec_ld __builtin_vec_ld
#define vec_lde __builtin_vec_lde
#define vec_ldl __builtin_vec_ldl
#define vec_lvebx __builtin_vec_lvebx
#define vec_lvehx __builtin_vec_lvehx
#define vec_lvewx __builtin_vec_lvewx
#define vec_pmsum_be __builtin_vec_vpmsum
#define vec_shasigma_be __builtin_crypto_vshasigma
/* Cell only intrinsics.  */
#ifdef __PPU__
#define vec_lvlx __builtin_vec_lvlx
#define vec_lvlxl __builtin_vec_lvlxl
#define vec_lvrx __builtin_vec_lvrx
#define vec_lvrxl __builtin_vec_lvrxl
#endif
#define vec_lvsl __builtin_vec_lvsl
#define vec_lvsr __builtin_vec_lvsr
#define vec_max __builtin_vec_max
#define vec_mergee __builtin_vec_vmrgew
#define vec_mergeh __builtin_vec_mergeh
#define vec_mergel __builtin_vec_mergel
#define vec_mergeo __builtin_vec_vmrgow
#define vec_min __builtin_vec_min
#define vec_mladd __builtin_vec_mladd
#define vec_msum __builtin_vec_msum
#define vec_msums __builtin_vec_msums
#define vec_mule __builtin_vec_mule
#define vec_mulo __builtin_vec_mulo
#define vec_nor __builtin_vec_nor
#define vec_or __builtin_vec_or
#define vec_pack __builtin_vec_pack
#define vec_packs __builtin_vec_packs
#define vec_packsu __builtin_vec_packsu
#define vec_perm __builtin_vec_perm
#define vec_rl __builtin_vec_rl
#define vec_sel __builtin_vec_sel
#define vec_sl __builtin_vec_sl
#define vec_sld __builtin_vec_sld
#define vec_sll __builtin_vec_sll
#define vec_slo __builtin_vec_slo
#define vec_splat __builtin_vec_splat
#define vec_sr __builtin_vec_sr
#define vec_sra __builtin_vec_sra
#define vec_srl __builtin_vec_srl
#define vec_sro __builtin_vec_sro
#define vec_st __builtin_vec_st
#define vec_ste __builtin_vec_ste
#define vec_stl __builtin_vec_stl
#define vec_stvebx __builtin_vec_stvebx
#define vec_stvehx __builtin_vec_stvehx
#define vec_stvewx __builtin_vec_stvewx
/* Cell only intrinsics.  */
#ifdef __PPU__
#define vec_stvlx __builtin_vec_stvlx
#define vec_stvlxl __builtin_vec_stvlxl
#define vec_stvrx __builtin_vec_stvrx
#define vec_stvrxl __builtin_vec_stvrxl
#endif
#define vec_sub __builtin_vec_sub
#define vec_subs __builtin_vec_subs
#define vec_sum __builtin_vec_sum
#define vec_unpackh __builtin_vec_unpackh
#define vec_unpackl __builtin_vec_unpackl
#define vec_vaddubm __builtin_vec_vaddubm
#define vec_vaddubs __builtin_vec_vaddubs
#define vec_vadduhm __builtin_vec_vadduhm
#define vec_vadduhs __builtin_vec_vadduhs
#define vec_vadduwm __builtin_vec_vadduwm
#define vec_vadduws __builtin_vec_vadduws
#define vec_vcmpequb __builtin_vec_vcmpequb
#define vec_vcmpequh __builtin_vec_vcmpequh
#define vec_vcmpequw __builtin_vec_vcmpequw
#define vec_vmaxub __builtin_vec_vmaxub
#define vec_vmaxuh __builtin_vec_vmaxuh
#define vec_vmaxuw __builtin_vec_vmaxuw
#define vec_vminub __builtin_vec_vminub
#define vec_vminuh __builtin_vec_vminuh
#define vec_vminuw __builtin_vec_vminuw
#define vec_vmrghb __builtin_vec_vmrghb
#define vec_vmrghh __builtin_vec_vmrghh
#define vec_vmrghw __builtin_vec_vmrghw
#define vec_vmrglb __builtin_vec_vmrglb
#define vec_vmrglh __builtin_vec_vmrglh
#define vec_vmrglw __builtin_vec_vmrglw
#define vec_vpkuhum __builtin_vec_vpkuhum
#define vec_vpkuwum __builtin_vec_vpkuwum
#define vec_vrlb __builtin_vec_vrlb
#define vec_vrlh __builtin_vec_vrlh
#define vec_vrlw __builtin_vec_vrlw
#define vec_vslb __builtin_vec_vslb
#define vec_vslh __builtin_vec_vslh
#define vec_vslw __builtin_vec_vslw
#define vec_vspltb __builtin_vec_vspltb
#define vec_vsplth __builtin_vec_vsplth
#define vec_vspltw __builtin_vec_vspltw
#define vec_vsrab __builtin_vec_vsrab
#define vec_vsrah __builtin_vec_vsrah
#define vec_vsraw __builtin_vec_vsraw
#define vec_vsrb __builtin_vec_vsrb
#define vec_vsrh __builtin_vec_vsrh
#define vec_vsrw __builtin_vec_vsrw
#define vec_vsububs __builtin_vec_vsububs
#define vec_vsububm __builtin_vec_vsububm
#define vec_vsubuhm __builtin_vec_vsubuhm
#define vec_vsubuhs __builtin_vec_vsubuhs
#define vec_vsubuwm __builtin_vec_vsubuwm
#define vec_vsubuws __builtin_vec_vsubuws
#define vec_xor __builtin_vec_xor

#define vec_extract __builtin_vec_extract
#define vec_insert __builtin_vec_insert
#define vec_splats __builtin_vec_splats
#define vec_promote __builtin_vec_promote

#ifdef __VSX__
/* VSX additions */
#define vec_div __builtin_vec_div
#define vec_mul __builtin_vec_mul
#define vec_msub __builtin_vec_msub
#define vec_nmadd __builtin_vec_nmadd
#define vec_nearbyint __builtin_vec_nearbyint
#define vec_rint __builtin_vec_rint
#define vec_sqrt __builtin_vec_sqrt
#define vec_vsx_ld __builtin_vec_vsx_ld
#define vec_vsx_st __builtin_vec_vsx_st
#define vec_xl __builtin_vec_vsx_ld
#define vec_xst __builtin_vec_vsx_st

/* Note, xxsldi and xxpermdi were added as __builtin_vsx_<xxx> functions
   instead of __builtin_vec_<xxx>  */
#define vec_xxsldwi __builtin_vsx_xxsldwi
#define vec_xxpermdi __builtin_vsx_xxpermdi
#endif

#ifdef _ARCH_PWR8
/* Vector additions added in ISA 2.07.  */
#define vec_eqv __builtin_vec_eqv
#define vec_nand __builtin_vec_nand
#define vec_orc __builtin_vec_orc
#define vec_vaddcuq __builtin_vec_vaddcuq
#define vec_vaddudm __builtin_vec_vaddudm
#define vec_vadduqm __builtin_vec_vadduqm
#define vec_vbpermq __builtin_vec_vbpermq
#define vec_bperm __builtin_vec_vbpermq
#define vec_vclz __builtin_vec_vclz
#define vec_cntlz __builtin_vec_vclz
#define vec_vclzb __builtin_vec_vclzb
#define vec_vclzd __builtin_vec_vclzd
#define vec_vclzh __builtin_vec_vclzh
#define vec_vclzw __builtin_vec_vclzw
#define vec_vaddecuq __builtin_vec_vaddecuq
#define vec_vaddeuqm __builtin_vec_vaddeuqm
#define vec_vsubecuq __builtin_vec_vsubecuq
#define vec_vsubeuqm __builtin_vec_vsubeuqm
#define vec_vgbbd __builtin_vec_vgbbd
#define vec_gb __builtin_vec_vgbbd
#define vec_vmaxsd __builtin_vec_vmaxsd
#define vec_vmaxud __builtin_vec_vmaxud
#define vec_vminsd __builtin_vec_vminsd
#define vec_vminud __builtin_vec_vminud
#define vec_vmrgew __builtin_vec_vmrgew
#define vec_vmrgow __builtin_vec_vmrgow
#define vec_vpksdss __builtin_vec_vpksdss
#define vec_vpksdus __builtin_vec_vpksdus
#define vec_vpkudum __builtin_vec_vpkudum
#define vec_vpkudus __builtin_vec_vpkudus
#define vec_vpopcnt __builtin_vec_vpopcnt
#define vec_vpopcntb __builtin_vec_vpopcntb
#define vec_vpopcntd __builtin_vec_vpopcntd
#define vec_vpopcnth __builtin_vec_vpopcnth
#define vec_vpopcntw __builtin_vec_vpopcntw
#define vec_vrld __builtin_vec_vrld
#define vec_vsld __builtin_vec_vsld
#define vec_vsrad __builtin_vec_vsrad
#define vec_vsrd __builtin_vec_vsrd
#define vec_vsubcuq __builtin_vec_vsubcuq
#define vec_vsubudm __builtin_vec_vsubudm
#define vec_vsubuqm __builtin_vec_vsubuqm
#define vec_vupkhsw __builtin_vec_vupkhsw
#define vec_vupklsw __builtin_vec_vupklsw
#endif

#ifdef _ARCH_PWR9
/* Vector additions added in ISA 3.0.  */
#define vec_vctz __builtin_vec_vctz
#define vec_cntlz __builtin_vec_vctz
#define vec_vctzb __builtin_vec_vctzb
#define vec_vctzd __builtin_vec_vctzd
#define vec_vctzh __builtin_vec_vctzh
#define vec_vctzw __builtin_vec_vctzw
#define vec_vprtyb __builtin_vec_vprtyb
#define vec_vprtybd __builtin_vec_vprtybd
#define vec_vprtybw __builtin_vec_vprtybw

#ifdef _ARCH_PPC64
#define vec_vprtybq __builtin_vec_vprtybq
#endif

#define vec_slv __builtin_vec_vslv
#define vec_srv __builtin_vec_vsrv

#define vec_absd __builtin_vec_vadu
#define vec_absdb __builtin_vec_vadub
#define vec_absdh __builtin_vec_vaduh
#define vec_absdw __builtin_vec_vaduw
#endif

/* Predicates.
   For C++, we use templates in order to allow non-parenthesized arguments.
   For C, instead, we use macros since non-parenthesized arguments were
   not allowed even in older GCC implementation of AltiVec.

   In the future, we may add more magic to the back-end, so that no
   one- or two-argument macros are used.  */

#ifdef __cplusplus__
#define __altivec_unary_pred(NAME, CALL) \
template <class T> int NAME (T a1) { return CALL; }

#define __altivec_scalar_pred(NAME, CALL) \
template <class T, class U> int NAME (T a1, U a2) { return CALL; }

/* Given the vec_step of a type, return the corresponding bool type.  */
template <int STEP> class __altivec_bool_ret { };
template <> class __altivec_bool_ret <4> {
  typedef __vector __bool int __ret;
};
template <> class __altivec_bool_ret <8> {
  typedef __vector __bool short __ret;
};
template <> class __altivec_bool_ret <16> {
  typedef __vector __bool char __ret;
};

/* Be very liberal in the pairs we accept.  Mistakes such as passing
   a `vector char' and `vector short' will be caught by the middle-end,
   while any attempt to detect them here would produce hard to understand
   error messages involving the implementation details of AltiVec.  */
#define __altivec_binary_pred(NAME, CALL) \
template <class T, class U> \
typename __altivec_bool_ret <vec_step (T)>::__ret \
NAME (T a1, U a2) \
{ \
  return CALL; \
}

__altivec_binary_pred(vec_cmplt,
  __builtin_vec_cmpgt (a2, a1))
__altivec_binary_pred(vec_cmple,
  __builtin_vec_cmpge (a2, a1))

__altivec_scalar_pred(vec_all_in,
  __builtin_altivec_vcmpbfp_p (__CR6_EQ, a1, a2))
__altivec_scalar_pred(vec_any_out,
  __builtin_altivec_vcmpbfp_p (__CR6_EQ_REV, a1, a2))

__altivec_unary_pred(vec_all_nan,
  __builtin_altivec_vcmpeq_p (__CR6_EQ, a1, a1))
__altivec_unary_pred(vec_any_nan,
  __builtin_altivec_vcmpeq_p (__CR6_LT_REV, a1, a1))

__altivec_unary_pred(vec_all_numeric,
  __builtin_altivec_vcmpeq_p (__CR6_LT, a1, a1))
__altivec_unary_pred(vec_any_numeric,
  __builtin_altivec_vcmpeq_p (__CR6_EQ_REV, a1, a1))

__altivec_scalar_pred(vec_all_eq,
  __builtin_vec_vcmpeq_p (__CR6_LT, a1, a2))
__altivec_scalar_pred(vec_all_ne,
  __builtin_vec_vcmpeq_p (__CR6_EQ, a1, a2))
__altivec_scalar_pred(vec_any_eq,
  __builtin_vec_vcmpeq_p (__CR6_EQ_REV, a1, a2))
__altivec_scalar_pred(vec_any_ne,
  __builtin_vec_vcmpeq_p (__CR6_LT_REV, a1, a2))

__altivec_scalar_pred(vec_all_gt,
  __builtin_vec_vcmpgt_p (__CR6_LT, a1, a2))
__altivec_scalar_pred(vec_all_lt,
  __builtin_vec_vcmpgt_p (__CR6_LT, a2, a1))
__altivec_scalar_pred(vec_any_gt,
  __builtin_vec_vcmpgt_p (__CR6_EQ_REV, a1, a2))
__altivec_scalar_pred(vec_any_lt,
  __builtin_vec_vcmpgt_p (__CR6_EQ_REV, a2, a1))

__altivec_scalar_pred(vec_all_ngt,
  __builtin_altivec_vcmpgt_p (__CR6_EQ, a1, a2))
__altivec_scalar_pred(vec_all_nlt,
  __builtin_altivec_vcmpgt_p (__CR6_EQ, a2, a1))
__altivec_scalar_pred(vec_any_ngt,
  __builtin_altivec_vcmpgt_p (__CR6_LT_REV, a1, a2))
__altivec_scalar_pred(vec_any_nlt,
  __builtin_altivec_vcmpgt_p (__CR6_LT_REV, a2, a1))

/* __builtin_vec_vcmpge_p is vcmpgefp for floating-point vector types,
   while for integer types it is converted to __builtin_vec_vcmpgt_p,
   with inverted args and condition code.  */
__altivec_scalar_pred(vec_all_le,
  __builtin_vec_vcmpge_p (__CR6_LT, a2, a1))
__altivec_scalar_pred(vec_all_ge,
  __builtin_vec_vcmpge_p (__CR6_LT, a1, a2))
__altivec_scalar_pred(vec_any_le,
  __builtin_vec_vcmpge_p (__CR6_EQ_REV, a2, a1))
__altivec_scalar_pred(vec_any_ge,
  __builtin_vec_vcmpge_p (__CR6_EQ_REV, a1, a2))

__altivec_scalar_pred(vec_all_nge,
  __builtin_altivec_vcmpge_p (__CR6_EQ, a1, a2))
__altivec_scalar_pred(vec_all_nle,
  __builtin_altivec_vcmpge_p (__CR6_EQ, a2, a1))
__altivec_scalar_pred(vec_any_nge,
  __builtin_altivec_vcmpge_p (__CR6_LT_REV, a1, a2))
__altivec_scalar_pred(vec_any_nle,
  __builtin_altivec_vcmpge_p (__CR6_LT_REV, a2, a1))

#undef __altivec_scalar_pred
#undef __altivec_unary_pred
#undef __altivec_binary_pred
#else
#define vec_cmplt(a1, a2) __builtin_vec_cmpgt ((a2), (a1))
#define vec_cmple(a1, a2) __builtin_vec_cmpge ((a2), (a1))

#define vec_all_in(a1, a2) __builtin_altivec_vcmpbfp_p (__CR6_EQ, (a1), (a2))
#define vec_any_out(a1, a2) __builtin_altivec_vcmpbfp_p (__CR6_EQ_REV, (a1), (a2))

#define vec_all_nan(a1) __builtin_vec_vcmpeq_p (__CR6_EQ, (a1), (a1))
#define vec_any_nan(a1) __builtin_vec_vcmpeq_p (__CR6_LT_REV, (a1), (a1))

#define vec_all_numeric(a1) __builtin_vec_vcmpeq_p (__CR6_LT, (a1), (a1))
#define vec_any_numeric(a1) __builtin_vec_vcmpeq_p (__CR6_EQ_REV, (a1), (a1))

#define vec_all_eq(a1, a2) __builtin_vec_vcmpeq_p (__CR6_LT, (a1), (a2))
#define vec_all_ne(a1, a2) __builtin_vec_vcmpeq_p (__CR6_EQ, (a1), (a2))
#define vec_any_eq(a1, a2) __builtin_vec_vcmpeq_p (__CR6_EQ_REV, (a1), (a2))
#define vec_any_ne(a1, a2) __builtin_vec_vcmpeq_p (__CR6_LT_REV, (a1), (a2))

#define vec_all_gt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_LT, (a1), (a2))
#define vec_all_lt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_LT, (a2), (a1))
#define vec_any_gt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_EQ_REV, (a1), (a2))
#define vec_any_lt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_EQ_REV, (a2), (a1))

#define vec_all_ngt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_EQ, (a1), (a2))
#define vec_all_nlt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_EQ, (a2), (a1))
#define vec_any_ngt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_LT_REV, (a1), (a2))
#define vec_any_nlt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_LT_REV, (a2), (a1))

/* __builtin_vec_vcmpge_p is vcmpgefp for floating-point vector types,
   while for integer types it is converted to __builtin_vec_vcmpgt_p,
   with inverted args and condition code.  */
#define vec_all_le(a1, a2) __builtin_vec_vcmpge_p (__CR6_LT, (a2), (a1))
#define vec_all_ge(a1, a2) __builtin_vec_vcmpge_p (__CR6_LT, (a1), (a2))
#define vec_any_le(a1, a2) __builtin_vec_vcmpge_p (__CR6_EQ_REV, (a2), (a1))
#define vec_any_ge(a1, a2) __builtin_vec_vcmpge_p (__CR6_EQ_REV, (a1), (a2))

#define vec_all_nge(a1, a2) __builtin_vec_vcmpge_p (__CR6_EQ, (a1), (a2))
#define vec_all_nle(a1, a2) __builtin_vec_vcmpge_p (__CR6_EQ, (a2), (a1))
#define vec_any_nge(a1, a2) __builtin_vec_vcmpge_p (__CR6_LT_REV, (a1), (a2))
#define vec_any_nle(a1, a2) __builtin_vec_vcmpge_p (__CR6_LT_REV, (a2), (a1))
#endif

/* These do not accept vectors, so they do not have a __builtin_vec_*
   counterpart.  */
#define vec_dss(x) __builtin_altivec_dss((x))
#define vec_dssall() __builtin_altivec_dssall ()
#define vec_mfvscr() ((__vector unsigned short) __builtin_altivec_mfvscr ())
#define vec_splat_s8(x) __builtin_altivec_vspltisb ((x))
#define vec_splat_s16(x) __builtin_altivec_vspltish ((x))
#define vec_splat_s32(x) __builtin_altivec_vspltisw ((x))
#define vec_splat_u8(x) ((__vector unsigned char) vec_splat_s8 ((x)))
#define vec_splat_u16(x) ((__vector unsigned short) vec_splat_s16 ((x)))
#define vec_splat_u32(x) ((__vector unsigned int) vec_splat_s32 ((x)))

/* This also accepts a type for its parameter, so it is not enough
   to #define vec_step to __builtin_vec_step.  */
#define vec_step(x) __builtin_vec_step (* (__typeof__ (x) *) 0)

#endif /* _ALTIVEC_H */
