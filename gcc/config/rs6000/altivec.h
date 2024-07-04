/* PowerPC AltiVec include file.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
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
   if any), so we do not need to define them as macros.  Also,
   avoid defining them as macros for C++ with strict ANSI, as
   this is not compatible.  */

#if !defined(__APPLE_ALTIVEC__) \
    && (!defined(__STRICT_ANSI__) || !defined(__cplusplus))
#define vector __vector
#define pixel __pixel
#define bool __bool
#endif

/* Condition register codes for AltiVec predicates. */

#define __CR6_EQ		0
#define __CR6_EQ_REV		1
#define __CR6_LT		2
#define __CR6_LT_REV		3

#include "rs6000-vecdefines.h"

/* Deprecated interfaces.  */
#define vec_lvx vec_ld
#define vec_lvxl vec_ldl
#define vec_stvx vec_st
#define vec_stvxl vec_stl
#define vec_vaddcuw vec_addc
#define vec_vand vec_and
#define vec_vandc vec_andc
#define vec_vcmpbfp vec_cmpb
#define vec_vcmpgefp vec_cmpge
#define vec_vctsxs vec_cts
#define vec_vctuxs vec_ctu
#define vec_vexptefp vec_expte
#define vec_vlogefp vec_loge
#define vec_vmaddfp vec_madd
#define vec_vmhaddshs vec_madds
#define vec_vmhraddshs vec_mradds
#define vec_vmladduhm vec_mladd
#define vec_vnmsubfp vec_nmsub
#define vec_vnor vec_nor
#define vec_vor vec_or
#define vec_vperm vec_perm
#define vec_vpkpx vec_packpx
#define vec_vrefp vec_re
#define vec_vrfim vec_floor
#define vec_vrfin vec_round
#define vec_vrfip vec_ceil
#define vec_vrfiz vec_trunc
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
#define vec_vsubcuw vec_subc
#define vec_vsum2sws vec_sum2s
#define vec_vsumsws vec_sums
#define vec_vxor vec_xor

/* For _ARCH_PWR8.  Always define to support #pragma GCC target.  */
#define vec_vclz vec_cntlz
#define vec_vgbbd vec_gb
#define vec_vmrgew vec_mergee
#define vec_vmrgow vec_mergeo
#define vec_vpopcntu vec_popcnt
#define vec_vrld vec_rl
#define vec_vsld vec_sl
#define vec_vsrd vec_sr
#define vec_vsrad vec_sra

/* For _ARCH_PWR9.  Always define to support #pragma GCC target.  */
#define vec_extract_fp_from_shorth vec_extract_fp32_from_shorth
#define vec_extract_fp_from_shortl vec_extract_fp32_from_shortl
#define vec_vctz vec_cnttz

/* Synonyms.  */
/* Functions that are resolved by the backend to one of the
   typed builtins.  */
#define vec_cpsgn(x,y) __builtin_vec_copysign(y,x)
#define vec_rlnm(a,b,c) (__builtin_vec_rlnm((a),((c)<<8)|(b)))

#ifdef __VSX__
/* VSX additions */
#define vec_vsx_ld __builtin_vec_vsx_ld
#define vec_vsx_st __builtin_vec_vsx_st
#define __builtin_vec_xl __builtin_vec_vsx_ld
#define __builtin_vec_xst __builtin_vec_vsx_st

#define __builtin_bcdadd_ofl __builtin_vec_bcdadd_ov
#define __builtin_bcdsub_ofl __builtin_vec_bcdsub_ov
#define __builtin_bcdcmpeq(a,b)   __builtin_vec_bcdsub_eq(a,b,0)
#define __builtin_bcdcmpgt(a,b)   __builtin_vec_bcdsub_gt(a,b,0)
#define __builtin_bcdcmplt(a,b)   __builtin_vec_bcdsub_lt(a,b,0)
#define __builtin_bcdcmpge(a,b)   __builtin_vec_bcdsub_ge(a,b,0)
#define __builtin_bcdcmple(a,b)   __builtin_vec_bcdsub_le(a,b,0)
#endif

/* For _ARCH_PWR10.  Always define to support #pragma GCC target.  */
#define __builtin_vec_se_lxvrx __builtin_vec_xl_sext
#define __builtin_vec_tr_stxvrx __builtin_vec_xst_trunc
#define __builtin_vec_ze_lxvrx __builtin_vec_xl_zext
#define __builtin_vsx_xxpermx __builtin_vec_xxpermx

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

#ifndef __POWER9_VECTOR__
__altivec_scalar_pred(vec_all_ne,
  __builtin_vec_vcmpeq_p (__CR6_EQ, a1, a2))
__altivec_scalar_pred(vec_any_eq,
  __builtin_vec_vcmpeq_p (__CR6_EQ_REV, a1, a2))
#else
__altivec_scalar_pred(vec_all_nez,
  __builtin_vec_vcmpnez_p (__CR6_LT, a1, a2))
__altivec_scalar_pred(vec_any_eqz,
  __builtin_vec_vcmpnez_p (__CR6_LT_REV, a1, a2))
__altivec_scalar_pred(vec_all_ne,
  __builtin_vec_vcmpne_p (a1, a2))
__altivec_scalar_pred(vec_any_eq,
  __builtin_vec_vcmpae_p (a1, a2))
#endif

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

#ifdef __POWER9_VECTOR__
#define vec_all_nez(a1, a2) __builtin_vec_vcmpnez_p (__CR6_LT, (a1), (a2))
#define vec_any_eqz(a1, a2) __builtin_vec_vcmpnez_p (__CR6_LT_REV, (a1), (a2))
#define vec_all_ne(a1, a2) __builtin_vec_vcmpne_p ((a1), (a2))
#define vec_any_eq(a1, a2) __builtin_vec_vcmpae_p ((a1), (a2))
#else
#define vec_all_ne(a1, a2) __builtin_vec_vcmpeq_p (__CR6_EQ, (a1), (a2))
#define vec_any_eq(a1, a2) __builtin_vec_vcmpeq_p (__CR6_EQ_REV, (a1), (a2))
#endif

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

/* Miscellaneous definitions.  */
#define vec_dss(x) __builtin_altivec_dss((x))
#define vec_dssall() __builtin_altivec_dssall ()
#define vec_splat_u8(x) ((__vector unsigned char) vec_splat_s8 ((x)))
#define vec_splat_u16(x) ((__vector unsigned short) vec_splat_s16 ((x)))
#define vec_splat_u32(x) ((__vector unsigned int) vec_splat_s32 ((x)))

/* This also accepts a type for its parameter, so it is not enough
   to #define vec_step to __builtin_vec_step.  */
#define vec_step(x) __builtin_vec_step (* (__typeof__ (x) *) 0)

#endif /* _ALTIVEC_H */
