/* PowerPC AltiVec include file.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez (aldyh@redhat.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */

/* Implemented to conform to the specification included in the AltiVec
   Technology Programming Interface Manual (ALTIVECPIM/D 6/1999 Rev 0).  */

#ifndef _ALTIVEC_H
#define _ALTIVEC_H 1

/* Required by Motorola specs.  */
#define __VEC__ 10206

#ifndef __ALTIVEC__
#define __ALTIVEC__ 1
#endif

#define __vector __attribute__((vector_size(16)))

/* You are allowed to undef this for C++ compatability.  */
#define vector __vector

#define bool signed
#define pixel unsigned short
#define __pixel unsigned short

/* Dummy prototype.  */
extern int __altivec_link_error_invalid_argument ();

/* Helper macros.  */

#define __CR6_EQ		0
#define __CR6_EQ_REV		1
#define __CR6_LT		2
#define __CR6_LT_REV		3

#define __bin_args_eq(xtype, x, ytype, y)				\
	(__builtin_types_compatible_p (xtype, typeof (x))		\
	 && __builtin_types_compatible_p (ytype, typeof (y)))

#define __un_args_eq(xtype, x)						\
	__builtin_types_compatible_p (xtype, typeof (x))

#define __tern_args_eq(xtype, x, ytype, y, ztype, z)                    \
        (__builtin_types_compatible_p (xtype, typeof (x))               \
         && __builtin_types_compatible_p (ytype, typeof (y))		\
	 && __builtin_types_compatible_p (ztype, typeof (z)))

#define __ch(x, y, z)	__builtin_choose_expr (x, y, z)

/* These are easy... Same exact arguments.  */

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

#ifdef __cplusplus

/* Prototypes for builtins that take literals and must always be
   inlined.  */
inline vector float vec_ctf (vector unsigned int, const char) __attribute__ ((always_inline));
inline vector float vec_ctf (vector signed int, const char) __attribute__ ((always_inline));
inline vector float vec_vcfsx (vector signed int a1, const char a2) __attribute__ ((always_inline));
inline vector float vec_vcfux (vector unsigned int a1, const char a2) __attribute__ ((always_inline));
inline vector signed int vec_cts (vector float, const char) __attribute__ ((always_inline));
inline vector unsigned int vec_ctu (vector float, const char) __attribute__ ((always_inline));
inline void vec_dss (const char) __attribute__ ((always_inline));

inline void vec_dst (vector unsigned char *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (vector signed char *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (vector unsigned short *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (vector signed short *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (vector unsigned int *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (vector signed int *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (vector float *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (unsigned char *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (signed char *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (unsigned short *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (signed short *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (unsigned int *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (signed int *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (unsigned long *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (signed long *, int, const char) __attribute__ ((always_inline));
inline void vec_dst (float *, int, const char) __attribute__ ((always_inline));

inline void vec_dstst (vector unsigned char *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (vector signed char *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (vector unsigned short *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (vector signed short *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (vector unsigned int *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (vector signed int *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (vector float *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (unsigned char *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (signed char *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (unsigned short *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (signed short *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (unsigned int *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (signed int *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (unsigned long *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (signed long *, int, const char) __attribute__ ((always_inline));
inline void vec_dstst (float *, int, const char) __attribute__ ((always_inline));

inline void vec_dststt (vector unsigned char *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (vector signed char *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (vector unsigned short *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (vector signed short *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (vector unsigned int *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (vector signed int *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (vector float *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (unsigned char *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (signed char *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (unsigned short *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (signed short *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (unsigned int *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (signed int *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (unsigned long *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (signed long *, int, const char) __attribute__ ((always_inline));
inline void vec_dststt (float *, int, const char) __attribute__ ((always_inline));

inline void vec_dstt (vector unsigned char *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (vector signed char *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (vector unsigned short *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (vector signed short *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (vector unsigned int *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (vector signed int *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (vector float *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (unsigned char *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (signed char *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (unsigned short *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (signed short *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (unsigned int *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (signed int *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (unsigned long *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (signed long *, int, const char) __attribute__ ((always_inline));
inline void vec_dstt (float *, int, const char) __attribute__ ((always_inline));

inline vector float vec_sld (vector float, vector float, const char) __attribute__ ((always_inline));
inline vector signed int vec_sld (vector signed int, vector signed int, const char) __attribute__ ((always_inline));
inline vector unsigned int vec_sld (vector unsigned int, vector unsigned int, const char) __attribute__ ((always_inline));
inline vector signed short vec_sld (vector signed short, vector signed short, const char) __attribute__ ((always_inline));
inline vector unsigned short vec_sld (vector unsigned short, vector unsigned short, const char) __attribute__ ((always_inline));
inline vector signed char vec_sld (vector signed char, vector signed char, const char) __attribute__ ((always_inline));
inline vector unsigned char vec_sld (vector unsigned char, vector unsigned char, const char) __attribute__ ((always_inline));
inline vector signed char vec_splat (vector signed char, const char) __attribute__ ((always_inline));
inline vector unsigned char vec_splat (vector unsigned char, const char) __attribute__ ((always_inline));
inline vector signed short vec_splat (vector signed short, const char) __attribute__ ((always_inline));
inline vector unsigned short vec_splat (vector unsigned short, const char) __attribute__ ((always_inline));
inline vector float vec_splat (vector float, const char) __attribute__ ((always_inline));
inline vector signed int vec_splat (vector signed int, const char) __attribute__ ((always_inline));
inline vector unsigned int vec_splat (vector unsigned int, const char) __attribute__ ((always_inline));
inline vector signed char vec_splat_s8 (const char) __attribute__ ((always_inline));
inline vector signed short vec_splat_s16 (const char) __attribute__ ((always_inline));
inline vector signed int vec_splat_s32 (const char) __attribute__ ((always_inline));
inline vector unsigned char vec_splat_u8 (const char) __attribute__ ((always_inline));
inline vector unsigned short vec_splat_u16 (const char) __attribute__ ((always_inline));
inline vector unsigned int vec_splat_u32 (const char) __attribute__ ((always_inline));
inline vector float vec_vspltw (vector float a1, const char a2) __attribute__ ((always_inline));
inline vector signed int vec_vspltw (vector signed int a1, const char a2) __attribute__ ((always_inline));
inline vector unsigned int vec_vspltw (vector unsigned int a1, const char a2) __attribute__ ((always_inline));
inline vector signed short vec_vsplth (vector signed short a1, const char a2) __attribute__ ((always_inline));
inline vector unsigned short vec_vsplth (vector unsigned short a1, const char a2) __attribute__ ((always_inline));
inline vector signed char vec_vspltb (vector signed char a1, const char a2) __attribute__ ((always_inline));
inline vector unsigned char vec_vspltb (vector unsigned char a1, const char a2) __attribute__ ((always_inline));

/* vec_abs */

inline vector signed char
vec_abs (vector signed char a1)
{
  return __builtin_altivec_abs_v16qi (a1);
}

inline vector signed short
vec_abs (vector signed short a1)
{
  return __builtin_altivec_abs_v8hi (a1);
}

inline vector signed int
vec_abs (vector signed int a1)
{
  return __builtin_altivec_abs_v4si (a1);
}

inline vector float
vec_abs (vector float a1)
{
  return __builtin_altivec_abs_v4sf (a1);
}

/* vec_abss */

inline vector signed char
vec_abss (vector signed char a1)
{
  return __builtin_altivec_abss_v16qi (a1);
}

inline vector signed short
vec_abss (vector signed short a1)
{
  return __builtin_altivec_abss_v8hi (a1);
}

inline vector signed int
vec_abss (vector signed int a1)
{
  return __builtin_altivec_abss_v4si (a1);
}

/* vec_add */

inline vector signed char
vec_add (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_add (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_add (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_add (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_add (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_add (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_add (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_add (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_add (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_add (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_add (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_add (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_add (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vaddfp ((vector float) a1, (vector float) a2);
}

/* vec_vaddfp */

inline vector float
vec_vaddfp (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vaddfp ((vector float) a1, (vector float) a2);
}

/* vec_vadduwm */

inline vector signed int
vec_vadduwm (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vadduwm (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vadduwm (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vadduwm (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vadduhm */

inline vector signed short
vec_vadduhm (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vadduhm (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vadduhm (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vadduhm (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vaddubm */

inline vector signed char
vec_vaddubm (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vaddubm (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vaddubm (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vaddubm (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2);
}

/* vec_addc */

inline vector unsigned int
vec_addc (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vaddcuw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_adds */

inline vector unsigned char
vec_adds (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_adds (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_adds (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed char
vec_adds (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vaddsbs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned short
vec_adds (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_adds (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_adds (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed short
vec_adds (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vaddshs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned int
vec_adds (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduws ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_adds (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduws ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_adds (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduws ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_adds (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vaddsws ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vaddsws */

inline vector signed int
vec_vaddsws (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vaddsws ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vadduws */

inline vector unsigned int
vec_vadduws (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduws ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vadduws (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduws ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vadduws (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vadduws ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vaddshs */
inline vector signed short
vec_vaddshs (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vaddshs ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vadduhs */

inline vector unsigned short
vec_vadduhs (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vadduhs (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vadduhs (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vaddsbs */

inline vector signed char
vec_vaddsbs (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vaddsbs ((vector signed char) a1, (vector signed char) a2);
}

/* vec_vaddubs */

inline vector unsigned char
vec_vaddubs (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vaddubs (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vaddubs (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) a1, (vector signed char) a2);
}

/* vec_and */

inline vector float
vec_and (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_and (vector float a1, vector signed int a2)
{
  return (vector float) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_and (vector signed int a1, vector float a2)
{
  return (vector float) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_and (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_and (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_and (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_and (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_and (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_and (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_and (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_and (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_and (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_and (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_and (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_and (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2);
}

/* vec_andc */

inline vector float
vec_andc (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_andc (vector float a1, vector signed int a2)
{
  return (vector float) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_andc (vector signed int a1, vector float a2)
{
  return (vector float) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_andc (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_andc (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_andc (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_andc (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_andc (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_andc (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_andc (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_andc (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_andc (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_andc (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_andc (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_andc (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2);
}

/* vec_avg */

inline vector unsigned char
vec_avg (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vavgub ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed char
vec_avg (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vavgsb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned short
vec_avg (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vavguh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed short
vec_avg (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vavgsh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned int
vec_avg (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vavguw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_avg (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vavgsw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vavgsw */

inline vector signed int
vec_vavgsw (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vavgsw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vavguw */

inline vector unsigned int
vec_vavguw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vavguw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vavgsh */

inline vector signed short
vec_vavgsh (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vavgsh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vavguh */

inline vector unsigned short
vec_vavguh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vavguh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vavgsb */

inline vector signed char
vec_vavgsb (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vavgsb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_vavgub */

inline vector unsigned char
vec_vavgub (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vavgub ((vector signed char) a1, (vector signed char) a2);
}

/* vec_ceil */

inline vector float
vec_ceil (vector float a1)
{
  return (vector float) __builtin_altivec_vrfip ((vector float) a1);
}

/* vec_cmpb */

inline vector signed int
vec_cmpb (vector float a1, vector float a2)
{
  return (vector signed int) __builtin_altivec_vcmpbfp ((vector float) a1, (vector float) a2);
}

/* vec_cmpeq */

inline vector signed char
vec_cmpeq (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vcmpequb ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed char
vec_cmpeq (vector unsigned char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vcmpequb ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_cmpeq (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vcmpequh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed short
vec_cmpeq (vector unsigned short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vcmpequh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_cmpeq (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vcmpequw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_cmpeq (vector unsigned int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vcmpequw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_cmpeq (vector float a1, vector float a2)
{
  return (vector signed int) __builtin_altivec_vcmpeqfp ((vector float) a1, (vector float) a2);
}

/* vec_vcmpeqfp */

inline vector signed int
vec_vcmpeqfp (vector float a1, vector float a2)
{
  return (vector signed int) __builtin_altivec_vcmpeqfp ((vector float) a1, (vector float) a2);
}

/* vec_vcmpequw */

inline vector signed int
vec_vcmpequw (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vcmpequw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_vcmpequw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vcmpequw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vcmpequh */

inline vector signed short
vec_vcmpequh (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vcmpequh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed short
vec_vcmpequh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vcmpequh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vcmpequb */

inline vector signed char
vec_vcmpequb (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vcmpequb ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed char
vec_vcmpequb (vector unsigned char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vcmpequb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_cmpge */

inline vector signed int
vec_cmpge (vector float a1, vector float a2)
{
  return (vector signed int) __builtin_altivec_vcmpgefp ((vector float) a1, (vector float) a2);
}

/* vec_cmpgt */

inline vector signed char
vec_cmpgt (vector unsigned char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vcmpgtub ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed char
vec_cmpgt (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vcmpgtsb ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_cmpgt (vector unsigned short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vcmpgtuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed short
vec_cmpgt (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vcmpgtsh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_cmpgt (vector unsigned int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vcmpgtuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_cmpgt (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vcmpgtsw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_cmpgt (vector float a1, vector float a2)
{
  return (vector signed int) __builtin_altivec_vcmpgtfp ((vector float) a1, (vector float) a2);
}

/* vec_vcmpgtfp */

inline vector signed int
vec_vcmpgtfp (vector float a1, vector float a2)
{
  return (vector signed int) __builtin_altivec_vcmpgtfp ((vector float) a1, (vector float) a2);
}

/* vec_vcmpgtsw */

inline vector signed int
vec_vcmpgtsw (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vcmpgtsw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vcmpgtuw */

inline vector signed int
vec_vcmpgtuw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vcmpgtuw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vcmpgtsh */

inline vector signed short
vec_cmpgtsh (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vcmpgtsh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vcmpgtuh */

inline vector signed short
vec_vcmpgtuh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vcmpgtuh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vcmpgtsb */

inline vector signed char
vec_vcmpgtsb (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vcmpgtsb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_vcmpgtub */

inline vector signed char
vec_vcmpgtub (vector unsigned char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vcmpgtub ((vector signed char) a1, (vector signed char) a2);
}

/* vec_cmple */

inline vector signed int
vec_cmple (vector float a1, vector float a2)
{
  return (vector signed int) __builtin_altivec_vcmpgefp ((vector float) a1, (vector float) a2);
}

/* vec_cmplt */

inline vector signed char
vec_cmplt (vector unsigned char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vcmpgtub ((vector signed char) a2, (vector signed char) a1);
}

inline vector signed char
vec_cmplt (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vcmpgtsb ((vector signed char) a2, (vector signed char) a1);
}

inline vector signed short
vec_cmplt (vector unsigned short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vcmpgtuh ((vector signed short) a2, (vector signed short) a1);
}

inline vector signed short
vec_cmplt (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vcmpgtsh ((vector signed short) a2, (vector signed short) a1);
}

inline vector signed int
vec_cmplt (vector unsigned int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vcmpgtuw ((vector signed int) a2, (vector signed int) a1);
}

inline vector signed int
vec_cmplt (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vcmpgtsw ((vector signed int) a2, (vector signed int) a1);
}

inline vector signed int
vec_cmplt (vector float a1, vector float a2)
{
  return (vector signed int) __builtin_altivec_vcmpgtfp ((vector float) a2, (vector float) a1);
}

/* vec_ctf */

inline vector float
vec_ctf (vector unsigned int a1, const char a2)
{
  return (vector float) __builtin_altivec_vcfux ((vector signed int) a1, a2);
}

inline vector float
vec_ctf (vector signed int a1, const char a2)
{
  return (vector float) __builtin_altivec_vcfsx ((vector signed int) a1, a2);
}

/* vec_vcfsx */

inline vector float
vec_vcfsx (vector signed int a1, const char a2)
{
  return (vector float) __builtin_altivec_vcfsx ((vector signed int) a1, a2);
}

/* vec_vcfux */

inline vector float
vec_vcfux (vector unsigned int a1, const char a2)
{
  return (vector float) __builtin_altivec_vcfux ((vector signed int) a1, a2);
}

/* vec_cts */

inline vector signed int
vec_cts (vector float a1, const char a2)
{
  return (vector signed int) __builtin_altivec_vctsxs ((vector float) a1, a2);
}

/* vec_ctu */

inline vector unsigned int
vec_ctu (vector float a1, const char a2)
{
  return (vector unsigned int) __builtin_altivec_vctuxs ((vector float) a1, a2);
}

/* vec_dss */

inline void
vec_dss (const char a1)
{
  __builtin_altivec_dss (a1);
}

/* vec_dssall */

inline void
vec_dssall ()
{
  __builtin_altivec_dssall ();
}

/* vec_dst */

inline void
vec_dst (vector unsigned char *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (vector signed char *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (vector unsigned short *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (vector signed short *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (vector unsigned int *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (vector signed int *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (vector float *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (unsigned char *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (signed char *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (unsigned short *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (signed short *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (unsigned int *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (signed int *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (unsigned long *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (signed long *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

inline void
vec_dst (float *a1, int a2, const char a3)
{
  __builtin_altivec_dst ((void *) a1, a2, a3);
}

/* vec_dstst */

inline void
vec_dstst (vector unsigned char *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (vector signed char *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (vector unsigned short *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (vector signed short *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (vector unsigned int *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (vector signed int *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (vector float *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (unsigned char *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (signed char *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (unsigned short *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (signed short *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (unsigned int *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (signed int *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (unsigned long *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (signed long *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

inline void
vec_dstst (float *a1, int a2, const char a3)
{
  __builtin_altivec_dstst ((void *) a1, a2, a3);
}

/* vec_dststt */

inline void
vec_dststt (vector unsigned char *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (vector signed char *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (vector unsigned short *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (vector signed short *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (vector unsigned int *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (vector signed int *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (vector float *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (unsigned char *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (signed char *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (unsigned short *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (signed short *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (unsigned int *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (signed int *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (unsigned long *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (signed long *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

inline void
vec_dststt (float *a1, int a2, const char a3)
{
  __builtin_altivec_dststt ((void *) a1, a2, a3);
}

/* vec_dstt */

inline void
vec_dstt (vector unsigned char *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (vector signed char *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (vector unsigned short *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (vector signed short *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (vector unsigned int *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (vector signed int *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (vector float *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (unsigned char *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (signed char *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (unsigned short *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (signed short *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (unsigned int *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (signed int *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (unsigned long *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (signed long *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

inline void
vec_dstt (float *a1, int a2, const char a3)
{
  __builtin_altivec_dstt ((void *) a1, a2, a3);
}

/* vec_expte */

inline vector float
vec_expte (vector float a1)
{
  return (vector float) __builtin_altivec_vexptefp ((vector float) a1);
}

/* vec_floor */

inline vector float
vec_floor (vector float a1)
{
  return (vector float) __builtin_altivec_vrfim ((vector float) a1);
}

/* vec_ld */

inline vector float
vec_ld (int a1, vector float *a2)
{
  return (vector float) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector float
vec_ld (int a1, float *a2)
{
  return (vector float) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector signed int
vec_ld (int a1, vector signed int *a2)
{
  return (vector signed int) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector signed int
vec_ld (int a1, signed int *a2)
{
  return (vector signed int) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector signed int
vec_ld (int a1, signed long *a2)
{
  return (vector signed int) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector unsigned int
vec_ld (int a1, vector unsigned int *a2)
{
  return (vector unsigned int) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector unsigned int
vec_ld (int a1, unsigned int *a2)
{
  return (vector unsigned int) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector unsigned int
vec_ld (int a1, unsigned long *a2)
{
  return (vector unsigned int) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector signed short
vec_ld (int a1, vector signed short *a2)
{
  return (vector signed short) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector signed short
vec_ld (int a1, signed short *a2)
{
  return (vector signed short) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector unsigned short
vec_ld (int a1, vector unsigned short *a2)
{
  return (vector unsigned short) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector unsigned short
vec_ld (int a1, unsigned short *a2)
{
  return (vector unsigned short) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector signed char
vec_ld (int a1, vector signed char *a2)
{
  return (vector signed char) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector signed char
vec_ld (int a1, signed char *a2)
{
  return (vector signed char) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector unsigned char
vec_ld (int a1, vector unsigned char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvx (a1, (void *) a2);
}

inline vector unsigned char
vec_ld (int a1, unsigned char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvx (a1, (void *) a2);
}

/* vec_lde */

inline vector signed char
vec_lde (int a1, signed char *a2)
{
  return (vector signed char) __builtin_altivec_lvebx (a1, (void *) a2);
}

inline vector unsigned char
vec_lde (int a1, unsigned char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvebx (a1, (void *) a2);
}

inline vector signed short
vec_lde (int a1, signed short *a2)
{
  return (vector signed short) __builtin_altivec_lvehx (a1, (void *) a2);
}

inline vector unsigned short
vec_lde (int a1, unsigned short *a2)
{
  return (vector unsigned short) __builtin_altivec_lvehx (a1, (void *) a2);
}

inline vector float
vec_lde (int a1, float *a2)
{
  return (vector float) __builtin_altivec_lvewx (a1, (void *) a2);
}

inline vector signed int
vec_lde (int a1, signed int *a2)
{
  return (vector signed int) __builtin_altivec_lvewx (a1, (void *) a2);
}

inline vector unsigned int
vec_lde (int a1, unsigned int *a2)
{
  return (vector unsigned int) __builtin_altivec_lvewx (a1, (void *) a2);
}

inline vector signed int
vec_lde (int a1, signed long *a2)
{
  return (vector signed int) __builtin_altivec_lvewx (a1, (void *) a2);
}

inline vector unsigned int
vec_lde (int a1, unsigned long *a2)
{
  return (vector unsigned int) __builtin_altivec_lvewx (a1, (void *) a2);
}

/* vec_lvewx */

inline vector float
vec_lvewx (int a1, float *a2)
{
  return (vector float) __builtin_altivec_lvewx (a1, (void *) a2);
}

inline vector signed int
vec_lvewx (int a1, signed int *a2)
{
  return (vector signed int) __builtin_altivec_lvewx (a1, (void *) a2);
}

inline vector unsigned int
vec_lvewx (int a1, unsigned int *a2)
{
  return (vector unsigned int) __builtin_altivec_lvewx (a1, (void *) a2);
}

inline vector signed int
vec_lvewx (int a1, signed long *a2)
{
  return (vector signed int) __builtin_altivec_lvewx (a1, (void *) a2);
}

inline vector unsigned int
vec_lvewx (int a1, unsigned long *a2)
{
  return (vector unsigned int) __builtin_altivec_lvewx (a1, (void *) a2);
}

/* vec_lvehx */

inline vector signed short
vec_lvehx (int a1, signed short *a2)
{
  return (vector signed short) __builtin_altivec_lvehx (a1, (void *) a2);
}

inline vector unsigned short
vec_lvehx (int a1, unsigned short *a2)
{
  return (vector unsigned short) __builtin_altivec_lvehx (a1, (void *) a2);
}

/* vec_lvebx */

inline vector signed char
vec_lvebx (int a1, signed char *a2)
{
  return (vector signed char) __builtin_altivec_lvebx (a1, (void *) a2);
}

inline vector unsigned char
vec_lvebx (int a1, unsigned char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvebx (a1, (void *) a2);
}

/* vec_ldl */

inline vector float
vec_ldl (int a1, vector float *a2)
{
  return (vector float) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector float
vec_ldl (int a1, float *a2)
{
  return (vector float) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector signed int
vec_ldl (int a1, vector signed int *a2)
{
  return (vector signed int) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector signed int
vec_ldl (int a1, signed int *a2)
{
  return (vector signed int) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector signed int
vec_ldl (int a1, signed long *a2)
{
  return (vector signed int) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector unsigned int
vec_ldl (int a1, vector unsigned int *a2)
{
  return (vector unsigned int) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector unsigned int
vec_ldl (int a1, unsigned int *a2)
{
  return (vector unsigned int) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector unsigned int
vec_ldl (int a1, unsigned long *a2)
{
  return (vector unsigned int) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector signed short
vec_ldl (int a1, vector signed short *a2)
{
  return (vector signed short) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector signed short
vec_ldl (int a1, signed short *a2)
{
  return (vector signed short) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector unsigned short
vec_ldl (int a1, vector unsigned short *a2)
{
  return (vector unsigned short) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector unsigned short
vec_ldl (int a1, unsigned short *a2)
{
  return (vector unsigned short) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector signed char
vec_ldl (int a1, vector signed char *a2)
{
  return (vector signed char) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector signed char
vec_ldl (int a1, signed char *a2)
{
  return (vector signed char) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector unsigned char
vec_ldl (int a1, vector unsigned char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvxl (a1, (void *) a2);
}

inline vector unsigned char
vec_ldl (int a1, unsigned char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvxl (a1, (void *) a2);
}

/* vec_loge */

inline vector float
vec_loge (vector float a1)
{
  return (vector float) __builtin_altivec_vlogefp ((vector float) a1);
}

/* vec_lvsl */

inline vector unsigned char
vec_lvsl (int a1, unsigned char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsl (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsl (int a1, signed char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsl (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsl (int a1, unsigned short *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsl (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsl (int a1, signed short *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsl (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsl (int a1, unsigned int *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsl (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsl (int a1, signed int *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsl (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsl (int a1, unsigned long *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsl (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsl (int a1, signed long *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsl (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsl (int a1, float *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsl (a1, (void *) a2);
}

/* vec_lvsr */

inline vector unsigned char
vec_lvsr (int a1, unsigned char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsr (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsr (int a1, signed char *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsr (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsr (int a1, unsigned short *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsr (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsr (int a1, signed short *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsr (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsr (int a1, unsigned int *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsr (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsr (int a1, signed int *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsr (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsr (int a1, unsigned long *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsr (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsr (int a1, signed long *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsr (a1, (void *) a2);
}

inline vector unsigned char
vec_lvsr (int a1, float *a2)
{
  return (vector unsigned char) __builtin_altivec_lvsr (a1, (void *) a2);
}

/* vec_madd */

inline vector float
vec_madd (vector float a1, vector float a2, vector float a3)
{
  return (vector float) __builtin_altivec_vmaddfp ((vector float) a1, (vector float) a2, (vector float) a3);
}


/* vec_madds */

inline vector signed short
vec_madds (vector signed short a1, vector signed short a2, vector signed short a3)
{
  return (vector signed short) __builtin_altivec_vmhaddshs ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3);
}

/* vec_max */

inline vector unsigned char
vec_max (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_max (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_max (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed char
vec_max (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vmaxsb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned short
vec_max (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_max (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_max (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed short
vec_max (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vmaxsh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned int
vec_max (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_max (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_max (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_max (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vmaxsw ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_max (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vmaxfp ((vector float) a1, (vector float) a2);
}

/* vec_vmaxfp */

inline vector float
vec_vmaxfp (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vmaxfp ((vector float) a1, (vector float) a2);
}

/* vec_vmaxsw */

inline vector signed int
vec_vmaxsw (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vmaxsw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vmaxuw */

inline vector unsigned int
vec_vmaxuw (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vmaxuw (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vmaxuw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vmaxsh */

inline vector signed short
vec_vmaxsh (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vmaxsh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmaxuh */

inline vector unsigned short
vec_vmaxuh (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vmaxuh (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vmaxuh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmaxsb */

inline vector signed char
vec_vmaxsb (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vmaxsb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_vmaxub */

inline vector unsigned char
vec_vmaxub (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vmaxub (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vmaxub (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) a1, (vector signed char) a2);
}

/* vec_mergeh */

inline vector signed char
vec_mergeh (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vmrghb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_mergeh (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vmrghb ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_mergeh (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vmrghh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_mergeh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vmrghh ((vector signed short) a1, (vector signed short) a2);
}

inline vector float
vec_mergeh (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vmrghw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_mergeh (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vmrghw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_mergeh (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vmrghw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vmrghw */

inline vector float
vec_vmrghw (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vmrghw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_vmrghw (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vmrghw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vmrghw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vmrghw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vmrghh */

inline vector signed short
vec_vmrghh (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vmrghh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vmrghh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vmrghh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmrghb */

inline vector signed char
vec_vmrghb (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vmrghb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vmrghb (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vmrghb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_mergel */

inline vector signed char
vec_mergel (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vmrglb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_mergel (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vmrglb ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_mergel (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vmrglh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_mergel (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vmrglh ((vector signed short) a1, (vector signed short) a2);
}

inline vector float
vec_mergel (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vmrglw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_mergel (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vmrglw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_mergel (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vmrglw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vmrglw */

inline vector float
vec_vmrglw (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vmrglw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_vmrglw (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vmrglw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vmrglw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vmrglw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vmrglh */

inline vector signed short
vec_vmrglh (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vmrglh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vmrglh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vmrglh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmrglb */

inline vector signed char
vec_vmrglb (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vmrglb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vmrglb (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vmrglb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_mfvscr */

inline vector unsigned short
vec_mfvscr ()
{
  return (vector unsigned short) __builtin_altivec_mfvscr ();
}

/* vec_min */

inline vector unsigned char
vec_min (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vminub ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_min (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vminub ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_min (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vminub ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed char
vec_min (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vminsb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned short
vec_min (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vminuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_min (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vminuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_min (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vminuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed short
vec_min (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vminsh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned int
vec_min (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vminuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_min (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vminuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_min (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vminuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_min (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vminsw ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_min (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vminfp ((vector float) a1, (vector float) a2);
}

/* vec_vminfp */

inline vector float
vec_vminfp (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vminfp ((vector float) a1, (vector float) a2);
}

/* vec_vminsw */

inline vector signed int
vec_vminsw (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vminsw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vminuw */

inline vector unsigned int
vec_vminuw (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vminuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vminuw (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vminuw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vminuw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vminuw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vminsh */

inline vector signed short
vec_vminsh (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vminsh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vminuh */

inline vector unsigned short
vec_vminuh (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vminuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vminuh (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vminuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vminuh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vminuh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vminsb */

inline vector signed char
vec_vminsb (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vminsb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_vminub */

inline vector unsigned char
vec_vminub (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vminub ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vminub (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vminub ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vminub (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vminub ((vector signed char) a1, (vector signed char) a2);
}

/* vec_mladd */

inline vector signed short
vec_mladd (vector signed short a1, vector signed short a2, vector signed short a3)
{
  return (vector signed short) __builtin_altivec_vmladduhm ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3);
}

inline vector signed short
vec_mladd (vector signed short a1, vector unsigned short a2, vector unsigned short a3)
{
  return (vector signed short) __builtin_altivec_vmladduhm ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3);
}

inline vector signed short
vec_mladd (vector unsigned short a1, vector signed short a2, vector signed short a3)
{
  return (vector signed short) __builtin_altivec_vmladduhm ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3);
}

inline vector unsigned short
vec_mladd (vector unsigned short a1, vector unsigned short a2, vector unsigned short a3)
{
  return (vector unsigned short) __builtin_altivec_vmladduhm ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3);
}

/* vec_mradds */

inline vector signed short
vec_mradds (vector signed short a1, vector signed short a2, vector signed short a3)
{
  return (vector signed short) __builtin_altivec_vmhraddshs ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3);
}

/* vec_msum */

inline vector unsigned int
vec_msum (vector unsigned char a1, vector unsigned char a2, vector unsigned int a3)
{
  return (vector unsigned int) __builtin_altivec_vmsumubm ((vector signed char) a1, (vector signed char) a2, (vector signed int) a3);
}

inline vector signed int
vec_msum (vector signed char a1, vector unsigned char a2, vector signed int a3)
{
  return (vector signed int) __builtin_altivec_vmsummbm ((vector signed char) a1, (vector signed char) a2, (vector signed int) a3);
}

inline vector unsigned int
vec_msum (vector unsigned short a1, vector unsigned short a2, vector unsigned int a3)
{
  return (vector unsigned int) __builtin_altivec_vmsumuhm ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3);
}

inline vector signed int
vec_msum (vector signed short a1, vector signed short a2, vector signed int a3)
{
  return (vector signed int) __builtin_altivec_vmsumshm ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3);
}

/* vec_vmsumshm */

inline vector signed int
vec_vmsumshm (vector signed short a1, vector signed short a2, vector signed int a3)
{
  return (vector signed int) __builtin_altivec_vmsumshm ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3);
}

/* vec_vmsumuhm */

inline vector unsigned int
vec_vmsumuhm (vector unsigned short a1, vector unsigned short a2, vector unsigned int a3)
{
  return (vector unsigned int) __builtin_altivec_vmsumuhm ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3);
}

/* vec_vmsummbm */

inline vector signed int
vec_vmsummbm (vector signed char a1, vector unsigned char a2, vector signed int a3)
{
  return (vector signed int) __builtin_altivec_vmsummbm ((vector signed char) a1, (vector signed char) a2, (vector signed int) a3);
}

/* vec_vmsumubm */

inline vector unsigned int
vec_vmsumubm (vector unsigned char a1, vector unsigned char a2, vector unsigned int a3)
{
  return (vector unsigned int) __builtin_altivec_vmsumubm ((vector signed char) a1, (vector signed char) a2, (vector signed int) a3);
}

/* vec_msums */

inline vector unsigned int
vec_msums (vector unsigned short a1, vector unsigned short a2, vector unsigned int a3)
{
  return (vector unsigned int) __builtin_altivec_vmsumuhs ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3);
}

inline vector signed int
vec_msums (vector signed short a1, vector signed short a2, vector signed int a3)
{
  return (vector signed int) __builtin_altivec_vmsumshs ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3);
}

/* vec_vmsumshs */

inline vector signed int
vec_vmsumshs (vector signed short a1, vector signed short a2, vector signed int a3)
{
  return (vector signed int) __builtin_altivec_vmsumshs ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3);
}

/* vec_vmsumuhs */

inline vector unsigned int
vec_vmsumuhs (vector unsigned short a1, vector unsigned short a2, vector unsigned int a3)
{
  return (vector unsigned int) __builtin_altivec_vmsumuhs ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3);
}

/* vec_mtvscr */

inline void
vec_mtvscr (vector signed int a1)
{
  __builtin_altivec_mtvscr ((vector signed int) a1);
}

inline void
vec_mtvscr (vector unsigned int a1)
{
  __builtin_altivec_mtvscr ((vector signed int) a1);
}

inline void
vec_mtvscr (vector signed short a1)
{
  __builtin_altivec_mtvscr ((vector signed int) a1);
}

inline void
vec_mtvscr (vector unsigned short a1)
{
  __builtin_altivec_mtvscr ((vector signed int) a1);
}

inline void
vec_mtvscr (vector signed char a1)
{
  __builtin_altivec_mtvscr ((vector signed int) a1);
}

inline void
vec_mtvscr (vector unsigned char a1)
{
  __builtin_altivec_mtvscr ((vector signed int) a1);
}

/* vec_mule */

inline vector unsigned short
vec_mule (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned short) __builtin_altivec_vmuleub ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_mule (vector signed char a1, vector signed char a2)
{
  return (vector signed short) __builtin_altivec_vmulesb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned int
vec_mule (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned int) __builtin_altivec_vmuleuh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_mule (vector signed short a1, vector signed short a2)
{
  return (vector signed int) __builtin_altivec_vmulesh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmulesh */

inline vector signed int
vec_vmulesh (vector signed short a1, vector signed short a2)
{
  return (vector signed int) __builtin_altivec_vmulesh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmuleuh */

inline vector unsigned int
vec_vmuleuh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned int) __builtin_altivec_vmuleuh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmuleub */
inline vector unsigned short
vec_vmuleub (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned short) __builtin_altivec_vmuleub ((vector signed char) a1, (vector signed char) a2);
}

/* vec_mulo */

inline vector unsigned short
vec_mulo (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned short) __builtin_altivec_vmuloub ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_mulo (vector signed char a1, vector signed char a2)
{
  return (vector signed short) __builtin_altivec_vmulosb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned int
vec_mulo (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned int) __builtin_altivec_vmulouh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_mulo (vector signed short a1, vector signed short a2)
{
  return (vector signed int) __builtin_altivec_vmulosh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmulosh */

inline vector signed int
vec_vmulosh (vector signed short a1, vector signed short a2)
{
  return (vector signed int) __builtin_altivec_vmulosh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmulouh */

inline vector unsigned int
vec_vmulouh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned int) __builtin_altivec_vmulouh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vmulosb */

inline vector signed short
vec_vmulosb (vector signed char a1, vector signed char a2)
{
  return (vector signed short) __builtin_altivec_vmulosb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_vmuloub */

inline vector unsigned short
vec_vmuloub (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned short) __builtin_altivec_vmuloub ((vector signed char) a1, (vector signed char) a2);
}

/* vec_nmsub */

inline vector float
vec_nmsub (vector float a1, vector float a2, vector float a3)
{
  return (vector float) __builtin_altivec_vnmsubfp ((vector float) a1, (vector float) a2, (vector float) a3);
}

/* vec_nor */

inline vector float
vec_nor (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_nor (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_nor (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_nor (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_nor (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_nor (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_nor (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2);
}

/* vec_or */

inline vector float
vec_or (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_or (vector float a1, vector signed int a2)
{
  return (vector float) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_or (vector signed int a1, vector float a2)
{
  return (vector float) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_or (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_or (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_or (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_or (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_or (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_or (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_or (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_or (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_or (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_or (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_or (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_or (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2);
}

/* vec_pack */

inline vector signed char
vec_pack (vector signed short a1, vector signed short a2)
{
  return (vector signed char) __builtin_altivec_vpkuhum ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned char
vec_pack (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned char) __builtin_altivec_vpkuhum ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed short
vec_pack (vector signed int a1, vector signed int a2)
{
  return (vector signed short) __builtin_altivec_vpkuwum ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_pack (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned short) __builtin_altivec_vpkuwum ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vpkuwum */

inline vector signed short
vec_vpkuwum (vector signed int a1, vector signed int a2)
{
  return (vector signed short) __builtin_altivec_vpkuwum ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_vpkuwum (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned short) __builtin_altivec_vpkuwum ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vpkuhum */

inline vector signed char
vec_vpkuhum (vector signed short a1, vector signed short a2)
{
  return (vector signed char) __builtin_altivec_vpkuhum ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned char
vec_vpkuhum (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned char) __builtin_altivec_vpkuhum ((vector signed short) a1, (vector signed short) a2);
}

/* vec_packpx */

inline vector unsigned short
vec_packpx (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned short) __builtin_altivec_vpkpx ((vector signed int) a1, (vector signed int) a2);
}

/* vec_packs */

inline vector unsigned char
vec_packs (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned char) __builtin_altivec_vpkuhus ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed char
vec_packs (vector signed short a1, vector signed short a2)
{
  return (vector signed char) __builtin_altivec_vpkshss ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_packs (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned short) __builtin_altivec_vpkuwus ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_packs (vector signed int a1, vector signed int a2)
{
  return (vector signed short) __builtin_altivec_vpkswss ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vpkswss */

inline vector signed short
vec_vpkswss (vector signed int a1, vector signed int a2)
{
  return (vector signed short) __builtin_altivec_vpkswss ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vpkuwus */

inline vector unsigned short
vec_vpkuwus (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned short) __builtin_altivec_vpkuwus ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vpkshss */

inline vector signed char
vec_vpkshss (vector signed short a1, vector signed short a2)
{
  return (vector signed char) __builtin_altivec_vpkshss ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vpkuhus */

inline vector unsigned char
vec_vpkuhus (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned char) __builtin_altivec_vpkuhus ((vector signed short) a1, (vector signed short) a2);
}

/* vec_packsu */

inline vector unsigned char
vec_packsu (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned char) __builtin_altivec_vpkuhus ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned char
vec_packsu (vector signed short a1, vector signed short a2)
{
  return (vector unsigned char) __builtin_altivec_vpkshus ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_packsu (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned short) __builtin_altivec_vpkuwus ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_packsu (vector signed int a1, vector signed int a2)
{
  return (vector unsigned short) __builtin_altivec_vpkswus ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vpkswus */

inline vector unsigned short
vec_vpkswus (vector signed int a1, vector signed int a2)
{
  return (vector unsigned short) __builtin_altivec_vpkswus ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vpkshus */

inline vector unsigned char
vec_vpkshus (vector signed short a1, vector signed short a2)
{
  return (vector unsigned char) __builtin_altivec_vpkshus ((vector signed short) a1, (vector signed short) a2);
}

/* vec_perm */

inline vector float
vec_perm (vector float a1, vector float a2, vector unsigned char a3)
{
  return (vector float) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3);
}

inline vector signed int
vec_perm (vector signed int a1, vector signed int a2, vector unsigned char a3)
{
  return (vector signed int) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3);
}

inline vector unsigned int
vec_perm (vector unsigned int a1, vector unsigned int a2, vector unsigned char a3)
{
  return (vector unsigned int) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3);
}

inline vector signed short
vec_perm (vector signed short a1, vector signed short a2, vector unsigned char a3)
{
  return (vector signed short) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3);
}

inline vector unsigned short
vec_perm (vector unsigned short a1, vector unsigned short a2, vector unsigned char a3)
{
  return (vector unsigned short) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3);
}

inline vector signed char
vec_perm (vector signed char a1, vector signed char a2, vector unsigned char a3)
{
  return (vector signed char) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3);
}

inline vector unsigned char
vec_perm (vector unsigned char a1, vector unsigned char a2, vector unsigned char a3)
{
  return (vector unsigned char) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3);
}

/* vec_re */

inline vector float
vec_re (vector float a1)
{
  return (vector float) __builtin_altivec_vrefp ((vector float) a1);
}

/* vec_rl */

inline vector signed char
vec_rl (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vrlb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_rl (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vrlb ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_rl (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vrlh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_rl (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vrlh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_rl (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vrlw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_rl (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vrlw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vrlw */

inline vector signed int
vec_vrlw (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vrlw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vrlw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vrlw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vrlh */

inline vector signed short
vec_vrlh (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vrlh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vrlh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vrlh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vrlb */

inline vector signed char
vec_vrlb (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vrlb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vrlb (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vrlb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_round */

inline vector float
vec_round (vector float a1)
{
  return (vector float) __builtin_altivec_vrfin ((vector float) a1);
}

/* vec_rsqrte */

inline vector float
vec_rsqrte (vector float a1)
{
  return (vector float) __builtin_altivec_vrsqrtefp ((vector float) a1);
}

/* vec_sel */

inline vector float
vec_sel (vector float a1, vector float a2, vector signed int a3)
{
  return (vector float) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector float
vec_sel (vector float a1, vector float a2, vector unsigned int a3)
{
  return (vector float) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector signed int
vec_sel (vector signed int a1, vector signed int a2, vector signed int a3)
{
  return (vector signed int) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector signed int
vec_sel (vector signed int a1, vector signed int a2, vector unsigned int a3)
{
  return (vector signed int) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector unsigned int
vec_sel (vector unsigned int a1, vector unsigned int a2, vector signed int a3)
{
  return (vector unsigned int) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector unsigned int
vec_sel (vector unsigned int a1, vector unsigned int a2, vector unsigned int a3)
{
  return (vector unsigned int) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector signed short
vec_sel (vector signed short a1, vector signed short a2, vector signed short a3)
{
  return (vector signed short) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector signed short
vec_sel (vector signed short a1, vector signed short a2, vector unsigned short a3)
{
  return (vector signed short) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector unsigned short
vec_sel (vector unsigned short a1, vector unsigned short a2, vector signed short a3)
{
  return (vector unsigned short) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector unsigned short
vec_sel (vector unsigned short a1, vector unsigned short a2, vector unsigned short a3)
{
  return (vector unsigned short) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector signed char
vec_sel (vector signed char a1, vector signed char a2, vector signed char a3)
{
  return (vector signed char) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector signed char
vec_sel (vector signed char a1, vector signed char a2, vector unsigned char a3)
{
  return (vector signed char) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector unsigned char
vec_sel (vector unsigned char a1, vector unsigned char a2, vector signed char a3)
{
  return (vector unsigned char) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

inline vector unsigned char
vec_sel (vector unsigned char a1, vector unsigned char a2, vector unsigned char a3)
{
  return (vector unsigned char) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3);
}

/* vec_sl */

inline vector signed char
vec_sl (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vslb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_sl (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vslb ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_sl (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vslh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_sl (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vslh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_sl (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vslw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sl (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vslw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vslw */

inline vector signed int
vec_vslw (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vslw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vslw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vslw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vslh */

inline vector signed short
vec_vslh (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vslh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vslh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vslh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vslb */

inline vector signed char
vec_vslb (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vslb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vslb (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vslb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_sld */

inline vector float
vec_sld (vector float a1, vector float a2, const char a3)
{
  return (vector float) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, a3);
}

inline vector signed int
vec_sld (vector signed int a1, vector signed int a2, const char a3)
{
  return (vector signed int) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, a3);
}

inline vector unsigned int
vec_sld (vector unsigned int a1, vector unsigned int a2, const char a3)
{
  return (vector unsigned int) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, a3);
}

inline vector signed short
vec_sld (vector signed short a1, vector signed short a2, const char a3)
{
  return (vector signed short) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, a3);
}

inline vector unsigned short
vec_sld (vector unsigned short a1, vector unsigned short a2, const char a3)
{
  return (vector unsigned short) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, a3);
}

inline vector signed char
vec_sld (vector signed char a1, vector signed char a2, const char a3)
{
  return (vector signed char) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, a3);
}

inline vector unsigned char
vec_sld (vector unsigned char a1, vector unsigned char a2, const char a3)
{
  return (vector unsigned char) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, a3);
}

/* vec_sll */

inline vector signed int
vec_sll (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_sll (vector signed int a1, vector unsigned short a2)
{
  return (vector signed int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_sll (vector signed int a1, vector unsigned char a2)
{
  return (vector signed int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sll (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sll (vector unsigned int a1, vector unsigned short a2)
{
  return (vector unsigned int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sll (vector unsigned int a1, vector unsigned char a2)
{
  return (vector unsigned int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_sll (vector signed short a1, vector unsigned int a2)
{
  return (vector signed short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_sll (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_sll (vector signed short a1, vector unsigned char a2)
{
  return (vector signed short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_sll (vector unsigned short a1, vector unsigned int a2)
{
  return (vector unsigned short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_sll (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_sll (vector unsigned short a1, vector unsigned char a2)
{
  return (vector unsigned short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_sll (vector signed char a1, vector unsigned int a2)
{
  return (vector signed char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_sll (vector signed char a1, vector unsigned short a2)
{
  return (vector signed char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_sll (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_sll (vector unsigned char a1, vector unsigned int a2)
{
  return (vector unsigned char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_sll (vector unsigned char a1, vector unsigned short a2)
{
  return (vector unsigned char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_sll (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2);
}

/* vec_slo */

inline vector float
vec_slo (vector float a1, vector signed char a2)
{
  return (vector float) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_slo (vector float a1, vector unsigned char a2)
{
  return (vector float) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_slo (vector signed int a1, vector signed char a2)
{
  return (vector signed int) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_slo (vector signed int a1, vector unsigned char a2)
{
  return (vector signed int) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_slo (vector unsigned int a1, vector signed char a2)
{
  return (vector unsigned int) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_slo (vector unsigned int a1, vector unsigned char a2)
{
  return (vector unsigned int) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_slo (vector signed short a1, vector signed char a2)
{
  return (vector signed short) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_slo (vector signed short a1, vector unsigned char a2)
{
  return (vector signed short) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_slo (vector unsigned short a1, vector signed char a2)
{
  return (vector unsigned short) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_slo (vector unsigned short a1, vector unsigned char a2)
{
  return (vector unsigned short) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_slo (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_slo (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_slo (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_slo (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2);
}

/* vec_splat */

inline vector signed char
vec_splat (vector signed char a1, const char a2)
{
  return (vector signed char) __builtin_altivec_vspltb ((vector signed char) a1,  a2);
}

inline vector unsigned char
vec_splat (vector unsigned char a1, const char a2)
{
  return (vector unsigned char) __builtin_altivec_vspltb ((vector signed char) a1,  a2);
}

inline vector signed short
vec_splat (vector signed short a1, const char a2)
{
  return (vector signed short) __builtin_altivec_vsplth ((vector signed short) a1,  a2);
}

inline vector unsigned short
vec_splat (vector unsigned short a1, const char a2)
{
  return (vector unsigned short) __builtin_altivec_vsplth ((vector signed short) a1,  a2);
}

inline vector float
vec_splat (vector float a1, const char a2)
{
  return (vector float) __builtin_altivec_vspltw ((vector signed int) a1,  a2);
}

inline vector signed int
vec_splat (vector signed int a1, const char a2)
{
  return (vector signed int) __builtin_altivec_vspltw ((vector signed int) a1,  a2);
}

inline vector unsigned int
vec_splat (vector unsigned int a1, const char a2)
{
  return (vector unsigned int) __builtin_altivec_vspltw ((vector signed int) a1,  a2);
}

/* vec_vspltw */

inline vector float
vec_vspltw (vector float a1, const char a2)
{
  return (vector float) __builtin_altivec_vspltw ((vector signed int) a1,  a2);
}

inline vector signed int
vec_vspltw (vector signed int a1, const char a2)
{
  return (vector signed int) __builtin_altivec_vspltw ((vector signed int) a1,  a2);
}

inline vector unsigned int
vec_vspltw (vector unsigned int a1, const char a2)
{
  return (vector unsigned int) __builtin_altivec_vspltw ((vector signed int) a1,  a2);
}

/* vec_vsplth */

inline vector signed short
vec_vsplth (vector signed short a1, const char a2)
{
  return (vector signed short) __builtin_altivec_vsplth ((vector signed short) a1,  a2);
}

inline vector unsigned short
vec_vsplth (vector unsigned short a1, const char a2)
{
  return (vector unsigned short) __builtin_altivec_vsplth ((vector signed short) a1,  a2);
}

/* vec_vspltb */

inline vector signed char
vec_vspltb (vector signed char a1, const char a2)
{
  return (vector signed char) __builtin_altivec_vspltb ((vector signed char) a1,  a2);
}

inline vector unsigned char
vec_vspltb (vector unsigned char a1, const char a2)
{
  return (vector unsigned char) __builtin_altivec_vspltb ((vector signed char) a1,  a2);
}

/* vec_splat_s8 */

inline vector signed char
vec_splat_s8 (const char a1)
{
  return (vector signed char) __builtin_altivec_vspltisb (a1);
}

/* vec_splat_s16 */

inline vector signed short
vec_splat_s16 (const char a1)
{
  return (vector signed short) __builtin_altivec_vspltish (a1);
}

/* vec_splat_s32 */

inline vector signed int
vec_splat_s32 (const char a1)
{
  return (vector signed int) __builtin_altivec_vspltisw (a1);
}

/* vec_splat_u8 */

inline vector unsigned char
vec_splat_u8 (const char a1)
{
  return (vector unsigned char) __builtin_altivec_vspltisb (a1);
}

/* vec_splat_u16 */

inline vector unsigned short
vec_splat_u16 (const char a1)
{
  return (vector unsigned short) __builtin_altivec_vspltish (a1);
}

/* vec_splat_u32 */

inline vector unsigned int
vec_splat_u32 (const char a1)
{
  return (vector unsigned int) __builtin_altivec_vspltisw (a1);
}

/* vec_sr */

inline vector signed char
vec_sr (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vsrb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_sr (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsrb ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_sr (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vsrh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_sr (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsrh ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_sr (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vsrw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sr (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsrw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vsrw */

inline vector signed int
vec_vsrw (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vsrw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vsrw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsrw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vsrh */

inline vector signed short
vec_vsrh (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vsrh ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vsrh (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsrh ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vsrb */

inline vector signed char
vec_vsrb (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vsrb ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vsrb (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsrb ((vector signed char) a1, (vector signed char) a2);
}

/* vec_sra */

inline vector signed char
vec_sra (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vsrab ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_sra (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsrab ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_sra (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vsrah ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_sra (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsrah ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_sra (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vsraw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sra (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsraw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vsraw */

inline vector signed int
vec_vsraw (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vsraw ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vsraw (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsraw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vsrah */

inline vector signed short
vec_vsrah (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vsrah ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vsrah (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsrah ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vsrab */

inline vector signed char
vec_vsrab (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vsrab ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vsrab (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsrab ((vector signed char) a1, (vector signed char) a2);
}

/* vec_srl */

inline vector signed int
vec_srl (vector signed int a1, vector unsigned int a2)
{
  return (vector signed int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_srl (vector signed int a1, vector unsigned short a2)
{
  return (vector signed int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_srl (vector signed int a1, vector unsigned char a2)
{
  return (vector signed int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_srl (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_srl (vector unsigned int a1, vector unsigned short a2)
{
  return (vector unsigned int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_srl (vector unsigned int a1, vector unsigned char a2)
{
  return (vector unsigned int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_srl (vector signed short a1, vector unsigned int a2)
{
  return (vector signed short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_srl (vector signed short a1, vector unsigned short a2)
{
  return (vector signed short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_srl (vector signed short a1, vector unsigned char a2)
{
  return (vector signed short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_srl (vector unsigned short a1, vector unsigned int a2)
{
  return (vector unsigned short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_srl (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_srl (vector unsigned short a1, vector unsigned char a2)
{
  return (vector unsigned short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_srl (vector signed char a1, vector unsigned int a2)
{
  return (vector signed char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_srl (vector signed char a1, vector unsigned short a2)
{
  return (vector signed char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_srl (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_srl (vector unsigned char a1, vector unsigned int a2)
{
  return (vector unsigned char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_srl (vector unsigned char a1, vector unsigned short a2)
{
  return (vector unsigned char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_srl (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2);
}

/* vec_sro */

inline vector float
vec_sro (vector float a1, vector signed char a2)
{
  return (vector float) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_sro (vector float a1, vector unsigned char a2)
{
  return (vector float) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_sro (vector signed int a1, vector signed char a2)
{
  return (vector signed int) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_sro (vector signed int a1, vector unsigned char a2)
{
  return (vector signed int) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sro (vector unsigned int a1, vector signed char a2)
{
  return (vector unsigned int) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sro (vector unsigned int a1, vector unsigned char a2)
{
  return (vector unsigned int) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_sro (vector signed short a1, vector signed char a2)
{
  return (vector signed short) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_sro (vector signed short a1, vector unsigned char a2)
{
  return (vector signed short) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_sro (vector unsigned short a1, vector signed char a2)
{
  return (vector unsigned short) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_sro (vector unsigned short a1, vector unsigned char a2)
{
  return (vector unsigned short) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_sro (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_sro (vector signed char a1, vector unsigned char a2)
{
  return (vector signed char) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_sro (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_sro (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2);
}

/* vec_st */

inline void
vec_st (vector float a1, int a2, void *a3)
{
  __builtin_altivec_stvx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_st (vector signed int a1, int a2, void *a3)
{
  __builtin_altivec_stvx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_st (vector unsigned int a1, int a2, void *a3)
{
  __builtin_altivec_stvx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_st (vector signed short a1, int a2, void *a3)
{
  __builtin_altivec_stvx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_st (vector unsigned short a1, int a2, void *a3)
{
  __builtin_altivec_stvx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_st (vector signed char a1, int a2, void *a3)
{
  __builtin_altivec_stvx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_st (vector unsigned char a1, int a2, void *a3)
{
  __builtin_altivec_stvx ((vector signed int) a1, a2, (void *) a3);
}

/* vec_ste */

inline void
vec_ste (vector signed char a1, int a2, void *a3)
{
  __builtin_altivec_stvebx ((vector signed char) a1, a2, (void *) a3);
}

inline void
vec_ste (vector unsigned char a1, int a2, void *a3)
{
  __builtin_altivec_stvebx ((vector signed char) a1, a2, (void *) a3);
}

inline void
vec_ste (vector signed short a1, int a2, void *a3)
{
  __builtin_altivec_stvehx ((vector signed short) a1, a2, (void *) a3);
}

inline void
vec_ste (vector unsigned short a1, int a2, void *a3)
{
  __builtin_altivec_stvehx ((vector signed short) a1, a2, (void *) a3);
}

inline void
vec_ste (vector float a1, int a2, void *a3)
{
  __builtin_altivec_stvewx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_ste (vector signed int a1, int a2, void *a3)
{
  __builtin_altivec_stvewx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_ste (vector unsigned int a1, int a2, void *a3)
{
  __builtin_altivec_stvewx ((vector signed int) a1, a2, (void *) a3);
}

/* vec_stvewx */

inline void
vec_stvewx (vector float a1, int a2, void *a3)
{
  __builtin_altivec_stvewx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_stvewx (vector signed int a1, int a2, void *a3)
{
  __builtin_altivec_stvewx ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_stvewx (vector unsigned int a1, int a2, void *a3)
{
  __builtin_altivec_stvewx ((vector signed int) a1, a2, (void *) a3);
}

/* vec_stvehx */

inline void
vec_stvehx (vector signed short a1, int a2, void *a3)
{
  __builtin_altivec_stvehx ((vector signed short) a1, a2, (void *) a3);
}

inline void
vec_stvehx (vector unsigned short a1, int a2, void *a3)
{
  __builtin_altivec_stvehx ((vector signed short) a1, a2, (void *) a3);
}

/* vec_stvebx */

inline void
vec_stvebx (vector signed char a1, int a2, void *a3)
{
  __builtin_altivec_stvebx ((vector signed char) a1, a2, (void *) a3);
}

inline void
vec_stvebx (vector unsigned char a1, int a2, void *a3)
{
  __builtin_altivec_stvebx ((vector signed char) a1, a2, (void *) a3);
}

/* vec_stl */

inline void
vec_stl (vector float a1, int a2, void *a3)
{
  __builtin_altivec_stvxl ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_stl (vector signed int a1, int a2, void *a3)
{
  __builtin_altivec_stvxl ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_stl (vector unsigned int a1, int a2, void *a3)
{
  __builtin_altivec_stvxl ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_stl (vector signed short a1, int a2, void *a3)
{
  __builtin_altivec_stvxl ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_stl (vector unsigned short a1, int a2, void *a3)
{
  __builtin_altivec_stvxl ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_stl (vector signed char a1, int a2, void *a3)
{
  __builtin_altivec_stvxl ((vector signed int) a1, a2, (void *) a3);
}

inline void
vec_stl (vector unsigned char a1, int a2, void *a3)
{
  __builtin_altivec_stvxl ((vector signed int) a1, a2, (void *) a3);
}

/* vec_sub */

inline vector signed char
vec_sub (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_sub (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_sub (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_sub (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed short
vec_sub (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_sub (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_sub (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_sub (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed int
vec_sub (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sub (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sub (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_sub (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_sub (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vsubfp ((vector float) a1, (vector float) a2);
}

/* vec_vsubfp */

inline vector float
vec_vsubfp (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vsubfp ((vector float) a1, (vector float) a2);
}

/* vec_vsubuwm */

inline vector signed int
vec_vsubuwm (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vsubuwm (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vsubuwm (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vsubuwm (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vsubuhm */

inline vector signed short
vec_vsubuhm (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vsubuhm (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vsubuhm (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vsubuhm (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vsububm */

inline vector signed char
vec_vsububm (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vsububm (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vsububm (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vsububm (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2);
}

/* vec_subc */

inline vector unsigned int
vec_subc (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubcuw ((vector signed int) a1, (vector signed int) a2);
}

/* vec_subs */

inline vector unsigned char
vec_subs (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_subs (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_subs (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububs ((vector signed char) a1, (vector signed char) a2);
}

inline vector signed char
vec_subs (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vsubsbs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned short
vec_subs (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_subs (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_subs (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector signed short
vec_subs (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vsubshs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned int
vec_subs (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_subs (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_subs (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_subs (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsubsws ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vsubsws */

inline vector signed int
vec_vsubsws (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsubsws ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vsubuws */

inline vector unsigned int
vec_vsubuws (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vsubuws (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_vsubuws (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) a1, (vector signed int) a2);
}

/* vec_vsubshs */

inline vector signed short
vec_vsubshs (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vsubshs ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vsubuhs */

inline vector unsigned short
vec_vsubuhs (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vsubuhs (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vsubuhs */

inline vector unsigned short
vec_vsubsuhs (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vsubsuhs (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2);
}

inline vector unsigned short
vec_vsubsuhs (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2);
}

/* vec_vsubsbs */

inline vector signed char
vec_vsubsbs (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vsubsbs ((vector signed char) a1, (vector signed char) a2);
}

/* vec_vsububs */

inline vector unsigned char
vec_vsubsubs (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vsubsubs (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububs ((vector signed char) a1, (vector signed char) a2);
}

inline vector unsigned char
vec_vsubsubs (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vsububs ((vector signed char) a1, (vector signed char) a2);
}

/* vec_sum4s */

inline vector unsigned int
vec_sum4s (vector unsigned char a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsum4ubs ((vector signed char) a1, (vector signed int) a2);
}

inline vector signed int
vec_sum4s (vector signed char a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsum4sbs ((vector signed char) a1, (vector signed int) a2);
}

inline vector signed int
vec_sum4s (vector signed short a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsum4shs ((vector signed short) a1, (vector signed int) a2);
}

/* vec_vsum4shs */

inline vector signed int
vec_vsum4shss (vector signed short a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsum4shs ((vector signed short) a1, (vector signed int) a2);
}

/* vec_vsum4sbs */

inline vector signed int
vec_vsum4sbs (vector signed char a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsum4sbs ((vector signed char) a1, (vector signed int) a2);
}

/* vec_vsum4ubs */

inline vector unsigned int
vec_vsum4ubs (vector unsigned char a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vsum4ubs ((vector signed char) a1, (vector signed int) a2);
}

/* vec_sum2s */

inline vector signed int
vec_sum2s (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsum2sws ((vector signed int) a1, (vector signed int) a2);
}

/* vec_sums */

inline vector signed int
vec_sums (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vsumsws ((vector signed int) a1, (vector signed int) a2);
}

/* vec_trunc */

inline vector float
vec_trunc (vector float a1)
{
  return (vector float) __builtin_altivec_vrfiz ((vector float) a1);
}

/* vec_unpackh */

inline vector signed short
vec_unpackh (vector signed char a1)
{
  return (vector signed short) __builtin_altivec_vupkhsb ((vector signed char) a1);
}

inline vector signed int
vec_unpackh (vector signed short a1)
{
  return (vector signed int) __builtin_altivec_vupkhsh ((vector signed short) a1);
}

inline vector unsigned int
vec_unpackh (vector unsigned short a1)
{
  return (vector unsigned int) __builtin_altivec_vupkhpx ((vector signed short) a1);
}

/* vec_vupkhsh */

inline vector signed int
vec_vupkhsh (vector signed short a1)
{
  return (vector signed int) __builtin_altivec_vupkhsh ((vector signed short) a1);
}

/* vec_vupkhpx */

inline vector unsigned int
vec_vupkhpx (vector unsigned short a1)
{
  return (vector unsigned int) __builtin_altivec_vupkhpx ((vector signed short) a1);
}

/* vec_vupkhsb */

inline vector signed short
vec_vupkhsb (vector signed char a1)
{
  return (vector signed short) __builtin_altivec_vupkhsb ((vector signed char) a1);
}

/* vec_unpackl */

inline vector signed short
vec_unpackl (vector signed char a1)
{
  return (vector signed short) __builtin_altivec_vupklsb ((vector signed char) a1);
}

inline vector unsigned int
vec_unpackl (vector unsigned short a1)
{
  return (vector unsigned int) __builtin_altivec_vupklpx ((vector signed short) a1);
}

inline vector signed int
vec_unpackl (vector signed short a1)
{
  return (vector signed int) __builtin_altivec_vupklsh ((vector signed short) a1);
}

/* vec_vupklpx */

inline vector unsigned int
vec_vupklpx (vector unsigned short a1)
{
  return (vector unsigned int) __builtin_altivec_vupklpx ((vector signed short) a1);
}

/* vec_upklsh */

inline vector signed int
vec_vupklsh (vector signed short a1)
{
  return (vector signed int) __builtin_altivec_vupklsh ((vector signed short) a1);
}

/* vec_vupklsb */

inline vector signed short
vec_vupklsb (vector signed char a1)
{
  return (vector signed short) __builtin_altivec_vupklsb ((vector signed char) a1);
}

/* vec_xor */

inline vector float
vec_xor (vector float a1, vector float a2)
{
  return (vector float) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_xor (vector float a1, vector signed int a2)
{
  return (vector float) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector float
vec_xor (vector signed int a1, vector float a2)
{
  return (vector float) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed int
vec_xor (vector signed int a1, vector signed int a2)
{
  return (vector signed int) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_xor (vector signed int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_xor (vector unsigned int a1, vector signed int a2)
{
  return (vector unsigned int) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned int
vec_xor (vector unsigned int a1, vector unsigned int a2)
{
  return (vector unsigned int) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed short
vec_xor (vector signed short a1, vector signed short a2)
{
  return (vector signed short) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_xor (vector signed short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_xor (vector unsigned short a1, vector signed short a2)
{
  return (vector unsigned short) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned short
vec_xor (vector unsigned short a1, vector unsigned short a2)
{
  return (vector unsigned short) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector signed char
vec_xor (vector signed char a1, vector signed char a2)
{
  return (vector signed char) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_xor (vector signed char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_xor (vector unsigned char a1, vector signed char a2)
{
  return (vector unsigned char) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

inline vector unsigned char
vec_xor (vector unsigned char a1, vector unsigned char a2)
{
  return (vector unsigned char) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2);
}

/* vec_all_eq */

inline int
vec_all_eq (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_LT, a1, (vector signed char) a2);
}

inline int
vec_all_eq (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_LT, a1, a2);
}

inline int
vec_all_eq (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_LT, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_eq (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_LT, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_eq (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_LT, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_eq (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_LT, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_eq (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_LT, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_eq (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_LT, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_eq (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_LT, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_eq (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_LT, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_eq (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_LT, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_eq (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_LT, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_eq (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpeqfp_p (__CR6_LT, a1, a2);
}

/* vec_all_ge */

inline int
vec_all_ge (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_all_ge (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_all_ge (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_all_ge (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtsb_p (__CR6_EQ, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_all_ge (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_all_ge (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_all_ge (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_all_ge (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtsh_p (__CR6_EQ, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_all_ge (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_all_ge (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_all_ge (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_all_ge (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtsw_p (__CR6_EQ, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_all_ge (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgefp_p (__CR6_LT, a1, a2);
}

/* vec_all_gt */

inline int
vec_all_gt (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_gt (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_gt (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_gt (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtsb_p (__CR6_LT, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_gt (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_gt (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_gt (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_gt (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtsh_p (__CR6_LT, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_gt (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_gt (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_gt (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_gt (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtsw_p (__CR6_LT, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_gt (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgtfp_p (__CR6_LT, a1, a2);
}

/* vec_all_in */

inline int
vec_all_in (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpbfp_p (__CR6_EQ, a1, a2);
}

/* vec_all_le */

inline int
vec_all_le (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_le (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_le (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_le (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtsb_p (__CR6_EQ, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_le (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_le (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_le (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_le (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtsh_p (__CR6_EQ, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_le (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_le (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_le (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_le (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtsw_p (__CR6_EQ, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_le (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgefp_p (__CR6_LT, a2, a1);
}

/* vec_all_lt */

inline int
vec_all_lt (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_all_lt (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_all_lt (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_all_lt (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtsb_p (__CR6_LT, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_all_lt (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_all_lt (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_all_lt (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_all_lt (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtsh_p (__CR6_LT, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_all_lt (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_all_lt (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_all_lt (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_all_lt (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtsw_p (__CR6_LT, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_all_lt (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgtfp_p (__CR6_LT, a2, a1);
}

/* vec_all_nan */

inline int
vec_all_nan (vector float a1)
{
  return __builtin_altivec_vcmpeqfp_p (__CR6_EQ, a1, a1);
}

/* vec_all_ne */

inline int
vec_all_ne (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_EQ, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_ne (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_EQ, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_ne (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_EQ, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_ne (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_EQ, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_all_ne (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_EQ, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_ne (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_EQ, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_ne (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_EQ, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_ne (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_EQ, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_all_ne (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_EQ, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_ne (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_EQ, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_ne (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_EQ, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_ne (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_EQ, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_all_ne (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpeqfp_p (__CR6_EQ, a1, a2);
}

/* vec_all_nge */

inline int
vec_all_nge (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgefp_p (__CR6_EQ, a1, a2);
}

/* vec_all_ngt */

inline int
vec_all_ngt (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgtfp_p (__CR6_EQ, a1, a2);
}

/* vec_all_nle */

inline int
vec_all_nle (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgefp_p (__CR6_EQ, a2, a1);
}

/* vec_all_nlt */

inline int
vec_all_nlt (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgtfp_p (__CR6_EQ, a2, a1);
}

/* vec_all_numeric */

inline int
vec_all_numeric (vector float a1)
{
  return __builtin_altivec_vcmpeqfp_p (__CR6_EQ, a1, a1);
}

/* vec_any_eq */

inline int
vec_any_eq (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_EQ_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_eq (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_EQ_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_eq (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_EQ_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_eq (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_EQ_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_eq (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_EQ_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_eq (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_EQ_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_eq (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_EQ_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_eq (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_EQ_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_eq (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_EQ_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_eq (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_EQ_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_eq (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_EQ_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_eq (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_EQ_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_eq (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpeqfp_p (__CR6_EQ_REV, a1, a2);
}

/* vec_any_ge */

inline int
vec_any_ge (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_any_ge (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_any_ge (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_any_ge (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtsb_p (__CR6_LT_REV, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_any_ge (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_any_ge (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_any_ge (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_any_ge (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtsh_p (__CR6_LT_REV, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_any_ge (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_any_ge (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_any_ge (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_any_ge (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtsw_p (__CR6_LT_REV, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_any_ge (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgefp_p (__CR6_EQ_REV, a1, a2);
}

/* vec_any_gt */

inline int
vec_any_gt (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_gt (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_gt (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_gt (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtsb_p (__CR6_EQ_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_gt (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_gt (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_gt (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_gt (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtsh_p (__CR6_EQ_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_gt (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_gt (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_gt (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_gt (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtsw_p (__CR6_EQ_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_gt (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgtfp_p (__CR6_EQ_REV, a1, a2);
}

/* vec_any_le */

inline int
vec_any_le (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_le (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_le (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_le (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtsb_p (__CR6_LT_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_le (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_le (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_le (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_le (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtsh_p (__CR6_LT_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_le (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_le (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_le (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_le (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtsw_p (__CR6_LT_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_le (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgefp_p (__CR6_EQ_REV, a2, a1);
}

/* vec_any_lt */

inline int
vec_any_lt (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_any_lt (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_any_lt (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_any_lt (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpgtsb_p (__CR6_EQ_REV, (vector signed char) a2, (vector signed char) a1);
}

inline int
vec_any_lt (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_any_lt (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_any_lt (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_any_lt (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpgtsh_p (__CR6_EQ_REV, (vector signed short) a2, (vector signed short) a1);
}

inline int
vec_any_lt (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_any_lt (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_any_lt (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_any_lt (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpgtsw_p (__CR6_EQ_REV, (vector signed int) a2, (vector signed int) a1);
}

inline int
vec_any_lt (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgtfp_p (__CR6_EQ_REV, a2, a1);
}

/* vec_any_nan */

inline int
vec_any_nan (vector float a1)
{
  return __builtin_altivec_vcmpeqfp_p (__CR6_LT_REV, a1, a1);
}

/* vec_any_ne */

inline int
vec_any_ne (vector signed char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_LT_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_ne (vector signed char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_LT_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_ne (vector unsigned char a1, vector signed char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_LT_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_ne (vector unsigned char a1, vector unsigned char a2)
{
  return __builtin_altivec_vcmpequb_p (__CR6_LT_REV, (vector signed char) a1, (vector signed char) a2);
}

inline int
vec_any_ne (vector signed short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_LT_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_ne (vector signed short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_LT_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_ne (vector unsigned short a1, vector signed short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_LT_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_ne (vector unsigned short a1, vector unsigned short a2)
{
  return __builtin_altivec_vcmpequh_p (__CR6_LT_REV, (vector signed short) a1, (vector signed short) a2);
}

inline int
vec_any_ne (vector signed int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_LT_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_ne (vector signed int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_LT_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_ne (vector unsigned int a1, vector signed int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_LT_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_ne (vector unsigned int a1, vector unsigned int a2)
{
  return __builtin_altivec_vcmpequw_p (__CR6_LT_REV, (vector signed int) a1, (vector signed int) a2);
}

inline int
vec_any_ne (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpeqfp_p (__CR6_LT_REV, a1, a2);
}

/* vec_any_nge */

inline int
vec_any_nge (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgefp_p (__CR6_LT_REV, a1, a2);
}

/* vec_any_ngt */

inline int
vec_any_ngt (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgtfp_p (__CR6_LT_REV, a1, a2);
}

/* vec_any_nle */

inline int
vec_any_nle (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgefp_p (__CR6_LT_REV, a2, a1);
}

/* vec_any_nlt */

inline int
vec_any_nlt (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpgtfp_p (__CR6_LT_REV, a2, a1);
}

/* vec_any_numeric */

inline int
vec_any_numeric (vector float a1)
{
  return __builtin_altivec_vcmpeqfp_p (__CR6_EQ_REV, a1, a1);
}

/* vec_any_out */

inline int
vec_any_out (vector float a1, vector float a2)
{
  return __builtin_altivec_vcmpbfp_p (__CR6_EQ_REV, a1, a2);
}

/* vec_step */

template<typename _Tp>
struct __vec_step_help
{
  // All proper vector types will specialize _S_elem.
};

template<>
struct __vec_step_help<vector signed short>
{
  static const int _S_elem = 8;
};

template<>
struct __vec_step_help<vector unsigned short>
{
  static const int _S_elem = 8;
};

template<>
struct __vec_step_help<vector signed int>
{
  static const int _S_elem = 4;
};

template<>
struct __vec_step_help<vector unsigned int>
{
  static const int _S_elem = 4;
};

template<>
struct __vec_step_help<vector unsigned char>
{
  static const int _S_elem = 16;
};

template<>
struct __vec_step_help<vector signed char>
{
  static const int _S_elem = 16;
};

template<>
struct __vec_step_help<vector float>
{
  static const int _S_elem = 4;
};

#define vec_step(t)  __vec_step_help<typeof(t)>::_S_elem

#else /* not C++ */

/* "... and so I think no man in a century will suffer as greatly as
   you will."  */

#define vec_abs(a) \
  __ch (__un_args_eq (vector signed char, (a)), \
        ((vector signed char) __builtin_altivec_abs_v16qi ((vector signed char) (a))), \
  __ch (__un_args_eq (vector signed short, (a)), \
        ((vector signed short) __builtin_altivec_abs_v8hi ((vector signed short) (a))), \
  __ch (__un_args_eq (vector signed int, (a)), \
        ((vector signed int) __builtin_altivec_abs_v4si ((vector signed int) (a))), \
  __ch (__un_args_eq (vector float, (a)), \
        ((vector float) __builtin_altivec_abs_v4sf ((vector float) (a))), \
  __altivec_link_error_invalid_argument ()))))

#define vec_abss(a) \
  __ch (__un_args_eq (vector signed char, (a)), \
        ((vector signed char) __builtin_altivec_abss_v16qi ((vector signed char) (a))), \
  __ch (__un_args_eq (vector signed short, (a)), \
        ((vector signed short) __builtin_altivec_abss_v8hi ((vector signed short) (a))), \
  __ch (__un_args_eq (vector signed int, (a)), \
        ((vector signed int) __builtin_altivec_abss_v4si ((vector signed int) (a))), \
  __altivec_link_error_invalid_argument ())))

#define vec_step(t) \
  __ch (__builtin_types_compatible_p (typeof (t), vector signed int), 4,      \
  __ch (__builtin_types_compatible_p (typeof (t), vector unsigned int), 4,    \
  __ch (__builtin_types_compatible_p (typeof (t), vector signed short), 8,    \
  __ch (__builtin_types_compatible_p (typeof (t), vector unsigned short), 8,  \
  __ch (__builtin_types_compatible_p (typeof (t), vector signed char), 16,    \
  __ch (__builtin_types_compatible_p (typeof (t), vector unsigned char), 16,  \
  __ch (__builtin_types_compatible_p (typeof (t), vector float), 4,           \
  __altivec_link_error_invalid_argument ())))))))

#define vec_vaddubm(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vaddubm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) (a1), (vector signed char) (a2))), \
  __altivec_link_error_invalid_argument ()))))

#define vec_vadduhm(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vadduhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) (a1), (vector signed short) (a2))), \
  __altivec_link_error_invalid_argument ()))))

#define vec_vadduwm(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vadduwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) (a1), (vector signed int) (a2))), \
  __altivec_link_error_invalid_argument ()))))

#define vec_vaddfp(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vaddfp ((vector float) (a1), (vector float) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_add(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vaddubm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vadduhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vadduwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vaddfp ((vector float) (a1), (vector float) (a2))), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_addc(a1, a2) \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vaddcuw ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ())

#define vec_adds(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vaddsbs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vaddshs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vaddsws ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))))))))

#define vec_vaddsws(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vaddsws ((vector signed int) (a1), (vector signed int) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vadduws(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vadduws ((vector signed int) (a1), (vector signed int) (a2))), \
  __altivec_link_error_invalid_argument ())))

#define vec_vaddshs(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vaddshs ((vector signed short) (a1), (vector signed short) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vadduhs(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) (a1), (vector signed short) (a2))), \
  __altivec_link_error_invalid_argument ())))

#define vec_vaddsbs(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vaddsbs ((vector signed char) (a1), (vector signed char) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vaddubs(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) (a1), (vector signed char) (a2))), \
  __altivec_link_error_invalid_argument ())))

#define vec_and(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector signed int, (a2)), \
      ((vector float) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vand ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ())))))))))))))))

#define vec_andc(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector unsigned int, (a2)), \
      ((vector float) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector signed int, (a2)), \
      ((vector float) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vandc ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ())))))))))))))))))))))))

#define vec_avg(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vavgub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vavgsb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vavguh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vavgsh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vavguw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vavgsw ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_vavgsw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vavgsw ((vector signed int) (a1), (vector signed int) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vavguw(a1, a2) \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vavguw ((vector signed int) (a1), (vector signed int) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vavgsh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vavgsh ((vector signed short) (a1), (vector signed short) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vavguh(a1, a2) \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vavguh ((vector signed short) (a1), (vector signed short) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vavgsb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vavgsb ((vector signed char) (a1), (vector signed char) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vavgub(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vavgub ((vector signed char) (a1), (vector signed char) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_ceil(a1) __builtin_altivec_vrfip ((a1))

#define vec_cmpb(a1, a2) __builtin_altivec_vcmpbfp ((a1), (a2))

#define vec_cmpeq(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpequb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpequb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpequh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpequh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpequw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpequw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpeqfp ((vector float) (a1), (vector float) (a2))), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_vcmpeqfp(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpeqfp ((vector float) (a1), (vector float) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vcmpequw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpequw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpequw ((vector signed int) (a1), (vector signed int) (a2))), \
  __altivec_link_error_invalid_argument ()))

#define vec_vcmpequh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpequh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpequh ((vector signed short) (a1), (vector signed short) (a2))), \
  __altivec_link_error_invalid_argument ()))

#define vec_vcmpequb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpequb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpequb ((vector signed char) (a1), (vector signed char) (a2))), \
  __altivec_link_error_invalid_argument ()))

#define vec_cmpge(a1, a2) (vector signed int) __builtin_altivec_vcmpgefp ((a1), (a2))

#define vec_cmpgt(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpgtub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpgtsb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpgtuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpgtsh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpgtuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpgtsw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpgtfp ((vector float) (a1), (vector float) (a2))), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_vcmpgtfp(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpgtfp ((vector float) (a1), (vector float) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vcmpgtsw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpgtsw ((vector signed int) (a1), (vector signed int) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vcmpgtuw(a1, a2) \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpgtuw ((vector signed int) (a1), (vector signed int) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vcmpgtsh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpgtsh ((vector signed short) (a1), (vector signed short) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vcmpgtuh(a1, a2) \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpgtuh ((vector signed short) (a1), (vector signed short) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vcmpgtsb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpgtsb ((vector signed char) (a1), (vector signed char) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_vcmpgtub(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpgtub ((vector signed char) (a1), (vector signed char) (a2))), \
  __altivec_link_error_invalid_argument ())

#define vec_cmple(a1, a2) __builtin_altivec_vcmpgefp ((a1), (a2))

#define vec_cmplt(a2, a1) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpgtub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vcmpgtsb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpgtuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vcmpgtsh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpgtuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpgtsw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector signed int) __builtin_altivec_vcmpgtfp ((vector float) (a1), (vector float) (a2))), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_ctf(a1, a2) \
__ch (__bin_args_eq (vector unsigned int, (a1), int, (a2)), \
      ((vector float) __builtin_altivec_vcfux ((vector signed int) (a1), (const char) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), int, (a2)), \
      ((vector float) __builtin_altivec_vcfsx ((vector signed int) (a1), (const char) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), unsigned int, (a2)), \
      ((vector float) __builtin_altivec_vcfux ((vector signed int) (a1), (const char) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), unsigned int, (a2)), \
      ((vector float) __builtin_altivec_vcfsx ((vector signed int) (a1), (const char) (a2))), \
    __altivec_link_error_invalid_argument ()))))

#define vec_vcfsx(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), int, (a2)), \
      ((vector float) __builtin_altivec_vcfsx ((vector signed int) (a1), (const char) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), unsigned int, (a2)), \
      ((vector float) __builtin_altivec_vcfsx ((vector signed int) (a1), (const char) (a2))), \
  __altivec_link_error_invalid_argument ()))

#define vec_vcfux(a1, a2) \
__ch (__bin_args_eq (vector unsigned int, (a1), int, (a2)), \
      ((vector float) __builtin_altivec_vcfux ((vector signed int) (a1), (const char) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), unsigned int, (a2)), \
      ((vector float) __builtin_altivec_vcfux ((vector signed int) (a1), (const char) (a2))), \
  __altivec_link_error_invalid_argument ()))

#define vec_cts(a1, a2) __builtin_altivec_vctsxs ((a1), (a2))

#define vec_ctu(a1, a2) (vector unsigned int) __builtin_altivec_vctuxs ((a1), (a2))

#define vec_dss(a1) __builtin_altivec_dss ((a1))

#define vec_dssall() __builtin_altivec_dssall ()

#define vec_dst(a1, a2, a3) \
__ch (__un_args_eq (vector unsigned char, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed char, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector unsigned short, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed short, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector unsigned int, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed int, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector float, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned char, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed char, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned short, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed short, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned int, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed int, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned long, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed long, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (float, *(a1)), \
      __builtin_altivec_dst ((void *) (a1), (a2), (a3)), \
  __altivec_link_error_invalid_argument ()))))))))))))))))

#define vec_dstst(a1, a2, a3) \
__ch (__un_args_eq (vector unsigned char, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed char, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector unsigned short, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed short, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector unsigned int, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed int, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector float, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned char, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed char, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned short, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed short, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned int, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed int, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned long, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed long, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (float, *(a1)), \
      __builtin_altivec_dstst ((void *) (a1), (a2), (a3)), \
  __altivec_link_error_invalid_argument ()))))))))))))))))

#define vec_dststt(a1, a2, a3) \
__ch (__un_args_eq (vector unsigned char, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed char, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector unsigned short, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed short, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector unsigned int, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed int, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector float, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned char, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed char, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned short, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed short, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned int, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed int, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned long, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed long, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (float, *(a1)), \
      __builtin_altivec_dststt ((void *) (a1), (a2), (a3)), \
  __altivec_link_error_invalid_argument ()))))))))))))))))

#define vec_dstt(a1, a2, a3) \
__ch (__un_args_eq (vector unsigned char, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed char, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector unsigned short, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed short, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector unsigned int, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector signed int, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (vector float, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned char, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed char, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned short, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed short, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned int, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed int, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (unsigned long, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (signed long, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
__ch (__un_args_eq (float, *(a1)), \
      __builtin_altivec_dstt ((void *) (a1), (a2), (a3)), \
  __altivec_link_error_invalid_argument ()))))))))))))))))

#define vec_expte(a1) __builtin_altivec_vexptefp ((a1))

#define vec_floor(a1) __builtin_altivec_vrfim (a1)

#define vec_ld(a, b) \
__ch (__un_args_eq (vector unsigned char, *(b)), \
      ((vector unsigned char) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (unsigned char, *(b)), \
      ((vector unsigned char) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (vector signed char, *(b)), \
      ((vector signed char) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (signed char, *(b)), \
      ((vector signed char) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (vector unsigned short, *(b)), \
      ((vector unsigned short) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (unsigned short, *(b)), \
      ((vector unsigned short) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (vector signed short, *(b)), \
      ((vector signed short) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (signed short, *(b)), \
      ((vector signed short) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (vector unsigned int, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (unsigned int, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (unsigned long, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (vector signed int, *(b)), \
      ((vector signed int) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (signed int, *(b)), \
      ((vector signed int) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (signed long, *(b)), \
      ((vector signed int) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (vector float, *(b)), \
      ((vector float) __builtin_altivec_lvx ((a), (b))), \
__ch (__un_args_eq (float, *(b)), \
      ((vector float) __builtin_altivec_lvx ((a), (b))), \
__altivec_link_error_invalid_argument ()))))))))))))))))

#define vec_lde(a, b) \
__ch (__un_args_eq (unsigned char, *(b)), \
      ((vector unsigned char) __builtin_altivec_lvebx ((a), (b))), \
__ch (__un_args_eq (signed char, *(b)), \
      ((vector signed char) __builtin_altivec_lvebx ((a), (b))), \
__ch (__un_args_eq (unsigned short, *(b)), \
      ((vector unsigned short) __builtin_altivec_lvehx ((a), (b))), \
__ch (__un_args_eq (signed short, *(b)), \
      ((vector signed short) __builtin_altivec_lvehx ((a), (b))), \
__ch (__un_args_eq (unsigned long, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvewx ((a), (b))), \
__ch (__un_args_eq (signed long, *(b)), \
      ((vector signed int) __builtin_altivec_lvewx ((a), (b))), \
__ch (__un_args_eq (unsigned int, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvewx ((a), (b))), \
__ch (__un_args_eq (signed int, *(b)), \
      ((vector signed int) __builtin_altivec_lvewx ((a), (b))), \
__ch (__un_args_eq (float, *(b)), \
      ((vector float) __builtin_altivec_lvewx ((a), (b))), \
__altivec_link_error_invalid_argument ())))))))))

#define vec_lvewx(a, b) \
__ch (__un_args_eq (unsigned int, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvewx ((a), (b))), \
__ch (__un_args_eq (signed int, *(b)), \
      ((vector signed int) __builtin_altivec_lvewx ((a), (b))), \
__ch (__un_args_eq (unsigned long, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvewx ((a), (b))), \
__ch (__un_args_eq (signed long, *(b)), \
      ((vector signed int) __builtin_altivec_lvewx ((a), (b))), \
__ch (__un_args_eq (float, *(b)), \
      ((vector float) __builtin_altivec_lvewx ((a), (b))), \
__altivec_link_error_invalid_argument ())))))

#define vec_lvehx(a, b) \
__ch (__un_args_eq (unsigned short, *(b)), \
      ((vector unsigned short) __builtin_altivec_lvehx ((a), (b))), \
__ch (__un_args_eq (signed short, *(b)), \
      ((vector signed short) __builtin_altivec_lvehx ((a), (b))), \
__altivec_link_error_invalid_argument ()))

#define vec_lvebx(a, b) \
__ch (__un_args_eq (unsigned char, *(b)), \
      ((vector unsigned char) __builtin_altivec_lvebx ((a), (b))), \
__ch (__un_args_eq (signed char, *(b)), \
      ((vector signed char) __builtin_altivec_lvebx ((a), (b))), \
__altivec_link_error_invalid_argument ()))

#define vec_ldl(a, b) \
__ch (__un_args_eq (vector unsigned char, *(b)), \
      ((vector unsigned char) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (unsigned char, *(b)), \
      ((vector unsigned char) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (vector signed char, *(b)), \
      ((vector signed char) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (signed char, *(b)), \
      ((vector signed char) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (vector unsigned short, *(b)), \
      ((vector unsigned short) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (unsigned short, *(b)), \
      ((vector unsigned short) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (vector signed short, *(b)), \
      ((vector signed short) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (signed short, *(b)), \
      ((vector signed short) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (vector unsigned int, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (unsigned int, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (unsigned long, *(b)), \
      ((vector unsigned int) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (vector signed int, *(b)), \
      ((vector signed int) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (signed int, *(b)), \
      ((vector signed int) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (signed long, *(b)), \
      ((vector signed int) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (vector float, *(b)), \
      ((vector float) __builtin_altivec_lvxl ((a), (b))), \
__ch (__un_args_eq (float, *(b)), \
      ((vector float) __builtin_altivec_lvxl ((a), (b))), \
__altivec_link_error_invalid_argument ()))))))))))))))))

#define vec_loge(a1) __builtin_altivec_vlogefp ((a1))

#define vec_lvsl(a1, a2) \
__ch (__un_args_eq (unsigned char, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsl ((a1), (void *) (a2))), \
__ch (__un_args_eq (signed char, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsl ((a1), (void *) (a2))), \
__ch (__un_args_eq (unsigned short, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsl ((a1), (void *) (a2))), \
__ch (__un_args_eq (signed short, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsl ((a1), (void *) (a2))), \
__ch (__un_args_eq (unsigned int, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsl ((a1), (void *) (a2))), \
__ch (__un_args_eq (signed int, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsl ((a1), (void *) (a2))), \
__ch (__un_args_eq (unsigned long, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsl ((a1), (void *) (a2))), \
__ch (__un_args_eq (signed long, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsl ((a1), (void *) (a2))), \
__ch (__un_args_eq (float, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsl ((a1), (void *) (a2))), \
__altivec_link_error_invalid_argument ())))))))))

#define vec_lvsr(a1, a2) \
__ch (__un_args_eq (unsigned char, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsr ((a1), (void *) (a2))), \
__ch (__un_args_eq (signed char, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsr ((a1), (void *) (a2))), \
__ch (__un_args_eq (unsigned short, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsr ((a1), (void *) (a2))), \
__ch (__un_args_eq (signed short, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsr ((a1), (void *) (a2))), \
__ch (__un_args_eq (unsigned int, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsr ((a1), (void *) (a2))), \
__ch (__un_args_eq (signed int, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsr ((a1), (void *) (a2))), \
__ch (__un_args_eq (unsigned long, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsr ((a1), (void *) (a2))), \
__ch (__un_args_eq (signed long, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsr ((a1), (void *) (a2))), \
__ch (__un_args_eq (float, *(a2)), \
      ((vector unsigned char) __builtin_altivec_lvsr ((a1), (void *) (a2))), \
__altivec_link_error_invalid_argument ())))))))))

#define vec_madd(a1, a2, a3) (__builtin_altivec_vmaddfp ((a1), (a2), (a3)))

#define vec_madds(a1, a2, a3) __builtin_altivec_vmhaddshs ((a1), (a2), (a3))

#define vec_max(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vmaxsb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vmaxsh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vmaxsw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vmaxfp ((vector float) (a1), (vector float) (a2))), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_vmaxfp(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vmaxfp ((vector float) (a1), (vector float) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmaxsw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vmaxsw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmaxuw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_vmaxsh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vmaxsh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmaxuh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_vmaxsb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vmaxsb ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmaxub(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_mergeh(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vmrghb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmrghb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vmrghh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmrghh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vmrghw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vmrghw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmrghw ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_vmrghw(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vmrghw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vmrghw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmrghw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_vmrghh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vmrghh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmrghh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vmrghb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vmrghb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmrghb ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_mergel(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vmrglb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmrglb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vmrglh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmrglh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vmrglw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vmrglw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmrglw ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_vmrglw(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vmrglw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vmrglw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmrglw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_vmrglh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vmrglh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmrglh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vmrglb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vmrglb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vmrglb ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_mfvscr()  (((vector unsigned short) __builtin_altivec_mfvscr ()))

#define vec_min(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vminub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vminub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vminub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vminsb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vminuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vminuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vminuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vminsh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vminuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vminuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vminuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vminsw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vminfp ((vector float) (a1), (vector float) (a2))), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_vminfp(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vminfp ((vector float) (a1), (vector float) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vminsw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vminsw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vminuw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vminuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vminuw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vminuw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_vminsh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vminsh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vminuh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vminuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vminuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vminuh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_vminsb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vminsb ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vminub(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vminub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vminub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vminub ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_mladd(a1, a2, a3) \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), vector signed short, (a3)), \
      ((vector signed short) __builtin_altivec_vmladduhm ((vector signed short) (a1), (vector signed short) (a2), (vector signed short) (a3))), \
__ch (__tern_args_eq (vector signed short, (a1), vector unsigned short, (a2), vector unsigned short, (a3)), \
      ((vector signed short) __builtin_altivec_vmladduhm ((vector signed short) (a1), (vector signed short) (a2), (vector signed short) (a3))), \
__ch (__tern_args_eq (vector unsigned short, (a1), vector signed short, (a2), vector signed short, (a3)), \
      ((vector signed short) __builtin_altivec_vmladduhm ((vector signed short) (a1), (vector signed short) (a2), (vector signed short) (a3))), \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), vector unsigned short, (a3)), \
      ((vector unsigned short) __builtin_altivec_vmladduhm ((vector signed short) (a1), (vector signed short) (a2), (vector signed short) (a3))), \
    __altivec_link_error_invalid_argument ()))))

#define vec_mradds(a1, a2, a3) __builtin_altivec_vmhraddshs ((a1), (a2), (a3))

#define vec_msum(a1, a2, a3) \
__ch (__tern_args_eq (vector unsigned char, (a1), vector unsigned char, (a2), vector unsigned int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vmsumubm ((vector signed char) (a1), (vector signed char) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector signed char, (a1), vector unsigned char, (a2), vector signed int, (a3)), \
      ((vector signed int) __builtin_altivec_vmsummbm ((vector signed char) (a1), (vector signed char) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), vector unsigned int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vmsumuhm ((vector signed short) (a1), (vector signed short) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), vector signed int, (a3)), \
      ((vector signed int) __builtin_altivec_vmsumshm ((vector signed short) (a1), (vector signed short) (a2), (vector signed int) (a3))), \
    __altivec_link_error_invalid_argument ()))))

#define vec_vmsumshm(a1, a2, a3) \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), vector signed int, (a3)), \
      ((vector signed int) __builtin_altivec_vmsumshm ((vector signed short) (a1), (vector signed short) (a2), (vector signed int) (a3))), \
__altivec_link_error_invalid_argument ())

#define vec_vmsumuhm(a1, a2, a3) \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), vector unsigned int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vmsumuhm ((vector signed short) (a1), (vector signed short) (a2), (vector signed int) (a3))), \
__altivec_link_error_invalid_argument ())

#define vec_vmsummbm(a1, a2, a3) \
__ch (__tern_args_eq (vector signed char, (a1), vector unsigned char, (a2), vector signed int, (a3)), \
      ((vector signed int) __builtin_altivec_vmsummbm ((vector signed char) (a1), (vector signed char) (a2), (vector signed int) (a3))), \
__altivec_link_error_invalid_argument ())

#define vec_vmsumubm(a1, a2, a3) \
__ch (__tern_args_eq (vector unsigned char, (a1), vector unsigned char, (a2), vector unsigned int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vmsumubm ((vector signed char) (a1), (vector signed char) (a2), (vector signed int) (a3))), \
__altivec_link_error_invalid_argument ())

#define vec_msums(a1, a2, a3) \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), vector unsigned int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vmsumuhs ((vector signed short) (a1), (vector signed short) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), vector signed int, (a3)), \
      ((vector signed int) __builtin_altivec_vmsumshs ((vector signed short) (a1), (vector signed short) (a2), (vector signed int) (a3))), \
    __altivec_link_error_invalid_argument ()))

#define vec_vmsumshs(a1, a2, a3) \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), vector signed int, (a3)), \
      ((vector signed int) __builtin_altivec_vmsumshs ((vector signed short) (a1), (vector signed short) (a2), (vector signed int) (a3))), \
__altivec_link_error_invalid_argument ())

#define vec_vmsumuhs(a1, a2, a3) \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), vector unsigned int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vmsumuhs ((vector signed short) (a1), (vector signed short) (a2), (vector signed int) (a3))), \
__altivec_link_error_invalid_argument ())

#define vec_mtvscr(a1) \
__ch (__un_args_eq (vector signed int, (a1)), \
      __builtin_altivec_mtvscr ((vector signed int) (a1)), \
__ch (__un_args_eq (vector unsigned int, (a1)), \
      __builtin_altivec_mtvscr ((vector signed int) (a1)), \
__ch (__un_args_eq (vector signed short, (a1)), \
      __builtin_altivec_mtvscr ((vector signed int) (a1)), \
__ch (__un_args_eq (vector unsigned short, (a1)), \
      __builtin_altivec_mtvscr ((vector signed int) (a1)), \
__ch (__un_args_eq (vector signed char, (a1)), \
      __builtin_altivec_mtvscr ((vector signed int) (a1)), \
__ch (__un_args_eq (vector unsigned char, (a1)), \
      __builtin_altivec_mtvscr ((vector signed int) (a1)), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_mule(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmuleub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed short) __builtin_altivec_vmulesb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmuleuh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed int) __builtin_altivec_vmulesh ((vector signed short) (a1), (vector signed short) (a2))), \
    __altivec_link_error_invalid_argument ()))))

#define vec_vmulesh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed int) __builtin_altivec_vmulesh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmuleuh(a1, a2) \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmuleuh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmulesb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed short) __builtin_altivec_vmulesb ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmuleub(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmuleub ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_mulo(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmuloub ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed short) __builtin_altivec_vmulosb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmulouh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed int) __builtin_altivec_vmulosh ((vector signed short) (a1), (vector signed short) (a2))), \
    __altivec_link_error_invalid_argument ()))))

#define vec_vmulosh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed int) __builtin_altivec_vmulosh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmulouh(a1, a2) \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned int) __builtin_altivec_vmulouh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmulosb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed short) __builtin_altivec_vmulosb ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vmuloub(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vmuloub ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_nmsub(a1, a2, a3) \
__ch (__tern_args_eq (vector float, ((a1)), vector float, ((a2)) , vector float, ((a3))), \
      ((vector float) __builtin_altivec_vnmsubfp ((vector float) ((a1)), (vector float) ((a2)), (vector float)((a3)))), \
    __altivec_link_error_invalid_argument ())

#define vec_nor(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vnor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vnor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vnor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vnor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vnor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vnor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vnor ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_or(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector signed int, (a2)), \
      ((vector float) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vor ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ())))))))))))))))

#define vec_pack(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed char) __builtin_altivec_vpkuhum ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned char) __builtin_altivec_vpkuhum ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed short) __builtin_altivec_vpkuwum ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned short) __builtin_altivec_vpkuwum ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))

#define vec_vpkuwum(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed short) __builtin_altivec_vpkuwum ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned short) __builtin_altivec_vpkuwum ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vpkuhum(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed char) __builtin_altivec_vpkuhum ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned char) __builtin_altivec_vpkuhum ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_packpx(a1, a2) \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
  (vector unsigned short) __builtin_altivec_vpkpx ((vector signed int) (a1), (vector signed int) (a2)), \
__altivec_link_error_invalid_argument ())

#define vec_packs(a1, a2) \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned char) __builtin_altivec_vpkuhus ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed char) __builtin_altivec_vpkshss ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned short) __builtin_altivec_vpkuwus ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed short) __builtin_altivec_vpkswss ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))

#define vec_vpkswss(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed short) __builtin_altivec_vpkswss ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vpkuwus(a1, a2) \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned short) __builtin_altivec_vpkuwus ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vpkshss(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed char) __builtin_altivec_vpkshss ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vpkuhus(a1, a2) \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned char) __builtin_altivec_vpkuhus ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_packsu(a1, a2) \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned char) __builtin_altivec_vpkuhus ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector unsigned char) __builtin_altivec_vpkshus ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned short) __builtin_altivec_vpkuwus ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector unsigned short) __builtin_altivec_vpkswus ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))

#define vec_vpkswus(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector unsigned short) __builtin_altivec_vpkswus ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vpkshus(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector unsigned char) __builtin_altivec_vpkshus ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_perm(a1, a2, a3) \
__ch (__tern_args_eq (vector float, (a1), vector float, (a2), vector unsigned char, (a3)), \
      ((vector float) __builtin_altivec_vperm_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed char) (a3))), \
__ch (__tern_args_eq (vector signed int, (a1), vector signed int, (a2), vector unsigned char, (a3)), \
      ((vector signed int) __builtin_altivec_vperm_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed char) (a3))), \
__ch (__tern_args_eq (vector unsigned int, (a1), vector unsigned int, (a2), vector unsigned char, (a3)), \
      ((vector unsigned int) __builtin_altivec_vperm_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed char) (a3))), \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), vector unsigned char, (a3)), \
      ((vector signed short) __builtin_altivec_vperm_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed char) (a3))), \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), vector unsigned char, (a3)), \
      ((vector unsigned short) __builtin_altivec_vperm_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed char) (a3))), \
__ch (__tern_args_eq (vector signed char, (a1), vector signed char, (a2), vector unsigned char, (a3)), \
      ((vector signed char) __builtin_altivec_vperm_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed char) (a3))), \
__ch (__tern_args_eq (vector unsigned char, (a1), vector unsigned char, (a2), vector unsigned char, (a3)), \
      ((vector unsigned char) __builtin_altivec_vperm_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed char) (a3))), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_re(a1) __builtin_altivec_vrefp ((a1))

#define vec_rl(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vrlb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vrlb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vrlh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vrlh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vrlw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vrlw ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_vrlw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vrlw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vrlw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vrlh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vrlh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vrlh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vrlb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vrlb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vrlb ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_round(a1) __builtin_altivec_vrfin ((a1))

#define vec_rsqrte(a1) __builtin_altivec_vrsqrtefp ((a1))

#define vec_sel(a1, a2, a3) \
__ch (__tern_args_eq (vector float, (a1), vector float, (a2), vector signed int, (a3)), \
      ((vector float) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector float, (a1), vector float, (a2), vector unsigned int, (a3)), \
      ((vector float) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector signed int, (a1), vector signed int, (a2), vector signed int, (a3)), \
      ((vector signed int) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector signed int, (a1), vector signed int, (a2), vector unsigned int, (a3)), \
      ((vector signed int) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector unsigned int, (a1), vector unsigned int, (a2), vector signed int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector unsigned int, (a1), vector unsigned int, (a2), vector unsigned int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), vector signed short, (a3)), \
      ((vector signed short) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), vector unsigned short, (a3)), \
      ((vector signed short) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), vector signed short, (a3)), \
      ((vector unsigned short) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), vector unsigned short, (a3)), \
      ((vector unsigned short) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector signed char, (a1), vector signed char, (a2), vector signed char, (a3)), \
      ((vector signed char) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector signed char, (a1), vector signed char, (a2), vector unsigned char, (a3)), \
      ((vector signed char) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector unsigned char, (a1), vector unsigned char, (a2), vector signed char, (a3)), \
      ((vector unsigned char) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
__ch (__tern_args_eq (vector unsigned char, (a1), vector unsigned char, (a2), vector unsigned char, (a3)), \
      ((vector unsigned char) __builtin_altivec_vsel_4si ((vector signed int) (a1), (vector signed int) (a2), (vector signed int) (a3))), \
    __altivec_link_error_invalid_argument ()))))))))))))))

#define vec_sl(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vslb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vslb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vslh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vslh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vslw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vslw ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_vslw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vslw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vslw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vslh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vslh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vslh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vslb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vslb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vslb ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_sld(a1, a2, a3) \
__ch (__tern_args_eq (vector float, (a1), vector float, (a2), int, (a3)), \
      ((vector float) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector float, (a1), vector float, (a2), unsigned int, (a3)), \
      ((vector float) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector signed int, (a1), vector signed int, (a2), int, (a3)), \
      ((vector signed int) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector signed int, (a1), vector signed int, (a2), unsigned int, (a3)), \
      ((vector signed int) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector unsigned int, (a1), vector unsigned int, (a2), int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector unsigned int, (a1), vector unsigned int, (a2), unsigned int, (a3)), \
      ((vector unsigned int) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), int, (a3)), \
      ((vector signed short) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector signed short, (a1), vector signed short, (a2), unsigned int, (a3)), \
      ((vector signed short) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), int, (a3)), \
      ((vector unsigned short) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector unsigned short, (a1), vector unsigned short, (a2), unsigned int, (a3)), \
      ((vector unsigned short) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector signed char, (a1), vector signed char, (a2), int, (a3)), \
      ((vector signed char) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector signed char, (a1), vector signed char, (a2), unsigned int, (a3)), \
      ((vector signed char) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector unsigned char, (a1), vector unsigned char, (a2), int, (a3)), \
      ((vector unsigned char) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
__ch (__tern_args_eq (vector unsigned char, (a1), vector unsigned char, (a2), unsigned int, (a3)), \
      ((vector unsigned char) __builtin_altivec_vsldoi_4si ((vector signed int) (a1), (vector signed int) (a2), (const char) (a3))), \
    __altivec_link_error_invalid_argument ()))))))))))))))

#define vec_sll(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned short, (a2)), \
      ((vector signed int) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned char, (a2)), \
      ((vector signed int) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned short, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned char, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned int, (a2)), \
      ((vector signed short) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned char, (a2)), \
      ((vector signed short) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned int, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned int, (a2)), \
      ((vector signed char) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned short, (a2)), \
      ((vector signed char) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned int, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned short, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsl ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))))))))))))))

#define vec_slo(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector signed char, (a2)), \
      ((vector float) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector unsigned char, (a2)), \
      ((vector float) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed char, (a2)), \
      ((vector signed int) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned char, (a2)), \
      ((vector signed int) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed char, (a2)), \
      ((vector unsigned int) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned char, (a2)), \
      ((vector unsigned int) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed char, (a2)), \
      ((vector signed short) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned char, (a2)), \
      ((vector signed short) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vslo ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))))))))))

#define vec_splat(a1, a2) \
__ch (__bin_args_eq (vector signed char, ((a1)), int, ((a2))), \
      ((vector signed char) __builtin_altivec_vspltb ((vector signed char) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector signed char, ((a1)), unsigned int, ((a2))), \
      ((vector signed char) __builtin_altivec_vspltb ((vector signed char) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned char, ((a1)), int, ((a2))), \
      ((vector unsigned char) __builtin_altivec_vspltb ((vector signed char) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned char, ((a1)), unsigned int, ((a2))), \
      ((vector unsigned char) __builtin_altivec_vspltb ((vector signed char) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector signed short, ((a1)), int, ((a2))), \
      ((vector signed short) __builtin_altivec_vsplth ((vector signed short) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector signed short, ((a1)), unsigned int, ((a2))), \
      ((vector signed short) __builtin_altivec_vsplth ((vector signed short) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned short, ((a1)), int, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vsplth ((vector signed short) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned short, ((a1)), unsigned int, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vsplth ((vector signed short) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector float, ((a1)), int, ((a2))), \
      ((vector float) __builtin_altivec_vspltw ((vector signed int) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector float, ((a1)), unsigned int, ((a2))), \
      ((vector float) __builtin_altivec_vspltw ((vector signed int) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector signed int, ((a1)), int, ((a2))), \
      ((vector signed int) __builtin_altivec_vspltw ((vector signed int) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector signed int, ((a1)), unsigned int, ((a2))), \
      ((vector signed int) __builtin_altivec_vspltw ((vector signed int) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned int, ((a1)), int, ((a2))), \
      ((vector unsigned int) __builtin_altivec_vspltw ((vector signed int) (a1), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned int, ((a1)), unsigned int, ((a2))), \
      ((vector unsigned int) __builtin_altivec_vspltw ((vector signed int) (a1), (const char) ((a2)))), \
    __altivec_link_error_invalid_argument ()))))))))))))))

#define vec_vspltw(a1, a2) \
__ch (__bin_args_eq (vector float, ((a1)), int, ((a2))), \
      ((vector float) __builtin_altivec_vspltw ((vector signed int) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector float, ((a1)), unsigned int, ((a2))), \
      ((vector float) __builtin_altivec_vspltw ((vector signed int) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector signed int, ((a1)), int, ((a2))), \
      ((vector signed int) __builtin_altivec_vspltw ((vector signed int) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector signed int, ((a1)), unsigned int, ((a2))), \
      ((vector signed int) __builtin_altivec_vspltw ((vector signed int) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned int, ((a1)), int, ((a2))), \
      ((vector unsigned int) __builtin_altivec_vspltw ((vector signed int) (a1), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned int, ((a1)), unsigned int, ((a2))), \
      ((vector unsigned int) __builtin_altivec_vspltw ((vector signed int) (a1), (const char) ((a2)))), \
__altivec_link_error_invalid_argument ()))))))

#define vec_vsplth(a1, a2) \
__ch (__bin_args_eq (vector signed short, ((a1)), int, ((a2))), \
      ((vector signed short) __builtin_altivec_vsplth ((vector signed short) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector signed short, ((a1)), unsigned int, ((a2))), \
      ((vector signed short) __builtin_altivec_vsplth ((vector signed short) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned short, ((a1)), int, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vsplth ((vector signed short) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned short, ((a1)), unsigned int, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vsplth ((vector signed short) ((a1)), (const char) ((a2)))), \
__altivec_link_error_invalid_argument ()))))

#define vec_vspltb(a1, a2) \
__ch (__bin_args_eq (vector signed char, ((a1)), int, ((a2))), \
      ((vector signed char) __builtin_altivec_vspltb ((vector signed char) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector signed char, ((a1)), unsigned int, ((a2))), \
      ((vector signed char) __builtin_altivec_vspltb ((vector signed char) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned char, ((a1)), int, ((a2))), \
      ((vector unsigned char) __builtin_altivec_vspltb ((vector signed char) ((a1)), (const char) ((a2)))), \
__ch (__bin_args_eq (vector unsigned char, ((a1)), unsigned int, ((a2))), \
      ((vector unsigned char) __builtin_altivec_vspltb ((vector signed char) ((a1)), (const char) ((a2)))), \
__altivec_link_error_invalid_argument ()))))

#define vec_splat_s8(a1) __builtin_altivec_vspltisb ((a1))

#define vec_splat_s16(a1) __builtin_altivec_vspltish ((a1))

#define vec_splat_s32(a1) __builtin_altivec_vspltisw ((a1))

#define vec_splat_u8(a1) ((vector unsigned char) __builtin_altivec_vspltisb ((a1)))

#define vec_splat_u16(a1) ((vector unsigned short) __builtin_altivec_vspltish ((a1)))

#define vec_splat_u32(a1) ((vector unsigned int) __builtin_altivec_vspltisw ((a1)))

#define vec_sr(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vsrb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsrb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vsrh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsrh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vsrw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsrw ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_vsrw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vsrw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsrw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vsrh(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vsrh ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsrh ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vsrb(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vsrb ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsrb ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_sra(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vsrab ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsrab ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vsrah ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsrah ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vsraw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsraw ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_vsraw(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vsraw ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsraw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vsrah(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vsrah ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsrah ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_vsrab(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vsrab ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsrab ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ()))

#define vec_srl(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector signed int) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned short, (a2)), \
      ((vector signed int) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned char, (a2)), \
      ((vector signed int) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned short, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned char, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned int, (a2)), \
      ((vector signed short) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector signed short) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned char, (a2)), \
      ((vector signed short) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned int, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned int, (a2)), \
      ((vector signed char) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned short, (a2)), \
      ((vector signed char) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned int, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned short, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsr ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))))))))))))))

#define vec_sro(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector signed char, (a2)), \
      ((vector float) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector unsigned char, (a2)), \
      ((vector float) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed char, (a2)), \
      ((vector signed int) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned char, (a2)), \
      ((vector signed int) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed char, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned char, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed char, (a2)), \
      ((vector signed short) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned char, (a2)), \
      ((vector signed short) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned char, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector signed char) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsro ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))))))))))

#define vec_st(a1, a2, a3) \
  __builtin_altivec_stvx ((vector signed int) (a1), (a2), (a3))

#define vec_stl(a1, a2, a3) \
  __builtin_altivec_stvxl ((vector signed int) (a1), (a2), (a3))

#define vec_ste(a, b, c) \
__ch (__un_args_eq (vector unsigned char, (a)), \
      __builtin_altivec_stvebx ((vector signed char) (a), (b), (c)), \
__ch (__un_args_eq (vector signed char, (a)), \
      __builtin_altivec_stvebx ((vector signed char) (a), (b), (c)), \
__ch (__un_args_eq (vector unsigned short, (a)), \
     __builtin_altivec_stvehx ((vector signed short) (a), (b), (c)), \
__ch (__un_args_eq (vector signed short, (a)), \
     __builtin_altivec_stvehx ((vector signed short) (a), (b), (c)), \
__ch (__un_args_eq (vector unsigned int, (a)), \
     __builtin_altivec_stvewx ((vector signed int) (a), (b), (c)), \
__ch (__un_args_eq (vector signed int, (a)), \
     __builtin_altivec_stvewx ((vector signed int) (a), (b), (c)), \
__ch (__un_args_eq (vector float, (a)), \
     __builtin_altivec_stvewx ((vector signed int) (a), (b), (c)), \
     __altivec_link_error_invalid_argument ())))))))

#define vec_stvewx(a, b, c) \
__ch (__un_args_eq (vector unsigned int, (a)), \
     __builtin_altivec_stvewx ((vector signed int) (a), (b), (c)), \
__ch (__un_args_eq (vector signed int, (a)), \
     __builtin_altivec_stvewx ((vector signed int) (a), (b), (c)), \
__ch (__un_args_eq (vector float, (a)), \
     __builtin_altivec_stvewx ((vector signed int) (a), (b), (c)), \
__altivec_link_error_invalid_argument ())))

#define vec_stvehx(a, b, c) \
__ch (__un_args_eq (vector unsigned short, (a)), \
     __builtin_altivec_stvehx ((vector signed short) (a), (b), (c)), \
__ch (__un_args_eq (vector signed short, (a)), \
     __builtin_altivec_stvehx ((vector signed short) (a), (b), (c)), \
__altivec_link_error_invalid_argument ()))

#define vec_stvebx(a, b, c) \
__ch (__un_args_eq (vector unsigned char, (a)), \
      __builtin_altivec_stvebx ((vector signed char) (a), (b), (c)), \
__ch (__un_args_eq (vector signed char, (a)), \
      __builtin_altivec_stvebx ((vector signed char) (a), (b), (c)), \
__altivec_link_error_invalid_argument ()))

#define vec_sub(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vsububm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vsubuhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vsubuwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vsubfp ((vector float) (a1), (vector float) (a2))), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_vsubfp(a1, a2) \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      ((vector float) __builtin_altivec_vsubfp ((vector float) (a1), (vector float) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vsubuwm(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vsubuwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ()))))

#define vec_vsubuhm(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vsubuhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ()))))

#define vec_vsububm(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vsububm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububm ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububm ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ()))))

#define vec_subc(a1, a2) \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
  ((vector unsigned int) __builtin_altivec_vsubcuw ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_subs(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vsubsbs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vsubshs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vsubsws ((vector signed int) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ()))))))))))))

#define vec_vsubsws(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vsubsws ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vsubuws(a1, a2) \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_vsubshs(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      ((vector signed short) __builtin_altivec_vsubshs ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vsubuhs(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) (a1), (vector signed short) (a2))), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      ((vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) (a1), (vector signed short) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_vsubsbs(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      ((vector signed char) __builtin_altivec_vsubsbs ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vsububs(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububs ((vector signed char) (a1), (vector signed char) (a2))), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      ((vector unsigned char) __builtin_altivec_vsububs ((vector signed char) (a1), (vector signed char) (a2))), \
__altivec_link_error_invalid_argument ())))

#define vec_sum4s(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsum4ubs ((vector signed char) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vsum4sbs ((vector signed char) (a1), (vector signed int) (a2))), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vsum4shs ((vector signed short) (a1), (vector signed int) (a2))), \
    __altivec_link_error_invalid_argument ())))

#define vec_vsum4shs(a1, a2) \
__ch (__bin_args_eq (vector signed short, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vsum4shs ((vector signed short) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vsum4sbs(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector signed int, (a2)), \
      ((vector signed int) __builtin_altivec_vsum4sbs ((vector signed char) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_vsum4ubs(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned int, (a2)), \
      ((vector unsigned int) __builtin_altivec_vsum4ubs ((vector signed char) (a1), (vector signed int) (a2))), \
__altivec_link_error_invalid_argument ())

#define vec_sum2s(a1, a2) __builtin_altivec_vsum2sws ((a1), (a2))

#define vec_sums(a1, a2) __builtin_altivec_vsumsws ((a1), (a2))

#define vec_trunc(a1) __builtin_altivec_vrfiz ((a1))

#define vec_unpackh(a1) \
__ch (__un_args_eq (vector signed char, (a1)), \
      ((vector signed short) __builtin_altivec_vupkhsb ((vector signed char) (a1))), \
__ch (__un_args_eq (vector unsigned short, (a1)), \
      ((vector unsigned int) __builtin_altivec_vupkhpx ((vector signed short) (a1))), \
__ch (__un_args_eq (vector signed short, (a1)), \
      ((vector signed int) __builtin_altivec_vupkhsh ((vector signed short) (a1))), \
    __altivec_link_error_invalid_argument ())))

#define vec_vupkhsh(a1) \
__ch (__un_args_eq (vector signed short, (a1)), \
      ((vector signed int) __builtin_altivec_vupkhsh ((vector signed short) (a1))), \
__altivec_link_error_invalid_argument ())

#define vec_vupkhpx(a1) \
__ch (__un_args_eq (vector unsigned short, (a1)), \
      ((vector unsigned int) __builtin_altivec_vupkhpx ((vector signed short) (a1))), \
__altivec_link_error_invalid_argument ())

#define vec_vupkhsb(a1) \
__ch (__un_args_eq (vector signed char, (a1)), \
      ((vector signed short) __builtin_altivec_vupkhsb ((vector signed char) (a1))), \
__altivec_link_error_invalid_argument ())

#define vec_unpackl(a1) \
__ch (__un_args_eq (vector signed char, (a1)), \
      ((vector signed short) __builtin_altivec_vupklsb ((vector signed char) (a1))), \
__ch (__un_args_eq (vector unsigned short, (a1)), \
      ((vector unsigned int) __builtin_altivec_vupklpx ((vector signed short) (a1))), \
__ch (__un_args_eq (vector signed short, (a1)), \
      ((vector signed int) __builtin_altivec_vupklsh ((vector signed short) (a1))), \
    __altivec_link_error_invalid_argument ())))

#define vec_vupklsh(a1) \
__ch (__un_args_eq (vector signed short, (a1)), \
      ((vector signed int) __builtin_altivec_vupklsh ((vector signed short) (a1))), \
__altivec_link_error_invalid_argument ())

#define vec_vupklpx(a1) \
__ch (__un_args_eq (vector unsigned short, (a1)), \
      ((vector unsigned int) __builtin_altivec_vupklpx ((vector signed short) (a1))), \
__altivec_link_error_invalid_argument ())

#define vec_vupklsb(a1) \
__ch (__un_args_eq (vector signed char, (a1)), \
      ((vector signed short) __builtin_altivec_vupklsb ((vector signed char) (a1))), \
__altivec_link_error_invalid_argument ())

#define vec_xor(a1, a2) \
__ch (__bin_args_eq (vector float, ((a1)), vector float, ((a2))), \
      ((vector float) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector float, ((a1)), vector unsigned int, ((a2))), \
      ((vector float) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned int, ((a1)), vector float, ((a2))), \
      ((vector float) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector signed int, ((a1)), vector float, ((a2))), \
      ((vector float) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector float, ((a1)), vector signed int, ((a2))), \
      ((vector float) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector signed int, ((a1)), vector signed int, ((a2))), \
      ((vector signed int) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector signed int, ((a1)), vector unsigned int, ((a2))), \
      ((vector unsigned int) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned int, ((a1)), vector signed int, ((a2))), \
      ((vector unsigned int) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned int, ((a1)), vector unsigned int, ((a2))), \
      ((vector unsigned int) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned short, ((a1)), vector unsigned short, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector signed short, ((a1)), vector unsigned short, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned short, ((a1)), vector signed short, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned short, ((a1)), vector unsigned short, ((a2))), \
      ((vector signed short) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector signed short, ((a1)), vector signed short, ((a2))), \
      ((vector signed short) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector signed short, ((a1)), vector unsigned short, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned short, ((a1)), vector signed short, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned short, ((a1)), vector unsigned short, ((a2))), \
      ((vector unsigned short) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned char, ((a1)), vector unsigned char, ((a2))), \
      ((vector unsigned char) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector signed char, ((a1)), vector unsigned char, ((a2))), \
      ((vector unsigned char) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector signed char, ((a1)), vector signed char, ((a2))), \
      ((vector signed char) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned char, ((a1)), vector unsigned char, ((a2))), \
      ((vector signed char) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector signed char, ((a1)), vector unsigned char, ((a2))), \
      ((vector signed char) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
__ch (__bin_args_eq (vector unsigned char, ((a1)), vector signed char, ((a2))), \
      ((vector unsigned char) __builtin_altivec_vxor ((vector signed int) ((a1)), (vector signed int) ((a2)))), \
    __altivec_link_error_invalid_argument ())))))))))))))))))))))))

/* Predicates.  */

#define vec_all_eq(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_LT, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_LT, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_LT, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_LT, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_LT, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_LT, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_LT, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_LT, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_LT, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_LT, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_LT, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_LT, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpeqfp_p (__CR6_LT, (vector float) (a1), (vector float) (a2)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_ge(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtsb_p (__CR6_EQ, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtsh_p (__CR6_EQ, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtsw_p (__CR6_EQ, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpgefp_p (__CR6_LT, (vector float) (a1), (vector float) (a2)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_gt(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtsb_p (__CR6_LT, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtsh_p (__CR6_LT, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtsw_p (__CR6_LT, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpgtfp_p (__CR6_LT, (vector float) (a1), (vector float) (a2)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_in(a1, a2) __builtin_altivec_vcmpbfp_p (__CR6_EQ, (a1), (a2))

#define vec_all_le(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtsb_p (__CR6_EQ, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtsh_p (__CR6_EQ, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtsw_p (__CR6_EQ, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpgefp_p (__CR6_LT, (vector float) (a2), (vector float) (a1)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_lt(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtsb_p (__CR6_LT, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtsh_p (__CR6_LT, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtsw_p (__CR6_LT, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpgtfp_p (__CR6_LT, (vector float) (a2), (vector float) (a1)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_nan(a1) __builtin_altivec_vcmpeqfp_p (__CR6_EQ, (a1), (a1))

#define vec_all_ne(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_EQ, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_EQ, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_EQ, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_EQ, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_EQ, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_EQ, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_EQ, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_EQ, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_EQ, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_EQ, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_EQ, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_EQ, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpeqfp_p (__CR6_EQ, (vector float) (a1), (vector float) (a2)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_nge(a1, a2) __builtin_altivec_vcmpgefp_p (__CR6_EQ, (a1), (a2))

#define vec_all_ngt(a1, a2) __builtin_altivec_vcmpgtfp_p (__CR6_EQ, (a1), (a2))

#define vec_all_nle(a1, a2) __builtin_altivec_vcmpgefp_p (__CR6_EQ, (a2), (a1))

#define vec_all_nlt(a1, a2) __builtin_altivec_vcmpgtfp_p (__CR6_EQ, (a2), (a1))

#define vec_all_numeric(a1) __builtin_altivec_vcmpeqfp_p (__CR6_EQ, (a1), (a1))

#define vec_any_eq(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_EQ_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_EQ_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_EQ_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_EQ_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_EQ_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_EQ_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_EQ_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_EQ_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_EQ_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_EQ_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_EQ_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_EQ_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpeqfp_p (__CR6_EQ_REV, (vector float) (a1), (vector float) (a2)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_ge(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtsb_p (__CR6_LT_REV, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtsh_p (__CR6_LT_REV, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtsw_p (__CR6_LT_REV, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpgefp_p (__CR6_EQ_REV, (vector float) (a1), (vector float) (a2)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_gt(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtsb_p (__CR6_EQ_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtsh_p (__CR6_EQ_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtsw_p (__CR6_EQ_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpgtfp_p (__CR6_EQ_REV, (vector float) (a1), (vector float) (a2)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_le(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_LT_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtsb_p (__CR6_LT_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_LT_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtsh_p (__CR6_LT_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_LT_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtsw_p (__CR6_LT_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpgefp_p (__CR6_EQ_REV, (vector float) (a2), (vector float) (a1)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_lt(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpgtub_p (__CR6_EQ_REV, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpgtsb_p (__CR6_EQ_REV, (vector signed char) (a2), (vector signed char) (a1)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpgtuh_p (__CR6_EQ_REV, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpgtsh_p (__CR6_EQ_REV, (vector signed short) (a2), (vector signed short) (a1)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpgtuw_p (__CR6_EQ_REV, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpgtsw_p (__CR6_EQ_REV, (vector signed int) (a2), (vector signed int) (a1)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpgtfp_p (__CR6_EQ_REV, (vector float) (a2), (vector float) (a1)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_nan(a1) __builtin_altivec_vcmpeqfp_p (__CR6_LT_REV, a1, a1)

#define vec_any_ne(a1, a2) \
__ch (__bin_args_eq (vector signed char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_LT_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_LT_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector signed char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_LT_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector unsigned char, (a1), vector unsigned char, (a2)), \
      __builtin_altivec_vcmpequb_p (__CR6_LT_REV, (vector signed char) (a1), (vector signed char) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_LT_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_LT_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector signed short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_LT_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector unsigned short, (a1), vector unsigned short, (a2)), \
      __builtin_altivec_vcmpequh_p (__CR6_LT_REV, (vector signed short) (a1), (vector signed short) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_LT_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector signed int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_LT_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector signed int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_LT_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector unsigned int, (a1), vector unsigned int, (a2)), \
      __builtin_altivec_vcmpequw_p (__CR6_LT_REV, (vector signed int) (a1), (vector signed int) (a2)), \
__ch (__bin_args_eq (vector float, (a1), vector float, (a2)), \
      __builtin_altivec_vcmpeqfp_p (__CR6_LT_REV, (vector float) (a1), (vector float) (a2)), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_nge(a1, a2) __builtin_altivec_vcmpgefp_p (__CR6_LT_REV, (a1), (a2))

#define vec_any_ngt(a1, a2) __builtin_altivec_vcmpgtfp_p (__CR6_LT_REV, (a1), (a2))

#define vec_any_nle(a1, a2) __builtin_altivec_vcmpgefp_p (__CR6_LT_REV, (a2), (a1))

#define vec_any_nlt(a1, a2) __builtin_altivec_vcmpgtfp_p (__CR6_LT_REV, (a2), (a1))

#define vec_any_numeric(a1) __builtin_altivec_vcmpeqfp_p (__CR6_EQ_REV, (a1), (a1))

#define vec_any_out(a1, a2) __builtin_altivec_vcmpbfp_p (__CR6_EQ_REV, (a1), (a2))

#endif /* __cplusplus */

#endif /* _ALTIVEC_H */
