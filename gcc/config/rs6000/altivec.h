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

#define __ALTIVEC__ 1

#define __vector __attribute__((vector_size(16)))

/* Dummy prototype.  */
extern void __altivec_link_error_invalid_argument ();

/* You are allowed to undef this for C++ compatability.  */
#define vector __vector

/* Helper macros.  */

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

#ifdef __cplusplus

/* C++ stuff here.  */

#else /* not C++ */

/* Hairy macros that implement the AltiVec high-level programming
   interface for C.  */

#define vec_add(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vaddubm ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vadduhm ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vadduwm ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vaddfp ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_addc(a1, a2) __builtin_altivec_vaddcuw (a1, a2)

#define vec_adds(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vaddubs ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vaddsbs ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vadduhs ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vaddshs ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vadduws ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vadduws ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vadduws ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vaddsws ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))))))))

#define vec_and(a1, a2) \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector signed int, a2), \
      (vector float) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector float, a2), \
      (vector float) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vand ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))))

#define vec_andc(a1, a2) \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector signed int, a2), \
      (vector float) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector float, a2), \
      (vector float) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vandc ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))))

#define vec_avg(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vavgub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vavgsb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vavguh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vavgsh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vavguw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vavgsw ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_ceil(a1) __builtin_altivec_vrfip (a1)

#define vec_cmpb(a1, a2) __builtin_altivec_vcmpbfp (a1, a2)

#define vec_cmpeq(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vcmpequb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vcmpequb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vcmpequh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed short) __builtin_altivec_vcmpequh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpeqfp ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_cmpge(a1, a2) __builtin_altivec_vcmpgefp (a1, a2)

#define vec_cmpgt(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vcmpgtub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vcmpgtsb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed short) __builtin_altivec_vcmpgtuh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vcmpgtsh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgtfp ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_cmple(a1, a2) __builtin_altivec_vcmpgefp (a1, a2)

#define vec_cmplt(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vcmpgtub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vcmpgtsb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed short) __builtin_altivec_vcmpgtuh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vcmpgtsh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgtfp ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_ctf(a1, a2) \
__ch (__bin_args_eq (vector unsigned int, a1, const char, a2), \
      (vector float) __builtin_altivec_vcfux ((vector signed int) a1, (const char) a2), \
__ch (__bin_args_eq (vector signed int, a1, const char, a2), \
      (vector float) __builtin_altivec_vcfsx ((vector signed int) a1, (const char) a2), \
    __altivec_link_error_invalid_argument ()))

#define vec_cts(a1, a2) __builtin_altivec_vctsxs (a1, a2)

#define vec_ctu(a1, a2) __builtin_altivec_vctuxs (a1, a2)

#define vec_dss(a1) __builtin_altivec_dss (a1)

#define vec_dssall() __builtin_altivec_dssall ()

#define vec_dst(a1, a2, a3) __builtin_altivec_dst (a1, a2, a3)

#define vec_dstst(a1, a2, a3) __builtin_altivec_dstst (a1, a2, a3)

#define vec_dststt(a1, a2, a3) __builtin_altivec_dststt (a1, a2, a3)

#define vec_dstt(a1, a2, a3) __builtin_altivec_dstt (a1, a2, a3)

#define vec_expte(a1) __builtin_altivec_vexptefp (a1)

#define vec_floor(a1) __builtin_altivec_vrfim (a1)

#define vec_ld(a, b) \
__ch (__un_args_eq (vector unsigned char *, b), \
      (vector unsigned char) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (unsigned char *, b), \
      (vector unsigned char) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (vector signed char *, b), \
      (vector signed char) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (signed char *, b), \
      (vector signed char) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (vector unsigned short *, b), \
      (vector unsigned short) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (unsigned short *, b), \
      (vector unsigned short) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (vector signed short *, b), \
      (vector signed short) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (signed short *, b), \
      (vector signed short) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (vector unsigned int *, b), \
      (vector unsigned int) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (unsigned int *, b), \
      (vector unsigned int) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (vector signed int *, b), \
      (vector signed int) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (signed int *, b), \
      (vector signed int) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (vector float *, b), \
      (vector float) __builtin_altivec_lvx (a, b), \
__ch (__un_args_eq (float *, b), \
      (vector float) __builtin_altivec_lvx (a, b), \
__altivec_link_error_invalid_argument ()))))))))))))))

#define vec_lde(a, b) \
__ch (__un_args_eq (unsigned char *, b), \
      (vector unsigned char) __builtin_altivec_lvebx (a, b), \
__ch (__un_args_eq (signed char *, b), \
      (vector signed char) __builtin_altivec_lvebx (a, b), \
__ch (__un_args_eq (unsigned short *, b), \
      (vector unsigned short) __builtin_altivec_lvehx (a, b), \
__ch (__un_args_eq (signed short *, b), \
      (vector signed short) __builtin_altivec_lvehx (a, b), \
__ch (__un_args_eq (unsigned int *, b), \
      (vector unsigned int) __builtin_altivec_lvewx (a, b), \
__ch (__un_args_eq (signed int *, b), \
      (vector signed int) __builtin_altivec_lvewx (a, b), \
__altivec_link_error_invalid_argument ()))))))

#define vec_ldl(a, b) \
__ch (__un_args_eq (vector unsigned char *, b), \
      (vector unsigned char) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (unsigned char *, b), \
      (vector unsigned char) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (vector signed char *, b), \
      (vector signed char) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (signed char *, b), \
      (vector signed char) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (vector unsigned short *, b), \
      (vector unsigned short) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (unsigned short *, b), \
      (vector unsigned short) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (vector signed short *, b), \
      (vector signed short) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (signed short *, b), \
      (vector signed short) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (vector unsigned int *, b), \
      (vector unsigned int) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (unsigned int *, b), \
      (vector unsigned int) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (vector signed int *, b), \
      (vector signed int) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (signed int *, b), \
      (vector signed int) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (vector float *, b), \
      (vector float) __builtin_altivec_lvxl (a, b), \
__ch (__un_args_eq (float *, b), \
      (vector float) __builtin_altivec_lvxl (a, b), \
__altivec_link_error_invalid_argument ()))))))))))))))

#define vec_loge(a1) __builtin_altivec_vlogefp (a1)

#define vec_lvsl(a1, a2) __builtin_altivec_lvsl (a1, a2)

#define vec_lvsr(a1, a2) __builtin_altivec_lvsr (a1, a2)

#define vec_madd(a1, a2, a3) __builtin_altivec_vmaddfp (a1, a2, a3)

#define vec_madds(a1, a2, a3) __builtin_altivec_vmhaddshs (a1, a2, a3)

#define vec_max(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vmaxub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vmaxsb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vmaxuh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vmaxsh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vmaxuw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vmaxsw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vmaxfp ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_mergeh(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vmrghb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vmrghb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vmrghh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vmrghh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vmrghw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vmrghw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vmrghw ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_mergel(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vmrglb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vmrglb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vmrglh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vmrglh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vmrglw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vmrglw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vmrglw ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_mfvscr() __builtin_altivec_mfvscr ()

#define vec_min(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vminub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vminub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vminub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vminsb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vminuh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vminuh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vminuh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vminsh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vminuw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vminuw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vminuw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vminsw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vminfp ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_mladd(a1, a2, a3) \
__ch (__tern_args_eq (vector signed short, a1, vector signed short, a2, vector signed short, a3), \
      (vector signed short) __builtin_altivec_vmladduhm ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3), \
__ch (__tern_args_eq (vector signed short, a1, vector unsigned short, a2, vector unsigned short, a3), \
      (vector signed short) __builtin_altivec_vmladduhm ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3), \
__ch (__tern_args_eq (vector unsigned short, a1, vector signed short, a2, vector signed short, a3), \
      (vector signed short) __builtin_altivec_vmladduhm ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3), \
__ch (__tern_args_eq (vector unsigned short, a1, vector unsigned short, a2, vector unsigned short, a3), \
      (vector unsigned short) __builtin_altivec_vmladduhm ((vector signed short) a1, (vector signed short) a2, (vector signed short) a3), \
    __altivec_link_error_invalid_argument ()))))

#define vec_mradds(a1, a2, a3) __builtin_altivec_vmhraddshs (a1, a2, a3)

#define vec_msum(a1, a2, a3) \
__ch (__tern_args_eq (vector unsigned char, a1, vector unsigned char, a2, vector unsigned int, a3), \
      (vector unsigned int) __builtin_altivec_vmsumubm ((vector signed char) a1, (vector signed char) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector signed char, a1, vector unsigned char, a2, vector signed int, a3), \
      (vector signed int) __builtin_altivec_vmsummbm ((vector signed char) a1, (vector signed char) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector unsigned short, a1, vector unsigned short, a2, vector unsigned int, a3), \
      (vector unsigned int) __builtin_altivec_vmsumuhm ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector signed short, a1, vector signed short, a2, vector signed int, a3), \
      (vector signed int) __builtin_altivec_vmsumshm ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3), \
    __altivec_link_error_invalid_argument ()))))

#define vec_msums(a1, a2, a3) \
__ch (__tern_args_eq (vector unsigned short, a1, vector unsigned short, a2, vector unsigned int, a3), \
      (vector unsigned int) __builtin_altivec_vmsumuhs ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector signed short, a1, vector signed short, a2, vector signed int, a3), \
      (vector signed int) __builtin_altivec_vmsumshs ((vector signed short) a1, (vector signed short) a2, (vector signed int) a3), \
    __altivec_link_error_invalid_argument ()))

#define vec_mtvscr(a1) \
__ch (__un_args_eq (vector signed int, a1), \
      __builtin_altivec_mtvscr ((vector signed int) a1), \
__ch (__un_args_eq (vector unsigned int, a1), \
      __builtin_altivec_mtvscr ((vector signed int) a1), \
__ch (__un_args_eq (vector signed short, a1), \
      __builtin_altivec_mtvscr ((vector signed int) a1), \
__ch (__un_args_eq (vector unsigned short, a1), \
      __builtin_altivec_mtvscr ((vector signed int) a1), \
__ch (__un_args_eq (vector signed char, a1), \
      __builtin_altivec_mtvscr ((vector signed int) a1), \
__ch (__un_args_eq (vector unsigned char, a1), \
      __builtin_altivec_mtvscr ((vector signed int) a1), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_mule(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned short) __builtin_altivec_vmuleub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed short) __builtin_altivec_vmulesb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned int) __builtin_altivec_vmuleuh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vmulesh ((vector signed short) a1, (vector signed short) a2), \
    __altivec_link_error_invalid_argument ()))))

#define vec_mulo(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned short) __builtin_altivec_vmuloub ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed short) __builtin_altivec_vmulosb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned int) __builtin_altivec_vmulouh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vmulosh ((vector signed short) a1, (vector signed short) a2), \
    __altivec_link_error_invalid_argument ()))))

#define vec_nmsub(a1, a2, a3) __builtin_altivec_vnmsubfp (a1, a2, a3)

#define vec_nor(a1, a2) \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vnor ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_or(a1, a2) \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector signed int, a2), \
      (vector float) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector float, a2), \
      (vector float) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vor ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))))

#define vec_pack(a1, a2) \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed char) __builtin_altivec_vpkuhum ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned char) __builtin_altivec_vpkuhum ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed short) __builtin_altivec_vpkuwum ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned short) __builtin_altivec_vpkuwum ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))

#define vec_packpx(a1, a2) __builtin_altivec_vpkpx (a1, a2)

#define vec_packs(a1, a2) \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned char) __builtin_altivec_vpkuhus ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed char) __builtin_altivec_vpkshss ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned short) __builtin_altivec_vpkuwus ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed short) __builtin_altivec_vpkswss ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))

#define vec_packsu(a1, a2) \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned char) __builtin_altivec_vpkuhus ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector unsigned char) __builtin_altivec_vpkshus ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned short) __builtin_altivec_vpkuwus ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector unsigned short) __builtin_altivec_vpkswus ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))

#define vec_perm(a1, a2, a3, a4) \
__ch (__tern_args_eq (vector float, a1, vector float, a2, vector unsigned char, a3), \
      (vector float) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3), \
__ch (__tern_args_eq (vector signed int, a1, vector signed int, a2, vector unsigned char, a3), \
      (vector signed int) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3), \
__ch (__tern_args_eq (vector unsigned int, a1, vector unsigned int, a2, vector unsigned char, a3), \
      (vector unsigned int) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3), \
__ch (__tern_args_eq (vector signed short, a1, vector signed short, a2, vector unsigned char, a3), \
      (vector signed short) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3), \
__ch (__tern_args_eq (vector unsigned short, a1, vector unsigned short, a2, vector unsigned char, a3), \
      (vector unsigned short) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3), \
__ch (__tern_args_eq (vector signed char, a1, vector signed char, a2, vector unsigned char, a3), \
      (vector signed char) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3), \
__ch (__tern_args_eq (vector unsigned char, a1, vector unsigned char, a2, vector unsigned char, a3), \
      (vector unsigned char) __builtin_altivec_vperm_4si ((vector signed int) a1, (vector signed int) a2, (vector signed char) a3), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_re(a1) __builtin_altivec_vrefp (a1)

#define vec_rl(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vrlb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vrlb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed short) __builtin_altivec_vrlh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vrlh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vrlw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vrlw ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_round(a1) __builtin_altivec_vrfin (a1)

#define vec_rsqrte(a1) __builtin_altivec_vrsqrtefp (a1)

#define vec_sel(a1, a2, a3) \
__ch (__tern_args_eq (vector float, a1, vector float, a2, vector signed int, a3), \
      (vector float) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector float, a1, vector float, a2, vector unsigned int, a3), \
      (vector float) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector signed int, a1, vector signed int, a2, vector signed int, a3), \
      (vector signed int) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector signed int, a1, vector signed int, a2, vector unsigned int, a3), \
      (vector signed int) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector unsigned int, a1, vector unsigned int, a2, vector signed int, a3), \
      (vector unsigned int) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector unsigned int, a1, vector unsigned int, a2, vector unsigned int, a3), \
      (vector unsigned int) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector signed short, a1, vector signed short, a2, vector signed short, a3), \
      (vector signed short) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector signed short, a1, vector signed short, a2, vector unsigned short, a3), \
      (vector signed short) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector unsigned short, a1, vector unsigned short, a2, vector signed short, a3), \
      (vector unsigned short) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector unsigned short, a1, vector unsigned short, a2, vector unsigned short, a3), \
      (vector unsigned short) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector signed char, a1, vector signed char, a2, vector signed char, a3), \
      (vector signed char) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector signed char, a1, vector signed char, a2, vector unsigned char, a3), \
      (vector signed char) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector unsigned char, a1, vector unsigned char, a2, vector signed char, a3), \
      (vector unsigned char) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
__ch (__tern_args_eq (vector unsigned char, a1, vector unsigned char, a2, vector unsigned char, a3), \
      (vector unsigned char) __builtin_altivec_vsel_4si ((vector signed int) a1, (vector signed int) a2, (vector signed int) a3), \
    __altivec_link_error_invalid_argument ()))))))))))))))

#define vec_sl(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vslb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vslb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed short) __builtin_altivec_vslh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vslh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vslw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vslw ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_sld(a1, a2, a3) \
__ch (__tern_args_eq (vector float, a1, vector float, a2, const char, a3), \
      (vector float) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, (const char) a3), \
__ch (__tern_args_eq (vector signed int, a1, vector signed int, a2, const char, a3), \
      (vector signed int) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, (const char) a3), \
__ch (__tern_args_eq (vector unsigned int, a1, vector unsigned int, a2, const char, a3), \
      (vector unsigned int) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, (const char) a3), \
__ch (__tern_args_eq (vector signed short, a1, vector signed short, a2, const char, a3), \
      (vector signed short) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, (const char) a3), \
__ch (__tern_args_eq (vector unsigned short, a1, vector unsigned short, a2, const char, a3), \
      (vector unsigned short) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, (const char) a3), \
__ch (__tern_args_eq (vector signed char, a1, vector signed char, a2, const char, a3), \
      (vector signed char) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, (const char) a3), \
__ch (__tern_args_eq (vector unsigned char, a1, vector unsigned char, a2, const char, a3), \
      (vector unsigned char) __builtin_altivec_vsldoi_4si ((vector signed int) a1, (vector signed int) a2, (const char) a3), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_sll(a1, a2) \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned short, a2), \
      (vector unsigned int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned char, a2), \
      (vector unsigned int) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned int, a2), \
      (vector signed short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned char, a2), \
      (vector signed short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned int, a2), \
      (vector unsigned short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned char, a2), \
      (vector unsigned short) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned int, a2), \
      (vector signed char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned short, a2), \
      (vector signed char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned int, a2), \
      (vector unsigned char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned short, a2), \
      (vector unsigned char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vsl ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))))))))))))))

#define vec_slo(a1, a2) \
__ch (__bin_args_eq (vector float, a1, vector signed char, a2), \
      (vector float) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector unsigned char, a2), \
      (vector float) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed char, a2), \
      (vector unsigned int) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned char, a2), \
      (vector unsigned int) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed char, a2), \
      (vector signed short) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned char, a2), \
      (vector signed short) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed char, a2), \
      (vector unsigned short) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned char, a2), \
      (vector unsigned short) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vslo ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))))))))))

#define vec_splat(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, const char, a2), \
      (vector signed char) __builtin_altivec_vspltb ((vector signed char) a1, (const char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, const char, a2), \
      (vector unsigned char) __builtin_altivec_vspltb ((vector signed char) a1, (const char) a2), \
__ch (__bin_args_eq (vector signed short, a1, const char, a2), \
      (vector signed short) __builtin_altivec_vsplth ((vector signed short) a1, (const char) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, const char, a2), \
      (vector unsigned short) __builtin_altivec_vsplth ((vector signed short) a1, (const char) a2), \
__ch (__bin_args_eq (vector float, a1, const char, a2), \
      (vector float) __builtin_altivec_vspltw ((vector signed int) a1, (const char) a2), \
__ch (__bin_args_eq (vector signed int, a1, const char, a2), \
      (vector signed int) __builtin_altivec_vspltw ((vector signed int) a1, (const char) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, const char, a2), \
      (vector unsigned int) __builtin_altivec_vspltw ((vector signed int) a1, (const char) a2), \
    __altivec_link_error_invalid_argument ())))))))

#define vec_splat_s8(a1) __builtin_altivec_vspltisb (a1)

#define vec_splat_s16(a1) __builtin_altivec_vspltish (a1)

#define vec_splat_s32(a1) __builtin_altivec_vspltisw (a1)

#define vec_splat_u8(a1) __builtin_altivec_vspltisb (a1)

#define vec_splat_u16(a1) __builtin_altivec_vspltish (a1)

#define vec_splat_u32(a1) __builtin_altivec_vspltisw (a1)

#define vec_sr(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vsrb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vsrb ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed short) __builtin_altivec_vsrh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vsrh ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vsrw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vsrw ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_sra(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vsrab ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vsrab ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed short) __builtin_altivec_vsrah ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vsrah ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vsraw ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vsraw ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))

#define vec_srl(a1, a2) \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned short, a2), \
      (vector unsigned int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned char, a2), \
      (vector unsigned int) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned int, a2), \
      (vector signed short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned char, a2), \
      (vector signed short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned int, a2), \
      (vector unsigned short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned char, a2), \
      (vector unsigned short) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned int, a2), \
      (vector signed char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned short, a2), \
      (vector signed char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned int, a2), \
      (vector unsigned char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned short, a2), \
      (vector unsigned char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vsr ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))))))))))))))

#define vec_sro(a1, a2) \
__ch (__bin_args_eq (vector float, a1, vector signed char, a2), \
      (vector float) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector unsigned char, a2), \
      (vector float) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed char, a2), \
      (vector unsigned int) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned char, a2), \
      (vector unsigned int) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed char, a2), \
      (vector signed short) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned char, a2), \
      (vector signed short) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed char, a2), \
      (vector unsigned short) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned char, a2), \
      (vector unsigned short) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed char) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vsro ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))))))))))

#define vec_st(a1, a2, a3) \
  __builtin_altivec_stvx ((vector signed int) a1, a2, a3)

#define vec_stl(a1, a2, a3) \
  __builtin_altivec_stvxl ((vector signed int) a1, a2, a3)

#define vec_ste(a, b, c) \
__ch (__un_args_eq (vector unsigned char, a), \
      __builtin_altivec_stvebx ((vector signed char) a, b, c), \
__ch (__un_args_eq (vector signed char, a), \
      __builtin_altivec_stvebx ((vector signed char) a, b, c), \
__ch (__un_args_eq (vector unsigned short, a), \
     __builtin_altivec_stvehx ((vector signed short) a, b, c), \
__ch (__un_args_eq (vector signed short, a), \
     __builtin_altivec_stvehx ((vector signed short) a, b, c), \
__ch (__un_args_eq (vector unsigned int, a), \
     __builtin_altivec_stvewx ((vector signed int) a, b, c), \
__ch (__un_args_eq (vector signed int, a), \
     __builtin_altivec_stvewx ((vector signed int) a, b, c), \
__ch (__un_args_eq (vector float, a), \
     __builtin_altivec_stvewx ((vector signed int) a, b, c), \
     __altivec_link_error_invalid_argument ())))))))

#define vec_sub(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vsububm ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vsubuhm ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vsubuwm ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vsubfp ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_subc(a1, a2) __builtin_altivec_vsubcuw (a1, a2)

#define vec_subs(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vsububs ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vsububs ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vsububs ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vsubsbs ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vsubuhs ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vsubshs ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vsubuws ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vsubsws ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ()))))))))))))

#define vec_sum4s(a1, a2) \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vsum4ubs ((vector signed char) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vsum4sbs ((vector signed char) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vsum4shs ((vector signed short) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ())))

#define vec_sum2s(a1, a2) __builtin_altivec_vsum2sws (a1, a2)

#define vec_sums(a1, a2) __builtin_altivec_vsumsws (a1, a2)

#define vec_trunc(a1) __builtin_altivec_vrfiz (a1)

#define vec_unpackh(a1) \
__ch (__un_args_eq (vector signed char, a1), \
      (vector signed short) __builtin_altivec_vupkhsb ((vector signed char) a1), \
__ch (__un_args_eq (vector signed short, a1), \
      (vector unsigned int) __builtin_altivec_vupkhpx ((vector signed short) a1), \
__ch (__un_args_eq (vector signed short, a1), \
      (vector signed int) __builtin_altivec_vupkhsh ((vector signed short) a1), \
    __altivec_link_error_invalid_argument ())))

#define vec_unpackl(a1) \
__ch (__un_args_eq (vector signed char, a1), \
      (vector signed short) __builtin_altivec_vupklsb ((vector signed char) a1), \
__ch (__un_args_eq (vector signed short, a1), \
      (vector unsigned int) __builtin_altivec_vupklpx ((vector signed short) a1), \
__ch (__un_args_eq (vector signed short, a1), \
      (vector signed int) __builtin_altivec_vupklsh ((vector signed short) a1), \
    __altivec_link_error_invalid_argument ())))

#define vec_xor(a1, a2) \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector float) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector signed int, a2), \
      (vector float) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector float, a2), \
      (vector float) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector unsigned int) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector unsigned int) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed short) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector unsigned short) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector unsigned short) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed char) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector unsigned char) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector unsigned char) __builtin_altivec_vxor ((vector signed int) a1, (vector signed int) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))))

#define vec_all_eq(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpeqfp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_ge(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgefp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_gt(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgtfp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_in(a1, a2) __builtin_altivec_vcmpbfp_p (a1, a2)

#define vec_all_le(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgefp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_lt(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgtfp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_nan(a1) __builtin_altivec_vcmpeqfp_p (a1)

#define vec_all_ne(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpeqfp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_all_nge(a1, a2) __builtin_altivec_vcmpgefp_p (a1, a2)

#define vec_all_ngt(a1, a2) __builtin_altivec_vcmpgtfp_p (a1, a2)

#define vec_all_nle(a1, a2) __builtin_altivec_vcmpgefp_p (a1, a2)

#define vec_all_nlt(a1, a2) __builtin_altivec_vcmpgtfp_p (a1, a2)

#define vec_all_numeric(a1) __builtin_altivec_vcmpeqfp_p (a1)

#define vec_any_eq(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpeqfp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_ge(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgefp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_gt(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgtfp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_le(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgefp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_lt(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtub_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtuw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpgtsw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpgtfp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_nan(a1) __builtin_altivec_vcmpeqfp_p (a1)

#define vec_any_ne(a1, a2) \
__ch (__bin_args_eq (vector signed char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector signed char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector unsigned char, a1, vector unsigned char, a2), \
      (vector signed int) __builtin_altivec_vcmpequb_p ((vector signed char) a1, (vector signed char) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector signed short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector unsigned short, a1, vector unsigned short, a2), \
      (vector signed int) __builtin_altivec_vcmpequh_p ((vector signed short) a1, (vector signed short) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector signed int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector signed int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector unsigned int, a1, vector unsigned int, a2), \
      (vector signed int) __builtin_altivec_vcmpequw_p ((vector signed int) a1, (vector signed int) a2), \
__ch (__bin_args_eq (vector float, a1, vector float, a2), \
      (vector signed int) __builtin_altivec_vcmpeqfp_p ((vector float) a1, (vector float) a2), \
    __altivec_link_error_invalid_argument ())))))))))))))

#define vec_any_nge(a1, a2) __builtin_altivec_vcmpgefp_p (a1, a2)

#define vec_any_ngt(a1, a2) __builtin_altivec_vcmpgtfp_p (a1, a2)

#define vec_any_nle(a1, a2) __builtin_altivec_vcmpgefp_p (a1, a2)

#define vec_any_nlt(a1, a2) __builtin_altivec_vcmpgtfp_p (a1, a2)

#define vec_any_numeric(a1) __builtin_altivec_vcmpeqfp_p (a1)

#define vec_any_out(a1, a2) __builtin_altivec_vcmpbfp_p (a1, a2)

#endif /* __cplusplus */

#endif /* _ALTIVEC_H */
