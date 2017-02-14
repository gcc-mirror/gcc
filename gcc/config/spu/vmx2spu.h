/* Copyright (C) 2006-2017 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _VMX2SPU_H_
#define _VMX2SPU_H_	1

#ifdef __cplusplus

#ifdef __SPU__

#include <spu_intrinsics.h>
#include <vec_types.h>

/* This file maps generic VMX intrinsics and predicates to the SPU using 
 * overloaded C++ functions.
 */

/************************************************************************
 *                        INTRINSICS 
 ************************************************************************/

/* vec_abs (vector absolute value)
 * =======
 */
static inline vec_char16 vec_abs(vec_char16 a)
{
  vec_char16 minus_a;

  minus_a = (vec_char16)(spu_add((vec_ushort8)(spu_and(spu_xor(a, 0xFF), 0x7F)), 0x101));
  return (spu_sel(minus_a, a, spu_cmpgt(a, -1)));
}

static inline vec_short8 vec_abs(vec_short8 a)
{
  return (spu_sel(spu_sub(0, a), a, spu_cmpgt(a, -1)));
}

static inline vec_int4 vec_abs(vec_int4 a)
{
  return (spu_sel(spu_sub(0, a), a, spu_cmpgt(a, -1)));
}

static inline vec_float4 vec_abs(vec_float4 a)
{
  return ((vec_float4)(spu_rlmask(spu_sl((vec_uint4)(a), 1), -1)));
}

/* vec_abss (vector absolute value saturate)
 * ========
 */
static inline vec_char16 vec_abss(vec_char16 a)
{
  vec_char16 minus_a;

  minus_a = (vec_char16)spu_add((vec_short8)(spu_xor(a, -1)), 
				(vec_short8)(spu_and(spu_cmpgt((vec_uchar16)(a), 0x80), 1)));
  return (spu_sel(minus_a, a, spu_cmpgt(a, -1)));
}

static inline vec_short8 vec_abss(vec_short8 a)
{
  vec_short8 minus_a;

  minus_a = spu_add(spu_sub(0, a), (vec_short8)(spu_cmpeq(a, ((vec_short8){0x8000,0x8000,0x8000,0x8000,0x8000,0x8000,0x8000,0x8000}))));
  return (spu_sel(minus_a, a, spu_cmpgt(a, -1)));
}

static inline vec_int4 vec_abss(vec_int4 a)
{
  vec_int4 minus_a;

  minus_a = spu_add(spu_sub(0, a), (vec_int4)(spu_cmpeq(a, ((vec_int4){0x80000000,0x80000000,0x80000000,0x80000000}))));
  return (spu_sel(minus_a, a, spu_cmpgt(a, -1)));
}


/* vec_add (vector add)
 * =======
 */
static inline vec_uchar16 vec_add(vec_uchar16 a, vec_uchar16 b)
{
  return ((vec_uchar16)(spu_sel(spu_add((vec_ushort8)(a), (vec_ushort8)(b)),
				spu_add(spu_and((vec_ushort8)(a), 0xFF00), spu_and((vec_ushort8)(b), 0xFF00)),
				spu_splats((unsigned short)(0xFF00)))));
}

static inline vec_char16 vec_add(vec_char16 a, vec_char16 b)
{
  return ((vec_char16)vec_add((vec_uchar16)(a), (vec_uchar16)(b)));
}

static inline vec_char16 vec_add(vec_bchar16 a, vec_char16 b)
{
  return ((vec_char16)vec_add((vec_uchar16)(a), (vec_uchar16)(b)));
}

static inline vec_char16 vec_add(vec_char16 a, vec_bchar16 b)
{
  return ((vec_char16)vec_add((vec_uchar16)(a), (vec_uchar16)(b)));
}

static inline vec_ushort8 vec_add(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_add(a, b));
}

static inline vec_short8 vec_add(vec_short8 a, vec_short8 b)
{
  return (spu_add(a, b));
}

static inline vec_short8 vec_add(vec_bshort8 a, vec_short8 b)
{
  return (spu_add((vec_short8)(a), b));
}

static inline vec_short8 vec_add(vec_short8 a, vec_bshort8 b)
{
  return (spu_add(a, (vec_short8)(b)));
}

static inline vec_uint4 vec_add(vec_uint4 a, vec_uint4 b)
{
  return (spu_add(a, b));
}

static inline vec_int4 vec_add(vec_int4 a, vec_int4 b)
{
  return (spu_add(a, b));
}

static inline vec_int4 vec_add(vec_bint4 a, vec_int4 b)
{
  return (spu_add((vec_int4)(a), b));
}

static inline vec_int4 vec_add(vec_int4 a, vec_bint4 b)
{
  return (spu_add(a, (vec_int4)(b)));
}

static inline vec_float4 vec_add(vec_float4 a, vec_float4 b)
{
  return (spu_add(a, b));
}

/* vec_addc (vector add carryout unsigned word)
 * ========
 */
#define vec_addc(_a, _b)	spu_genc(_a, _b)

/* vec_adds (vector add saturated)
 * ========
 */
static inline vec_uchar16 vec_adds(vec_uchar16 a, vec_uchar16 b)
{
  vec_uchar16 s1, s2, s, d;

  s1 = (vec_uchar16)(spu_add(spu_rlmask((vec_ushort8)(a), -8), spu_rlmask((vec_ushort8)(b), -8)));
  s2 = (vec_uchar16)(spu_add(spu_and((vec_ushort8)(a), 0xFF), spu_and((vec_ushort8)(b), 0xFF)));
  s  = spu_shuffle(s1, s2, ((vec_uchar16){0, 16,  2, 18,  4, 20,  6, 22,
				          8, 24, 10, 26, 12, 28, 14, 30}));
  d  = spu_shuffle(s1, s2, ((vec_uchar16){1, 17,  3, 19,  5, 21,  7, 23,
				          9, 25, 11, 27, 13, 29, 15, 31}));
  return (spu_or(d, spu_cmpeq(s, 1)));
}

static inline vec_char16 vec_adds(vec_char16 a, vec_char16 b)
{
  vec_uchar16 s1, s2, s, d;

  s1 = (vec_uchar16)(spu_add(spu_rlmask((vec_ushort8)(a), -8), spu_rlmask((vec_ushort8)(b), -8)));
  s2 = (vec_uchar16)(spu_add(spu_and((vec_ushort8)(a), 0xFF), spu_and((vec_ushort8)(b), 0xFF)));
  s  = spu_shuffle(s1, s2, ((vec_uchar16){1, 17,  3, 19,  5, 21,  7, 23,
				          9, 25, 11, 27, 13, 29, 15, 31}));
  d = spu_sel(s, spu_splats((unsigned char)0x7F), spu_cmpgt(spu_and(s, (vec_uchar16)(spu_nor(a, b))), 0x7F));
  d = spu_sel(d, spu_splats((unsigned char)0x80), spu_cmpgt(spu_nor(s, (vec_uchar16)(spu_nand(a, b))), 0x7F));
  return ((vec_char16)(d));
}

static inline vec_char16 vec_adds(vec_bchar16 a, vec_char16 b)
{
  return (vec_adds((vec_char16)(a), b));
}

static inline vec_char16 vec_adds(vec_char16 a, vec_bchar16 b)
{
  return (vec_adds(a, (vec_char16)(b)));
}

static inline vec_ushort8 vec_adds(vec_ushort8 a, vec_ushort8 b)
{
  vec_ushort8 s, d;
  
  s = spu_add(a, b);
  d = spu_or(s, spu_rlmaska(spu_sel(spu_xor(s, -1), a, spu_eqv(a, b)), -15));
  return (d);
}

static inline vec_short8 vec_adds(vec_short8 a, vec_short8 b)
{
  vec_short8 s, d;
  
  s = spu_add(a, b);
  d = spu_sel(s, spu_splats((signed short)0x7FFF), (vec_ushort8)(spu_rlmaska(spu_and(s, spu_nor(a, b)), -15)));
  d = spu_sel(d, spu_splats((signed short)0x8000), (vec_ushort8)(spu_rlmaska(spu_nor(s, spu_nand(a, b)), -15)));
  return (d);
}

static inline vec_short8 vec_adds(vec_bshort8 a, vec_short8 b)
{
  return (vec_adds((vec_short8)(a), b));
}

static inline vec_short8 vec_adds(vec_short8 a, vec_bshort8 b)
{
  return (vec_adds(a, (vec_short8)(b)));
}

static inline vec_uint4 vec_adds(vec_uint4 a, vec_uint4 b)
{
  return (spu_or(spu_add(a, b), spu_rlmaska(spu_sl(spu_genc(a, b), 31), -31)));
}

static inline vec_int4 vec_adds(vec_int4 a, vec_int4 b)
{
  vec_int4 s, d;
  
  s = spu_add(a, b);
  d = spu_sel(s, spu_splats((signed int)0x7FFFFFFF), (vec_uint4)spu_rlmaska(spu_and(s, spu_nor(a, b)), -31));
  d = spu_sel(d, spu_splats((signed int)0x80000000), (vec_uint4)spu_rlmaska(spu_nor(s, spu_nand(a, b)), -31));
  return (d);
}

static inline vec_int4 vec_adds(vec_bint4 a, vec_int4 b)
{
  return (vec_adds((vec_int4)(a), b));
}

static inline vec_int4 vec_adds(vec_int4 a, vec_bint4 b)
{
  return (vec_adds(a, (vec_int4)(b)));
}

/* vec_and (vector logical and)
 * =======
 */
static inline vec_uchar16 vec_and(vec_uchar16 a, vec_uchar16 b)
{
  return (spu_and(a, b));
}

static inline vec_char16 vec_and(vec_char16 a, vec_char16 b)
{
  return (spu_and(a, b));
}

static inline vec_char16 vec_and(vec_bchar16 a, vec_char16 b)
{
  return (spu_and((vec_char16)(a), b));
}

static inline vec_char16 vec_and(vec_char16 a, vec_bchar16 b)
{
  return (spu_and(a, (vec_char16)(b)));
}

static inline vec_ushort8 vec_and(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_and(a, b));
}

static inline vec_short8 vec_and(vec_short8 a, vec_short8 b)
{
  return (spu_and(a, b));
}

static inline vec_short8 vec_and(vec_bshort8 a, vec_short8 b)
{
  return (spu_and((vec_short8)(a), b));
}

static inline vec_short8 vec_and(vec_short8 a, vec_bshort8 b)
{
  return (spu_and(a, (vec_short8)(b)));
}

static inline vec_uint4 vec_and(vec_uint4 a, vec_uint4 b)
{
  return (spu_and(a, b));
}

static inline vec_int4 vec_and(vec_int4 a, vec_int4 b)
{
  return (spu_and(a, b));
}

static inline vec_int4 vec_and(vec_bint4 a, vec_int4 b)
{
  return (spu_and((vec_int4)(a), b));
}

static inline vec_int4 vec_and(vec_int4 a, vec_bint4 b)
{
  return (spu_and(a, (vec_int4)(b)));
}

static inline vec_float4 vec_and(vec_float4 a, vec_float4 b)
{
  return (spu_and(a, b));
}

static inline vec_float4 vec_and(vec_bint4 a, vec_float4 b)
{
  return (spu_and((vec_float4)(a),b));
}

static inline vec_float4 vec_and(vec_float4 a, vec_bint4 b)
{
  return (spu_and(a, (vec_float4)(b)));
}


/* vec_andc (vector logical and with complement) 
 * ========
 */
static inline vec_uchar16 vec_andc(vec_uchar16 a, vec_uchar16 b)
{
  return (spu_andc(a, b));
}

static inline vec_char16 vec_andc(vec_char16 a, vec_char16 b)
{
  return (spu_andc(a, b));
}

static inline vec_char16 vec_andc(vec_bchar16 a, vec_char16 b)
{
  return (spu_andc((vec_char16)(a), b));
}

static inline vec_char16 vec_andc(vec_char16 a, vec_bchar16 b)
{
  return (spu_andc(a, (vec_char16)(b)));
}

static inline vec_ushort8 vec_andc(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_andc(a, b));
}

static inline vec_short8 vec_andc(vec_short8 a, vec_short8 b)
{
  return (spu_andc(a, b));
}

static inline vec_short8 vec_andc(vec_bshort8 a, vec_short8 b)
{
  return (spu_andc((vec_short8)(a), b));
}

static inline vec_short8 vec_andc(vec_short8 a, vec_bshort8 b)
{
  return (spu_andc(a, (vec_short8)(b)));
}

static inline vec_uint4 vec_andc(vec_uint4 a, vec_uint4 b)
{
  return (spu_andc(a, b));
}

static inline vec_int4 vec_andc(vec_int4 a, vec_int4 b)
{
  return (spu_andc(a, b));
}

static inline vec_int4 vec_andc(vec_bint4 a, vec_int4 b)
{
  return (spu_andc((vec_int4)(a), b));
}

static inline vec_int4 vec_andc(vec_int4 a, vec_bint4 b)
{
  return (spu_andc(a, (vec_int4)(b)));
}

static inline vec_float4 vec_andc(vec_float4 a, vec_float4 b)
{
  return (spu_andc(a,b));
}

static inline vec_float4 vec_andc(vec_bint4 a, vec_float4 b)
{
  return (spu_andc((vec_float4)(a),b));
}

static inline vec_float4 vec_andc(vec_float4 a, vec_bint4 b)
{
  return (spu_andc(a, (vec_float4)(b)));
}

/* vec_avg (vector average)
 * =======
 */
static inline vec_uchar16 vec_avg(vec_uchar16 a, vec_uchar16 b)
{
  return (spu_avg(a, b));
}

static inline vec_char16 vec_avg(vec_char16 a, vec_char16 b)
{
  return ((vec_char16)(spu_xor(spu_avg((vec_uchar16)(a), (vec_uchar16)(b)), 
			       (vec_uchar16)(spu_and(spu_xor(a,b), 0x80)))));
}

static inline vec_ushort8 vec_avg(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_add(spu_add(spu_rlmask(a, -1), spu_rlmask(b, -1)), 
		  spu_and(spu_or(a, b), 1)));
}

static inline vec_short8 vec_avg(vec_short8 a, vec_short8 b)
{
  return (spu_add(spu_add(spu_rlmaska(a, -1), spu_rlmaska(b, -1)), 
		  spu_and(spu_or(a, b), 1)));
}

static inline vec_uint4 vec_avg(vec_uint4 a, vec_uint4 b)
{
  return (spu_add(spu_add(spu_rlmask(a, -1), spu_rlmask(b, -1)), 
		  spu_and(spu_or(a, b), 1)));
}

static inline vec_int4 vec_avg(vec_int4 a, vec_int4 b)
{
  return (spu_add(spu_add(spu_rlmaska(a, -1), spu_rlmaska(b, -1)), 
		  spu_and(spu_or(a, b), 1)));
}


/* vec_ceil (vector ceiling)
 * ========
 */
static inline vec_float4 vec_ceil(vec_float4 a)
{
  vec_int4  exp;
  vec_uint4 mask;

  a = spu_add(a, (vec_float4)(spu_and(spu_xor(spu_rlmaska((vec_int4)a, -31), -1), spu_splats((signed int)0x3F7FFFFF))));
  exp = spu_sub(127, (vec_int4)(spu_and(spu_rlmask((vec_uint4)(a), -23), 0xFF)));
  mask = spu_rlmask(spu_splats((unsigned int)0x7FFFFF), exp);
  mask = spu_sel(spu_splats((unsigned int)0), mask, spu_cmpgt(exp, -31));
  mask = spu_or(mask, spu_xor((vec_uint4)(spu_rlmaska(spu_add(exp, -1), -31)), -1));

  return ((vec_float4)(spu_andc((vec_uint4)(a), mask)));
}


/* vec_cmpb (vector compare bounds floating-point)
 * ========
 */
static inline vec_int4 vec_cmpb(vec_float4 a, vec_float4 b)
{
  vec_int4 b0 = (vec_int4)spu_splats(0x80000000);
  vec_int4 b1 = (vec_int4)spu_splats(0x40000000);

  return (spu_or(spu_and((vec_int4)spu_cmpgt(a, b), b0), 
		 spu_and((vec_int4)spu_cmpgt(spu_xor(b, (vec_float4)(b0)), a), b1)));
}

/* vec_cmpeq (vector compare equal)
 * =========
 */
#define vec_cmpeq(_a, _b)	spu_cmpeq(_a, _b)


/* vec_cmpge (vector compare greater than or equal)
 * =========
 */
static inline vec_bint4 vec_cmpge(vec_float4 a, vec_float4 b)
{
  return (spu_xor(spu_cmpgt(b, a), -1));
}


/* vec_cmpgt (vector compare greater than)
 * =========
 */
#define vec_cmpgt(_a, _b)	spu_cmpgt(_a, _b)


/* vec_cmple (vector compare less than or equal)
 * =========
 */
static inline vec_bint4 vec_cmple(vec_float4 a, vec_float4 b)
{
  return (spu_xor(spu_cmpgt(a, b), -1));
}


/* vec_cmplt (vector compare less than)
 * =========
 */
#define vec_cmplt(_a, _b)	spu_cmpgt(_b, _a)


/* vec_ctf (vector convert from fixed-point word)
 * =======
 */
#define vec_ctf(_a, _b)		spu_convtf(_a, _b)


/* vec_cts (vector convert to signed fixed-point word saturate)
 * =======
 */
#define vec_cts(_a, _b)		spu_convts(_a, _b)


/* vec_ctu (vector convert to unsigned fixed-point word saturate)
 * =======
 */
#define vec_ctu(_a, _b)		spu_convtu(_a, _b)


/* vec_dss (vector data stream stop)
 * =======
 */
#define vec_dss(_a)


/* vec_dssall (vector data stream stop all)
 * ==========
 */
#define vec_dssall()


/* vec_dst (vector data stream touch)
 * =======
 */
#define vec_dst(_a, _b, _c)


/* vec_dstst (vector data stream touch for store)
 * =========
 */
#define vec_dstst(_a, _b, _c)


/* vec_dststt (vector data stream touch for store transient)
 * ==========
 */
#define vec_dststt(_a, _b, _c)


/* vec_dstt (vector data stream touch transient)
 * ========
 */
#define vec_dstt(_a, _b, _c)


/* vec_expte (vector is 2 raised tp the exponent estimate floating-point)
 * =========
 */
static inline vec_float4 vec_expte(vec_float4 a)
{
  vec_float4 bias, frac, exp;
  vec_int4 ia;

  bias = (vec_float4)(spu_andc(spu_splats((signed int)0x3F7FFFFF), spu_rlmaska((vec_int4)(a), -31)));
  ia   = spu_convts(spu_add(a, bias), 0);
  frac = spu_sub(spu_convtf(ia, 0), a);
  exp  = (vec_float4)(spu_sl(spu_add(ia, 127), 23));

  return (spu_mul(spu_madd(spu_madd(spu_splats(0.17157287f), frac, spu_splats(-0.67157287f)),
			   frac, spu_splats(1.0f)), exp));
}


/* vec_floor (vector floor)
 * =========
 */
static inline vec_float4 vec_floor(vec_float4 a)
{
  vec_int4  exp;
  vec_uint4 mask;

  a = spu_sub(a, (vec_float4)(spu_and(spu_rlmaska((vec_int4)a, -31), spu_splats((signed int)0x3F7FFFFF))));
  exp = spu_sub(127, (vec_int4)(spu_and(spu_rlmask((vec_uint4)(a), -23), 0xFF)));
  mask = spu_rlmask(spu_splats((unsigned int)0x7FFFFF), exp);
  mask = spu_sel(spu_splats((unsigned int)0), mask, spu_cmpgt(exp, -31));
  mask = spu_or(mask, spu_xor((vec_uint4)(spu_rlmaska(spu_add(exp, -1), -31)), -1));

  return ((vec_float4)(spu_andc((vec_uint4)(a), mask)));
}


/* vec_ld (vector load indexed)
 * ======
 */
static inline vec_uchar16 vec_ld(int a, unsigned char *b)
{
  return (*((vec_uchar16 *)(b+a)));
}

static inline vec_uchar16 vec_ld(int a, vec_uchar16 *b)
{
  return (*((vec_uchar16 *)((unsigned char *)(b)+a)));
}

static inline vec_char16 vec_ld(int a, signed char *b)
{
  return (*((vec_char16 *)(b+a)));
}

static inline vec_char16 vec_ld(int a, vec_char16 *b)
{
  return (*((vec_char16 *)((signed char *)(b)+a)));
}

static inline vec_ushort8 vec_ld(int a, unsigned short *b)
{
  return (*((vec_ushort8 *)((unsigned char *)(b)+a)));
}

static inline vec_ushort8 vec_ld(int a, vec_ushort8 *b)
{
  return (*((vec_ushort8 *)((unsigned char *)(b)+a)));
}

static inline vec_short8 vec_ld(int a, signed short *b)
{
  return (*((vec_short8 *)((unsigned char *)(b)+a)));
}

static inline vec_short8 vec_ld(int a, vec_short8 *b)
{
  return (*((vec_short8 *)((signed char *)(b)+a)));
}

static inline vec_uint4 vec_ld(int a, unsigned int *b)
{
  return (*((vec_uint4 *)((unsigned char *)(b)+a)));
}

static inline vec_uint4 vec_ld(int a, vec_uint4 *b)
{
  return (*((vec_uint4 *)((unsigned char *)(b)+a)));
}

static inline vec_int4 vec_ld(int a, signed int *b)
{
  return (*((vec_int4 *)((unsigned char *)(b)+a)));
}

static inline vec_int4 vec_ld(int a, vec_int4 *b)
{
  return (*((vec_int4 *)((signed char *)(b)+a)));
}

static inline vec_float4 vec_ld(int a, float *b)
{
  return (*((vec_float4 *)((unsigned char *)(b)+a)));
}

static inline vec_float4 vec_ld(int a, vec_float4 *b)
{
  return (*((vec_float4 *)((unsigned char *)(b)+a)));
}

/* vec_lde (vector load element indexed)
 * =======
 */
static inline vec_uchar16 vec_lde(int a, unsigned char *b)
{
  return (*((vec_uchar16 *)(b+a)));
}

static inline vec_char16 vec_lde(int a, signed char *b)
{
  return (*((vec_char16 *)(b+a)));
}

static inline vec_ushort8 vec_lde(int a, unsigned short *b)
{
  return (*((vec_ushort8 *)((unsigned char *)(b)+a)));
}

static inline vec_short8 vec_lde(int a, signed short *b)
{
  return (*((vec_short8 *)((unsigned char *)(b)+a)));
}


static inline vec_uint4 vec_lde(int a, unsigned int *b)
{
  return (*((vec_uint4 *)((unsigned char *)(b)+a)));
}

static inline vec_int4 vec_lde(int a, signed int *b)
{
  return (*((vec_int4 *)((unsigned char *)(b)+a)));
}


static inline vec_float4 vec_lde(int a, float *b)
{
  return (*((vec_float4 *)((unsigned char *)(b)+a)));
}

/* vec_ldl (vector load indexed LRU)
 * =======
 */
#define vec_ldl(_a, _b)		vec_ld(_a, _b)


/* vec_loge (vector log2 estimate floating-point)
 * ========
 */
static inline vec_float4 vec_loge(vec_float4 a)
{
  vec_int4 exp;
  vec_float4 frac;

  exp  = spu_add((vec_int4)(spu_and(spu_rlmask((vec_uint4)(a), -23), 0xFF)), -127);
  frac = (vec_float4)(spu_sub((vec_int4)(a), spu_sl(exp, 23)));

  return (spu_madd(spu_madd(spu_splats(-0.33985f), frac, spu_splats(2.01955f)), 
		   frac, spu_sub(spu_convtf(exp, 0), spu_splats(1.6797f))));
}


/* vec_lvsl (vector load for shift left)
 * ========
 */
static inline vec_uchar16 vec_lvsl(int a, unsigned char *b)
{
  return ((vec_uchar16)spu_add((vec_ushort8)(spu_splats((unsigned char)((a + (int)(b)) & 0xF))), 
			       ((vec_ushort8){0x0001, 0x0203, 0x0405, 0x0607,
				              0x0809, 0x0A0B, 0x0C0D, 0x0E0F})));
}

static inline vec_uchar16 vec_lvsl(int a, signed char *b)
{
  return (vec_lvsl(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsl(int a, unsigned short *b)
{
  return (vec_lvsl(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsl(int a, short *b)
{
  return (vec_lvsl(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsl(int a, unsigned int *b)
{
  return (vec_lvsl(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsl(int a, int *b)
{
  return (vec_lvsl(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsl(int a, float *b)
{
  return (vec_lvsl(a, (unsigned char *)b));
}


/* vec_lvsr (vector load for shift right)
 * ========
 */
static  inline vec_uchar16 vec_lvsr(int a, unsigned char *b)
{
  return ((vec_uchar16)(spu_sub(((vec_ushort8){0x1011, 0x1213, 0x1415, 0x1617,
				               0x1819, 0x1A1B, 0x1C1D, 0x1E1F}),
				(vec_ushort8)(spu_splats((unsigned char)((a + (int)(b)) & 0xF))))));
}

static inline vec_uchar16 vec_lvsr(int a, signed char *b)
{
  return (vec_lvsr(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsr(int a, unsigned short *b)
{
  return (vec_lvsr(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsr(int a, short *b)
{
  return (vec_lvsr(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsr(int a, unsigned int *b)
{
  return (vec_lvsr(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsr(int a, int *b)
{
  return (vec_lvsr(a, (unsigned char *)b));
}

static inline vec_uchar16 vec_lvsr(int a, float *b)
{
  return (vec_lvsr(a, (unsigned char *)b));
}

/* vec_madd (vector multiply add)
 * ========
 */
#define vec_madd(_a, _b, _c)	spu_madd(_a, _b, _c)



/* vec_madds (vector multiply add saturate)
 * =========
 */
static inline vec_short8 vec_madds(vec_short8 a, vec_short8 b, vec_short8 c)
{
  return (vec_adds(c, spu_sel((vec_short8)(spu_sl(spu_mule(a, b), 1)),
			      (vec_short8)(spu_rlmask(spu_mulo(a, b), -15)),
			      ((vec_ushort8){0, 0xFFFF, 0, 0xFFFF, 0, 0xFFFF, 0, 0xFFFF}))));
}

/* vec_max (vector maximum)
 * =======
 */
static inline vec_uchar16 vec_max(vec_uchar16 a, vec_uchar16 b)
{
  return (spu_sel(b, a, spu_cmpgt(a, b)));
}

static inline vec_char16 vec_max(vec_char16 a, vec_char16 b)
{
  return (spu_sel(b, a, spu_cmpgt(a, b)));
}

static inline vec_char16 vec_max(vec_bchar16 a, vec_char16 b)
{
  return (spu_sel(b, (vec_char16)(a), spu_cmpgt((vec_char16)(a), b)));
}

static inline vec_char16 vec_max(vec_char16 a, vec_bchar16 b)
{
  return (spu_sel((vec_char16)(b), a, spu_cmpgt(a, (vec_char16)(b))));
}

static inline vec_ushort8 vec_max(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_sel(b, a, spu_cmpgt(a, b)));
}

static inline vec_short8 vec_max(vec_short8 a, vec_short8 b)
{
  return (spu_sel(b, a, spu_cmpgt(a, b)));
}

static inline vec_short8 vec_max(vec_bshort8 a, vec_short8 b)
{
  return (spu_sel(b, (vec_short8)(a), spu_cmpgt((vec_short8)(a), b)));
}

static inline vec_short8 vec_max(vec_short8 a, vec_bshort8 b)
{
  return (spu_sel((vec_short8)(b), a, spu_cmpgt(a, (vec_short8)(b))));
}

static inline vec_uint4 vec_max(vec_uint4 a, vec_uint4 b)
{
  return (spu_sel(b, a, spu_cmpgt(a, b)));
}

static inline vec_int4 vec_max(vec_int4 a, vec_int4 b)
{
  return (spu_sel(b, a, spu_cmpgt(a, b)));
}

static inline vec_int4 vec_max(vec_bint4 a, vec_int4 b)
{
  return (spu_sel(b, (vec_int4)(a), spu_cmpgt((vec_int4)(a), b)));
}

static inline vec_int4 vec_max(vec_int4 a, vec_bint4 b)
{
  return (spu_sel((vec_int4)(b), a, spu_cmpgt(a, (vec_int4)(b))));
}

static inline vec_float4 vec_max(vec_float4 a, vec_float4 b)
{
  return (spu_sel(b, a, spu_cmpgt(a, b)));
}


/* vec_mergeh (vector merge high)
 * ==========
 */
static inline vec_uchar16 vec_mergeh(vec_uchar16 a, vec_uchar16 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){0, 16, 1, 17, 2, 18, 3, 19,
				           4, 20, 5, 21, 6, 22, 7, 23})));
}

static inline vec_char16 vec_mergeh(vec_char16 a, vec_char16 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){0, 16, 1, 17, 2, 18, 3, 19,
				           4, 20, 5, 21, 6, 22, 7, 23})));
}

static inline vec_ushort8 vec_mergeh(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){0, 1, 16, 17, 2, 3, 18, 19, 
				           4, 5, 20, 21, 6, 7, 22, 23})));
}

static inline vec_short8 vec_mergeh(vec_short8 a, vec_short8 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){0, 1, 16, 17, 2, 3, 18, 19, 
				           4, 5, 20, 21, 6, 7, 22, 23})));
}

static inline vec_uint4 vec_mergeh(vec_uint4 a, vec_uint4 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){0, 1, 2, 3, 16, 17, 18, 19, 
				           4, 5, 6, 7, 20, 21, 22, 23})));
}

static inline vec_int4 vec_mergeh(vec_int4 a, vec_int4 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){0, 1, 2, 3, 16, 17, 18, 19, 
				           4, 5, 6, 7, 20, 21, 22, 23})));
}

static inline vec_float4 vec_mergeh(vec_float4 a, vec_float4 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){0, 1, 2, 3, 16, 17, 18, 19, 
				           4, 5, 6, 7, 20, 21, 22, 23})));
}

/* vec_mergel (vector merge low)
 * ==========
 */
static inline vec_uchar16 vec_mergel(vec_uchar16 a, vec_uchar16 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){ 8, 24,  9, 25, 10, 26, 11, 27, 
				           12, 28, 13, 29, 14, 30, 15, 31})));
}

static inline vec_char16 vec_mergel(vec_char16 a, vec_char16 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){ 8, 24,  9, 25, 10, 26, 11, 27, 
				           12, 28, 13, 29, 14, 30, 15, 31})));
}

static inline vec_ushort8 vec_mergel(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){ 8,  9, 24, 25, 10, 11, 26, 27, 
				           12, 13, 28, 29, 14, 15, 30, 31})));
}

static inline vec_short8 vec_mergel(vec_short8 a, vec_short8 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){ 8,  9, 24, 25, 10, 11, 26, 27, 
				           12, 13, 28, 29, 14, 15, 30, 31})));
}

static inline vec_uint4 vec_mergel(vec_uint4 a, vec_uint4 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){ 8,  9, 10, 11, 24, 25, 26, 27, 
				           12, 13, 14, 15, 28, 29, 30, 31})));
}

static inline vec_int4 vec_mergel(vec_int4 a, vec_int4 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){ 8,  9, 10, 11, 24, 25, 26, 27, 
				           12, 13, 14, 15, 28, 29, 30, 31})));
}

static inline vec_float4 vec_mergel(vec_float4 a, vec_float4 b)
{
  return (spu_shuffle(a, b, ((vec_uchar16){ 8,  9, 10, 11, 24, 25, 26, 27, 
				           12, 13, 14, 15, 28, 29, 30, 31})));
}

/* vec_mfvscr (vector move from vector status and control register)
 * ==========
 */
static inline vec_ushort8 vec_mfvscr()
{
  return ((vec_ushort8)spu_splats(0)); 		/* not supported */
}


/* vec_min (vector minimum)
 * =======
 */
static inline vec_uchar16 vec_min(vec_uchar16 a, vec_uchar16 b)
{
  return (spu_sel(a, b, spu_cmpgt(a, b)));
}

static inline vec_char16 vec_min(vec_char16 a, vec_char16 b)
{
  return (spu_sel(a, b, spu_cmpgt(a, b)));
}

static inline vec_char16 vec_min(vec_bchar16 a, vec_char16 b)
{
  return (spu_sel((vec_char16)(a), b, spu_cmpgt((vec_char16)(a), b)));
}

static inline vec_char16 vec_min(vec_char16 a, vec_bchar16 b)
{
  return (spu_sel(a, (vec_char16)(b), spu_cmpgt(a, (vec_char16)(b))));
}

static inline vec_ushort8 vec_min(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_sel(a, b, spu_cmpgt(a, b)));
}

static inline vec_short8 vec_min(vec_short8 a, vec_short8 b)
{
  return (spu_sel(a, b, spu_cmpgt(a, b)));
}

static inline vec_short8 vec_min(vec_bshort8 a, vec_short8 b)
{
  return (spu_sel((vec_short8)(a), b, spu_cmpgt((vec_short8)(a), b)));
}

static inline vec_short8 vec_min(vec_short8 a, vec_bshort8 b)
{
  return (spu_sel(a, (vec_short8)(b), spu_cmpgt(a, (vec_short8)(b))));
}

static inline vec_uint4 vec_min(vec_uint4 a, vec_uint4 b)
{
  return (spu_sel(a, b, spu_cmpgt(a, b)));
}

static inline vec_int4 vec_min(vec_int4 a, vec_int4 b)
{
  return (spu_sel(a, b, spu_cmpgt(a, b)));
}

static inline vec_int4 vec_min(vec_bint4 a, vec_int4 b)
{
  return (spu_sel((vec_int4)(a), b, spu_cmpgt((vec_int4)(a), b)));
}

static inline vec_int4 vec_min(vec_int4 a, vec_bint4 b)
{
  return (spu_sel(a, (vec_int4)(b), spu_cmpgt(a, (vec_int4)(b))));
}

static inline vec_float4 vec_min(vec_float4 a, vec_float4 b)
{
  return (spu_sel(a, b, spu_cmpgt(a, b)));
}

/* vec_mladd (vector multiply low and add unsigned half word)
 * =========
 */
static inline vec_short8 vec_mladd(vec_short8 a, vec_short8 b, vec_short8 c)
{
  return ((vec_short8)(spu_shuffle(spu_madd((vec_short8)(spu_rl((vec_uint4)(a), -16)),
					    (vec_short8)(spu_rl((vec_uint4)(b), -16)),
					    (vec_int4)(spu_rl((vec_uint4)(c), -16))),
				   spu_madd(a, b, spu_extend(c)),
				   ((vec_uchar16){ 2,  3, 18, 19,  6,  7, 22, 23,
					          10, 11, 26, 27, 14, 15, 30, 31}))));
}


static inline vec_ushort8 vec_mladd(vec_ushort8 a, vec_ushort8 b, vec_ushort8 c)
{
  return ((vec_ushort8)(vec_mladd((vec_short8)(a), (vec_short8)(b), (vec_short8)(c))));
}

static inline vec_short8 vec_mladd(vec_ushort8 a, vec_short8 b, vec_short8 c)
{
  return (vec_mladd((vec_short8)(a), b, c));
}

static inline vec_short8 vec_mladd(vec_short8 a, vec_ushort8 b, vec_ushort8 c)
{
  return (vec_mladd(a, (vec_short8)(b), (vec_short8)(c)));
}


/* vec_mradds (vector multiply round and add saturate)
 * ==========
 */
static inline vec_short8 vec_mradds(vec_short8 a, vec_short8 b, vec_short8 c)
{
  vec_int4 round = (vec_int4)spu_splats(0x4000);
  vec_short8 hi, lo;

  hi = (vec_short8)(spu_sl(spu_add(spu_mule(a, b), round), 1));
  lo = (vec_short8)(spu_rlmask(spu_add(spu_mulo(a, b), round), -15));

  return (vec_adds(spu_sel(hi, lo, ((vec_ushort8){0, 0xFFFF, 0, 0xFFFF, 0, 0xFFFF, 0, 0xFFFF})), c));
}


/* vec_msum (vector multiply sum)
 * ========
 */
static inline vec_uint4 vec_msum(vec_uchar16 a, vec_uchar16 b, vec_uint4 c)
{
  vec_ushort8 a1, a2, b1, b2;
  vec_uint4 p1, p2;

  a1 = spu_and((vec_ushort8)(a), 0xFF);
  a2 = spu_rlmask((vec_ushort8)(a), -8);
  b1 = spu_and((vec_ushort8)(b), 0xFF);
  b2 = spu_rlmask((vec_ushort8)(b), -8);

  p1 = spu_add(spu_mulo(a1, b1), spu_mulo(spu_rlqwbyte(a1, -2), spu_rlqwbyte(b1, -2)));
  p2 = spu_add(spu_mulo(a2, b2), spu_mulo(spu_rlqwbyte(a2, -2), spu_rlqwbyte(b2, -2)));
  return (spu_add(p2, spu_add(p1, c)));
}

static inline vec_int4 vec_msum(vec_char16 a, vec_uchar16 b, vec_int4 c)
{
  vec_short8 a1, a2, b1, b2;
  vec_int4 p1, p2;

  a1 = (vec_short8)(spu_extend(a));
  a2 = spu_rlmaska((vec_short8)(a), -8);
  b1 = (vec_short8)(spu_and((vec_ushort8)(b), 0xFF));
  b2 = (vec_short8)spu_rlmask((vec_ushort8)(b), -8);

  p1 = spu_add(spu_mulo(a1, b1), spu_mulo(spu_rlqwbyte(a1, -2), spu_rlqwbyte(b1, -2)));
  p2 = spu_add(spu_mulo(a2, b2), spu_mulo(spu_rlqwbyte(a2, -2), spu_rlqwbyte(b2, -2)));
  return (spu_add(p2, spu_add(p1, c)));
}

static inline vec_uint4 vec_msum(vec_ushort8 a, vec_ushort8 b, vec_uint4 c)
{
  return (spu_add(spu_add(spu_mulo(a, b), spu_mulo(spu_rlqwbyte(a, -2), spu_rlqwbyte(b, -2))), c));
}

static inline vec_int4 vec_msum(vec_short8 a, vec_short8 b, vec_int4 c)
{
  return (spu_add(spu_add(spu_mulo(a, b), spu_mulo(spu_rlqwbyte(a, -2), spu_rlqwbyte(b, -2))), c));
}


/* vec_msums (vector multiply sum saturate)
 * ========
 */
static inline vec_uint4 vec_msums(vec_ushort8 a, vec_ushort8 b, vec_uint4 c)
{
  vec_uint4 p1, p2;

  p1 = spu_mulo(a, b);
  p2 = spu_mulo(spu_rlqwbyte(a, -2), spu_rlqwbyte(b, -2));

  return (vec_adds(p2, vec_adds(p1, c)));
}

static inline vec_int4 vec_msums(vec_short8 a, vec_short8 b, vec_int4 c)
{
  return (vec_adds(spu_add(spu_mulo(a, b), spu_mulo(spu_rlqwbyte(a, -2), spu_rlqwbyte(b, -2))), c));
}

/* vec_mtvscr (vector move to vector status and control register)
 * ==========
 */
#define vec_mtvscr(_a)		/* not supported */


/* vec_mule (vector multiply even)
 * ========
 */
static inline vec_ushort8 vec_mule(vec_uchar16 a, vec_uchar16 b)
{
  vec_ushort8 hi, lo;

  hi = (vec_ushort8)spu_mulo((vec_ushort8)(spu_rlmask((vec_uint4)(a), -24)), 
			     (vec_ushort8)(spu_rlmask((vec_uint4)(b), -24)));
  lo = (vec_ushort8)spu_mulo((vec_ushort8)(spu_rlmask((vec_short8)(a), -8)), 
			     (vec_ushort8)(spu_rlmask((vec_short8)(b), -8)));

  return (spu_shuffle(hi, lo, ((vec_uchar16){ 2,  3, 18, 19,  6,  7, 22, 23,
				             10, 11, 26, 27, 14, 15, 30, 31})));
}

static inline vec_short8 vec_mule(vec_char16 a, vec_char16 b)
{
  vec_short8 hi, lo;

  hi = (vec_short8)spu_mulo((vec_short8)(spu_rlmaska((vec_uint4)(a), -24)), 
			    (vec_short8)(spu_rlmaska((vec_uint4)(b), -24)));
  lo = (vec_short8)spu_mulo((vec_short8)(spu_rlmaska((vec_short8)(a), -8)), 
			    (vec_short8)(spu_rlmaska((vec_short8)(b), -8)));

  return (spu_shuffle(hi, lo, ((vec_uchar16){ 2,  3, 18, 19,  6,  7, 22, 23,
				             10, 11, 26, 27, 14, 15, 30, 31})));
}

static inline vec_uint4 vec_mule(vec_ushort8 a, vec_ushort8 b)
{
 return (spu_mulo((vec_ushort8)spu_rlmask((vec_uint4)(a), -16),
		  (vec_ushort8)spu_rlmask((vec_uint4)(b), -16)));
}


static inline vec_int4 vec_mule(vec_short8 a, vec_short8 b)
{
 return (spu_mulo((vec_short8)spu_rlmaska((vec_int4)(a), -16),
		  (vec_short8)spu_rlmaska((vec_int4)(b), -16)));
}


/* vec_mulo (vector multiply odd)
 * ========
 */
static inline vec_ushort8 vec_mulo(vec_uchar16 a, vec_uchar16 b)
{
  vec_ushort8 hi, lo;

  hi = (vec_ushort8)spu_mulo((vec_ushort8)(spu_and(spu_rlmask((vec_uint4)(a), -16), 0xFF)), 
			     (vec_ushort8)(spu_and(spu_rlmask((vec_uint4)(b), -16), 0xFF)));
  lo = (vec_ushort8)spu_mulo(spu_and((vec_ushort8)(a), 0xFF), spu_and((vec_ushort8)(b), 0xFF));

  return (spu_shuffle(hi, lo, ((vec_uchar16){ 2,  3, 18, 19,  6,  7, 22, 23,
				             10, 11, 26, 27, 14, 15, 30, 31})));
}

static inline vec_short8 vec_mulo(vec_char16 a, vec_char16 b)
{
  vec_short8 aa, bb, hi, lo;

  aa = spu_extend(a);
  bb = spu_extend(b);

  hi = (vec_short8)spu_mulo((vec_short8)(spu_rlmaska((vec_uint4)(aa), -16)), 
		(vec_short8)(spu_rlmaska((vec_uint4)(bb), -16)));
  lo = (vec_short8)spu_mulo(aa, bb);
  return (spu_shuffle(hi, lo, ((vec_uchar16){ 2,  3, 18, 19,  6,  7, 22, 23,
				             10, 11, 26, 27, 14, 15, 30, 31})));
}

static inline vec_uint4 vec_mulo(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_mulo(a, b));
}


static inline vec_int4 vec_mulo(vec_short8 a, vec_short8 b)
{
  return (spu_mulo(a, b));
}


/* vec_nmsub (vector negative multiply subtract)
 * =========
 */
#define vec_nmsub(_a, _b, _c)	spu_nmsub(_a, _b, _c)


/* vec_nor (vector logical nor)
 * =======
 */
#define vec_nor(_a, _b)		spu_nor(_a, _b)


/* vec_or (vector logical or)
 * ======
 */
static inline vec_uchar16 vec_or(vec_uchar16 a, vec_uchar16 b)
{
  return (spu_or(a, b));
}

static inline vec_char16 vec_or(vec_char16 a, vec_char16 b)
{
  return (spu_or(a, b));
}

static inline vec_char16 vec_or(vec_bchar16 a, vec_char16 b)
{
  return (spu_or((vec_char16)(a), b));
}

static inline vec_char16 vec_or(vec_char16 a, vec_bchar16 b)
{
  return (spu_or(a, (vec_char16)(b)));
}

static inline vec_ushort8 vec_or(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_or(a, b));
}

static inline vec_short8 vec_or(vec_short8 a, vec_short8 b)
{
  return (spu_or(a, b));
}

static inline vec_short8 vec_or(vec_bshort8 a, vec_short8 b)
{
  return (spu_or((vec_short8)(a), b));
}

static inline vec_short8 vec_or(vec_short8 a, vec_bshort8 b)
{
  return (spu_or(a, (vec_short8)(b)));
}

static inline vec_uint4 vec_or(vec_uint4 a, vec_uint4 b)
{
  return (spu_or(a, b));
}

static inline vec_int4 vec_or(vec_int4 a, vec_int4 b)
{
  return (spu_or(a, b));
}

static inline vec_int4 vec_or(vec_bint4 a, vec_int4 b)
{
  return (spu_or((vec_int4)(a), b));
}

static inline vec_int4 vec_or(vec_int4 a, vec_bint4 b)
{
  return (spu_or(a, (vec_int4)(b)));
}

static inline vec_float4 vec_or(vec_float4 a, vec_float4 b)
{
  return (spu_or(a, b));
}

static inline vec_float4 vec_or(vec_bint4 a, vec_float4 b)
{
  return (spu_or((vec_float4)(a),b));
}

static inline vec_float4 vec_or(vec_float4 a, vec_bint4 b)
{
  return (spu_or(a, (vec_float4)(b)));
}


/* vec_pack (vector pack)
 * ========
 */
static inline vec_uchar16 vec_pack(vec_ushort8 a, vec_ushort8 b)
{
  return ((vec_uchar16)spu_shuffle(a, b, ((vec_uchar16){ 1,  3,  5,  7,  9, 11, 13, 15,
					                17, 19, 21, 23, 25, 27, 29, 31})));
}

static inline vec_char16 vec_pack(vec_short8 a, vec_short8 b)
{
  return ((vec_char16)spu_shuffle(a, b, ((vec_uchar16){ 1,  3,  5,  7,  9, 11, 13, 15,
					               17, 19, 21, 23, 25, 27, 29, 31})));
}

static inline vec_ushort8 vec_pack(vec_uint4 a, vec_uint4 b)
{
  return ((vec_ushort8)spu_shuffle(a, b, ((vec_uchar16){ 2,  3,  6,  7, 10, 11, 14, 15,
					                18, 19, 22, 23, 26, 27, 30, 31})));
}

static inline vec_short8 vec_pack(vec_int4 a, vec_int4 b)
{
  return ((vec_short8)spu_shuffle(a, b, ((vec_uchar16){ 2,  3,  6,  7, 10, 11, 14, 15,
					               18, 19, 22, 23, 26, 27, 30, 31})));
}


/* vec_packpx (vector pack pixel)
 * ==========
 */
static inline vec_pixel8 vec_packpx(vec_uint4 a, vec_uint4 b)
{
  vec_uint4 x03FF = (vec_uint4)(spu_splats((unsigned short)0x03FF));
  vec_uint4 x001F = (vec_uint4)(spu_splats((unsigned short)0x001F));

  return ((vec_pixel8)(spu_shuffle(spu_sel(spu_sel(spu_sl(a, 7), spu_sl(a, 10), x03FF),
					   spu_sl(a, 13), x001F),
				   spu_sel(spu_sel(spu_sl(b, 7), spu_sl(b, 10), x03FF),
					   spu_sl(b, 13), x001F),
				   ((vec_uchar16){ 0,  1,  4,  5,   8,  9, 12, 13,
					          16, 17, 20, 21, 24, 25, 28, 29}))));
}


/* vec_packs (vector pack saturate)
 * =========
 */
static inline vec_uchar16 vec_packs(vec_ushort8 a, vec_ushort8 b)
{
  vec_ushort8 max = spu_splats((unsigned short)0x00FF);
  
  return ((vec_uchar16)(spu_shuffle(spu_sel(a, max, spu_cmpgt(a, 255)),
				    spu_sel(b, max, spu_cmpgt(b, 255)),
				    ((vec_uchar16){ 1,  3,  5,  7,  9, 11, 13, 15,
					           17, 19, 21, 23, 25, 27, 29, 31}))));
}

static inline vec_char16 vec_packs(vec_short8 a, vec_short8 b)
{
  vec_short8 max = spu_splats((signed short)0x007F);
  vec_short8 min = spu_splats((signed short)0xFF80);
  
  return ((vec_char16)(spu_shuffle(spu_sel(min, spu_sel(a, max, spu_cmpgt(a, 127)), spu_cmpgt(a, -128)),
				    spu_sel(min, spu_sel(b, max, spu_cmpgt(b, 127)), spu_cmpgt(b, -128)),
				   ((vec_uchar16){ 1,  3,  5,  7,  9, 11, 13, 15,
					          17, 19, 21, 23, 25, 27, 29, 31}))));
}

static inline vec_ushort8 vec_packs(vec_uint4 a, vec_uint4 b)
{
  vec_uint4 max = spu_splats((unsigned int)0x0000FFFF);
  
  return ((vec_ushort8)(spu_shuffle(spu_sel(a, max, spu_cmpgt(a, max)), 
				    spu_sel(b, max, spu_cmpgt(b, max)), 
				    ((vec_uchar16){ 2,  3,  6,  7, 10, 11, 14, 15,
					           18, 19, 22, 23, 26, 27, 30, 31}))));
}  

static inline vec_short8 vec_packs(vec_int4 a, vec_int4 b)
{
  vec_int4 max = spu_splats((signed int)0x00007FFF);
  vec_int4 min = spu_splats((signed int)0xFFFF8000);
  
  return ((vec_short8)(spu_shuffle(spu_sel(min, spu_sel(a, max, spu_cmpgt(a, max)), spu_cmpgt(a, min)),
				   spu_sel(min, spu_sel(b, max, spu_cmpgt(b, max)), spu_cmpgt(b, min)),
				   ((vec_uchar16){ 2,  3,  6,  7, 10, 11, 14, 15,
					          18, 19, 22, 23, 26, 27, 30, 31}))));
}  


/* vec_packsu (vector pack saturate unsigned)
 * ==========
 */
static inline vec_uchar16 vec_packsu(vec_ushort8 a, vec_ushort8 b)
{
  return ((vec_uchar16)spu_shuffle(spu_or(a, (vec_ushort8)(spu_cmpgt(a, 255))),
				   spu_or(b, (vec_ushort8)(spu_cmpgt(b, 255))),
				   ((vec_uchar16){ 1,  3,  5,  7,  9, 11, 13, 15,
					          17, 19, 21, 23, 25, 27, 29, 31})));
}

static inline vec_uchar16 vec_packsu(vec_short8 a, vec_short8 b)
{
  vec_short8 max = spu_splats((signed short)0x00FF);
  vec_short8 min = spu_splats((signed short)0x0000);
  
  return ((vec_uchar16)(spu_shuffle(spu_sel(min, spu_sel(a, max, spu_cmpgt(a, 255)), spu_cmpgt(a, 0)),
				    spu_sel(min, spu_sel(b, max, spu_cmpgt(b, 255)), spu_cmpgt(b, 0)),
				    ((vec_uchar16){ 1,  3,  5,  7,  9, 11, 13, 15,
					           17, 19, 21, 23, 25, 27, 29, 31}))));

  return (vec_packsu((vec_ushort8)(a), (vec_ushort8)(b)));
}

static inline vec_ushort8 vec_packsu(vec_uint4 a, vec_uint4 b)
{
  vec_uint4 max = spu_splats((unsigned int)0xFFFF);

  return ((vec_ushort8)spu_shuffle(spu_or(a, (vec_uint4)(spu_cmpgt(a, max))),
				   spu_or(b, (vec_uint4)(spu_cmpgt(b, max))),
				   ((vec_uchar16){ 2,  3,  6,  7, 10, 11, 14, 15,
					          18, 19, 22, 23, 26, 27, 30, 31})));
}

static inline vec_ushort8 vec_packsu(vec_int4 a, vec_int4 b)
{
  vec_int4 max = spu_splats((signed int)0x0000FFFF);
  vec_int4 min = spu_splats((signed int)0x00000000);
  
  return ((vec_ushort8)(spu_shuffle(spu_sel(min, spu_sel(a, max, spu_cmpgt(a, max)), spu_cmpgt(a, min)),
				    spu_sel(min, spu_sel(b, max, spu_cmpgt(b, max)), spu_cmpgt(b, min)),
				    ((vec_uchar16){ 2,  3,  6,  7, 10, 11, 14, 15,
					           18, 19, 22, 23, 26, 27, 30, 31}))));
}


/* vec_perm (vector permute)
 * ========
 */
static inline vec_uchar16 vec_perm(vec_uchar16 a, vec_uchar16 b, vec_uchar16 c)
{
  return (spu_shuffle(a, b, spu_and(c, 0x1F)));
}

static inline vec_char16 vec_perm(vec_char16 a, vec_char16 b, vec_uchar16 c)
{
  return ((vec_char16)(vec_perm((vec_uchar16)(a), (vec_uchar16)(b), c)));
}

static inline vec_ushort8 vec_perm(vec_ushort8 a, vec_ushort8 b, vec_uchar16 c)
{
  return ((vec_ushort8)(vec_perm((vec_uchar16)(a), (vec_uchar16)(b), c)));
}

static inline vec_short8 vec_perm(vec_short8 a, vec_short8 b, vec_uchar16 c)
{
  return ((vec_short8)(vec_perm((vec_uchar16)(a), (vec_uchar16)(b), c)));
}

static inline vec_uint4 vec_perm(vec_uint4 a, vec_uint4 b, vec_uchar16 c)
{
  return ((vec_uint4)(vec_perm((vec_uchar16)(a), (vec_uchar16)(b), c)));
}

static inline vec_int4 vec_perm(vec_int4 a, vec_int4 b, vec_uchar16 c)
{
  return ((vec_int4)(vec_perm((vec_uchar16)(a), (vec_uchar16)(b), c)));
}

static inline vec_float4 vec_perm(vec_float4 a, vec_float4 b, vec_uchar16 c)
{
  return ((vec_float4)(vec_perm((vec_uchar16)(a), (vec_uchar16)(b), c)));
}


/* vec_re (vector reciprocal estimate)
 * ======
 */
#define vec_re(_a)	spu_re(_a)


/* vec_rl (vector rotate left)
 * ======
 */
static inline vec_uchar16 vec_rl(vec_uchar16 a, vec_uchar16 b)
{
  vec_ushort8 r1, r2;

  r1 = spu_rl(spu_and((vec_ushort8)(a), 0xFF), (vec_short8)spu_and((vec_ushort8)(b), 7));
  r2 = spu_rl(spu_and((vec_ushort8)(a), -256), (vec_short8)spu_and(spu_rlmask((vec_ushort8)(b), -8), 7));
  return ((vec_uchar16)(spu_sel(spu_or(r2, spu_sl(r2, 8)), spu_or(r1, spu_rlmask(r1, -8)), spu_splats((unsigned short)0xFF))));
}

static inline vec_char16 vec_rl(vec_char16 a, vec_uchar16 b)
{
  return ((vec_char16)(vec_rl((vec_uchar16)(a), b)));
}

static inline vec_ushort8 vec_rl(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_rl(a, (vec_short8)(b)));
}

static inline vec_short8 vec_rl(vec_short8 a, vec_ushort8 b)
{
  return (spu_rl(a, (vec_short8)(b)));
}

static inline vec_uint4 vec_rl(vec_uint4 a, vec_uint4 b)
{
  return (spu_rl(a, (vec_int4)(b)));
}

static inline vec_int4 vec_rl(vec_int4 a, vec_uint4 b)
{
  return (spu_rl(a, (vec_int4)(b)));
}


/* vec_round (vector round)
 * =========
 */
static inline vec_float4 vec_round(vec_float4 a)
{
  vec_float4 s_half, s_one, d;
  vec_uint4 odd;
  vec_uint4 msb = spu_splats((unsigned int)0x80000000);
  vec_float4 half = spu_splats(0.5f);
  vec_int4 exp;
  vec_uint4 mask;

  s_half = (vec_float4)(spu_sel((vec_uint4)(half), (vec_uint4)(a), msb));
  a = spu_add(a, s_half);
  s_one = spu_add(s_half, s_half);
  exp  = spu_sub(127, (vec_int4)(spu_and(spu_rlmask((vec_uint4)(a), -23), 0xFF)));
  mask = spu_rlmask(spu_splats((unsigned int)0x7FFFFF), exp);
  mask = spu_sel(spu_splats((unsigned int)0), mask, spu_cmpgt(exp, -31));
  mask = spu_or(mask, spu_xor((vec_uint4)(spu_rlmaska(spu_add(exp, -1), -31)), -1));

  odd = spu_and((vec_uint4)(spu_convts(a, 0)), 1);
  s_one = spu_andc(s_one, (vec_float4)spu_cmpeq(mask, 0));
  s_one = spu_and(s_one, spu_and((vec_float4)spu_cmpeq(spu_and((vec_uint4)(a), mask), 0),
				 (vec_float4)spu_cmpeq(odd, 1)));
  d = spu_andc(a, (vec_float4)(mask));
  d = spu_sub(d, s_one);
  return (d);
}

/* vec_rsqrte (vector reciprocal square root estimate)
 * ==========
 */
#define vec_rsqrte(_a)	spu_rsqrte(_a)


/* vec_sel (vector select)
 * =======
 */
#define vec_sel(_a, _b, _c)	spu_sel(_a, _b, _c)


/* vec_sl (vector shift left)
 * ======
 */
static inline vec_uchar16 vec_sl(vec_uchar16 a, vec_uchar16 b)
{
  vec_ushort8 hi, lo;

  lo = spu_and(spu_sl((vec_ushort8)(a), spu_and((vec_ushort8)(b), 7)), 0xFF);
  hi = spu_sl(spu_and((vec_ushort8)(a), -256), spu_and(spu_rlmask((vec_ushort8)(b), -8), 7));

  return ((vec_uchar16)(spu_or(hi, lo)));
}

static inline vec_char16 vec_sl(vec_char16 a, vec_uchar16 b)
{
  return ((vec_char16)(vec_sl((vec_uchar16)(a), b)));
}

static inline vec_ushort8 vec_sl(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_sl(a, spu_and(b, 15)));
}

static inline vec_short8 vec_sl(vec_short8 a, vec_ushort8 b)
{
  return (spu_sl(a, spu_and((vec_ushort8)(b), 15)));
}

static inline vec_uint4 vec_sl(vec_uint4 a, vec_uint4 b)
{
  return (spu_sl(a, spu_and(b, 31)));
}

static inline vec_int4 vec_sl(vec_int4 a, vec_uint4 b)
{
  return (spu_sl(a, spu_and(b, 31)));
}


/* vec_sld (vector shift left double)
 * =======
 */
#define vec_sld(_a, _b, _c)	spu_shuffle(_a, _b, ((vec_uchar16){ 0+(_c),  1+(_c),  2+(_c),  3+(_c),  \
								    4+(_c),  5+(_c),  6+(_c),  7+(_c), 	\
								    8+(_c),  9+(_c), 10+(_c), 11+(_c), 	\
							           12+(_c), 13+(_c), 14+(_c), 15+(_c)}))


/* vec_sll (vector shift left long)
 * =======
 */
#define vec_sll(_a, _b)		spu_slqw(_a, spu_extract((vec_uint4)(_b), 0))


/* vec_slo (vector shift left by octet)
 * =======
 */
#define vec_slo(_a, _b)		spu_slqwbytebc(_a, spu_extract((vec_uint4)(_b), 3) & 0x7F)


/* vec_splat (vector splat)
 * =========
 */
#define vec_splat(_a, _b)	spu_splats(spu_extract(_a, _b))


/* vec_splat_s8 (vector splat signed byte)
 * ============
 */
#define vec_splat_s8(_a)	spu_splats((signed char)(_a))


/* vec_splat_s16 (vector splat signed half-word)
 * =============
 */
#define vec_splat_s16(_a)	spu_splats((signed short)(_a))


/* vec_splat_s32 (vector splat signed word)
 * =============
 */
#define vec_splat_s32(_a)	spu_splats((signed int)(_a))


/* vec_splat_u8 (vector splat unsigned byte)
 * ============
 */
#define vec_splat_u8(_a)	spu_splats((unsigned char)(_a))


/* vec_splat_u16 (vector splat unsigned half-word)
 * =============
 */
#define vec_splat_u16(_a)	spu_splats((unsigned short)(_a))


/* vec_splat_u32 (vector splat unsigned word)
 * =============
 */
#define vec_splat_u32(_a)	spu_splats((unsigned int)(_a))


/* vec_sr (vector shift right)
 * ======
 */
static inline vec_uchar16 vec_sr(vec_uchar16 a, vec_uchar16 b)
{
  vec_ushort8 hi, lo;

  lo = spu_rlmask(spu_and((vec_ushort8)(a), 0xFF), spu_sub(0, (vec_short8)(spu_and((vec_ushort8)(b), 7))));
  hi = spu_and(spu_rlmask((vec_ushort8)(a), spu_sub(0, (vec_short8)(spu_and(spu_rlmask((vec_ushort8)(b), -8), 7)))), -256);

  return ((vec_uchar16)(spu_or(hi, lo)));
}

static inline vec_char16 vec_sr(vec_char16 a, vec_uchar16 b)
{
  return ((vec_char16)(vec_sr((vec_uchar16)(a), b)));
}

static inline vec_ushort8 vec_sr(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_rlmask(a, spu_sub(0, (vec_short8)(spu_and(b, 15)))));
}

static inline vec_short8 vec_sr(vec_short8 a, vec_ushort8 b)
{
  return ((vec_short8)(vec_sr((vec_ushort8)(a), b)));
}

static inline vec_uint4 vec_sr(vec_uint4 a, vec_uint4 b)
{
  return (spu_rlmask(a, spu_sub(0, (vec_int4)(spu_and(b, 31)))));
}

static inline vec_int4 vec_sr(vec_int4 a, vec_uint4 b)
{
  return ((vec_int4)(vec_sr((vec_uint4)(a), b)));
}


/* vec_sra (vector shift right algebraic)
 * =======
 */
static inline vec_char16 vec_sra(vec_char16 a, vec_uchar16 b)
{
  vec_short8 hi, lo;

  lo = spu_and(spu_rlmaska(spu_extend(a), spu_sub(0, (vec_short8)(spu_and((vec_ushort8)(b), 7)))), 0xFF);
  hi = spu_and(spu_rlmaska((vec_short8)(a), spu_sub(0, (vec_short8)(spu_and(spu_rlmask((vec_ushort8)(b), -8), 7)))), -256);

  return ((vec_char16)(spu_or(hi, lo)));
}

static inline vec_uchar16 vec_sra(vec_uchar16 a, vec_uchar16 b)
{
  return ((vec_uchar16)(vec_sra((vec_char16)(a), b)));
}

static inline vec_short8 vec_sra(vec_short8 a, vec_ushort8 b)
{
  return (spu_rlmaska(a, spu_sub(0, (vec_short8)(spu_and(b, 15)))));
}

static inline vec_ushort8 vec_sra(vec_ushort8 a, vec_ushort8 b)
{
  return ((vec_ushort8)(vec_sra((vec_short8)(a), b)));
}

static inline vec_int4 vec_sra(vec_int4 a, vec_uint4 b)
{
  return (spu_rlmaska(a, spu_sub(0, (vec_int4)(spu_and(b, 31)))));
}

static inline vec_uint4 vec_sra(vec_uint4 a, vec_uint4 b)
{
  return ((vec_uint4)(vec_sra((vec_int4)(a), b)));
}


/* vec_srl (vector shift right long)
 * =======
 */
#define vec_srl(_a, _b)		spu_rlmaskqw(_a, 0-spu_extract((vec_int4)(_b), 3))


/* vec_sro (vector shift right by octet)
 * =======
 */
#define vec_sro(_a, _b)		spu_rlmaskqwbyte(_a, 0 - ((spu_extract((vec_int4)(_b), 3) >> 3) & 0xF))

/* vec_st (vector store indexed)
 * ======
 */
static inline void vec_st(vec_uchar16 a, int b, unsigned char *c)
{
  *((vec_uchar16 *)(c+b)) = a;
}

static inline void vec_st(vec_uchar16 a, int b, vec_uchar16 *c)
{
  *((vec_uchar16 *)((unsigned char *)(c)+b)) = a;
}

static inline void vec_st(vec_char16 a, int b, signed char *c)
{
  *((vec_char16 *)(c+b)) = a;
}

static inline void vec_st(vec_char16 a, int b, vec_char16 *c)
{
  *((vec_char16 *)((signed char *)(c)+b)) = a;
}

static inline void vec_st(vec_bchar16 a, int b, signed char *c)
{
  *((vec_bchar16 *)((signed char *)(c)+b)) = a;
}

static inline void vec_st(vec_ushort8 a, int b, unsigned short *c)
{
  *((vec_ushort8 *)((unsigned char *)(c)+b)) = a;
}

static inline void vec_st(vec_ushort8 a, int b, vec_ushort8 *c)
{
  *((vec_ushort8 *)((unsigned char *)(c)+b)) = a;
}

static inline void vec_st(vec_short8 a, int b, signed short *c)
{
  *((vec_short8 *)((unsigned char *)(c)+b)) = a;
}

static inline void vec_st(vec_short8 a, int b, vec_short8 *c)
{
  *((vec_short8 *)((signed char *)(c)+b)) = a;
}

static inline void vec_st(vec_bshort8 a, int b, signed short *c)
{
  *((vec_bshort8 *)((signed char *)(c)+b)) = a;
}

static inline void vec_st(vec_uint4 a, int b, unsigned int *c)
{
  *((vec_uint4 *)((unsigned char *)(c)+b)) = a;
}

static inline void vec_st(vec_uint4 a, int b, vec_uint4 *c)
{
  *((vec_uint4 *)((unsigned char *)(c)+b)) = a;
}

static inline void vec_st(vec_int4 a, int b, signed int *c)
{
  *((vec_int4 *)((unsigned char *)(c)+b)) = a;
}

static inline void vec_st(vec_int4 a, int b, vec_int4 *c)
{
  *((vec_int4 *)((signed char *)(c)+b)) = a;
}

static inline void vec_st(vec_bint4 a, int b, signed int *c)
{
  *((vec_bint4 *)((signed char *)(c)+b)) = a;
}

static inline void vec_st(vec_float4 a, int b, float *c)
{
  *((vec_float4 *)((unsigned char *)(c)+b)) = a;
}

static inline void vec_st(vec_float4 a, int b, vec_float4 *c)
{
  *((vec_float4 *)((unsigned char *)(c)+b)) = a;
}


/* vec_ste (vector store element indexed)
 * =======
 */
static inline void vec_ste(vec_uchar16 a, int b, unsigned char *c)
{
  unsigned char *ptr;

  ptr = c + b;
  *ptr = spu_extract(a, (int)(ptr) & 15);
}

static inline void vec_ste(vec_char16 a, int b, signed char *c)
{
  vec_ste((vec_uchar16)(a), b, (unsigned char *)(c));
}

static inline void vec_ste(vec_bchar16 a, int b, signed char *c)
{
  vec_ste((vec_uchar16)(a), b, (unsigned char *)(c));
}

static inline void vec_ste(vec_ushort8 a, int b, unsigned short *c)
{
  unsigned short *ptr;

  ptr = (unsigned short *)(((unsigned int)(c) + b) & ~1);
  *ptr = spu_extract(a, ((int)(ptr) >> 1) & 7);
}

static inline void vec_ste(vec_short8 a, int b, signed short *c)
{
  vec_ste((vec_ushort8)(a), b, (unsigned short *)(c));
}

static inline void vec_ste(vec_bshort8 a, int b, signed short *c)
{
  vec_ste((vec_ushort8)(a), b, (unsigned short *)(c));
}

static inline void vec_ste(vec_uint4 a, int b, unsigned int *c)
{
  unsigned int *ptr;

  ptr = (unsigned int *)(((unsigned int)(c) + b) & ~3);
  *ptr = spu_extract(a, ((int)(ptr) >> 2) & 3);
}

static inline void vec_ste(vec_int4 a, int b, signed int *c)
{
  vec_ste((vec_uint4)(a), b, (unsigned int *)(c));
}

static inline void vec_ste(vec_bint4 a, int b, signed int *c)
{
  vec_ste((vec_uint4)(a), b, (unsigned int *)(c));
}

static inline void vec_ste(vec_float4 a, int b, float *c)
{
  vec_ste((vec_uint4)(a), b, (unsigned int *)(c));
}


/* vec_stl (vector store indexed LRU)
 * =======
 */
#define vec_stl(_a, _b, _c)		vec_st(_a, _b, _c)


/* vec_sub (vector subtract)
 * =======
 */
static inline vec_uchar16 vec_sub(vec_uchar16 a, vec_uchar16 b)
{
  return ((vec_uchar16)(spu_sel(spu_sub((vec_ushort8)(a), (vec_ushort8)(b)),
				spu_sub(spu_and((vec_ushort8)(a), -256), spu_and((vec_ushort8)(b), -256)),
				spu_splats((unsigned short)0xFF00))));
}

static inline vec_char16 vec_sub(vec_char16 a, vec_char16 b)
{
  return ((vec_char16)(vec_sub((vec_uchar16)(a), (vec_uchar16)(b))));
}

static inline vec_char16 vec_sub(vec_bchar16 a, vec_char16 b)
{
  return ((vec_char16)(vec_sub((vec_uchar16)(a), (vec_uchar16)(b))));
}

static inline vec_char16 vec_sub(vec_char16 a, vec_bchar16 b)
{
  return ((vec_char16)(vec_sub((vec_uchar16)(a), (vec_uchar16)(b))));
}

static inline vec_ushort8 vec_sub(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_sub(a, b));
}

static inline vec_short8 vec_sub(vec_short8 a, vec_short8 b)
{
  return (spu_sub(a, b));
}

static inline vec_short8 vec_sub(vec_bshort8 a, vec_short8 b)
{
  return (spu_sub((vec_short8)(a), b));
}

static inline vec_short8 vec_sub(vec_short8 a, vec_bshort8 b)
{
  return (spu_sub(a, (vec_short8)(b)));
}

static inline vec_uint4 vec_sub(vec_uint4 a, vec_uint4 b)
{
  return (spu_sub(a, b));
}

static inline vec_int4 vec_sub(vec_int4 a, vec_int4 b)
{
  return (spu_sub(a, b));
}

static inline vec_int4 vec_sub(vec_bint4 a, vec_int4 b)
{
  return (spu_sub((vec_int4)(a), b));
}

static inline vec_int4 vec_sub(vec_int4 a, vec_bint4 b)
{
  return (spu_sub(a, (vec_int4)(b)));
}

static inline vec_float4 vec_sub(vec_float4 a, vec_float4 b)
{
  return (spu_sub(a, b));
}


/* vec_subc (vector subtract carryout)
 * ========
 */
#define vec_subc(_a, _b)	spu_genb(_a, _b)


/* vec_subs (vector subtract saturate)
 * ========
 */
static inline vec_uchar16 vec_subs(vec_uchar16 a, vec_uchar16 b)
{
  vec_ushort8 s1, s2;
  vec_uchar16 s, d;

  s1 = spu_sub(spu_rlmask((vec_ushort8)(a), -8), spu_rlmask((vec_ushort8)(b), -8));
  s2 = spu_sub(spu_and((vec_ushort8)(a), 0xFF), spu_and((vec_ushort8)(b), 0xFF));
  s  = (vec_uchar16)(spu_shuffle(s1, s2, ((vec_uchar16){0, 16,  2, 18,  4, 20,  6, 22,
					                8, 24, 10, 26, 12, 28, 14, 30})));
  d  = (vec_uchar16)(spu_shuffle(s1, s2, ((vec_uchar16){1, 17,  3, 19,  5, 21,  7, 23,
					                9, 25, 11, 27, 13, 29, 15, 31})));
  return (spu_andc(d, s));
}

static inline vec_char16 vec_subs(vec_char16 a, vec_char16 b)
{
  vec_ushort8 s1, s2;
  vec_uchar16 s, d;

  s1 = spu_sub(spu_rlmask((vec_ushort8)(a), -8), spu_rlmask((vec_ushort8)(b), -8));
  s2 = spu_sub(spu_and((vec_ushort8)(a), 0xFF), spu_and((vec_ushort8)(b), 0xFF));
  s  = (vec_uchar16)(spu_shuffle(s1, s2, ((vec_uchar16){1, 17,  3, 19,  5, 21,  7, 23,
					                9, 25, 11, 27, 13, 29, 15, 31})));
  d  = spu_sel(s, spu_splats((unsigned char)0x7F), spu_cmpgt(spu_nor((vec_uchar16)(a), spu_nand(s, (vec_uchar16)(b))), 0x7F));
  d  = spu_sel(d, spu_splats((unsigned char)0x80), spu_cmpgt(spu_and((vec_uchar16)(a), spu_nor(s, (vec_uchar16)(b))), 0x7F));
  
  return ((vec_char16)(d));
}

static inline vec_char16 vec_subs(vec_bchar16 a, vec_char16 b)
{
  return (vec_subs((vec_char16)(a), b));
}

static inline vec_char16 vec_subs(vec_char16 a, vec_bchar16 b)
{
  return (vec_subs(a, (vec_char16)(b)));
}

static inline vec_ushort8 vec_subs(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_andc(spu_sub(a, b), spu_cmpgt(b, a)));
}

static inline vec_short8 vec_subs(vec_short8 a, vec_short8 b)
{
  vec_short8 s;
  vec_short8 d;
  
  s = spu_sub(a, b);
  d = spu_sel(s, spu_splats((signed short)0x7FFF), (vec_ushort8)(spu_rlmaska(spu_nor(a, spu_nand(s, b)), -15)));
  d = spu_sel(d, spu_splats((signed short)0x8000), (vec_ushort8)(spu_rlmaska(spu_and(a, spu_nor(s, b)), -15)));

  return (d);
}

static inline vec_short8 vec_subs(vec_bshort8 a, vec_short8 b)
{
  return ((vec_short8)(vec_subs((vec_short8)(a), b)));
}

static inline vec_short8 vec_subs(vec_short8 a, vec_bshort8 b)
{
  return ((vec_short8)(vec_subs(a, (vec_short8)(b))));
}

static inline vec_uint4 vec_subs(vec_uint4 a, vec_uint4 b)
{
  return (spu_andc(spu_sub(a, b), spu_cmpgt(b, a)));
}

static inline vec_int4 vec_subs(vec_int4 a, vec_int4 b)
{
  vec_int4 s;
  vec_int4 d;
  
  s = spu_sub(a, b);
  d = spu_sel(s, spu_splats((signed int)0x7FFFFFFF), (vec_uint4)(spu_rlmaska(spu_nor(a, spu_nand(s, b)), -31)));
  d = spu_sel(d, spu_splats((signed int)0x80000000), (vec_uint4)(spu_rlmaska(spu_and(a, spu_nor(s, b)), -31)));

  return (d);
}

static inline vec_int4 vec_subs(vec_bint4 a, vec_int4 b)
{
  return ((vec_int4)(vec_subs((vec_int4)(a), b)));
}

static inline vec_int4 vec_subs(vec_int4 a, vec_bint4 b)
{
  return ((vec_int4)(vec_subs(a, (vec_int4)(b))));
}


/* vec_sum4s (vector sum across partial (1/4) saturated)
 * =========
 */
static inline vec_uint4 vec_sum4s(vec_uchar16 a, vec_uint4 b)
{
  vec_uint4 a01_23, a0123;

  a01_23 = (vec_uint4)(spu_add(spu_rlmask((vec_ushort8)(a), -8),
			       spu_and((vec_ushort8)(a), 0xFF)));
  a0123 = spu_add(spu_rlmask(a01_23, -16), spu_and(a01_23, 0x1FF));
  return (vec_adds(a0123, b));
}

static inline vec_int4 vec_sum4s(vec_char16 a, vec_int4 b)
{
  vec_int4 a01_23, a0123;

  a01_23 = (vec_int4)(spu_add(spu_rlmaska((vec_short8)(a), -8),
			      spu_extend(a)));
  a0123 = spu_add(spu_rlmaska(a01_23, -16), spu_extend((vec_short8)(a01_23)));
  return (vec_adds(a0123, b));
}

static inline vec_int4 vec_sum4s(vec_short8 a, vec_int4 b)
{
  vec_int4 a0123;

  a0123 = spu_add(spu_rlmaska((vec_int4)(a), -16), spu_extend(a));
  return (vec_adds(a0123, b));
}


/* vec_sum2s (vector sum across partial (1/2) saturated)
 * =========
 */
static inline vec_int4 vec_sum2s(vec_int4 a, vec_int4 b)
{
  vec_int4 c, d;
  vec_int4 sign1, sign2, sign3;
  vec_int4 carry, sum_l, sum_h, sat, sat_val;

  sign1 = spu_rlmaska(a, -31);
  sign2 = spu_rlmaska(b, -31);

  c = spu_rlqwbyte(a, -4);
  sign3 = spu_rlqwbyte(sign1, -4);
  
  carry = spu_genc(a, b);
  sum_l = spu_add(a, b);
  sum_h = spu_addx(sign1, sign2, carry);

  carry = spu_genc(sum_l, c);
  sum_l = spu_add(sum_l, c);
  sum_h = spu_addx(sum_h, sign3, carry);
  
  sign1 = spu_rlmaska(sum_l, -31);
  sign2 = spu_rlmaska(sum_h, -31);

  sat_val = spu_xor(sign2, spu_splats((signed int)0x7FFFFFFF));

  sat = spu_orc(spu_xor(sign1, sign2), (vec_int4)spu_cmpeq(sum_h, sign2));

  d = spu_and(spu_sel(sum_l, sat_val, (vec_uint4)(sat)), (vec_int4){0, -1, 0, -1});

  return (d);
}


/* vec_sums (vector sum saturated)
 * ========
 */
static inline vec_int4 vec_sums(vec_int4 a, vec_int4 b)
{
  vec_int4 a0, a1, a2, c0, c1, c2, d;
  vec_int4 sign_a, sign_b, sign_l, sign_h;
  vec_int4 sum_l, sum_h, sat, sat_val;

  sign_a = spu_rlmaska(a, -31);
  sign_b = spu_rlmaska(b, -31);

  a0 = spu_rlqwbyte(a, -12);
  a1 = spu_rlqwbyte(a, -8);
  a2 = spu_rlqwbyte(a, -4);

  sum_l = spu_add(a, b);
  sum_h = spu_addx(sign_a, sign_b, spu_genc(a, b));
  
  c2 = spu_genc(sum_l, a2);
  sum_l = spu_add(sum_l, a2);
  sum_h = spu_addx(sum_h, spu_rlqwbyte(sign_a, -4), c2);

  c1 = spu_genc(sum_l, a1);
  sum_l = spu_add(sum_l, a1);
  sum_h = spu_addx(sum_h, spu_rlqwbyte(sign_a, -8), c1);

  c0 = spu_genc(sum_l, a0);
  sum_l = spu_add(sum_l, a0);
  sum_h = spu_addx(sum_h, spu_rlqwbyte(sign_a, -12), c0);

  sign_l = spu_rlmaska(sum_l, -31);
  sign_h = spu_rlmaska(sum_h, -31);

  sat_val = spu_xor(sign_h, spu_splats((signed int)0x7FFFFFFF));

  sat = spu_orc(spu_xor(sign_l, sign_h), (vec_int4)spu_cmpeq(sum_h, sign_h));

  d = spu_and(spu_sel(sum_l, sat_val, (vec_uint4)(sat)), ((vec_int4){0, 0, 0, -1}));

  return (d);
}


/* vec_trunc (vector truncate) 
 * =========
 */
static inline vec_float4 vec_trunc(vec_float4 a)
{
  vec_int4 exp;
  vec_uint4 mask;

  exp  = spu_sub(127, (vec_int4)(spu_and(spu_rlmask((vec_uint4)(a), -23), 0xFF)));
  mask = spu_rlmask(spu_splats((unsigned int)0x7FFFFF), exp);
  mask = spu_sel(spu_splats((unsigned int)0), mask, spu_cmpgt(exp, -31));
  mask = spu_or(mask, spu_xor((vec_uint4)(spu_rlmaska(spu_add(exp, -1), -31)), -1));
  return (spu_andc(a, (vec_float4)(mask)));
}

/* vec_unpackh (vector unpack high element) 
 * ===========
 */
static inline vec_short8 vec_unpackh(vec_char16 a)
{
  return (spu_extend(spu_shuffle(a, a, ((vec_uchar16){0, 0, 1, 1, 2, 2, 3, 3, 
					              4, 4, 5, 5, 6, 6, 7, 7}))));
}

static inline vec_bshort8 vec_unpackh(vec_bchar16 a)
{
  return ((vec_bshort8)(vec_unpackh((vec_char16)(a))));
}

static inline vec_int4 vec_unpackh(vec_short8 a)
{
  return (spu_extend(spu_shuffle(a, a, ((vec_uchar16){0, 0, 0, 1, 0, 0, 2, 3, 
					              0, 0, 4, 5, 0, 0, 6, 7}))));
}

#ifdef SUPPORT_UNPACK_PIXEL
/* Due to type conflicts, unpacking of pixel types and boolean shorts
 * can not simultaneously be supported. By default, the boolean short is
 * supported.
 */
static inline vec_uint4 vec_unpackh(vec_pixel8 a)
{
  vec_ushort8 p1, p2;

  p1 = spu_shuffle((vec_ushort8)(spu_rlmaska((vec_short8)(a.p), -7)),
		   spu_and((vec_ushort8)(a.p), 0x1F),
		   ((vec_uchar16){ 0, 128, 128, 17,  2, 128, 128, 19,
			           4, 128, 128, 21,  6, 128, 128, 23}));
  p2 = spu_shuffle(spu_and(spu_rlmask((vec_ushort8)(a.p), -5), 0x1F),
		   spu_and(spu_rlmask((vec_ushort8)(a.p), -10), 0x1F),
		   ((vec_uchar16){ 128,  17, 1, 128, 128,  19, 3, 128,
			           128,  21, 5, 128, 128,  23, 7, 128}));
  return ((vec_uint4)(spu_or(p1, p2)));
}

#else

static inline vec_bint4 vec_unpackh(vec_bshort8 a)
{
  return ((vec_bint4)(vec_unpackh((vec_short8)(a))));
}
#endif





/* vec_unpackl (vector unpack low element) 
 * ===========
 */
static inline vec_short8 vec_unpackl(vec_char16 a)
{
  return (spu_extend(spu_shuffle(a, a, ((vec_uchar16){8, 8, 9, 9, 10, 10, 11, 11,
					              12, 12, 13, 13, 14, 14, 15, 15}))));
}

static inline vec_bshort8 vec_unpackl(vec_bchar16 a)
{
  return ((vec_bshort8)(vec_unpackl((vec_char16)(a))));
}


static inline vec_int4 vec_unpackl(vec_short8 a)
{
  return (spu_extend(spu_shuffle(a, a, ((vec_uchar16){0, 0, 8, 9, 0, 0, 10, 11, 
					              0, 0,12,13, 0, 0, 14, 15}))));
}


#ifdef SUPPORT_UNPACK_PIXEL
/* Due to type conflicts, unpacking of pixel types and boolean shorts
 * can not simultaneously be supported. By default, the boolean short is
 * supported.
 */
static inline vec_uint4 vec_unpackl(vec_pixel8 a)
{
  vec_ushort8 p1, p2;

  p1 = spu_shuffle((vec_ushort8)(spu_rlmaska((vec_short8)(a), -7)),
		   spu_and((vec_ushort8)(a), 0x1F),
		   ((vec_uchar16){ 8, 128, 128, 25,  10, 128, 128, 27,
			          12, 128, 128, 29,  14, 128, 128, 31}));
  p2 = spu_shuffle(spu_and(spu_rlmask((vec_ushort8)(a), -5), 0x1F),
		   spu_and(spu_rlmask((vec_ushort8)(a), -10), 0x1F),
		   ((vec_uchar16){ 128, 25,  9, 128, 128, 27, 11, 128,
			           128, 29, 13, 128, 128, 31, 15, 128}));
  return ((vec_uint4)(spu_or(p1, p2)));
}

#else

static inline vec_bint4 vec_unpackl(vec_bshort8 a)
{
  return ((vec_bint4)(vec_unpackl((vec_short8)(a))));

}
#endif



/* vec_xor (vector logical xor)
 * ======
 */
static inline vec_uchar16 vec_xor(vec_uchar16 a, vec_uchar16 b)
{
  return (spu_xor(a, b));
}

static inline vec_char16 vec_xor(vec_char16 a, vec_char16 b)
{
  return (spu_xor(a, b));
}

static inline vec_char16 vec_xor(vec_bchar16 a, vec_char16 b)
{
  return (spu_xor((vec_char16)(a), b));
}

static inline vec_char16 vec_xor(vec_char16 a, vec_bchar16 b)
{
  return (spu_xor(a, (vec_char16)(b)));
}

static inline vec_ushort8 vec_xor(vec_ushort8 a, vec_ushort8 b)
{
  return (spu_xor(a, b));
}

static inline vec_short8 vec_xor(vec_short8 a, vec_short8 b)
{
  return (spu_xor(a, b));
}

static inline vec_short8 vec_xor(vec_bshort8 a, vec_short8 b)
{
  return (spu_xor((vec_short8)(a), b));
}

static inline vec_short8 vec_xor(vec_short8 a, vec_bshort8 b)
{
  return (spu_xor(a, (vec_short8)(b)));
}

static inline vec_uint4 vec_xor(vec_uint4 a, vec_uint4 b)
{
  return (spu_xor(a, b));
}

static inline vec_int4 vec_xor(vec_int4 a, vec_int4 b)
{
  return (spu_xor(a, b));
}

static inline vec_int4 vec_xor(vec_bint4 a, vec_int4 b)
{
  return (spu_xor((vec_int4)(a), b));
}

static inline vec_int4 vec_xor(vec_int4 a, vec_bint4 b)
{
  return (spu_xor(a, (vec_int4)(b)));
}

static inline vec_float4 vec_xor(vec_float4 a, vec_float4 b)
{
  return (spu_xor(a, b));
}

static inline vec_float4 vec_xor(vec_bint4 a, vec_float4 b)
{
  return (spu_xor((vec_float4)(a),b));
}

static inline vec_float4 vec_xor(vec_float4 a, vec_bint4 b)
{
  return (spu_xor(a, (vec_float4)(b)));
}

/************************************************************************
 *                        PREDICATES
 ************************************************************************/

/* vec_all_eq (all elements equal)
 * ==========
 */
static inline int vec_all_eq(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0xFFFF));
}

static inline int vec_all_eq(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0xFFFF));
}

static inline int vec_all_eq(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_char16)(a), b)), 0) == 0xFFFF));
}

static inline int vec_all_eq(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_char16)(b))), 0) == 0xFFFF));
}

static inline int vec_all_eq(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0xFF));
}

static inline int vec_all_eq(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0xFF));
}

static inline int vec_all_eq(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_short8)(a), b)), 0) == 0xFF));
}

static inline int vec_all_eq(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_short8)(b))), 0) == 0xFF));
}

static inline int vec_all_eq(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0xF));
}

static inline int vec_all_eq(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0xF));
}

static inline int vec_all_eq(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_int4)(a), b)), 0) == 0xF));
}

static inline int vec_all_eq(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_int4)(b))), 0) == 0xF));
}

static inline int vec_all_eq(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0xF));
}


/* vec_all_ge (all elements greater than or equal)
 * ==========
 */
static inline int vec_all_ge(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0));
}

static inline int vec_all_ge(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0));
}

static inline  int vec_all_ge(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_char16)(a))), 0) == 0));
}

static inline int vec_all_ge(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_char16)(b), a)), 0) == 0));
}

static inline int vec_all_ge(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0));
}

static inline int vec_all_ge(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0));
}

static inline int vec_all_ge(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_short8)(a))), 0) == 0));
}

static inline int vec_all_ge(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_short8)(b), a)), 0) == 0));
}

static inline int vec_all_ge(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0));
}

static inline int vec_all_ge(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0));
}

static inline int vec_all_ge(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_int4)(a))), 0) == 0));
}

static inline int vec_all_ge(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_int4)(b), a)), 0) == 0));
}

static inline int vec_all_ge(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0));
}


/* vec_all_gt (all elements greater than)
 * ==========
 */
static inline int vec_all_gt(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0xFFFF));
}

static inline int vec_all_gt(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0xFFFF));
}

static inline int vec_all_gt(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_char16)(a), b)), 0) == 0xFFFF));
}

static inline int vec_all_gt(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_char16)(b))), 0) == 0xFFFF));
}

static inline int vec_all_gt(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0xFF));
}

static inline int vec_all_gt(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0xFF));
}

static inline int vec_all_gt(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_short8)(a), b)), 0) == 0xFF));
}

static inline int vec_all_gt(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_short8)(b))), 0) == 0xFF));
}

static inline int vec_all_gt(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0xF));
}

static inline int vec_all_gt(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0xF));
}

static inline int vec_all_gt(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_int4)(a), b)), 0) == 0xF));
}

static inline int vec_all_gt(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_int4)(b))), 0) == 0xF));
}

static inline int vec_all_gt(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0xF));
}


/* vec_all_in (all elements in bounds)
 * ==========
 */
static inline int vec_all_in(vec_float4 a, vec_float4 b)
{
  return (spu_extract(spu_gather(spu_nor(spu_cmpabsgt(a, b), (vec_uint4)(spu_rlmaska((vec_int4)(b), -31)))), 0) == 0xF);
}


/* vec_all_le (all elements less than or equal)
 * ==========
 */
static inline int vec_all_le(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0));
}

static inline int vec_all_le(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0));
}

static inline int vec_all_le(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_char16)(a), b)), 0) == 0));
}

static inline int vec_all_le(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_char16)(b))), 0) == 0));
}

static inline int vec_all_le(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0));
}

static inline int vec_all_le(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0));
}

static inline int vec_all_le(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_short8)(a), b)), 0) == 0));
}

static inline int vec_all_le(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_short8)(b))), 0) == 0));
}

static inline int vec_all_le(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0));
}

static inline int vec_all_le(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0));
}

static inline int vec_all_le(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_int4)(a), b)), 0) == 0));
}

static inline int vec_all_le(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_int4)(b))), 0) == 0));
}

static inline int vec_all_le(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0));
}


/* vec_all_lt (all elements less than)
 * ==========
 */
static inline int vec_all_lt(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0xFFFF));
}

static inline int vec_all_lt(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0xFFFF));
}

static inline int vec_all_lt(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_char16)(a))), 0) == 0xFFFF));
}

static inline int vec_all_lt(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_char16)(b), a)), 0) == 0xFFFF));
}

static inline int vec_all_lt(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0xFF));
}

static inline int vec_all_lt(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0xFF));
}

static inline int vec_all_lt(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_short8)(a))), 0) == 0xFF));
}

static inline int vec_all_lt(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_short8)(b), a)), 0) == 0xFF));
}

static inline int vec_all_lt(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0xF));
}

static inline int vec_all_lt(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0xF));
}

static inline int vec_all_lt(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_int4)(a))), 0) == 0xF));
}

static inline int vec_all_lt(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_int4)(b), a)), 0) == 0xF));
}

static inline int vec_all_lt(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0xF));
}


/* vec_all_nan (all elements not a number)
 * ===========
 */
static inline int vec_all_nan(vec_float4 a)
{
  vec_uint4 exp, man;
  vec_uint4 exp_mask = spu_splats((unsigned int)0x7F800000);

  exp = spu_and((vec_uint4)(a), exp_mask);
  man = spu_and((vec_uint4)(a), spu_splats((unsigned int)0x007FFFFF));
  return ((int)(spu_extract(spu_gather(spu_andc(spu_cmpeq(exp, exp_mask), 
						spu_cmpeq(man, 0))), 0) == 0xF));
}

#define vec_all_nan(_a)		(0)


/* vec_all_ne (all elements not equal)
 * ==========
 */
static inline int vec_all_ne(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0));
}

static inline int vec_all_ne(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0));
}

static inline int vec_all_ne(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_char16)(a), b)), 0) == 0));
}

static inline int vec_all_ne(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_char16)(b))), 0) == 0));
}

static inline int vec_all_ne(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0));
}

static inline int vec_all_ne(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0));
}

static inline int vec_all_ne(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_short8)(a), b)), 0) == 0));
}

static inline int vec_all_ne(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_short8)(b))), 0) == 0));
}

static inline int vec_all_ne(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0));
}

static inline int vec_all_ne(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0));
}

static inline int vec_all_ne(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_int4)(a), b)), 0) == 0));
}

static inline int vec_all_ne(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_int4)(b))), 0) == 0));
}

static inline int vec_all_ne(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) == 0));
}


/* vec_all_nge (all elements not greater than or equal)
 * ===========
 */
static inline int vec_all_nge(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0xF));
}


/* vec_all_ngt (all elements not greater than)
 * ===========
 */
static inline int vec_all_ngt(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0));
}


/* vec_all_nle (all elements not less than or equal)
 * ===========
 */
static inline int vec_all_nle(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) == 0xF));
}


/* vec_all_nlt (all elements not less than)
 * ===========
 */
static inline int vec_all_nlt(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) == 0));
}


/* vec_all_numeric (all elements numeric)
 * ===========
 */
static inline int vec_all_numeric(vec_float4 a)
{
  vec_uint4 exp;

  exp = spu_and(spu_rlmask((vec_uint4)(a), -23), 0xFF);
  return ((int)(spu_extract(spu_gather(spu_cmpeq(exp, 255)), 0) == 0));
}



/* vec_any_eq (any elements equal)
 * ==========
 */
static inline int vec_any_eq(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0));
}

static inline int vec_any_eq(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0));
}

static inline int vec_any_eq(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_char16)(a), b)), 0) != 0));
}

static inline int vec_any_eq(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_char16)(b))), 0) != 0));
}

static inline int vec_any_eq(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0));
}

static inline int vec_any_eq(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0));
}

static inline int vec_any_eq(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_short8)(a), b)), 0) != 0));
}

static inline int vec_any_eq(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_short8)(b))), 0) != 0));
}

static inline int vec_any_eq(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpeq(a, b), -31)), 0)));
}

static inline int vec_any_eq(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpeq(a, b), -31)), 0)));
}

static inline int vec_any_eq(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpeq((vec_int4)(a), b), -31)), 0)));
}

static inline int vec_any_eq(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpeq(a, (vec_int4)(b)), -31)), 0)));
}

static inline int vec_any_eq(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpeq(a, b), -31)), 0)));
}

/* vec_any_ge (any elements greater than or equal)
 * ==========
 */
static inline int vec_any_ge(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0xFFFF));
}

static inline int vec_any_ge(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0xFFFF));
}

static inline int vec_any_ge(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_char16)(a))), 0) != 0xFFFF));
}

static inline int vec_any_ge(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_char16)(b), a)), 0) != 0xFFFF));
}

static inline int vec_any_ge(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0xFF));
}

static inline int vec_any_ge(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0xFF));
}

static inline int vec_any_ge(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_short8)(a))), 0) != 0xFF));
}

static inline int vec_any_ge(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_short8)(b), a)), 0) != 0xFF));
}

static inline int vec_any_ge(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0xF));
}

static inline int vec_any_ge(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0xF));
}

static inline int vec_any_ge(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_int4)(a))), 0) != 0xF));
}

static inline int vec_any_ge(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_int4)(b), a)), 0) != 0xF));
}

static inline int vec_any_ge(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0xF));
}


/* vec_any_gt (any elements greater than)
 * ==========
 */
static inline int vec_any_gt(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0));
}

static inline int vec_any_gt(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0));
}

static inline int vec_any_gt(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_char16)(a), b)), 0) != 0));
}

static inline int vec_any_gt(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_char16)(b))), 0) != 0));
}

static inline int vec_any_gt(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0));
}

static inline int vec_any_gt(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0));
}

static inline int vec_any_gt(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_short8)(a), b)), 0) != 0));
}

static inline int vec_any_gt(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_short8)(b))), 0) != 0));
}


static inline int vec_any_gt(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt(a, b), -31)), 0)));
}

static inline int vec_any_gt(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt(a, b), -31)), 0)));
}

static inline int vec_any_gt(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt((vec_int4)(a), b), -31)), 0)));
}

static inline int vec_any_gt(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt(a, (vec_int4)(b)), -31)), 0)));
}

static inline int vec_any_gt(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt(a, b), -31)), 0)));
}

/* vec_any_le (any elements less than or equal)
 * ==========
 */
static inline int vec_any_le(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0xFFFF));
}

static inline int vec_any_le(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0xFFFF));
}

static inline int vec_any_le(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_char16)(a), b)), 0) != 0xFFFF));
}

static inline int vec_any_le(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_char16)(b))), 0) != 0xFFFF));
}

static inline int vec_any_le(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0xFF));
}

static inline int vec_any_le(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0xFF));
}

static inline int vec_any_le(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_short8)(a), b)), 0) != 0xFF));
}

static inline int vec_any_le(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_short8)(b))), 0) != 0xFF));
}

static inline int vec_any_le(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0xF));
}

static inline int vec_any_le(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0xF));
}

static inline int vec_any_le(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_int4)(a), b)), 0) != 0xF));
}

static inline int vec_any_le(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, (vec_int4)(b))), 0) != 0xF));
}

static inline int vec_any_le(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0xF));
}


/* vec_any_lt (any elements less than)
 * ==========
 */
static inline int vec_any_lt(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0));
}

static inline int vec_any_lt(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0));
}

static inline int vec_any_lt(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_char16)(a))), 0) != 0));
}

static inline int vec_any_lt(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_char16)(b), a)), 0) != 0));
}

static inline int vec_any_lt(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0));
}

static inline int vec_any_lt(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0));
}

static inline int vec_any_lt(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, (vec_short8)(a))), 0) != 0));
}

static inline int vec_any_lt(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt((vec_short8)(b), a)), 0) != 0));
}

static inline int vec_any_lt(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt(b, a), -31)), 0)));
}

static inline int vec_any_lt(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt(b, a), -31)), 0)));
}

static inline int vec_any_lt(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt(b, (vec_int4)(a)), -31)), 0)));
}

static inline int vec_any_lt(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt((vec_int4)(b), a), -31)), 0)));
}

static inline int vec_any_lt(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt(b, a), -31)), 0)));
}

/* vec_any_nan (any elements not a number)
 * ===========
 */
static inline int vec_any_nan(vec_float4 a)
{
  vec_uint4 exp, man;
  vec_uint4 exp_mask = spu_splats((unsigned int)0x7F800000);

  exp = spu_and((vec_uint4)(a), exp_mask);
  man = spu_and((vec_uint4)(a), spu_splats((unsigned int)0x007FFFFF));
  return ((int)(spu_extract(spu_gather(spu_andc(spu_cmpeq(exp, exp_mask), 
						spu_cmpeq(man, 0))), 0) != 0));
}


/* vec_any_ne (any elements not equal)
 * ==========
 */
static inline int vec_any_ne(vec_uchar16 a, vec_uchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0xFFFF));
}

static inline int vec_any_ne(vec_char16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0xFFFF));
}

static inline int vec_any_ne(vec_bchar16 a, vec_char16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_char16)(a), b)), 0) != 0xFFFF));
}

static inline int vec_any_ne(vec_char16 a, vec_bchar16 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_char16)(b))), 0) != 0xFFFF));
}

static inline int vec_any_ne(vec_ushort8 a, vec_ushort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0xFF));
}

static inline int vec_any_ne(vec_short8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0xFF));
}

static inline int vec_any_ne(vec_bshort8 a, vec_short8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_short8)(a), b)), 0) != 0xFF));
}

static inline int vec_any_ne(vec_short8 a, vec_bshort8 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_short8)(b))), 0) != 0xFF));
}

static inline int vec_any_ne(vec_uint4 a, vec_uint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0xF));
}

static inline int vec_any_ne(vec_int4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0xF));
}

static inline int vec_any_ne(vec_bint4 a, vec_int4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq((vec_int4)(a), b)), 0) != 0xF));
}

static inline int vec_any_ne(vec_int4 a, vec_bint4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, (vec_int4)(b))), 0) != 0xF));
}

static inline int vec_any_ne(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpeq(a, b)), 0) != 0xF));
}


/* vec_any_nge (any elements not greater than or equal)
 * ===========
 */
static inline int vec_any_nge(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_orx(spu_rlmask(spu_cmpgt(b, a), -31)), 0)));
}

/* vec_any_ngt (any elements not greater than)
 * ===========
 */
static inline int vec_any_ngt(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0xF));
}


/* vec_any_nle (any elements not less than or equal)
 * ===========
 */
static inline int vec_any_nle(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(a, b)), 0) != 0));
}


/* vec_any_nlt (any elements not less than)
 * ===========
 */
static inline int vec_any_nlt(vec_float4 a, vec_float4 b)
{
  return ((int)(spu_extract(spu_gather(spu_cmpgt(b, a)), 0) != 0xF));
}


/* vec_any_numeric (any elements numeric)
 * ===============
 */
static inline int vec_any_numeric(vec_float4 a)
{
  vec_uint4 exp;

  exp = spu_and(spu_rlmask((vec_uint4)(a), -23), 0xFF);
  return ((int)(spu_extract(spu_gather(spu_cmpeq(exp, 255)), 0) != 0xF));
}


/* vec_any_out (any elements out of bounds)
 * ===========
 */
static inline int vec_any_out(vec_float4 a, vec_float4 b)
{
  return (spu_extract(spu_gather(spu_nor(spu_cmpabsgt(a, b), (vec_uint4)(spu_rlmaska((vec_int4)(b), -31)))), 0) != 0xF);
}


/* CBE Language Extension Intrinsics
 */

/* vec_extract (extract element from vector)
 * ===========
 */
#define vec_extract(_a, _element)	spu_extract(_a, _element)


/* vec_insert (insert scalar into specified vector element)
 * ==========
 */
#define vec_insert(_a, _b, _element)	spu_insert(_a, _b, _element)

/* vec_lvlx (load vector left indexed)
 * ========
 */
static inline vec_uchar16 vec_lvlx(int a, unsigned char *b)
{
  vec_uchar16 *p = (vec_uchar16 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_uchar16 vec_lvlx(int a, vec_uchar16 *b)
{
  vec_uchar16 *p = (vec_uchar16 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_char16 vec_lvlx(int a, signed char *b)
{
  vec_char16 *p = (vec_char16 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_char16 vec_lvlx(int a, vec_char16 *b)
{
  vec_char16 *p = (vec_char16 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_ushort8 vec_lvlx(int a, unsigned short *b)
{
  vec_ushort8 *p = (vec_ushort8 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_ushort8 vec_lvlx(int a, vec_ushort8 *b)
{
  vec_ushort8 *p = (vec_ushort8 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_short8 vec_lvlx(int a, signed short *b)
{
  vec_short8 *p = (vec_short8 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_short8 vec_lvlx(int a, vec_short8 *b)
{
  vec_short8 *p = (vec_short8 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_uint4 vec_lvlx(int a, unsigned int *b)
{
  vec_uint4 *p = (vec_uint4 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_uint4 vec_lvlx(int a, vec_uint4 *b)
{
  vec_uint4 *p = (vec_uint4 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_int4 vec_lvlx(int a, signed int *b)
{
  vec_int4 *p = (vec_int4 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_int4 vec_lvlx(int a, vec_int4 *b)
{
  vec_int4 *p = (vec_int4 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_float4 vec_lvlx(int a, float *b)
{
  vec_float4 *p = (vec_float4 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}

static inline vec_float4 vec_lvlx(int a, vec_float4 *b)
{
  vec_float4 *p = (vec_float4 *)((unsigned char *)(b) + a);
  return(spu_slqwbyte(*p, (unsigned int)p & 0xF));
}


/* vec_lvlxl (load vector left indexed last)
 * =========
 */
#define vec_lvlxl(_a, _b)	vec_lvlx(_a, _b)


/* vec_lvrx (load vector right indexed)
 * ========
 */
static inline vec_uchar16 vec_lvrx(int a, unsigned char *b)
{
  vec_uchar16 *p = (vec_uchar16 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_uchar16 vec_lvrx(int a, vec_uchar16 *b)
{
  vec_uchar16 *p = (vec_uchar16 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_char16 vec_lvrx(int a, signed char *b)
{
  vec_char16 *p = (vec_char16 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_char16 vec_lvrx(int a, vec_char16 *b)
{
  vec_char16 *p = (vec_char16 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_ushort8 vec_lvrx(int a, unsigned short *b)
{
  vec_ushort8 *p = (vec_ushort8 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_ushort8 vec_lvrx(int a, vec_ushort8 *b)
{
  vec_ushort8 *p = (vec_ushort8 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_short8 vec_lvrx(int a, signed short *b)
{
  vec_short8 *p = (vec_short8 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_short8 vec_lvrx(int a, vec_short8 *b)
{
  vec_short8 *p = (vec_short8 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_uint4 vec_lvrx(int a, unsigned int *b)
{
  vec_uint4 *p = (vec_uint4 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_uint4 vec_lvrx(int a, vec_uint4 *b)
{
  vec_uint4 *p = (vec_uint4 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_int4 vec_lvrx(int a, signed int *b)
{
  vec_int4 *p = (vec_int4 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_int4 vec_lvrx(int a, vec_int4 *b)
{
  vec_int4 *p = (vec_int4 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_float4 vec_lvrx(int a, float *b)
{
  vec_float4 *p = (vec_float4 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}

static inline vec_float4 vec_lvrx(int a, vec_float4 *b)
{
  vec_float4 *p = (vec_float4 *)((unsigned char *)(b) + a);
  return(spu_rlmaskqwbyte(*p, ((int)p & 0xF)-16));
}



/* vec_lvrxl (load vector right indexed last)
 * =========
 */
#define vec_lvrxl(_a, _b)	vec_lvrx(_a, _b)


/* vec_promote (promote scalar to a vector)
 * ===========
 */
#define vec_promote(_a, _element)	spu_promote(_a, _element)


/* vec_splats (splat scalar to a vector)
 * ==========
 */
#define vec_splats(_a)	spu_splats(_a)


/* vec_stvlx (store vector left indexed)
 * =========
 */
static inline void vec_stvlx(vec_uchar16 a, int b, unsigned char *c)
{
  int shift;
  vec_uchar16 *p = (vec_uchar16 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned char)0xFF), shift));
}

static inline void vec_stvlx(vec_uchar16 a, int b, vec_uchar16 *c)
{
  int shift;
  vec_uchar16 *p = (vec_uchar16 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned char)0xFF), shift));
}

static inline void vec_stvlx(vec_char16 a, int b, signed char *c)
{
  int shift;
  vec_char16 *p = (vec_char16 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned char)0xFF), shift));
}

static inline void vec_stvlx(vec_char16 a, int b, vec_char16 *c)
{
  int shift;
  vec_char16 *p = (vec_char16 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned char)0xFF), shift));
}

static inline void vec_stvlx(vec_ushort8 a, int b, unsigned short *c)
{
  int shift;
  vec_ushort8 *p = (vec_ushort8 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned short)0xFFFF), shift));
}

static inline void vec_stvlx(vec_ushort8 a, int b, vec_ushort8 *c)
{
  int shift;
  vec_ushort8 *p = (vec_ushort8 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned short)0xFFFF), shift));
}

static inline void vec_stvlx(vec_short8 a, int b, signed short *c)
{
  int shift;
  vec_short8 *p = (vec_short8 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned short)0xFFFF), shift));
}

static inline void vec_stvlx(vec_short8 a, int b, vec_short8 *c)
{
  int shift;
  vec_short8 *p = (vec_short8 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned short)0xFFFF), shift));
}

static inline void vec_stvlx(vec_uint4 a, int b, unsigned int *c)
{
  int shift;
  vec_uint4 *p = (vec_uint4 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvlx(vec_uint4 a, int b, vec_uint4 *c)
{
  int shift;
  vec_uint4 *p = (vec_uint4 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvlx(vec_int4 a, int b, signed int *c)
{
  int shift;
  vec_int4 *p = (vec_int4 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvlx(vec_int4 a, int b, vec_int4 *c)
{
  int shift;
  vec_int4 *p = (vec_int4 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvlx(vec_float4 a, int b, float *c)
{
  int shift;
  vec_float4 *p = (vec_float4 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvlx(vec_float4 a, int b, vec_float4 *c)
{
  int shift;
  vec_float4 *p = (vec_float4 *)((unsigned char *)(c) + b);

  shift = -((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_rlmaskqwbyte(a, shift),
	       spu_rlmaskqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

/* vec_stvlxl (store vector left indexed last)
 * ==========
 */
#define vec_stvlxl(_a, _b, _c)	vec_stvlx(_a, _b, _c)


/* vec_stvrx (store vector right indexed)
 * =========
 */
static inline void vec_stvrx(vec_uchar16 a, int b, unsigned char *c)
{
  int shift;
  vec_uchar16 *p = (vec_uchar16 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned char)0xFF), shift));
}

static inline void vec_stvrx(vec_uchar16 a, int b, vec_uchar16 *c)
{
  int shift;
  vec_uchar16 *p = (vec_uchar16 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned char)0xFF), shift));
}

static inline void vec_stvrx(vec_char16 a, int b, signed char *c)
{
  int shift;
  vec_char16 *p = (vec_char16 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned char)0xFF), shift));
}

static inline void vec_stvrx(vec_char16 a, int b, vec_char16 *c)
{
  int shift;
  vec_char16 *p = (vec_char16 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned char)0xFF), shift));
}

static inline void vec_stvrx(vec_ushort8 a, int b, unsigned short *c)
{
  int shift;
  vec_ushort8 *p = (vec_ushort8 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned short)0xFFFF), shift));
}

static inline void vec_stvrx(vec_ushort8 a, int b, vec_ushort8 *c)
{
  int shift;
  vec_ushort8 *p = (vec_ushort8 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned short)0xFFFF), shift));
}

static inline void vec_stvrx(vec_short8 a, int b, signed short *c)
{
  int shift;
  vec_short8 *p = (vec_short8 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned short)0xFFFF), shift));
}

static inline void vec_stvrx(vec_short8 a, int b, vec_short8 *c)
{
  int shift;
  vec_short8 *p = (vec_short8 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned short)0xFFFF), shift));
}

static inline void vec_stvrx(vec_uint4 a, int b, unsigned int *c)
{
  int shift;
  vec_uint4 *p = (vec_uint4 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvrx(vec_uint4 a, int b, vec_uint4 *c)
{
  int shift;
  vec_uint4 *p = (vec_uint4 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvrx(vec_int4 a, int b, signed int *c)
{
  int shift;
  vec_int4 *p = (vec_int4 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvrx(vec_int4 a, int b, vec_int4 *c)
{
  int shift;
  vec_int4 *p = (vec_int4 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvrx(vec_float4 a, int b, float *c)
{
  int shift;
  vec_float4 *p = (vec_float4 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

static inline void vec_stvrx(vec_float4 a, int b, vec_float4 *c)
{
  int shift;
  vec_float4 *p = (vec_float4 *)((unsigned char *)(c) + b);

  shift = 16-((int)p & 0xF);
  *p = spu_sel(*p,
	       spu_slqwbyte(a, shift),
	       spu_slqwbyte(spu_splats((unsigned int)0xFFFFFFFF), shift));
}

/* vec_stvrxl (store vector right indexed last)
 * ==========
 */
#define vec_stvrxl(_a, _b, _c)	vec_stvrx(_a, _b, _c)


#endif /* __SPU__ */
#endif /* __cplusplus */
#endif /* !_VMX2SPU_H_ */
