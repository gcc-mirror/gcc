/* Copyright (C) 2002 Free Software Foundation, Inc.

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

/* Implemented from the specification included in the Intel C++ Compiler
   User Guide and Reference, version 5.0.  */

#ifndef _XMMINTRIN_H_INCLUDED
#define _XMMINTRIN_H_INCLUDED

#ifndef __SSE__
# error "SSE instruction set not enabled"
#else

/* We need type definitions from the MMX header file.  */
#include <mmintrin.h>

/* The data type indended for user use.  */
typedef int __m128 __attribute__ ((__mode__(__V4SF__)));

/* Internal data types for implementing the instrinsics.  */
typedef int __v4sf __attribute__ ((__mode__(__V4SF__)));
typedef int __v4si __attribute__ ((__mode__(__V4SI__)));

/* Create a selector for use with the SHUFPS instruction.  */
#define _MM_SHUFFLE(fp3,fp2,fp1,fp0) \
 (((fp3) << 6) | ((fp2) << 4) | ((fp1) << 2) | (fp0))

/* Constants for use with _mm_prefetch.  */
enum _mm_hint
{
  _MM_HINT_T0 = 3,
  _MM_HINT_T1 = 2,
  _MM_HINT_T2 = 1,
  _MM_HINT_NTA = 0
};

/* Bits in the MXCSR.  */
#define _MM_EXCEPT_MASK       0x003f
#define _MM_EXCEPT_INVALID    0x0001
#define _MM_EXCEPT_DENORM     0x0002
#define _MM_EXCEPT_DIV_ZERO   0x0004
#define _MM_EXCEPT_OVERFLOW   0x0008
#define _MM_EXCEPT_UNDERFLOW  0x0010
#define _MM_EXCEPT_INEXACT    0x0020

#define _MM_MASK_MASK         0x1f80
#define _MM_MASK_INVALID      0x0080
#define _MM_MASK_DENORM       0x0100
#define _MM_MASK_DIV_ZERO     0x0200
#define _MM_MASK_OVERFLOW     0x0400
#define _MM_MASK_UNDERFLOW    0x0800
#define _MM_MASK_INEXACT      0x1000

#define _MM_ROUND_MASK        0x6000
#define _MM_ROUND_NEAREST     0x0000
#define _MM_ROUND_DOWN        0x2000
#define _MM_ROUND_UP          0x4000
#define _MM_ROUND_TOWARD_ZERO 0x6000

#define _MM_FLUSH_ZERO_MASK   0x8000
#define _MM_FLUSH_ZERO_ON     0x8000
#define _MM_FLUSH_ZERO_OFF    0x0000

/* Perform the respective operation on the lower SPFP (single-precision
   floating-point) values of A and B; the upper three SPFP values are
   passed through from A.  */

static __inline __m128
_mm_add_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_addss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_sub_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_subss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_mul_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_mulss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_div_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_divss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_sqrt_ss (__m128 __A)
{
  return (__m128) __builtin_ia32_sqrtss ((__v4sf)__A);
}

static __inline __m128
_mm_rcp_ss (__m128 __A)
{
  return (__m128) __builtin_ia32_rcpss ((__v4sf)__A);
}

static __inline __m128
_mm_rsqrt_ss (__m128 __A)
{
  return (__m128) __builtin_ia32_rsqrtss ((__v4sf)__A);
}

static __inline __m128
_mm_min_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_minss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_max_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_maxss ((__v4sf)__A, (__v4sf)__B);
}

/* Perform the respective operation on the four SPFP values in A and B.  */

static __inline __m128
_mm_add_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_addps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_sub_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_subps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_mul_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_mulps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_div_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_divps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_sqrt_ps (__m128 __A)
{
  return (__m128) __builtin_ia32_sqrtps ((__v4sf)__A);
}

static __inline __m128
_mm_rcp_ps (__m128 __A)
{
  return (__m128) __builtin_ia32_rcpps ((__v4sf)__A);
}

static __inline __m128
_mm_rsqrt_ps (__m128 __A)
{
  return (__m128) __builtin_ia32_rsqrtps ((__v4sf)__A);
}

static __inline __m128
_mm_min_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_minps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_max_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_maxps ((__v4sf)__A, (__v4sf)__B);
}

/* Perform logical bit-wise operations on 128-bit values.  */

static __inline __m128
_mm_and_ps (__m128 __A, __m128 __B)
{
  return __builtin_ia32_andps (__A, __B);
}

static __inline __m128
_mm_andnot_ps (__m128 __A, __m128 __B)
{
  return __builtin_ia32_andnps (__A, __B);
}

static __inline __m128
_mm_or_ps (__m128 __A, __m128 __B)
{
  return __builtin_ia32_orps (__A, __B);
}

static __inline __m128
_mm_xor_ps (__m128 __A, __m128 __B)
{
  return __builtin_ia32_xorps (__A, __B);
}

/* Perform a comparison on the lower SPFP values of A and B.  If the
   comparison is true, place a mask of all ones in the result, otherwise a
   mask of zeros.  The upper three SPFP values are passed through from A.  */

static __inline __m128
_mm_cmpeq_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpeqss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmplt_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpltss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmple_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpless ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpgt_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf) __A,
					(__v4sf)
					__builtin_ia32_cmpltss ((__v4sf) __B,
								(__v4sf)
								__A));
}

static __inline __m128
_mm_cmpge_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf) __A,
					(__v4sf)
					__builtin_ia32_cmpless ((__v4sf) __B,
								(__v4sf)
								__A));
}

static __inline __m128
_mm_cmpneq_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpneqss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpnlt_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpnltss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpnle_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpnless ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpngt_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf) __A,
					(__v4sf)
					__builtin_ia32_cmpnltss ((__v4sf) __B,
								 (__v4sf)
								 __A));
}

static __inline __m128
_mm_cmpnge_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf) __A,
					(__v4sf)
					__builtin_ia32_cmpnless ((__v4sf) __B,
								 (__v4sf)
								 __A));
}

static __inline __m128
_mm_cmpord_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpordss ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpunord_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpunordss ((__v4sf)__A, (__v4sf)__B);
}

/* Perform a comparison on the four SPFP values of A and B.  For each
   element, if the comparison is true, place a mask of all ones in the
   result, otherwise a mask of zeros.  */

static __inline __m128
_mm_cmpeq_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpeqps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmplt_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpltps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmple_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpleps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpgt_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpgtps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpge_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpgeps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpneq_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpneqps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpnlt_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpnltps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpnle_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpnleps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpngt_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpngtps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpnge_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpngeps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpord_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpordps ((__v4sf)__A, (__v4sf)__B);
}

static __inline __m128
_mm_cmpunord_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpunordps ((__v4sf)__A, (__v4sf)__B);
}

/* Compare the lower SPFP values of A and B and return 1 if true
   and 0 if false.  */

static __inline int
_mm_comieq_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comieq ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_comilt_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comilt ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_comile_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comile ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_comigt_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comigt ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_comige_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comige ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_comineq_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comineq ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_ucomieq_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomieq ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_ucomilt_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomilt ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_ucomile_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomile ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_ucomigt_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomigt ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_ucomige_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomige ((__v4sf)__A, (__v4sf)__B);
}

static __inline int
_mm_ucomineq_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomineq ((__v4sf)__A, (__v4sf)__B);
}

/* Convert the lower SPFP value to a 32-bit integer according to the current
   rounding mode.  */
static __inline int
_mm_cvtss_si32 (__m128 __A)
{
  return __builtin_ia32_cvtss2si ((__v4sf) __A);
}

#ifdef __x86_64__
/* Convert the lower SPFP value to a 32-bit integer according to the current
   rounding mode.  */
static __inline long long
_mm_cvtss_si64x (__m128 __A)
{
  return __builtin_ia32_cvtss2si64 ((__v4sf) __A);
}
#endif

/* Convert the two lower SPFP values to 32-bit integers according to the
   current rounding mode.  Return the integers in packed form.  */
static __inline __m64
_mm_cvtps_pi32 (__m128 __A)
{
  return (__m64) __builtin_ia32_cvtps2pi ((__v4sf) __A);
}

/* Truncate the lower SPFP value to a 32-bit integer.  */
static __inline int
_mm_cvttss_si32 (__m128 __A)
{
  return __builtin_ia32_cvttss2si ((__v4sf) __A);
}

#ifdef __x86_64__
/* Truncate the lower SPFP value to a 32-bit integer.  */
static __inline long long
_mm_cvttss_si64x (__m128 __A)
{
  return __builtin_ia32_cvttss2si64 ((__v4sf) __A);
}
#endif

/* Truncate the two lower SPFP values to 32-bit integers.  Return the
   integers in packed form.  */
static __inline __m64
_mm_cvttps_pi32 (__m128 __A)
{
  return (__m64) __builtin_ia32_cvttps2pi ((__v4sf) __A);
}

/* Convert B to a SPFP value and insert it as element zero in A.  */
static __inline __m128
_mm_cvtsi32_ss (__m128 __A, int __B)
{
  return (__m128) __builtin_ia32_cvtsi2ss ((__v4sf) __A, __B);
}

#ifdef __x86_64__
/* Convert B to a SPFP value and insert it as element zero in A.  */
static __inline __m128
_mm_cvtsi64x_ss (__m128 __A, long long __B)
{
  return (__m128) __builtin_ia32_cvtsi642ss ((__v4sf) __A, __B);
}
#endif

/* Convert the two 32-bit values in B to SPFP form and insert them
   as the two lower elements in A.  */
static __inline __m128
_mm_cvtpi32_ps (__m128 __A, __m64 __B)
{
  return (__m128) __builtin_ia32_cvtpi2ps ((__v4sf) __A, (__v2si)__B);
}

/* Convert the four signed 16-bit values in A to SPFP form.  */
static __inline __m128
_mm_cvtpi16_ps (__m64 __A)
{
  __v4hi __sign;
  __v2si __hisi, __losi;
  __v4sf __r;

  /* This comparison against zero gives us a mask that can be used to
     fill in the missing sign bits in the unpack operations below, so
     that we get signed values after unpacking.  */
  __sign = (__v4hi) __builtin_ia32_mmx_zero ();
  __sign = __builtin_ia32_pcmpgtw (__sign, (__v4hi)__A);

  /* Convert the four words to doublewords.  */
  __hisi = (__v2si) __builtin_ia32_punpckhwd ((__v4hi)__A, __sign);
  __losi = (__v2si) __builtin_ia32_punpcklwd ((__v4hi)__A, __sign);

  /* Convert the doublewords to floating point two at a time.  */
  __r = (__v4sf) __builtin_ia32_setzerops ();
  __r = __builtin_ia32_cvtpi2ps (__r, __hisi);
  __r = __builtin_ia32_movlhps (__r, __r);
  __r = __builtin_ia32_cvtpi2ps (__r, __losi);

  return (__m128) __r;
}

/* Convert the four unsigned 16-bit values in A to SPFP form.  */
static __inline __m128
_mm_cvtpu16_ps (__m64 __A)
{
  __v4hi __zero = (__v4hi) __builtin_ia32_mmx_zero ();
  __v2si __hisi, __losi;
  __v4sf __r;

  /* Convert the four words to doublewords.  */
  __hisi = (__v2si) __builtin_ia32_punpckhwd ((__v4hi)__A, __zero);
  __losi = (__v2si) __builtin_ia32_punpcklwd ((__v4hi)__A, __zero);

  /* Convert the doublewords to floating point two at a time.  */
  __r = (__v4sf) __builtin_ia32_setzerops ();
  __r = __builtin_ia32_cvtpi2ps (__r, __hisi);
  __r = __builtin_ia32_movlhps (__r, __r);
  __r = __builtin_ia32_cvtpi2ps (__r, __losi);

  return (__m128) __r;
}

/* Convert the low four signed 8-bit values in A to SPFP form.  */
static __inline __m128
_mm_cvtpi8_ps (__m64 __A)
{
  __v8qi __sign;

  /* This comparison against zero gives us a mask that can be used to
     fill in the missing sign bits in the unpack operations below, so
     that we get signed values after unpacking.  */
  __sign = (__v8qi) __builtin_ia32_mmx_zero ();
  __sign = __builtin_ia32_pcmpgtb (__sign, (__v8qi)__A);

  /* Convert the four low bytes to words.  */
  __A = (__m64) __builtin_ia32_punpcklbw ((__v8qi)__A, __sign);

  return _mm_cvtpi16_ps(__A);
}

/* Convert the low four unsigned 8-bit values in A to SPFP form.  */
static __inline __m128
_mm_cvtpu8_ps(__m64 __A)
{
  __v8qi __zero = (__v8qi) __builtin_ia32_mmx_zero ();
  __A = (__m64) __builtin_ia32_punpcklbw ((__v8qi)__A, __zero);
  return _mm_cvtpu16_ps(__A);
}

/* Convert the four signed 32-bit values in A and B to SPFP form.  */
static __inline __m128
_mm_cvtpi32x2_ps(__m64 __A, __m64 __B)
{
  __v4sf __zero = (__v4sf) __builtin_ia32_setzerops ();
  __v4sf __sfa = __builtin_ia32_cvtpi2ps (__zero, (__v2si)__A);
  __v4sf __sfb = __builtin_ia32_cvtpi2ps (__zero, (__v2si)__B);
  return (__m128) __builtin_ia32_movlhps (__sfa, __sfb);
}

/* Convert the four SPFP values in A to four signed 16-bit integers.  */
static __inline __m64
_mm_cvtps_pi16(__m128 __A)
{
  __v4sf __hisf = (__v4sf)__A;
  __v4sf __losf = __builtin_ia32_movhlps (__hisf, __hisf);
  __v2si __hisi = __builtin_ia32_cvtps2pi (__hisf);
  __v2si __losi = __builtin_ia32_cvtps2pi (__losf);
  return (__m64) __builtin_ia32_packssdw (__hisi, __losi);
}

/* Convert the four SPFP values in A to four signed 8-bit integers.  */
static __inline __m64
_mm_cvtps_pi8(__m128 __A)
{
  __v4hi __tmp = (__v4hi) _mm_cvtps_pi16 (__A);
  __v4hi __zero = (__v4hi) __builtin_ia32_mmx_zero ();
  return (__m64) __builtin_ia32_packsswb (__tmp, __zero);
}

/* Selects four specific SPFP values from A and B based on MASK.  */
#if 0
static __inline __m128
_mm_shuffle_ps (__m128 __A, __m128 __B, int __mask)
{
  return (__m128) __builtin_ia32_shufps ((__v4sf)__A, (__v4sf)__B, __mask);
}
#else
#define _mm_shuffle_ps(A, B, MASK) \
 ((__m128) __builtin_ia32_shufps ((__v4sf)(A), (__v4sf)(B), (MASK)))
#endif


/* Selects and interleaves the upper two SPFP values from A and B.  */
static __inline __m128
_mm_unpackhi_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_unpckhps ((__v4sf)__A, (__v4sf)__B);
}

/* Selects and interleaves the lower two SPFP values from A and B.  */
static __inline __m128
_mm_unpacklo_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_unpcklps ((__v4sf)__A, (__v4sf)__B);
}

/* Sets the upper two SPFP values with 64-bits of data loaded from P;
   the lower two values are passed through from A.  */
static __inline __m128
_mm_loadh_pi (__m128 __A, __m64 const *__P)
{
  return (__m128) __builtin_ia32_loadhps ((__v4sf)__A, (__v2si *)__P);
}

/* Stores the upper two SPFP values of A into P.  */
static __inline void
_mm_storeh_pi (__m64 *__P, __m128 __A)
{
  __builtin_ia32_storehps ((__v2si *)__P, (__v4sf)__A);
}

/* Moves the upper two values of B into the lower two values of A.  */
static __inline __m128
_mm_movehl_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movhlps ((__v4sf)__A, (__v4sf)__B);
}

/* Moves the lower two values of B into the upper two values of A.  */
static __inline __m128
_mm_movelh_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movlhps ((__v4sf)__A, (__v4sf)__B);
}

/* Sets the lower two SPFP values with 64-bits of data loaded from P;
   the upper two values are passed through from A.  */
static __inline __m128
_mm_loadl_pi (__m128 __A, __m64 const *__P)
{
  return (__m128) __builtin_ia32_loadlps ((__v4sf)__A, (__v2si *)__P);
}

/* Stores the lower two SPFP values of A into P.  */
static __inline void
_mm_storel_pi (__m64 *__P, __m128 __A)
{
  __builtin_ia32_storelps ((__v2si *)__P, (__v4sf)__A);
}

/* Creates a 4-bit mask from the most significant bits of the SPFP values.  */
static __inline int
_mm_movemask_ps (__m128 __A)
{
  return __builtin_ia32_movmskps ((__v4sf)__A);
}

/* Return the contents of the control register.  */
static __inline unsigned int
_mm_getcsr (void)
{
  return __builtin_ia32_stmxcsr ();
}

/* Read exception bits from the control register.  */
static __inline unsigned int
_MM_GET_EXCEPTION_STATE (void)
{
  return _mm_getcsr() & _MM_EXCEPT_MASK;
}

static __inline unsigned int
_MM_GET_EXCEPTION_MASK (void)
{
  return _mm_getcsr() & _MM_MASK_MASK;
}

static __inline unsigned int
_MM_GET_ROUNDING_MODE (void)
{
  return _mm_getcsr() & _MM_ROUND_MASK;
}

static __inline unsigned int
_MM_GET_FLUSH_ZERO_MODE (void)
{
  return _mm_getcsr() & _MM_FLUSH_ZERO_MASK;
}

/* Set the control register to I.  */
static __inline void
_mm_setcsr (unsigned int __I)
{
  __builtin_ia32_ldmxcsr (__I);
}

/* Set exception bits in the control register.  */
static __inline void
_MM_SET_EXCEPTION_STATE(unsigned int __mask)
{
  _mm_setcsr((_mm_getcsr() & ~_MM_EXCEPT_MASK) | __mask);
}

static __inline void
_MM_SET_EXCEPTION_MASK (unsigned int __mask)
{
  _mm_setcsr((_mm_getcsr() & ~_MM_MASK_MASK) | __mask);
}

static __inline void
_MM_SET_ROUNDING_MODE (unsigned int __mode)
{
  _mm_setcsr((_mm_getcsr() & ~_MM_ROUND_MASK) | __mode);
}

static __inline void
_MM_SET_FLUSH_ZERO_MODE (unsigned int __mode)
{
  _mm_setcsr((_mm_getcsr() & ~_MM_FLUSH_ZERO_MASK) | __mode);
}

/* Create a vector with element 0 as *P and the rest zero.  */
static __inline __m128
_mm_load_ss (float const *__P)
{
  return (__m128) __builtin_ia32_loadss (__P);
}

/* Create a vector with all four elements equal to *P.  */
static __inline __m128
_mm_load1_ps (float const *__P)
{
  __v4sf __tmp = __builtin_ia32_loadss (__P);
  return (__m128) __builtin_ia32_shufps (__tmp, __tmp, _MM_SHUFFLE (0,0,0,0));
}

static __inline __m128
_mm_load_ps1 (float const *__P)
{
  return _mm_load1_ps (__P);
}

/* Load four SPFP values from P.  The address must be 16-byte aligned.  */
static __inline __m128
_mm_load_ps (float const *__P)
{
  return (__m128) __builtin_ia32_loadaps (__P);
}

/* Load four SPFP values from P.  The address need not be 16-byte aligned.  */
static __inline __m128
_mm_loadu_ps (float const *__P)
{
  return (__m128) __builtin_ia32_loadups (__P);
}

/* Load four SPFP values in reverse order.  The address must be aligned.  */
static __inline __m128
_mm_loadr_ps (float const *__P)
{
  __v4sf __tmp = __builtin_ia32_loadaps (__P);
  return (__m128) __builtin_ia32_shufps (__tmp, __tmp, _MM_SHUFFLE (0,1,2,3));
}

/* Create a vector with element 0 as F and the rest zero.  */
static __inline __m128
_mm_set_ss (float __F)
{
  return (__m128) __builtin_ia32_loadss (&__F);
}

/* Create a vector with all four elements equal to F.  */
static __inline __m128
_mm_set1_ps (float __F)
{
  __v4sf __tmp = __builtin_ia32_loadss (&__F);
  return (__m128) __builtin_ia32_shufps (__tmp, __tmp, _MM_SHUFFLE (0,0,0,0));
}

static __inline __m128
_mm_set_ps1 (float __F)
{
  return _mm_set1_ps (__F);
}

/* Create the vector [Z Y X W].  */
static __inline __m128
_mm_set_ps (float __Z, float __Y, float __X, float __W)
{
  union {
    float __a[4];
    __m128 __v;
  } __u;

  __u.__a[0] = __W;
  __u.__a[1] = __X;
  __u.__a[2] = __Y;
  __u.__a[3] = __Z;

  return __u.__v;
}

/* Create the vector [W X Y Z].  */
static __inline __m128
_mm_setr_ps (float __Z, float __Y, float __X, float __W)
{
  return _mm_set_ps (__W, __X, __Y, __Z);
}

/* Create a vector of zeros.  */
static __inline __m128
_mm_setzero_ps (void)
{
  return (__m128) __builtin_ia32_setzerops ();
}

/* Stores the lower SPFP value.  */
static __inline void
_mm_store_ss (float *__P, __m128 __A)
{
  __builtin_ia32_storess (__P, (__v4sf)__A);
}

/* Store the lower SPFP value across four words.  */
static __inline void
_mm_store1_ps (float *__P, __m128 __A)
{
  __v4sf __va = (__v4sf)__A;
  __v4sf __tmp = __builtin_ia32_shufps (__va, __va, _MM_SHUFFLE (0,0,0,0));
  __builtin_ia32_storeaps (__P, __tmp);
}

static __inline void
_mm_store_ps1 (float *__P, __m128 __A)
{
  _mm_store1_ps (__P, __A);
}

/* Store four SPFP values.  The address must be 16-byte aligned.  */
static __inline void
_mm_store_ps (float *__P, __m128 __A)
{
  __builtin_ia32_storeaps (__P, (__v4sf)__A);
}

/* Store four SPFP values.  The address need not be 16-byte aligned.  */
static __inline void
_mm_storeu_ps (float *__P, __m128 __A)
{
  __builtin_ia32_storeups (__P, (__v4sf)__A);
}

/* Store four SPFP values in reverse order.  The address must be aligned.  */
static __inline void
_mm_storer_ps (float *__P, __m128 __A)
{
  __v4sf __va = (__v4sf)__A;
  __v4sf __tmp = __builtin_ia32_shufps (__va, __va, _MM_SHUFFLE (0,1,2,3));
  __builtin_ia32_storeaps (__P, __tmp);
}

/* Sets the low SPFP value of A from the low value of B.  */
static __inline __m128
_mm_move_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf)__A, (__v4sf)__B);
}

/* Extracts one of the four words of A.  The selector N must be immediate.  */
#if 0
static __inline int
_mm_extract_pi16 (__m64 __A, int __N)
{
  return __builtin_ia32_pextrw ((__v4hi)__A, __N);
}
#else
#define _mm_extract_pi16(A, N) \
  __builtin_ia32_pextrw ((__v4hi)(A), (N))
#endif

/* Inserts word D into one of four words of A.  The selector N must be
   immediate.  */
#if 0
static __inline __m64
_mm_insert_pi16 (__m64 __A, int __D, int __N)
{
  return (__m64)__builtin_ia32_pinsrw ((__v4hi)__A, __D, __N);
}
#else
#define _mm_insert_pi16(A, D, N) \
  ((__m64) __builtin_ia32_pinsrw ((__v4hi)(A), (D), (N)))
#endif

/* Compute the element-wise maximum of signed 16-bit values.  */
static __inline __m64
_mm_max_pi16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pmaxsw ((__v4hi)__A, (__v4hi)__B);
}

/* Compute the element-wise maximum of unsigned 8-bit values.  */
static __inline __m64
_mm_max_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pmaxub ((__v8qi)__A, (__v8qi)__B);
}

/* Compute the element-wise minimum of signed 16-bit values.  */
static __inline __m64
_mm_min_pi16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pminsw ((__v4hi)__A, (__v4hi)__B);
}

/* Compute the element-wise minimum of unsigned 8-bit values.  */
static __inline __m64
_mm_min_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pminub ((__v8qi)__A, (__v8qi)__B);
}

/* Create an 8-bit mask of the signs of 8-bit values.  */
static __inline int
_mm_movemask_pi8 (__m64 __A)
{
  return __builtin_ia32_pmovmskb ((__v8qi)__A);
}

/* Multiply four unsigned 16-bit values in A by four unsigned 16-bit values
   in B and produce the high 16 bits of the 32-bit results.  */
static __inline __m64
_mm_mulhi_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pmulhuw ((__v4hi)__A, (__v4hi)__B);
}

/* Return a combination of the four 16-bit values in A.  The selector
   must be an immediate.  */
#if 0
static __inline __m64
_mm_shuffle_pi16 (__m64 __A, int __N)
{
  return (__m64) __builtin_ia32_pshufw ((__v4hi)__A, __N);
}
#else
#define _mm_shuffle_pi16(A, N) \
  ((__m64) __builtin_ia32_pshufw ((__v4hi)(A), (N)))
#endif

/* Conditionally store byte elements of A into P.  The high bit of each
   byte in the selector N determines whether the corresponding byte from
   A is stored.  */
static __inline void
_mm_maskmove_si64 (__m64 __A, __m64 __N, char *__P)
{
  __builtin_ia32_maskmovq ((__v8qi)__A, (__v8qi)__N, __P);
}

/* Compute the rounded averages of the unsigned 8-bit values in A and B.  */
static __inline __m64
_mm_avg_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pavgb ((__v8qi)__A, (__v8qi)__B);
}

/* Compute the rounded averages of the unsigned 16-bit values in A and B.  */
static __inline __m64
_mm_avg_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pavgw ((__v4hi)__A, (__v4hi)__B);
}

/* Compute the sum of the absolute differences of the unsigned 8-bit
   values in A and B.  Return the value in the lower 16-bit word; the
   upper words are cleared.  */
static __inline __m64
_mm_sad_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_psadbw ((__v8qi)__A, (__v8qi)__B);
}

/* Loads one cache line from address P to a location "closer" to the
   processor.  The selector I specifies the type of prefetch operation.  */
#if 0
static __inline void
_mm_prefetch (void *__P, enum _mm_hint __I)
{
  __builtin_prefetch (__P, 0, __I);
}
#else
#define _mm_prefetch(P, I) \
  __builtin_prefetch ((P), 0, (I))
#endif

/* Stores the data in A to the address P without polluting the caches.  */
static __inline void
_mm_stream_pi (__m64 *__P, __m64 __A)
{
  __builtin_ia32_movntq ((unsigned long long *)__P, (unsigned long long)__A);
}

/* Likewise.  The address must be 16-byte aligned.  */
static __inline void
_mm_stream_ps (float *__P, __m128 __A)
{
  __builtin_ia32_movntps (__P, (__v4sf)__A);
}

/* Guarantees that every preceeding store is globally visible before
   any subsequent store.  */
static __inline void
_mm_sfence (void)
{
  __builtin_ia32_sfence ();
}

/* The execution of the next instruction is delayed by an implementation
   specific amount of time.  The instruction does not modify the
   architectural state.  */
static __inline void
_mm_pause (void)
{
  __asm__ __volatile__ ("rep; nop" : : );
}

/* Transpose the 4x4 matrix composed of row[0-3].  */
#define _MM_TRANSPOSE4_PS(row0, row1, row2, row3)			\
do {									\
  __v4sf __r0 = (row0), __r1 = (row1), __r2 = (row2), __r3 = (row3);	\
  __v4sf __t0 = __builtin_ia32_shufps (__r0, __r1, 0x44);		\
  __v4sf __t2 = __builtin_ia32_shufps (__r0, __r1, 0xEE);		\
  __v4sf __t1 = __builtin_ia32_shufps (__r2, __r3, 0x44);		\
  __v4sf __t3 = __builtin_ia32_shufps (__r2, __r3, 0xEE);		\
  (row0) = __builtin_ia32_shufps (__t0, __t1, 0x88);			\
  (row1) = __builtin_ia32_shufps (__t0, __t1, 0xDD);			\
  (row2) = __builtin_ia32_shufps (__t2, __t3, 0x88);			\
  (row3) = __builtin_ia32_shufps (__t2, __t3, 0xDD);			\
} while (0)

#ifdef __SSE2__
/* SSE2 */
typedef int __v2df __attribute__ ((mode (V2DF)));
typedef int __v2di __attribute__ ((mode (V2DI)));
typedef int __v4si __attribute__ ((mode (V4SI)));
typedef int __v8hi __attribute__ ((mode (V8HI)));
typedef int __v16qi __attribute__ ((mode (V16QI)));

/* Create a selector for use with the SHUFPD instruction.  */
#define _MM_SHUFFLE2(fp1,fp0) \
 (((fp1) << 1) | (fp0))

#define __m128i __v2di
#define __m128d __v2df

/* Create a vector with element 0 as *P and the rest zero.  */
static __inline __m128d
_mm_load_sd (double const *__P)
{
  return (__m128d) __builtin_ia32_loadsd (__P);
}

/* Create a vector with all two elements equal to *P.  */
static __inline __m128d
_mm_load1_pd (double const *__P)
{
  __v2df __tmp = __builtin_ia32_loadsd (__P);
  return (__m128d) __builtin_ia32_shufpd (__tmp, __tmp, _MM_SHUFFLE2 (0,0));
}

static __inline __m128d
_mm_load_pd1 (double const *__P)
{
  return _mm_load1_pd (__P);
}

/* Load two DPFP values from P.  The addresd must be 16-byte aligned.  */
static __inline __m128d
_mm_load_pd (double const *__P)
{
  return (__m128d) __builtin_ia32_loadapd (__P);
}

/* Load two DPFP values from P.  The addresd need not be 16-byte aligned.  */
static __inline __m128d
_mm_loadu_pd (double const *__P)
{
  return (__m128d) __builtin_ia32_loadupd (__P);
}

/* Load two DPFP values in reverse order.  The addresd must be aligned.  */
static __inline __m128d
_mm_loadr_pd (double const *__P)
{
  __v2df __tmp = __builtin_ia32_loadapd (__P);
  return (__m128d) __builtin_ia32_shufpd (__tmp, __tmp, _MM_SHUFFLE2 (0,1));
}

/* Create a vector with element 0 as F and the rest zero.  */
static __inline __m128d
_mm_set_sd (double __F)
{
  return (__m128d) __builtin_ia32_loadsd (&__F);
}

/* Create a vector with all two elements equal to F.  */
static __inline __m128d
_mm_set1_pd (double __F)
{
  __v2df __tmp = __builtin_ia32_loadsd (&__F);
  return (__m128d) __builtin_ia32_shufpd (__tmp, __tmp, _MM_SHUFFLE2 (0,0));
}

static __inline __m128d
_mm_set_pd1 (double __F)
{
  return _mm_set1_pd (__F);
}

/* Create the vector [Z Y].  */
static __inline __m128d
_mm_set_pd (double __Z, double __Y)
{
  union {
    double __a[2];
    __m128d __v;
  } __u;

  __u.__a[0] = __Y;
  __u.__a[1] = __Z;

  return __u.__v;
}

/* Create the vector [Y Z].  */
static __inline __m128d
_mm_setr_pd (double __Z, double __Y)
{
  return _mm_set_pd (__Y, __Z);
}

/* Create a vector of zeros.  */
static __inline __m128d
_mm_setzero_pd (void)
{
  return (__m128d) __builtin_ia32_setzeropd ();
}

/* Stores the lower DPFP value.  */
static __inline void
_mm_store_sd (double *__P, __m128d __A)
{
  __builtin_ia32_storesd (__P, (__v2df)__A);
}

/* Store the lower DPFP value acrosd two words.  */
static __inline void
_mm_store1_pd (double *__P, __m128d __A)
{
  __v2df __va = (__v2df)__A;
  __v2df __tmp = __builtin_ia32_shufpd (__va, __va, _MM_SHUFFLE2 (0,0));
  __builtin_ia32_storeapd (__P, __tmp);
}

static __inline void
_mm_store_pd1 (double *__P, __m128d __A)
{
  _mm_store1_pd (__P, __A);
}

/* Store two DPFP values.  The addresd must be 16-byte aligned.  */
static __inline void
_mm_store_pd (double *__P, __m128d __A)
{
  __builtin_ia32_storeapd (__P, (__v2df)__A);
}

/* Store two DPFP values.  The addresd need not be 16-byte aligned.  */
static __inline void
_mm_storeu_pd (double *__P, __m128d __A)
{
  __builtin_ia32_storeupd (__P, (__v2df)__A);
}

/* Store two DPFP values in reverse order.  The addresd must be aligned.  */
static __inline void
_mm_storer_pd (double *__P, __m128d __A)
{
  __v2df __va = (__v2df)__A;
  __v2df __tmp = __builtin_ia32_shufpd (__va, __va, _MM_SHUFFLE2 (0,1));
  __builtin_ia32_storeapd (__P, __tmp);
}

/* Sets the low DPFP value of A from the low value of B.  */
static __inline __m128d
_mm_move_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d
_mm_add_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_addpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_add_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_addsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_sub_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_subpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_sub_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_subsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_mul_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_mulpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_mul_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_mulsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_div_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_divpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_div_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_divsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_sqrt_pd (__m128d __A)
{
  return (__m128d)__builtin_ia32_sqrtpd ((__v2df)__A);
}

/* Return pair {sqrt (A[0), B[1]}.  */
static __inline __m128d
_mm_sqrt_sd (__m128d __A, __m128d __B)
{
  __v2df __tmp = __builtin_ia32_movsd ((__v2df)__A, (__v2df)__B);
  return (__m128d)__builtin_ia32_sqrtsd ((__v2df)__tmp);
}

static __inline __m128d
_mm_min_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_minpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_min_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_minsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_max_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_maxpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_max_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_maxsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_and_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_andpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_andnot_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_andnpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_or_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_orpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_xor_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_xorpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpeq_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpeqpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmplt_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpltpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmple_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmplepd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpgt_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpgtpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpge_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpgepd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpneq_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpneqpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpnlt_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpnltpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpnle_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpnlepd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpngt_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpngtpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpnge_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpngepd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpord_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpordpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpunord_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpunordpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpeq_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpeqsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmplt_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpltsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmple_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmplesd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpgt_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df) __A,
					 (__v2df)
					 __builtin_ia32_cmpltsd ((__v2df) __B,
								 (__v2df)
								 __A));
}

static __inline __m128d
_mm_cmpge_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df) __A,
					 (__v2df)
					 __builtin_ia32_cmplesd ((__v2df) __B,
								 (__v2df)
								 __A));
}

static __inline __m128d
_mm_cmpneq_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpneqsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpnlt_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpnltsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpnle_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpnlesd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpngt_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df) __A,
					 (__v2df)
					 __builtin_ia32_cmpnltsd ((__v2df) __B,
								  (__v2df)
								  __A));
}

static __inline __m128d
_mm_cmpnge_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df) __A,
					 (__v2df)
					 __builtin_ia32_cmpnlesd ((__v2df) __B,
								  (__v2df)
								  __A));
}

static __inline __m128d
_mm_cmpord_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpordsd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_cmpunord_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpunordsd ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_comieq_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdeq ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_comilt_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdlt ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_comile_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdle ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_comigt_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdgt ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_comige_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdge ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_comineq_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdneq ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_ucomieq_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdeq ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_ucomilt_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdlt ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_ucomile_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdle ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_ucomigt_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdgt ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_ucomige_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdge ((__v2df)__A, (__v2df)__B);
}

static __inline int
_mm_ucomineq_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdneq ((__v2df)__A, (__v2df)__B);
}

/* Create a vector with element 0 as *P and the rest zero.  */

static __inline __m128i
_mm_load_si128 (__m128i const *__P)
{
  return (__m128i) __builtin_ia32_loaddqa ((char const *)__P);
}

static __inline __m128i
_mm_loadu_si128 (__m128i const *__P)
{
  return (__m128i) __builtin_ia32_loaddqu ((char const *)__P);
}

static __inline __m128i
_mm_loadl_epi64 (__m128i const *__P)
{
  return (__m128i) __builtin_ia32_movq2dq (*(unsigned long long *)__P);
}

static __inline void
_mm_store_si128 (__m128i *__P, __m128i __B)
{
  __builtin_ia32_storedqa ((char *)__P, (__v16qi)__B);
}

static __inline void
_mm_storeu_si128 (__m128i *__P, __m128i __B)
{
  __builtin_ia32_storedqu ((char *)__P, (__v16qi)__B);
}

static __inline void
_mm_storel_epi64 (__m128i *__P, __m128i __B)
{
  *(long long *)__P = __builtin_ia32_movdq2q ((__v2di)__B);
}

static __inline __m64
_mm_movepi64_pi64 (__m128i __B)
{
  return (__m64) __builtin_ia32_movdq2q ((__v2di)__B);
}

static __inline __m128i
_mm_move_epi64 (__m128i __A)
{
  return (__m128i) __builtin_ia32_movq ((__v2di)__A);
}

/* Create a vector of zeros.  */
static __inline __m128i
_mm_setzero_si128 (void)
{
  return (__m128i) __builtin_ia32_setzero128 ();
}

static __inline __m128i
_mm_set_epi64 (__m64 __A,  __m64 __B)
{
  __v2di __tmp = (__v2di)__builtin_ia32_movq2dq ((unsigned long long)__A);
  __v2di __tmp2 = (__v2di)__builtin_ia32_movq2dq ((unsigned long long)__B);
  return (__m128i)__builtin_ia32_punpcklqdq128 (__tmp2, __tmp);
}

/* Create the vector [Z Y X W].  */
static __inline __m128i
_mm_set_epi32 (int __Z, int __Y, int __X, int __W)
{
  union {
    int __a[4];
    __m128i __v;
  } __u;

  __u.__a[0] = __W;
  __u.__a[1] = __X;
  __u.__a[2] = __Y;
  __u.__a[3] = __Z;

  return __u.__v;
}

#ifdef __x86_64__
/* Create the vector [Z Y].  */
static __inline __m128i
_mm_set_epi64x (long long __Z, long long __Y)
{
  union {
    long __a[2];
    __m128i __v;
  } __u;

  __u.__a[0] = __Y;
  __u.__a[1] = __Z;

  return __u.__v;
}
#endif

/* Create the vector [S T U V Z Y X W].  */
static __inline __m128i
_mm_set_epi16 (short __Z, short __Y, short __X, short __W,
	       short __V, short __U, short __T, short __S)
{
  union {
    short __a[8];
    __m128i __v;
  } __u;

  __u.__a[0] = __S;
  __u.__a[1] = __T;
  __u.__a[2] = __U;
  __u.__a[3] = __V;
  __u.__a[4] = __W;
  __u.__a[5] = __X;
  __u.__a[6] = __Y;
  __u.__a[7] = __Z;

  return __u.__v;
}

/* Create the vector [S T U V Z Y X W].  */
static __inline __m128i
_mm_set_epi8 (char __Z, char __Y, char __X, char __W,
	      char __V, char __U, char __T, char __S,
	      char __Z1, char __Y1, char __X1, char __W1,
	      char __V1, char __U1, char __T1, char __S1)
{
  union {
    char __a[16];
    __m128i __v;
  } __u;

  __u.__a[0] = __S1;
  __u.__a[1] = __T1;
  __u.__a[2] = __U1;
  __u.__a[3] = __V1;
  __u.__a[4] = __W1;
  __u.__a[5] = __X1;
  __u.__a[6] = __Y1;
  __u.__a[7] = __Z1;
  __u.__a[8] = __S;
  __u.__a[9] = __T;
  __u.__a[10] = __U;
  __u.__a[11] = __V;
  __u.__a[12] = __W;
  __u.__a[13] = __X;
  __u.__a[14] = __Y;
  __u.__a[15] = __Z;

  return __u.__v;
}

static __inline __m128i
_mm_set1_epi64 (__m64 __A)
{
  __v2di __tmp = (__v2di)__builtin_ia32_movq2dq ((unsigned long long)__A);
  return (__m128i)__builtin_ia32_punpcklqdq128 (__tmp, __tmp);
}

static __inline __m128i
_mm_set1_epi32 (int __A)
{
  __v4si __tmp = (__v4si)__builtin_ia32_loadd (&__A);
  return (__m128i) __builtin_ia32_pshufd ((__v4si)__tmp, _MM_SHUFFLE (0,0,0,0));
}

#ifdef __x86_64__
static __inline __m128i
_mm_set1_epi64x (long long __A)
{
  __v2di __tmp = (__v2di)__builtin_ia32_movq2dq ((unsigned long long)__A);
  return (__m128i) __builtin_ia32_shufpd ((__v2df)__tmp, (__v2df)__tmp, _MM_SHUFFLE2 (0,0));
}
#endif

static __inline __m128i
_mm_set1_epi16 (short __A)
{
  int __Acopy = (unsigned short)__A;
  __v4si __tmp = (__v4si)__builtin_ia32_loadd (&__Acopy);
  __tmp = (__v4si)__builtin_ia32_punpcklwd128 ((__v8hi)__tmp, (__v8hi)__tmp);
  return (__m128i) __builtin_ia32_pshufd ((__v4si)__tmp, _MM_SHUFFLE (0,0,0,0));
}

static __inline __m128i
_mm_set1_epi8 (char __A)
{
  int __Acopy = (unsigned char)__A;
  __v4si __tmp = (__v4si)__builtin_ia32_loadd (&__Acopy);
  __tmp = (__v4si)__builtin_ia32_punpcklbw128 ((__v16qi)__tmp, (__v16qi)__tmp);
  __tmp = (__v4si)__builtin_ia32_punpcklbw128 ((__v16qi)__tmp, (__v16qi)__tmp);
  return (__m128i) __builtin_ia32_pshufd ((__v4si)__tmp, _MM_SHUFFLE (0,0,0,0));
}

static __inline __m128i
_mm_setr_epi64 (__m64 __A,  __m64 __B)
{
  __v2di __tmp = (__v2di)__builtin_ia32_movq2dq ((unsigned long long)__A);
  __v2di __tmp2 = (__v2di)__builtin_ia32_movq2dq ((unsigned long long)__B);
  return (__m128i)__builtin_ia32_punpcklqdq128 (__tmp, __tmp2);
}

/* Create the vector [Z Y X W].  */
static __inline __m128i
_mm_setr_epi32 (int __W, int __X, int __Y, int __Z)
{
  union {
    int __a[4];
    __m128i __v;
  } __u;

  __u.__a[0] = __W;
  __u.__a[1] = __X;
  __u.__a[2] = __Y;
  __u.__a[3] = __Z;

  return __u.__v;
}
/* Create the vector [S T U V Z Y X W].  */
static __inline __m128i
_mm_setr_epi16 (short __S, short __T, short __U, short __V,
	        short __W, short __X, short __Y, short __Z)
{
  union {
    short __a[8];
    __m128i __v;
  } __u;

  __u.__a[0] = __S;
  __u.__a[1] = __T;
  __u.__a[2] = __U;
  __u.__a[3] = __V;
  __u.__a[4] = __W;
  __u.__a[5] = __X;
  __u.__a[6] = __Y;
  __u.__a[7] = __Z;

  return __u.__v;
}

/* Create the vector [S T U V Z Y X W].  */
static __inline __m128i
_mm_setr_epi8 (char __S1, char __T1, char __U1, char __V1,
	       char __W1, char __X1, char __Y1, char __Z1,
	       char __S, char __T, char __U, char __V,
	       char __W, char __X, char __Y, char __Z)
{
  union {
    char __a[16];
    __m128i __v;
  } __u;

  __u.__a[0] = __S1;
  __u.__a[1] = __T1;
  __u.__a[2] = __U1;
  __u.__a[3] = __V1;
  __u.__a[4] = __W1;
  __u.__a[5] = __X1;
  __u.__a[6] = __Y1;
  __u.__a[7] = __Z1;
  __u.__a[8] = __S;
  __u.__a[9] = __T;
  __u.__a[10] = __U;
  __u.__a[11] = __V;
  __u.__a[12] = __W;
  __u.__a[13] = __X;
  __u.__a[14] = __Y;
  __u.__a[15] = __Z;

  return __u.__v;
}

static __inline __m128d
_mm_cvtepi32_pd (__m128i __A)
{
  return (__m128d)__builtin_ia32_cvtdq2pd ((__v4si) __A);
}

static __inline __m128
_mm_cvtepi32_ps (__m128i __A)
{
  return (__m128)__builtin_ia32_cvtdq2ps ((__v4si) __A);
}

static __inline __m128i
_mm_cvtpd_epi32 (__m128d __A)
{
  return (__m128i)__builtin_ia32_cvtpd2dq ((__v2df) __A);
}

static __inline __m64
_mm_cvtpd_pi32 (__m128d __A)
{
  return (__m64)__builtin_ia32_cvtpd2pi ((__v2df) __A);
}

static __inline __m128
_mm_cvtpd_ps (__m128d __A)
{
  return (__m128)__builtin_ia32_cvtpd2ps ((__v2df) __A);
}

static __inline __m128i
_mm_cvttpd_epi32 (__m128d __A)
{
  return (__m128i)__builtin_ia32_cvttpd2dq ((__v2df) __A);
}

static __inline __m64
_mm_cvttpd_pi32 (__m128d __A)
{
  return (__m64)__builtin_ia32_cvttpd2pi ((__v2df) __A);
}

static __inline __m128d
_mm_cvtpi32_pd (__m64 __A)
{
  return (__m128d)__builtin_ia32_cvtpi2pd ((__v2si) __A);
}

static __inline __m128i
_mm_cvtps_epi32 (__m128 __A)
{
  return (__m128i)__builtin_ia32_cvtps2dq ((__v4sf) __A);
}

static __inline __m128i
_mm_cvttps_epi32 (__m128 __A)
{
  return (__m128i)__builtin_ia32_cvttps2dq ((__v4sf) __A);
}

static __inline __m128d
_mm_cvtps_pd (__m128 __A)
{
  return (__m128d)__builtin_ia32_cvtps2pd ((__v4sf) __A);
}

static __inline int
_mm_cvtsd_si32 (__m128d __A)
{
  return __builtin_ia32_cvtsd2si ((__v2df) __A);
}

#ifdef __x86_64__
static __inline long long
_mm_cvtsd_si64x (__m128d __A)
{
  return __builtin_ia32_cvtsd2si64 ((__v2df) __A);
}
#endif

static __inline int
_mm_cvttsd_si32 (__m128d __A)
{
  return __builtin_ia32_cvttsd2si ((__v2df) __A);
}

#ifdef __x86_64__
static __inline long long
_mm_cvttsd_si64x (__m128d __A)
{
  return __builtin_ia32_cvttsd2si64 ((__v2df) __A);
}
#endif

static __inline __m128
_mm_cvtsd_ss (__m128 __A, __m128d __B)
{
  return (__m128)__builtin_ia32_cvtsd2ss ((__v4sf) __A, (__v2df) __B);
}

static __inline __m128d
_mm_cvtsi32_sd (__m128d __A, int __B)
{
  return (__m128d)__builtin_ia32_cvtsi2sd ((__v2df) __A, __B);
}

#ifdef __x86_64__
static __inline __m128d
_mm_cvtsi64x_sd (__m128d __A, long long __B)
{
  return (__m128d)__builtin_ia32_cvtsi642sd ((__v2df) __A, __B);
}
#endif

static __inline __m128d
_mm_cvtss_sd (__m128d __A, __m128 __B)
{
  return (__m128d)__builtin_ia32_cvtss2sd ((__v2df) __A, (__v4sf)__B);
}

#define _mm_shuffle_pd(__A, __B, __C) ((__m128d)__builtin_ia32_shufpd ((__v2df)__A, (__v2df)__B, (__C)))

static __inline __m128d
_mm_unpackhi_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_unpckhpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_unpacklo_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_unpcklpd ((__v2df)__A, (__v2df)__B);
}

static __inline __m128d
_mm_loadh_pd (__m128d __A, double const *__B)
{
  return (__m128d)__builtin_ia32_loadhpd ((__v2df)__A, (__v2si *)__B);
}

static __inline void
_mm_storeh_pd (double *__A, __m128d __B)
{
  __builtin_ia32_storehpd ((__v2si *)__A, (__v2df)__B);
}

static __inline __m128d
_mm_loadl_pd (__m128d __A, double const *__B)
{
  return (__m128d)__builtin_ia32_loadlpd ((__v2df)__A, (__v2si *)__B);
}

static __inline void
_mm_storel_pd (double *__A, __m128d __B)
{
  __builtin_ia32_storelpd ((__v2si *)__A, (__v2df)__B);
}

static __inline int
_mm_movemask_pd (__m128d __A)
{
  return __builtin_ia32_movmskpd ((__v2df)__A);
}

static __inline __m128i
_mm_packs_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_packsswb128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_packs_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_packssdw128 ((__v4si)__A, (__v4si)__B);
}

static __inline __m128i
_mm_packus_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_packuswb128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_unpackhi_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckhbw128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_unpackhi_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckhwd128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_unpackhi_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckhdq128 ((__v4si)__A, (__v4si)__B);
}

static __inline __m128i
_mm_unpackhi_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckhqdq128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_unpacklo_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpcklbw128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_unpacklo_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpcklwd128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_unpacklo_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckldq128 ((__v4si)__A, (__v4si)__B);
}

static __inline __m128i
_mm_unpacklo_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpcklqdq128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_add_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddb128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_add_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_add_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddd128 ((__v4si)__A, (__v4si)__B);
}

static __inline __m128i
_mm_add_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddq128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_adds_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddsb128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_adds_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddsw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_adds_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddusb128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_adds_epu16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddusw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_sub_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubb128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_sub_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_sub_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubd128 ((__v4si)__A, (__v4si)__B);
}

static __inline __m128i
_mm_sub_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubq128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_subs_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubsb128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_subs_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubsw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_subs_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubusb128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_subs_epu16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubusw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_madd_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmaddwd128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_mulhi_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmulhw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_mullo_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmullw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m64
_mm_mul_su32 (__m64 __A, __m64 __B)
{
  return (__m64)__builtin_ia32_pmuludq ((__v2si)__A, (__v2si)__B);
}

static __inline __m128i
_mm_mul_epu32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmuludq128 ((__v4si)__A, (__v4si)__B);
}

static __inline __m128i
_mm_sll_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psllw128 ((__v8hi)__A, (__v2di)__B);
}

static __inline __m128i
_mm_sll_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pslld128 ((__v4si)__A, (__v2di)__B);
}

static __inline __m128i
_mm_sll_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psllq128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_sra_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psraw128 ((__v8hi)__A, (__v2di)__B);
}

static __inline __m128i
_mm_sra_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psrad128 ((__v4si)__A, (__v2di)__B);
}

static __inline __m128i
_mm_srl_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psrlw128 ((__v8hi)__A, (__v2di)__B);
}

static __inline __m128i
_mm_srl_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psrld128 ((__v4si)__A, (__v2di)__B);
}

static __inline __m128i
_mm_srl_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psrlq128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_slli_epi16 (__m128i __A, int __B)
{
  return (__m128i)__builtin_ia32_psllwi128 ((__v8hi)__A, __B);
}

static __inline __m128i
_mm_slli_epi32 (__m128i __A, int __B)
{
  return (__m128i)__builtin_ia32_pslldi128 ((__v4si)__A, __B);
}

static __inline __m128i
_mm_slli_epi64 (__m128i __A, int __B)
{
  return (__m128i)__builtin_ia32_psllqi128 ((__v2di)__A, __B);
}

static __inline __m128i
_mm_srai_epi16 (__m128i __A, int __B)
{
  return (__m128i)__builtin_ia32_psrawi128 ((__v8hi)__A, __B);
}

static __inline __m128i
_mm_srai_epi32 (__m128i __A, int __B)
{
  return (__m128i)__builtin_ia32_psradi128 ((__v4si)__A, __B);
}

#if 0
static __m128i __attribute__((__always_inline__))
_mm_srli_si128 (__m128i __A, const int __B)
{
  return ((__m128i)__builtin_ia32_psrldqi128 (__A, __B))
}

static __m128i __attribute__((__always_inline__))
_mm_srli_si128 (__m128i __A, const int __B)
{
  return ((__m128i)__builtin_ia32_pslldqi128 (__A, __B))
}
#endif
#define _mm_srli_si128(__A, __B) ((__m128i)__builtin_ia32_psrldqi128 (__A, __B))
#define _mm_slli_si128(__A, __B) ((__m128i)__builtin_ia32_pslldqi128 (__A, __B))

static __inline __m128i
_mm_srli_epi16 (__m128i __A, int __B)
{
  return (__m128i)__builtin_ia32_psrlwi128 ((__v8hi)__A, __B);
}

static __inline __m128i
_mm_srli_epi32 (__m128i __A, int __B)
{
  return (__m128i)__builtin_ia32_psrldi128 ((__v4si)__A, __B);
}

static __inline __m128i
_mm_srli_epi64 (__m128i __A, int __B)
{
  return (__m128i)__builtin_ia32_psrlqi128 ((__v2di)__A, __B);
}

static __inline __m128i
_mm_and_si128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pand128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_andnot_si128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pandn128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_or_si128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_por128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_xor_si128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pxor128 ((__v2di)__A, (__v2di)__B);
}

static __inline __m128i
_mm_cmpeq_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpeqb128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_cmpeq_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpeqw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_cmpeq_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpeqd128 ((__v4si)__A, (__v4si)__B);
}

static __inline __m128i
_mm_cmplt_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtb128 ((__v16qi)__B, (__v16qi)__A);
}

static __inline __m128i
_mm_cmplt_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtw128 ((__v8hi)__B, (__v8hi)__A);
}

static __inline __m128i
_mm_cmplt_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtd128 ((__v4si)__B, (__v4si)__A);
}

static __inline __m128i
_mm_cmpgt_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtb128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_cmpgt_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_cmpgt_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtd128 ((__v4si)__A, (__v4si)__B);
}

#define _mm_extract_epi16(__A, __B) __builtin_ia32_pextrw128 ((__v8hi)__A, __B)

#define _mm_insert_epi16(__A, __B, __C) ((__m128i)__builtin_ia32_pinsrw128 ((__v8hi)__A, __B, __C))

static __inline __m128i
_mm_max_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmaxsw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_max_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmaxub128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_min_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pminsw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_min_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pminub128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline int
_mm_movemask_epi8 (__m128i __A)
{
  return __builtin_ia32_pmovmskb128 ((__v16qi)__A);
}

static __inline __m128i
_mm_mulhi_epu16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmulhuw128 ((__v8hi)__A, (__v8hi)__B);
}

#define _mm_shufflehi_epi16(__A, __B) ((__m128i)__builtin_ia32_pshufhw ((__v8hi)__A, __B))
#define _mm_shufflelo_epi16(__A, __B) ((__m128i)__builtin_ia32_pshuflw ((__v8hi)__A, __B))
#define _mm_shuffle_epi32(__A, __B) ((__m128i)__builtin_ia32_pshufd ((__v4si)__A, __B))

static __inline void
_mm_maskmoveu_si128 (__m128i __A, __m128i __B, char *__C)
{
  __builtin_ia32_maskmovdqu ((__v16qi)__A, (__v16qi)__B, __C);
}

static __inline __m128i
_mm_avg_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pavgb128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline __m128i
_mm_avg_epu16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pavgw128 ((__v8hi)__A, (__v8hi)__B);
}

static __inline __m128i
_mm_sad_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psadbw128 ((__v16qi)__A, (__v16qi)__B);
}

static __inline void
_mm_stream_si32 (int *__A, int __B)
{
  __builtin_ia32_movnti (__A, __B);
}

static __inline void
_mm_stream_si128 (__m128i *__A, __m128i __B)
{
  __builtin_ia32_movntdq ((__v2di *)__A, (__v2di)__B);
}

static __inline void
_mm_stream_pd (double *__A, __m128d __B)
{
  __builtin_ia32_movntpd (__A, (__v2df)__B);
}

static __inline __m128i
_mm_movpi64_epi64 (__m64 __A)
{
  return (__m128i)__builtin_ia32_movq2dq ((unsigned long long)__A);
}

static __inline void
_mm_clflush (void const *__A)
{
  return __builtin_ia32_clflush (__A);
}

static __inline void
_mm_lfence (void)
{
  __builtin_ia32_lfence ();
}

static __inline void
_mm_mfence (void)
{
  __builtin_ia32_mfence ();
}

static __inline __m128i
_mm_cvtsi32_si128 (int __A)
{
  return (__m128i) __builtin_ia32_loadd (&__A);
}

#ifdef __x86_64__
static __inline __m128i
_mm_cvtsi64x_si128 (long long __A)
{
  return (__m128i) __builtin_ia32_movq2dq (__A);
}
#endif

static __inline int
_mm_cvtsi128_si32 (__m128i __A)
{
  int __tmp;
  __builtin_ia32_stored (&__tmp, (__v4si)__A);
  return __tmp;
}

#ifdef __x86_64__
static __inline long long
_mm_cvtsi128_si64x (__m128i __A)
{
  return __builtin_ia32_movdq2q ((__v2di)__A);
}
#endif

#endif /* __SSE2__  */

#endif /* __SSE__ */
#endif /* _XMMINTRIN_H_INCLUDED */
