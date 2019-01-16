/* Copyright (C) 2002-2019 Free Software Foundation, Inc.

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

#ifndef _MMINTRIN_H_INCLUDED
#define _MMINTRIN_H_INCLUDED

#ifndef __IWMMXT__
#error mmintrin.h included without enabling WMMX/WMMX2 instructions (e.g. -march=iwmmxt or -march=iwmmxt2)
#endif


#if defined __cplusplus
extern "C" {
/* Intrinsics use C name-mangling.  */
#endif /* __cplusplus */

/* The data type intended for user use.  */
typedef unsigned long long __m64, __int64;

/* Internal data types for implementing the intrinsics.  */
typedef int __v2si __attribute__ ((vector_size (8)));
typedef short __v4hi __attribute__ ((vector_size (8)));
typedef signed char __v8qi __attribute__ ((vector_size (8)));

/* Provided for source compatibility with MMX.  */
extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_empty (void)
{
}

/* "Convert" __m64 and __int64 into each other.  */
static __inline __m64
_mm_cvtsi64_m64 (__int64 __i)
{
  return __i;
}

static __inline __int64
_mm_cvtm64_si64 (__m64 __i)
{
  return __i;
}

static __inline int
_mm_cvtsi64_si32 (__int64 __i)
{
  return __i;
}

static __inline __int64
_mm_cvtsi32_si64 (int __i)
{
  return (__i & 0xffffffff);
}

/* Pack the four 16-bit values from M1 into the lower four 8-bit values of
   the result, and the four 16-bit values from M2 into the upper four 8-bit
   values of the result, all with signed saturation.  */
static __inline __m64
_mm_packs_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wpackhss ((__v4hi)__m1, (__v4hi)__m2);
}

/* Pack the two 32-bit values from M1 in to the lower two 16-bit values of
   the result, and the two 32-bit values from M2 into the upper two 16-bit
   values of the result, all with signed saturation.  */
static __inline __m64
_mm_packs_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wpackwss ((__v2si)__m1, (__v2si)__m2);
}

/* Copy the 64-bit value from M1 into the lower 32-bits of the result, and
   the 64-bit value from M2 into the upper 32-bits of the result, all with
   signed saturation for values that do not fit exactly into 32-bits.  */
static __inline __m64
_mm_packs_pi64 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wpackdss ((long long)__m1, (long long)__m2);
}

/* Pack the four 16-bit values from M1 into the lower four 8-bit values of
   the result, and the four 16-bit values from M2 into the upper four 8-bit
   values of the result, all with unsigned saturation.  */
static __inline __m64
_mm_packs_pu16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wpackhus ((__v4hi)__m1, (__v4hi)__m2);
}

/* Pack the two 32-bit values from M1 into the lower two 16-bit values of
   the result, and the two 32-bit values from M2 into the upper two 16-bit
   values of the result, all with unsigned saturation.  */
static __inline __m64
_mm_packs_pu32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wpackwus ((__v2si)__m1, (__v2si)__m2);
}

/* Copy the 64-bit value from M1 into the lower 32-bits of the result, and
   the 64-bit value from M2 into the upper 32-bits of the result, all with
   unsigned saturation for values that do not fit exactly into 32-bits.  */
static __inline __m64
_mm_packs_pu64 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wpackdus ((long long)__m1, (long long)__m2);
}

/* Interleave the four 8-bit values from the high half of M1 with the four
   8-bit values from the high half of M2.  */
static __inline __m64
_mm_unpackhi_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wunpckihb ((__v8qi)__m1, (__v8qi)__m2);
}

/* Interleave the two 16-bit values from the high half of M1 with the two
   16-bit values from the high half of M2.  */
static __inline __m64
_mm_unpackhi_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wunpckihh ((__v4hi)__m1, (__v4hi)__m2);
}

/* Interleave the 32-bit value from the high half of M1 with the 32-bit
   value from the high half of M2.  */
static __inline __m64
_mm_unpackhi_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wunpckihw ((__v2si)__m1, (__v2si)__m2);
}

/* Interleave the four 8-bit values from the low half of M1 with the four
   8-bit values from the low half of M2.  */
static __inline __m64
_mm_unpacklo_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wunpckilb ((__v8qi)__m1, (__v8qi)__m2);
}

/* Interleave the two 16-bit values from the low half of M1 with the two
   16-bit values from the low half of M2.  */
static __inline __m64
_mm_unpacklo_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wunpckilh ((__v4hi)__m1, (__v4hi)__m2);
}

/* Interleave the 32-bit value from the low half of M1 with the 32-bit
   value from the low half of M2.  */
static __inline __m64
_mm_unpacklo_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wunpckilw ((__v2si)__m1, (__v2si)__m2);
}

/* Take the four 8-bit values from the low half of M1, sign extend them,
   and return the result as a vector of four 16-bit quantities.  */
static __inline __m64
_mm_unpackel_pi8 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckelsb ((__v8qi)__m1);
}

/* Take the two 16-bit values from the low half of M1, sign extend them,
   and return the result as a vector of two 32-bit quantities.  */
static __inline __m64
_mm_unpackel_pi16 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckelsh ((__v4hi)__m1);
}

/* Take the 32-bit value from the low half of M1, and return it sign extended
  to 64 bits.  */
static __inline __m64
_mm_unpackel_pi32 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckelsw ((__v2si)__m1);
}

/* Take the four 8-bit values from the high half of M1, sign extend them,
   and return the result as a vector of four 16-bit quantities.  */
static __inline __m64
_mm_unpackeh_pi8 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckehsb ((__v8qi)__m1);
}

/* Take the two 16-bit values from the high half of M1, sign extend them,
   and return the result as a vector of two 32-bit quantities.  */
static __inline __m64
_mm_unpackeh_pi16 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckehsh ((__v4hi)__m1);
}

/* Take the 32-bit value from the high half of M1, and return it sign extended
  to 64 bits.  */
static __inline __m64
_mm_unpackeh_pi32 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckehsw ((__v2si)__m1);
}

/* Take the four 8-bit values from the low half of M1, zero extend them,
   and return the result as a vector of four 16-bit quantities.  */
static __inline __m64
_mm_unpackel_pu8 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckelub ((__v8qi)__m1);
}

/* Take the two 16-bit values from the low half of M1, zero extend them,
   and return the result as a vector of two 32-bit quantities.  */
static __inline __m64
_mm_unpackel_pu16 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckeluh ((__v4hi)__m1);
}

/* Take the 32-bit value from the low half of M1, and return it zero extended
  to 64 bits.  */
static __inline __m64
_mm_unpackel_pu32 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckeluw ((__v2si)__m1);
}

/* Take the four 8-bit values from the high half of M1, zero extend them,
   and return the result as a vector of four 16-bit quantities.  */
static __inline __m64
_mm_unpackeh_pu8 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckehub ((__v8qi)__m1);
}

/* Take the two 16-bit values from the high half of M1, zero extend them,
   and return the result as a vector of two 32-bit quantities.  */
static __inline __m64
_mm_unpackeh_pu16 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckehuh ((__v4hi)__m1);
}

/* Take the 32-bit value from the high half of M1, and return it zero extended
  to 64 bits.  */
static __inline __m64
_mm_unpackeh_pu32 (__m64 __m1)
{
  return (__m64) __builtin_arm_wunpckehuw ((__v2si)__m1);
}

/* Add the 8-bit values in M1 to the 8-bit values in M2.  */
static __inline __m64
_mm_add_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_waddb ((__v8qi)__m1, (__v8qi)__m2);
}

/* Add the 16-bit values in M1 to the 16-bit values in M2.  */
static __inline __m64
_mm_add_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_waddh ((__v4hi)__m1, (__v4hi)__m2);
}

/* Add the 32-bit values in M1 to the 32-bit values in M2.  */
static __inline __m64
_mm_add_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_waddw ((__v2si)__m1, (__v2si)__m2);
}

/* Add the 8-bit values in M1 to the 8-bit values in M2 using signed
   saturated arithmetic.  */
static __inline __m64
_mm_adds_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_waddbss ((__v8qi)__m1, (__v8qi)__m2);
}

/* Add the 16-bit values in M1 to the 16-bit values in M2 using signed
   saturated arithmetic.  */
static __inline __m64
_mm_adds_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_waddhss ((__v4hi)__m1, (__v4hi)__m2);
}

/* Add the 32-bit values in M1 to the 32-bit values in M2 using signed
   saturated arithmetic.  */
static __inline __m64
_mm_adds_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_waddwss ((__v2si)__m1, (__v2si)__m2);
}

/* Add the 8-bit values in M1 to the 8-bit values in M2 using unsigned
   saturated arithmetic.  */
static __inline __m64
_mm_adds_pu8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_waddbus ((__v8qi)__m1, (__v8qi)__m2);
}

/* Add the 16-bit values in M1 to the 16-bit values in M2 using unsigned
   saturated arithmetic.  */
static __inline __m64
_mm_adds_pu16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_waddhus ((__v4hi)__m1, (__v4hi)__m2);
}

/* Add the 32-bit values in M1 to the 32-bit values in M2 using unsigned
   saturated arithmetic.  */
static __inline __m64
_mm_adds_pu32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_waddwus ((__v2si)__m1, (__v2si)__m2);
}

/* Subtract the 8-bit values in M2 from the 8-bit values in M1.  */
static __inline __m64
_mm_sub_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wsubb ((__v8qi)__m1, (__v8qi)__m2);
}

/* Subtract the 16-bit values in M2 from the 16-bit values in M1.  */
static __inline __m64
_mm_sub_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wsubh ((__v4hi)__m1, (__v4hi)__m2);
}

/* Subtract the 32-bit values in M2 from the 32-bit values in M1.  */
static __inline __m64
_mm_sub_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wsubw ((__v2si)__m1, (__v2si)__m2);
}

/* Subtract the 8-bit values in M2 from the 8-bit values in M1 using signed
   saturating arithmetic.  */
static __inline __m64
_mm_subs_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wsubbss ((__v8qi)__m1, (__v8qi)__m2);
}

/* Subtract the 16-bit values in M2 from the 16-bit values in M1 using
   signed saturating arithmetic.  */
static __inline __m64
_mm_subs_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wsubhss ((__v4hi)__m1, (__v4hi)__m2);
}

/* Subtract the 32-bit values in M2 from the 32-bit values in M1 using
   signed saturating arithmetic.  */
static __inline __m64
_mm_subs_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wsubwss ((__v2si)__m1, (__v2si)__m2);
}

/* Subtract the 8-bit values in M2 from the 8-bit values in M1 using
   unsigned saturating arithmetic.  */
static __inline __m64
_mm_subs_pu8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wsubbus ((__v8qi)__m1, (__v8qi)__m2);
}

/* Subtract the 16-bit values in M2 from the 16-bit values in M1 using
   unsigned saturating arithmetic.  */
static __inline __m64
_mm_subs_pu16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wsubhus ((__v4hi)__m1, (__v4hi)__m2);
}

/* Subtract the 32-bit values in M2 from the 32-bit values in M1 using
   unsigned saturating arithmetic.  */
static __inline __m64
_mm_subs_pu32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wsubwus ((__v2si)__m1, (__v2si)__m2);
}

/* Multiply four 16-bit values in M1 by four 16-bit values in M2 producing
   four 32-bit intermediate results, which are then summed by pairs to
   produce two 32-bit results.  */
static __inline __m64
_mm_madd_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wmadds ((__v4hi)__m1, (__v4hi)__m2);
}

/* Multiply four 16-bit values in M1 by four 16-bit values in M2 producing
   four 32-bit intermediate results, which are then summed by pairs to
   produce two 32-bit results.  */
static __inline __m64
_mm_madd_pu16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wmaddu ((__v4hi)__m1, (__v4hi)__m2);
}

/* Multiply four signed 16-bit values in M1 by four signed 16-bit values in
   M2 and produce the high 16 bits of the 32-bit results.  */
static __inline __m64
_mm_mulhi_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wmulsm ((__v4hi)__m1, (__v4hi)__m2);
}

/* Multiply four signed 16-bit values in M1 by four signed 16-bit values in
   M2 and produce the high 16 bits of the 32-bit results.  */
static __inline __m64
_mm_mulhi_pu16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wmulum ((__v4hi)__m1, (__v4hi)__m2);
}

/* Multiply four 16-bit values in M1 by four 16-bit values in M2 and produce
   the low 16 bits of the results.  */
static __inline __m64
_mm_mullo_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wmulul ((__v4hi)__m1, (__v4hi)__m2);
}

/* Shift four 16-bit values in M left by COUNT.  */
static __inline __m64
_mm_sll_pi16 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wsllh ((__v4hi)__m, __count);
}

static __inline __m64
_mm_slli_pi16 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wsllhi ((__v4hi)__m, __count);
}

/* Shift two 32-bit values in M left by COUNT.  */
static __inline __m64
_mm_sll_pi32 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wsllw ((__v2si)__m, __count);
}

static __inline __m64
_mm_slli_pi32 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wsllwi ((__v2si)__m, __count);
}

/* Shift the 64-bit value in M left by COUNT.  */
static __inline __m64
_mm_sll_si64 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wslld (__m, __count);
}

static __inline __m64
_mm_slli_si64 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wslldi (__m, __count);
}

/* Shift four 16-bit values in M right by COUNT; shift in the sign bit.  */
static __inline __m64
_mm_sra_pi16 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wsrah ((__v4hi)__m, __count);
}

static __inline __m64
_mm_srai_pi16 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wsrahi ((__v4hi)__m, __count);
}

/* Shift two 32-bit values in M right by COUNT; shift in the sign bit.  */
static __inline __m64
_mm_sra_pi32 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wsraw ((__v2si)__m, __count);
}

static __inline __m64
_mm_srai_pi32 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wsrawi ((__v2si)__m, __count);
}

/* Shift the 64-bit value in M right by COUNT; shift in the sign bit.  */
static __inline __m64
_mm_sra_si64 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wsrad (__m, __count);
}

static __inline __m64
_mm_srai_si64 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wsradi (__m, __count);
}

/* Shift four 16-bit values in M right by COUNT; shift in zeros.  */
static __inline __m64
_mm_srl_pi16 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wsrlh ((__v4hi)__m, __count);
}

static __inline __m64
_mm_srli_pi16 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wsrlhi ((__v4hi)__m, __count);
}

/* Shift two 32-bit values in M right by COUNT; shift in zeros.  */
static __inline __m64
_mm_srl_pi32 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wsrlw ((__v2si)__m, __count);
}

static __inline __m64
_mm_srli_pi32 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wsrlwi ((__v2si)__m, __count);
}

/* Shift the 64-bit value in M left by COUNT; shift in zeros.  */
static __inline __m64
_mm_srl_si64 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wsrld (__m, __count);
}

static __inline __m64
_mm_srli_si64 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wsrldi (__m, __count);
}

/* Rotate four 16-bit values in M right by COUNT.  */
static __inline __m64
_mm_ror_pi16 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wrorh ((__v4hi)__m, __count);
}

static __inline __m64
_mm_rori_pi16 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wrorhi ((__v4hi)__m, __count);
}

/* Rotate two 32-bit values in M right by COUNT.  */
static __inline __m64
_mm_ror_pi32 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wrorw ((__v2si)__m, __count);
}

static __inline __m64
_mm_rori_pi32 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wrorwi ((__v2si)__m, __count);
}

/* Rotate two 64-bit values in M right by COUNT.  */
static __inline __m64
_mm_ror_si64 (__m64 __m, __m64 __count)
{
  return (__m64) __builtin_arm_wrord (__m, __count);
}

static __inline __m64
_mm_rori_si64 (__m64 __m, int __count)
{
  return (__m64) __builtin_arm_wrordi (__m, __count);
}

/* Bit-wise AND the 64-bit values in M1 and M2.  */
static __inline __m64
_mm_and_si64 (__m64 __m1, __m64 __m2)
{
  return __builtin_arm_wand (__m1, __m2);
}

/* Bit-wise complement the 64-bit value in M1 and bit-wise AND it with the
   64-bit value in M2.  */
static __inline __m64
_mm_andnot_si64 (__m64 __m1, __m64 __m2)
{
  return __builtin_arm_wandn (__m2, __m1);
}

/* Bit-wise inclusive OR the 64-bit values in M1 and M2.  */
static __inline __m64
_mm_or_si64 (__m64 __m1, __m64 __m2)
{
  return __builtin_arm_wor (__m1, __m2);
}

/* Bit-wise exclusive OR the 64-bit values in M1 and M2.  */
static __inline __m64
_mm_xor_si64 (__m64 __m1, __m64 __m2)
{
  return __builtin_arm_wxor (__m1, __m2);
}

/* Compare eight 8-bit values.  The result of the comparison is 0xFF if the
   test is true and zero if false.  */
static __inline __m64
_mm_cmpeq_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wcmpeqb ((__v8qi)__m1, (__v8qi)__m2);
}

static __inline __m64
_mm_cmpgt_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wcmpgtsb ((__v8qi)__m1, (__v8qi)__m2);
}

static __inline __m64
_mm_cmpgt_pu8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wcmpgtub ((__v8qi)__m1, (__v8qi)__m2);
}

/* Compare four 16-bit values.  The result of the comparison is 0xFFFF if
   the test is true and zero if false.  */
static __inline __m64
_mm_cmpeq_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wcmpeqh ((__v4hi)__m1, (__v4hi)__m2);
}

static __inline __m64
_mm_cmpgt_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wcmpgtsh ((__v4hi)__m1, (__v4hi)__m2);
}

static __inline __m64
_mm_cmpgt_pu16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wcmpgtuh ((__v4hi)__m1, (__v4hi)__m2);
}

/* Compare two 32-bit values.  The result of the comparison is 0xFFFFFFFF if
   the test is true and zero if false.  */
static __inline __m64
_mm_cmpeq_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wcmpeqw ((__v2si)__m1, (__v2si)__m2);
}

static __inline __m64
_mm_cmpgt_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wcmpgtsw ((__v2si)__m1, (__v2si)__m2);
}

static __inline __m64
_mm_cmpgt_pu32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_arm_wcmpgtuw ((__v2si)__m1, (__v2si)__m2);
}

/* Element-wise multiplication of unsigned 16-bit values __B and __C, followed
   by accumulate across all elements and __A.  */
static __inline __m64
_mm_mac_pu16 (__m64 __A, __m64 __B, __m64 __C)
{
  return __builtin_arm_wmacu (__A, (__v4hi)__B, (__v4hi)__C);
}

/* Element-wise multiplication of signed 16-bit values __B and __C, followed
   by accumulate across all elements and __A.  */
static __inline __m64
_mm_mac_pi16 (__m64 __A, __m64 __B, __m64 __C)
{
  return __builtin_arm_wmacs (__A, (__v4hi)__B, (__v4hi)__C);
}

/* Element-wise multiplication of unsigned 16-bit values __B and __C, followed
   by accumulate across all elements.  */
static __inline __m64
_mm_macz_pu16 (__m64 __A, __m64 __B)
{
  return __builtin_arm_wmacuz ((__v4hi)__A, (__v4hi)__B);
}

/* Element-wise multiplication of signed 16-bit values __B and __C, followed
   by accumulate across all elements.  */
static __inline __m64
_mm_macz_pi16 (__m64 __A, __m64 __B)
{
  return __builtin_arm_wmacsz ((__v4hi)__A, (__v4hi)__B);
}

/* Accumulate across all unsigned 8-bit values in __A.  */
static __inline __m64
_mm_acc_pu8 (__m64 __A)
{
  return __builtin_arm_waccb ((__v8qi)__A);
}

/* Accumulate across all unsigned 16-bit values in __A.  */
static __inline __m64
_mm_acc_pu16 (__m64 __A)
{
  return __builtin_arm_wacch ((__v4hi)__A);
}

/* Accumulate across all unsigned 32-bit values in __A.  */
static __inline __m64
_mm_acc_pu32 (__m64 __A)
{
  return __builtin_arm_waccw ((__v2si)__A);
}

static __inline __m64
_mm_mia_si64 (__m64 __A, int __B, int __C)
{
  return __builtin_arm_tmia (__A, __B, __C);
}

static __inline __m64
_mm_miaph_si64 (__m64 __A, int __B, int __C)
{
  return __builtin_arm_tmiaph (__A, __B, __C);
}

static __inline __m64
_mm_miabb_si64 (__m64 __A, int __B, int __C)
{
  return __builtin_arm_tmiabb (__A, __B, __C);
}

static __inline __m64
_mm_miabt_si64 (__m64 __A, int __B, int __C)
{
  return __builtin_arm_tmiabt (__A, __B, __C);
}

static __inline __m64
_mm_miatb_si64 (__m64 __A, int __B, int __C)
{
  return __builtin_arm_tmiatb (__A, __B, __C);
}

static __inline __m64
_mm_miatt_si64 (__m64 __A, int __B, int __C)
{
  return __builtin_arm_tmiatt (__A, __B, __C);
}

/* Extract one of the elements of A and sign extend.  The selector N must
   be immediate.  */
#define _mm_extract_pi8(A, N) __builtin_arm_textrmsb ((__v8qi)(A), (N))
#define _mm_extract_pi16(A, N) __builtin_arm_textrmsh ((__v4hi)(A), (N))
#define _mm_extract_pi32(A, N) __builtin_arm_textrmsw ((__v2si)(A), (N))

/* Extract one of the elements of A and zero extend.  The selector N must
   be immediate.  */
#define _mm_extract_pu8(A, N) __builtin_arm_textrmub ((__v8qi)(A), (N))
#define _mm_extract_pu16(A, N) __builtin_arm_textrmuh ((__v4hi)(A), (N))
#define _mm_extract_pu32(A, N) __builtin_arm_textrmuw ((__v2si)(A), (N))

/* Inserts word D into one of the elements of A.  The selector N must be
   immediate.  */
#define _mm_insert_pi8(A, D, N) \
  ((__m64) __builtin_arm_tinsrb ((__v8qi)(A), (D), (N)))
#define _mm_insert_pi16(A, D, N) \
  ((__m64) __builtin_arm_tinsrh ((__v4hi)(A), (D), (N)))
#define _mm_insert_pi32(A, D, N) \
  ((__m64) __builtin_arm_tinsrw ((__v2si)(A), (D), (N)))

/* Compute the element-wise maximum of signed 8-bit values.  */
static __inline __m64
_mm_max_pi8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wmaxsb ((__v8qi)__A, (__v8qi)__B);
}

/* Compute the element-wise maximum of signed 16-bit values.  */
static __inline __m64
_mm_max_pi16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wmaxsh ((__v4hi)__A, (__v4hi)__B);
}

/* Compute the element-wise maximum of signed 32-bit values.  */
static __inline __m64
_mm_max_pi32 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wmaxsw ((__v2si)__A, (__v2si)__B);
}

/* Compute the element-wise maximum of unsigned 8-bit values.  */
static __inline __m64
_mm_max_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wmaxub ((__v8qi)__A, (__v8qi)__B);
}

/* Compute the element-wise maximum of unsigned 16-bit values.  */
static __inline __m64
_mm_max_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wmaxuh ((__v4hi)__A, (__v4hi)__B);
}

/* Compute the element-wise maximum of unsigned 32-bit values.  */
static __inline __m64
_mm_max_pu32 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wmaxuw ((__v2si)__A, (__v2si)__B);
}

/* Compute the element-wise minimum of signed 16-bit values.  */
static __inline __m64
_mm_min_pi8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wminsb ((__v8qi)__A, (__v8qi)__B);
}

/* Compute the element-wise minimum of signed 16-bit values.  */
static __inline __m64
_mm_min_pi16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wminsh ((__v4hi)__A, (__v4hi)__B);
}

/* Compute the element-wise minimum of signed 32-bit values.  */
static __inline __m64
_mm_min_pi32 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wminsw ((__v2si)__A, (__v2si)__B);
}

/* Compute the element-wise minimum of unsigned 16-bit values.  */
static __inline __m64
_mm_min_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wminub ((__v8qi)__A, (__v8qi)__B);
}

/* Compute the element-wise minimum of unsigned 16-bit values.  */
static __inline __m64
_mm_min_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wminuh ((__v4hi)__A, (__v4hi)__B);
}

/* Compute the element-wise minimum of unsigned 32-bit values.  */
static __inline __m64
_mm_min_pu32 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wminuw ((__v2si)__A, (__v2si)__B);
}

/* Create an 8-bit mask of the signs of 8-bit values.  */
static __inline int
_mm_movemask_pi8 (__m64 __A)
{
  return __builtin_arm_tmovmskb ((__v8qi)__A);
}

/* Create an 8-bit mask of the signs of 16-bit values.  */
static __inline int
_mm_movemask_pi16 (__m64 __A)
{
  return __builtin_arm_tmovmskh ((__v4hi)__A);
}

/* Create an 8-bit mask of the signs of 32-bit values.  */
static __inline int
_mm_movemask_pi32 (__m64 __A)
{
  return __builtin_arm_tmovmskw ((__v2si)__A);
}

/* Return a combination of the four 16-bit values in A.  The selector
   must be an immediate.  */
#define _mm_shuffle_pi16(A, N) \
  ((__m64) __builtin_arm_wshufh ((__v4hi)(A), (N)))


/* Compute the rounded averages of the unsigned 8-bit values in A and B.  */
static __inline __m64
_mm_avg_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wavg2br ((__v8qi)__A, (__v8qi)__B);
}

/* Compute the rounded averages of the unsigned 16-bit values in A and B.  */
static __inline __m64
_mm_avg_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wavg2hr ((__v4hi)__A, (__v4hi)__B);
}

/* Compute the averages of the unsigned 8-bit values in A and B.  */
static __inline __m64
_mm_avg2_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wavg2b ((__v8qi)__A, (__v8qi)__B);
}

/* Compute the averages of the unsigned 16-bit values in A and B.  */
static __inline __m64
_mm_avg2_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wavg2h ((__v4hi)__A, (__v4hi)__B);
}

/* Compute the sum of the absolute differences of the unsigned 8-bit
   values in A and B.  Return the value in the lower 16-bit word; the
   upper words are cleared.  */
static __inline __m64
_mm_sad_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wsadbz ((__v8qi)__A, (__v8qi)__B);
}

static __inline __m64
_mm_sada_pu8 (__m64 __A, __m64 __B, __m64 __C)
{
  return (__m64) __builtin_arm_wsadb ((__v2si)__A, (__v8qi)__B, (__v8qi)__C);
}

/* Compute the sum of the absolute differences of the unsigned 16-bit
   values in A and B.  Return the value in the lower 32-bit word; the
   upper words are cleared.  */
static __inline __m64
_mm_sad_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wsadhz ((__v4hi)__A, (__v4hi)__B);
}

static __inline __m64
_mm_sada_pu16 (__m64 __A, __m64 __B, __m64 __C)
{
  return (__m64) __builtin_arm_wsadh ((__v2si)__A, (__v4hi)__B, (__v4hi)__C);
}


/* Compute the sum of the absolute differences of the unsigned 8-bit
   values in A and B.  Return the value in the lower 16-bit word; the
   upper words are cleared.  */
static __inline __m64
_mm_sadz_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wsadbz ((__v8qi)__A, (__v8qi)__B);
}

/* Compute the sum of the absolute differences of the unsigned 16-bit
   values in A and B.  Return the value in the lower 32-bit word; the
   upper words are cleared.  */
static __inline __m64
_mm_sadz_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_arm_wsadhz ((__v4hi)__A, (__v4hi)__B);
}

#define _mm_align_si64(__A,__B, N) \
  (__m64) __builtin_arm_walign ((__v8qi) (__A),(__v8qi) (__B), (N))

/* Creates a 64-bit zero.  */
static __inline __m64
_mm_setzero_si64 (void)
{
  return __builtin_arm_wzero ();
}

/* Set and Get arbitrary iWMMXt Control registers.
   Note only registers 0-3 and 8-11 are currently defined,
   the rest are reserved.  */

static __inline void
_mm_setwcx (const int __value, const int __regno)
{
  switch (__regno)
    {
    case 0:
      __asm __volatile ("tmcr wcid, %0" :: "r"(__value));
      break;
    case 1:
      __asm __volatile ("tmcr wcon, %0" :: "r"(__value));
      break;
    case 2:
      __asm __volatile ("tmcr wcssf, %0" :: "r"(__value));
      break;
    case 3:
      __asm __volatile ("tmcr wcasf, %0" :: "r"(__value));
      break;
    case 8:
      __builtin_arm_setwcgr0 (__value);
      break;
    case 9:
      __builtin_arm_setwcgr1 (__value);
      break;
    case 10:
      __builtin_arm_setwcgr2 (__value);
      break;
    case 11:
      __builtin_arm_setwcgr3 (__value);
      break;
    default:
      break;
    }
}

static __inline int
_mm_getwcx (const int __regno)
{
  int __value;
  switch (__regno)
    {
    case 0:
      __asm __volatile ("tmrc %0, wcid" : "=r"(__value));
      break;
    case 1:
      __asm __volatile ("tmrc %0, wcon" : "=r"(__value));
      break;
    case 2:
      __asm __volatile ("tmrc %0, wcssf" : "=r"(__value));
      break;
    case 3:
      __asm __volatile ("tmrc %0, wcasf" : "=r"(__value));
      break;
    case 8:
      return __builtin_arm_getwcgr0 ();
    case 9:
      return __builtin_arm_getwcgr1 ();
    case 10:
      return __builtin_arm_getwcgr2 ();
    case 11:
      return __builtin_arm_getwcgr3 ();
    default:
      break;
    }
  return __value;
}

/* Creates a vector of two 32-bit values; I0 is least significant.  */
static __inline __m64
_mm_set_pi32 (int __i1, int __i0)
{
  union
  {
    __m64 __q;
    struct
    {
      unsigned int __i0;
      unsigned int __i1;
    } __s;
  } __u;

  __u.__s.__i0 = __i0;
  __u.__s.__i1 = __i1;

  return __u.__q;
}

/* Creates a vector of four 16-bit values; W0 is least significant.  */
static __inline __m64
_mm_set_pi16 (short __w3, short __w2, short __w1, short __w0)
{
  unsigned int __i1 = (unsigned short) __w3 << 16 | (unsigned short) __w2;
  unsigned int __i0 = (unsigned short) __w1 << 16 | (unsigned short) __w0;

  return _mm_set_pi32 (__i1, __i0);
}

/* Creates a vector of eight 8-bit values; B0 is least significant.  */
static __inline __m64
_mm_set_pi8 (char __b7, char __b6, char __b5, char __b4,
	     char __b3, char __b2, char __b1, char __b0)
{
  unsigned int __i1, __i0;

  __i1 = (unsigned char)__b7;
  __i1 = __i1 << 8 | (unsigned char)__b6;
  __i1 = __i1 << 8 | (unsigned char)__b5;
  __i1 = __i1 << 8 | (unsigned char)__b4;

  __i0 = (unsigned char)__b3;
  __i0 = __i0 << 8 | (unsigned char)__b2;
  __i0 = __i0 << 8 | (unsigned char)__b1;
  __i0 = __i0 << 8 | (unsigned char)__b0;

  return _mm_set_pi32 (__i1, __i0);
}

/* Similar, but with the arguments in reverse order.  */
static __inline __m64
_mm_setr_pi32 (int __i0, int __i1)
{
  return _mm_set_pi32 (__i1, __i0);
}

static __inline __m64
_mm_setr_pi16 (short __w0, short __w1, short __w2, short __w3)
{
  return _mm_set_pi16 (__w3, __w2, __w1, __w0);
}

static __inline __m64
_mm_setr_pi8 (char __b0, char __b1, char __b2, char __b3,
	      char __b4, char __b5, char __b6, char __b7)
{
  return _mm_set_pi8 (__b7, __b6, __b5, __b4, __b3, __b2, __b1, __b0);
}

/* Creates a vector of two 32-bit values, both elements containing I.  */
static __inline __m64
_mm_set1_pi32 (int __i)
{
  return _mm_set_pi32 (__i, __i);
}

/* Creates a vector of four 16-bit values, all elements containing W.  */
static __inline __m64
_mm_set1_pi16 (short __w)
{
  unsigned int __i = (unsigned short)__w << 16 | (unsigned short)__w;
  return _mm_set1_pi32 (__i);
}

/* Creates a vector of four 16-bit values, all elements containing B.  */
static __inline __m64
_mm_set1_pi8 (char __b)
{
  unsigned int __w = (unsigned char)__b << 8 | (unsigned char)__b;
  unsigned int __i = __w << 16 | __w;
  return _mm_set1_pi32 (__i);
}

#ifdef __IWMMXT2__
static __inline __m64
_mm_abs_pi8 (__m64 m1)
{
  return (__m64) __builtin_arm_wabsb ((__v8qi)m1);
}

static __inline __m64
_mm_abs_pi16 (__m64 m1)
{
  return (__m64) __builtin_arm_wabsh ((__v4hi)m1);

}

static __inline __m64
_mm_abs_pi32 (__m64 m1)
{
  return (__m64) __builtin_arm_wabsw ((__v2si)m1);

}

static __inline __m64
_mm_addsubhx_pi16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_waddsubhx ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_absdiff_pu8 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wabsdiffb ((__v8qi)a, (__v8qi)b);
}

static __inline __m64
_mm_absdiff_pu16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wabsdiffh ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_absdiff_pu32 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wabsdiffw ((__v2si)a, (__v2si)b);
}

static __inline __m64
_mm_addc_pu16 (__m64 a, __m64 b)
{
  __m64 result;
  __asm__ __volatile__ ("waddhc	%0, %1, %2" : "=y" (result) : "y" (a),  "y" (b));
  return result;
}

static __inline __m64
_mm_addc_pu32 (__m64 a, __m64 b)
{
  __m64 result;
  __asm__ __volatile__ ("waddwc	%0, %1, %2" : "=y" (result) : "y" (a),  "y" (b));
  return result;
}

static __inline __m64
_mm_avg4_pu8 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wavg4 ((__v8qi)a, (__v8qi)b);
}

static __inline __m64
_mm_avg4r_pu8 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wavg4r ((__v8qi)a, (__v8qi)b);
}

static __inline __m64
_mm_maddx_pi16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmaddsx ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_maddx_pu16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmaddux ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_msub_pi16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmaddsn ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_msub_pu16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmaddun ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_mulhi_pi32 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmulwsm ((__v2si)a, (__v2si)b);
}

static __inline __m64
_mm_mulhi_pu32 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmulwum ((__v2si)a, (__v2si)b);
}

static __inline __m64
_mm_mulhir_pi16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmulsmr ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_mulhir_pi32 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmulwsmr ((__v2si)a, (__v2si)b);
}

static __inline __m64
_mm_mulhir_pu16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmulumr ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_mulhir_pu32 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmulwumr ((__v2si)a, (__v2si)b);
}

static __inline __m64
_mm_mullo_pi32 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wmulwl ((__v2si)a, (__v2si)b);
}

static __inline __m64
_mm_qmulm_pi16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wqmulm ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_qmulm_pi32 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wqmulwm ((__v2si)a, (__v2si)b);
}

static __inline __m64
_mm_qmulmr_pi16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wqmulmr ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_qmulmr_pi32 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wqmulwmr ((__v2si)a, (__v2si)b);
}

static __inline __m64
_mm_subaddhx_pi16 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_wsubaddhx ((__v4hi)a, (__v4hi)b);
}

static __inline __m64
_mm_addbhusl_pu8 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_waddbhusl ((__v4hi)a, (__v8qi)b);
}

static __inline __m64
_mm_addbhusm_pu8 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_waddbhusm ((__v4hi)a, (__v8qi)b);
}

#define _mm_qmiabb_pi32(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wqmiabb ((__v2si)_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_qmiabbn_pi32(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wqmiabbn ((__v2si)_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_qmiabt_pi32(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wqmiabt ((__v2si)_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_qmiabtn_pi32(acc, m1, m2) \
  ({\
   __m64 _acc=acc;\
   __m64 _m1=m1;\
   __m64 _m2=m2;\
   _acc = (__m64) __builtin_arm_wqmiabtn ((__v2si)_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_qmiatb_pi32(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wqmiatb ((__v2si)_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_qmiatbn_pi32(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wqmiatbn ((__v2si)_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_qmiatt_pi32(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wqmiatt ((__v2si)_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_qmiattn_pi32(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wqmiattn ((__v2si)_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_wmiabb_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiabb (_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_wmiabbn_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiabbn (_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_wmiabt_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiabt (_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_wmiabtn_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiabtn (_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_wmiatb_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiatb (_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_wmiatbn_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiatbn (_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_wmiatt_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiatt (_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_wmiattn_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiattn (_acc, (__v4hi)_m1, (__v4hi)_m2);\
   _acc;\
   })

#define _mm_wmiawbb_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiawbb (_acc, (__v2si)_m1, (__v2si)_m2);\
   _acc;\
   })

#define _mm_wmiawbbn_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiawbbn (_acc, (__v2si)_m1, (__v2si)_m2);\
   _acc;\
   })

#define _mm_wmiawbt_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiawbt (_acc, (__v2si)_m1, (__v2si)_m2);\
   _acc;\
   })

#define _mm_wmiawbtn_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiawbtn (_acc, (__v2si)_m1, (__v2si)_m2);\
   _acc;\
   })

#define _mm_wmiawtb_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiawtb (_acc, (__v2si)_m1, (__v2si)_m2);\
   _acc;\
   })

#define _mm_wmiawtbn_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiawtbn (_acc, (__v2si)_m1, (__v2si)_m2);\
   _acc;\
   })

#define _mm_wmiawtt_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiawtt (_acc, (__v2si)_m1, (__v2si)_m2);\
   _acc;\
   })

#define _mm_wmiawttn_si64(acc, m1, m2) \
  ({\
   __m64 _acc = acc;\
   __m64 _m1 = m1;\
   __m64 _m2 = m2;\
   _acc = (__m64) __builtin_arm_wmiawttn (_acc, (__v2si)_m1, (__v2si)_m2);\
   _acc;\
   })

/* The third arguments should be an immediate.  */
#define _mm_merge_si64(a, b, n) \
  ({\
   __m64 result;\
   result = (__m64) __builtin_arm_wmerge ((__m64) (a), (__m64) (b), (n));\
   result;\
   })
#endif  /* __IWMMXT2__ */

static __inline __m64
_mm_alignr0_si64 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_walignr0 ((__v8qi) a, (__v8qi) b);
}

static __inline __m64
_mm_alignr1_si64 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_walignr1 ((__v8qi) a, (__v8qi) b);
}

static __inline __m64
_mm_alignr2_si64 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_walignr2 ((__v8qi) a, (__v8qi) b);
}

static __inline __m64
_mm_alignr3_si64 (__m64 a, __m64 b)
{
  return (__m64) __builtin_arm_walignr3 ((__v8qi) a, (__v8qi) b);
}

static __inline void
_mm_tandcb ()
{
  __asm __volatile ("tandcb r15");
}

static __inline void
_mm_tandch ()
{
  __asm __volatile ("tandch r15");
}

static __inline void
_mm_tandcw ()
{
  __asm __volatile ("tandcw r15");
}

#define _mm_textrcb(n) \
  ({\
   __asm__ __volatile__ (\
     "textrcb r15, %0" : : "i" (n));\
   })

#define _mm_textrch(n) \
  ({\
   __asm__ __volatile__ (\
     "textrch r15, %0" : : "i" (n));\
   })

#define _mm_textrcw(n) \
  ({\
   __asm__ __volatile__ (\
     "textrcw r15, %0" : : "i" (n));\
   })

static __inline void
_mm_torcb ()
{
  __asm __volatile ("torcb r15");
}

static __inline void
_mm_torch ()
{
  __asm __volatile ("torch r15");
}

static __inline void
_mm_torcw ()
{
  __asm __volatile ("torcw r15");
}

#ifdef __IWMMXT2__
static __inline void
_mm_torvscb ()
{
  __asm __volatile ("torvscb r15");
}

static __inline void
_mm_torvsch ()
{
  __asm __volatile ("torvsch r15");
}

static __inline void
_mm_torvscw ()
{
  __asm __volatile ("torvscw r15");
}
#endif /* __IWMMXT2__ */

static __inline __m64
_mm_tbcst_pi8 (int value)
{
  return (__m64) __builtin_arm_tbcstb ((signed char) value);
}

static __inline __m64
_mm_tbcst_pi16 (int value)
{
  return (__m64) __builtin_arm_tbcsth ((short) value);
}

static __inline __m64
_mm_tbcst_pi32 (int value)
{
  return (__m64) __builtin_arm_tbcstw (value);
}

#define _m_empty _mm_empty
#define _m_packsswb _mm_packs_pi16
#define _m_packssdw _mm_packs_pi32
#define _m_packuswb _mm_packs_pu16
#define _m_packusdw _mm_packs_pu32
#define _m_packssqd _mm_packs_pi64
#define _m_packusqd _mm_packs_pu64
#define _mm_packs_si64 _mm_packs_pi64
#define _mm_packs_su64 _mm_packs_pu64
#define _m_punpckhbw _mm_unpackhi_pi8
#define _m_punpckhwd _mm_unpackhi_pi16
#define _m_punpckhdq _mm_unpackhi_pi32
#define _m_punpcklbw _mm_unpacklo_pi8
#define _m_punpcklwd _mm_unpacklo_pi16
#define _m_punpckldq _mm_unpacklo_pi32
#define _m_punpckehsbw _mm_unpackeh_pi8
#define _m_punpckehswd _mm_unpackeh_pi16
#define _m_punpckehsdq _mm_unpackeh_pi32
#define _m_punpckehubw _mm_unpackeh_pu8
#define _m_punpckehuwd _mm_unpackeh_pu16
#define _m_punpckehudq _mm_unpackeh_pu32
#define _m_punpckelsbw _mm_unpackel_pi8
#define _m_punpckelswd _mm_unpackel_pi16
#define _m_punpckelsdq _mm_unpackel_pi32
#define _m_punpckelubw _mm_unpackel_pu8
#define _m_punpckeluwd _mm_unpackel_pu16
#define _m_punpckeludq _mm_unpackel_pu32
#define _m_paddb _mm_add_pi8
#define _m_paddw _mm_add_pi16
#define _m_paddd _mm_add_pi32
#define _m_paddsb _mm_adds_pi8
#define _m_paddsw _mm_adds_pi16
#define _m_paddsd _mm_adds_pi32
#define _m_paddusb _mm_adds_pu8
#define _m_paddusw _mm_adds_pu16
#define _m_paddusd _mm_adds_pu32
#define _m_psubb _mm_sub_pi8
#define _m_psubw _mm_sub_pi16
#define _m_psubd _mm_sub_pi32
#define _m_psubsb _mm_subs_pi8
#define _m_psubsw _mm_subs_pi16
#define _m_psubuw _mm_subs_pi32
#define _m_psubusb _mm_subs_pu8
#define _m_psubusw _mm_subs_pu16
#define _m_psubusd _mm_subs_pu32
#define _m_pmaddwd _mm_madd_pi16
#define _m_pmadduwd _mm_madd_pu16
#define _m_pmulhw _mm_mulhi_pi16
#define _m_pmulhuw _mm_mulhi_pu16
#define _m_pmullw _mm_mullo_pi16
#define _m_pmacsw _mm_mac_pi16
#define _m_pmacuw _mm_mac_pu16
#define _m_pmacszw _mm_macz_pi16
#define _m_pmacuzw _mm_macz_pu16
#define _m_paccb _mm_acc_pu8
#define _m_paccw _mm_acc_pu16
#define _m_paccd _mm_acc_pu32
#define _m_pmia _mm_mia_si64
#define _m_pmiaph _mm_miaph_si64
#define _m_pmiabb _mm_miabb_si64
#define _m_pmiabt _mm_miabt_si64
#define _m_pmiatb _mm_miatb_si64
#define _m_pmiatt _mm_miatt_si64
#define _m_psllw _mm_sll_pi16
#define _m_psllwi _mm_slli_pi16
#define _m_pslld _mm_sll_pi32
#define _m_pslldi _mm_slli_pi32
#define _m_psllq _mm_sll_si64
#define _m_psllqi _mm_slli_si64
#define _m_psraw _mm_sra_pi16
#define _m_psrawi _mm_srai_pi16
#define _m_psrad _mm_sra_pi32
#define _m_psradi _mm_srai_pi32
#define _m_psraq _mm_sra_si64
#define _m_psraqi _mm_srai_si64
#define _m_psrlw _mm_srl_pi16
#define _m_psrlwi _mm_srli_pi16
#define _m_psrld _mm_srl_pi32
#define _m_psrldi _mm_srli_pi32
#define _m_psrlq _mm_srl_si64
#define _m_psrlqi _mm_srli_si64
#define _m_prorw _mm_ror_pi16
#define _m_prorwi _mm_rori_pi16
#define _m_prord _mm_ror_pi32
#define _m_prordi _mm_rori_pi32
#define _m_prorq _mm_ror_si64
#define _m_prorqi _mm_rori_si64
#define _m_pand _mm_and_si64
#define _m_pandn _mm_andnot_si64
#define _m_por _mm_or_si64
#define _m_pxor _mm_xor_si64
#define _m_pcmpeqb _mm_cmpeq_pi8
#define _m_pcmpeqw _mm_cmpeq_pi16
#define _m_pcmpeqd _mm_cmpeq_pi32
#define _m_pcmpgtb _mm_cmpgt_pi8
#define _m_pcmpgtub _mm_cmpgt_pu8
#define _m_pcmpgtw _mm_cmpgt_pi16
#define _m_pcmpgtuw _mm_cmpgt_pu16
#define _m_pcmpgtd _mm_cmpgt_pi32
#define _m_pcmpgtud _mm_cmpgt_pu32
#define _m_pextrb _mm_extract_pi8
#define _m_pextrw _mm_extract_pi16
#define _m_pextrd _mm_extract_pi32
#define _m_pextrub _mm_extract_pu8
#define _m_pextruw _mm_extract_pu16
#define _m_pextrud _mm_extract_pu32
#define _m_pinsrb _mm_insert_pi8
#define _m_pinsrw _mm_insert_pi16
#define _m_pinsrd _mm_insert_pi32
#define _m_pmaxsb _mm_max_pi8
#define _m_pmaxsw _mm_max_pi16
#define _m_pmaxsd _mm_max_pi32
#define _m_pmaxub _mm_max_pu8
#define _m_pmaxuw _mm_max_pu16
#define _m_pmaxud _mm_max_pu32
#define _m_pminsb _mm_min_pi8
#define _m_pminsw _mm_min_pi16
#define _m_pminsd _mm_min_pi32
#define _m_pminub _mm_min_pu8
#define _m_pminuw _mm_min_pu16
#define _m_pminud _mm_min_pu32
#define _m_pmovmskb _mm_movemask_pi8
#define _m_pmovmskw _mm_movemask_pi16
#define _m_pmovmskd _mm_movemask_pi32
#define _m_pshufw _mm_shuffle_pi16
#define _m_pavgb _mm_avg_pu8
#define _m_pavgw _mm_avg_pu16
#define _m_pavg2b _mm_avg2_pu8
#define _m_pavg2w _mm_avg2_pu16
#define _m_psadbw _mm_sad_pu8
#define _m_psadwd _mm_sad_pu16
#define _m_psadzbw _mm_sadz_pu8
#define _m_psadzwd _mm_sadz_pu16
#define _m_paligniq _mm_align_si64
#define _m_cvt_si2pi _mm_cvtsi64_m64
#define _m_cvt_pi2si _mm_cvtm64_si64
#define _m_from_int _mm_cvtsi32_si64
#define _m_to_int _mm_cvtsi64_si32

#if defined __cplusplus
}; /* End "C" */
#endif /* __cplusplus */

#endif /* _MMINTRIN_H_INCLUDED */
