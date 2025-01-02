/* Arm MVE intrinsics include file.

   Copyright (C) 2019-2025 Free Software Foundation, Inc.
   Contributed by Arm.

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

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef _GCC_ARM_MVE_H
#define _GCC_ARM_MVE_H

#if __ARM_BIG_ENDIAN
#error "MVE intrinsics are not supported in Big-Endian mode."
#elif !__ARM_FEATURE_MVE
#error "MVE feature not supported"
#else

#include <stdint.h>
#ifndef  __cplusplus
#include <stdbool.h>
#endif
#include "arm_mve_types.h"

#ifdef __ARM_MVE_PRESERVE_USER_NAMESPACE
#pragma GCC arm "arm_mve.h" true
#else
#pragma GCC arm "arm_mve.h" false
#endif

#ifndef __ARM_MVE_PRESERVE_USER_NAMESPACE
#define vuninitializedq(__v) __arm_vuninitializedq(__v)
#define vsetq_lane(__a, __b, __idx) __arm_vsetq_lane(__a, __b, __idx)
#define vgetq_lane(__a, __idx) __arm_vgetq_lane(__a, __idx)


#define vpnot(__a) __arm_vpnot(__a)
#define vuninitializedq_u8(void) __arm_vuninitializedq_u8(void)
#define vuninitializedq_u16(void) __arm_vuninitializedq_u16(void)
#define vuninitializedq_u32(void) __arm_vuninitializedq_u32(void)
#define vuninitializedq_u64(void) __arm_vuninitializedq_u64(void)
#define vuninitializedq_s8(void) __arm_vuninitializedq_s8(void)
#define vuninitializedq_s16(void) __arm_vuninitializedq_s16(void)
#define vuninitializedq_s32(void) __arm_vuninitializedq_s32(void)
#define vuninitializedq_s64(void) __arm_vuninitializedq_s64(void)
#define vuninitializedq_f16(void) __arm_vuninitializedq_f16(void)
#define vuninitializedq_f32(void) __arm_vuninitializedq_f32(void)
#define vsetq_lane_f16(__a, __b,  __idx) __arm_vsetq_lane_f16(__a, __b,  __idx)
#define vsetq_lane_f32(__a, __b,  __idx) __arm_vsetq_lane_f32(__a, __b,  __idx)
#define vsetq_lane_s16(__a, __b,  __idx) __arm_vsetq_lane_s16(__a, __b,  __idx)
#define vsetq_lane_s32(__a, __b,  __idx) __arm_vsetq_lane_s32(__a, __b,  __idx)
#define vsetq_lane_s8(__a, __b,  __idx) __arm_vsetq_lane_s8(__a, __b,  __idx)
#define vsetq_lane_s64(__a, __b,  __idx) __arm_vsetq_lane_s64(__a, __b,  __idx)
#define vsetq_lane_u8(__a, __b,  __idx) __arm_vsetq_lane_u8(__a, __b,  __idx)
#define vsetq_lane_u16(__a, __b,  __idx) __arm_vsetq_lane_u16(__a, __b,  __idx)
#define vsetq_lane_u32(__a, __b,  __idx) __arm_vsetq_lane_u32(__a, __b,  __idx)
#define vsetq_lane_u64(__a, __b,  __idx) __arm_vsetq_lane_u64(__a, __b,  __idx)
#define vgetq_lane_f16(__a,  __idx) __arm_vgetq_lane_f16(__a,  __idx)
#define vgetq_lane_f32(__a,  __idx) __arm_vgetq_lane_f32(__a,  __idx)
#define vgetq_lane_s16(__a,  __idx) __arm_vgetq_lane_s16(__a,  __idx)
#define vgetq_lane_s32(__a,  __idx) __arm_vgetq_lane_s32(__a,  __idx)
#define vgetq_lane_s8(__a,  __idx) __arm_vgetq_lane_s8(__a,  __idx)
#define vgetq_lane_s64(__a,  __idx) __arm_vgetq_lane_s64(__a,  __idx)
#define vgetq_lane_u8(__a,  __idx) __arm_vgetq_lane_u8(__a,  __idx)
#define vgetq_lane_u16(__a,  __idx) __arm_vgetq_lane_u16(__a,  __idx)
#define vgetq_lane_u32(__a,  __idx) __arm_vgetq_lane_u32(__a,  __idx)
#define vgetq_lane_u64(__a,  __idx) __arm_vgetq_lane_u64(__a,  __idx)
#define sqrshr(__p0, __p1) __arm_sqrshr(__p0, __p1)
#define sqrshrl(__p0, __p1) __arm_sqrshrl(__p0, __p1)
#define sqrshrl_sat48(__p0, __p1) __arm_sqrshrl_sat48(__p0, __p1)
#define sqshl(__p0, __p1) __arm_sqshl(__p0, __p1)
#define sqshll(__p0, __p1) __arm_sqshll(__p0, __p1)
#define srshr(__p0, __p1) __arm_srshr(__p0, __p1)
#define srshrl(__p0, __p1) __arm_srshrl(__p0, __p1)
#define uqrshl(__p0, __p1) __arm_uqrshl(__p0, __p1)
#define uqrshll(__p0, __p1) __arm_uqrshll(__p0, __p1)
#define uqrshll_sat48(__p0, __p1) __arm_uqrshll_sat48(__p0, __p1)
#define uqshl(__p0, __p1) __arm_uqshl(__p0, __p1)
#define uqshll(__p0, __p1) __arm_uqshll(__p0, __p1)
#define urshr(__p0, __p1) __arm_urshr(__p0, __p1)
#define urshrl(__p0, __p1) __arm_urshrl(__p0, __p1)
#define lsll(__p0, __p1) __arm_lsll(__p0, __p1)
#define asrl(__p0, __p1) __arm_asrl(__p0, __p1)
#endif

/* For big-endian, GCC's vector indices are reversed within each 64 bits
   compared to the architectural lane indices used by MVE intrinsics.  */
#define __ARM_NUM_LANES(__v) (sizeof (__v) / sizeof (__v[0]))
#ifdef __ARM_BIG_ENDIAN
#define __ARM_LANEQ(__vec, __idx) (__idx ^ (__ARM_NUM_LANES(__vec)/2 - 1))
#else
#define __ARM_LANEQ(__vec, __idx) __idx
#endif
#define __ARM_CHECK_LANEQ(__vec, __idx)		 \
  __builtin_arm_lane_check (__ARM_NUM_LANES(__vec),     \
			    __ARM_LANEQ(__vec, __idx))

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpnot (mve_pred16_t __a)
{
  return __builtin_mve_vpnotv16bi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_s16 (int16_t __a, int16x8_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_s32 (int32_t __a, int32x4_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_s8 (int8_t __a, int8x16_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_s64 (int64_t __a, int64x2_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_u8 (uint8_t __a, uint8x16_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_u16 (uint16_t __a, uint16x8_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_u32 (uint32_t __a, uint32x4_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_u64 (uint64_t __a, uint64x2_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_s16 (int16x8_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_s32 (int32x4_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}

__extension__ extern __inline int8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_s8 (int8x16_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_s64 (int64x2_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}

__extension__ extern __inline uint8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_u8 (uint8x16_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_u16 (uint16x8_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_u32 (uint32x4_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_u64 (uint64x2_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}

__extension__ extern __inline  uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_lsll (uint64_t value, int32_t shift)
{
  return (value << shift);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_asrl (int64_t value, int32_t shift)
{
  return (value >> shift);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_uqrshll (uint64_t value, int32_t shift)
{
  return __builtin_mve_uqrshll_sat64_di (value, shift);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_uqrshll_sat48 (uint64_t value, int32_t shift)
{
  return __builtin_mve_uqrshll_sat48_di (value, shift);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_sqrshrl (int64_t value, int32_t shift)
{
  return __builtin_mve_sqrshrl_sat64_di (value, shift);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_sqrshrl_sat48 (int64_t value, int32_t shift)
{
  return __builtin_mve_sqrshrl_sat48_di (value, shift);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_uqshll (uint64_t value, const int shift)
{
  return __builtin_mve_uqshll_di (value, shift);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_urshrl (uint64_t value, const int shift)
{
  return __builtin_mve_urshrl_di (value, shift);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_srshrl (int64_t value, const int shift)
{
  return __builtin_mve_srshrl_di (value, shift);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_sqshll (int64_t value, const int shift)
{
  return __builtin_mve_sqshll_di (value, shift);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_uqrshl (uint32_t value, int32_t shift)
{
  return __builtin_mve_uqrshl_si (value, shift);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_sqrshr (int32_t value, int32_t shift)
{
  return __builtin_mve_sqrshr_si (value, shift);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_uqshl (uint32_t value, const int shift)
{
  return  __builtin_mve_uqshl_si (value, shift);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_urshr (uint32_t value, const int shift)
{
  return __builtin_mve_urshr_si (value, shift);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_sqshl (int32_t value, const int shift)
{
  return __builtin_mve_sqshl_si (value, shift);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_srshr (int32_t value, const int shift)
{
  return __builtin_mve_srshr_si (value, shift);
}

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_f16 (float16_t __a, float16x8_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane_f32 (float32_t __a, float32x4_t __b, const int __idx)
{
  __ARM_CHECK_LANEQ (__b, __idx);
  __b[__ARM_LANEQ(__b,__idx)] = __a;
  return __b;
}

__extension__ extern __inline float16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_f16 (float16x8_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane_f32 (float32x4_t __a, const int __idx)
{
  __ARM_CHECK_LANEQ (__a, __idx);
  return __a[__ARM_LANEQ(__a,__idx)];
}
#endif

#ifdef __cplusplus

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (int16_t __a, int16x8_t __b, const int __idx)
{
 return __arm_vsetq_lane_s16 (__a, __b, __idx);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (int32_t __a, int32x4_t __b, const int __idx)
{
 return __arm_vsetq_lane_s32 (__a, __b, __idx);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (int8_t __a, int8x16_t __b, const int __idx)
{
 return __arm_vsetq_lane_s8 (__a, __b, __idx);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (int64_t __a, int64x2_t __b, const int __idx)
{
 return __arm_vsetq_lane_s64 (__a, __b, __idx);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (uint8_t __a, uint8x16_t __b, const int __idx)
{
 return __arm_vsetq_lane_u8 (__a, __b, __idx);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (uint16_t __a, uint16x8_t __b, const int __idx)
{
 return __arm_vsetq_lane_u16 (__a, __b, __idx);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (uint32_t __a, uint32x4_t __b, const int __idx)
{
 return __arm_vsetq_lane_u32 (__a, __b, __idx);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (uint64_t __a, uint64x2_t __b, const int __idx)
{
 return __arm_vsetq_lane_u64 (__a, __b, __idx);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (int16x8_t __a, const int __idx)
{
 return __arm_vgetq_lane_s16 (__a, __idx);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (int32x4_t __a, const int __idx)
{
 return __arm_vgetq_lane_s32 (__a, __idx);
}

__extension__ extern __inline int8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (int8x16_t __a, const int __idx)
{
 return __arm_vgetq_lane_s8 (__a, __idx);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (int64x2_t __a, const int __idx)
{
 return __arm_vgetq_lane_s64 (__a, __idx);
}

__extension__ extern __inline uint8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (uint8x16_t __a, const int __idx)
{
 return __arm_vgetq_lane_u8 (__a, __idx);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (uint16x8_t __a, const int __idx)
{
 return __arm_vgetq_lane_u16 (__a, __idx);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (uint32x4_t __a, const int __idx)
{
 return __arm_vgetq_lane_u32 (__a, __idx);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (uint64x2_t __a, const int __idx)
{
 return __arm_vgetq_lane_u64 (__a, __idx);
}

#if (__ARM_FEATURE_MVE & 2)  /* MVE Floating point.  */

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (float16_t __a, float16x8_t __b, const int __idx)
{
 return __arm_vsetq_lane_f16 (__a, __b, __idx);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsetq_lane (float32_t __a, float32x4_t __b, const int __idx)
{
 return __arm_vsetq_lane_f32 (__a, __b, __idx);
}

__extension__ extern __inline float16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (float16x8_t __a, const int __idx)
{
 return __arm_vgetq_lane_f16 (__a, __idx);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vgetq_lane (float32x4_t __a, const int __idx)
{
 return __arm_vgetq_lane_f32 (__a, __idx);
}
#endif /* MVE Floating point.  */


__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (uint8x16_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_u8 ();
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (uint16x8_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_u16 ();
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (uint32x4_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_u32 ();
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (uint64x2_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_u64 ();
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (int8x16_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_s8 ();
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (int16x8_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_s16 ();
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (int32x4_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_s32 ();
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (int64x2_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_s64 ();
}

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */
__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (float16x8_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_f16 ();
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq (float32x4_t /* __v ATTRIBUTE UNUSED */)
{
 return __arm_vuninitializedq_f32 ();
}
#endif /* __ARM_FEATURE_MVE & 2 (MVE floating point)  */

#else
enum {
    __ARM_mve_type_fp_n = 1,
    __ARM_mve_type_int_n,
    __ARM_mve_type_float16_t_ptr,
    __ARM_mve_type_float16x8_t,
    __ARM_mve_type_float16x8x2_t,
    __ARM_mve_type_float16x8x4_t,
    __ARM_mve_type_float32_t_ptr,
    __ARM_mve_type_float32x4_t,
    __ARM_mve_type_float32x4x2_t,
    __ARM_mve_type_float32x4x4_t,
    __ARM_mve_type_int16_t_ptr,
    __ARM_mve_type_int16x8_t,
    __ARM_mve_type_int16x8x2_t,
    __ARM_mve_type_int16x8x4_t,
    __ARM_mve_type_int32_t_ptr,
    __ARM_mve_type_int32x4_t,
    __ARM_mve_type_int32x4x2_t,
    __ARM_mve_type_int32x4x4_t,
    __ARM_mve_type_int64_t_ptr,
    __ARM_mve_type_int64x2_t,
    __ARM_mve_type_int8_t_ptr,
    __ARM_mve_type_int8x16_t,
    __ARM_mve_type_int8x16x2_t,
    __ARM_mve_type_int8x16x4_t,
    __ARM_mve_type_uint16_t_ptr,
    __ARM_mve_type_uint16x8_t,
    __ARM_mve_type_uint16x8x2_t,
    __ARM_mve_type_uint16x8x4_t,
    __ARM_mve_type_uint32_t_ptr,
    __ARM_mve_type_uint32x4_t,
    __ARM_mve_type_uint32x4x2_t,
    __ARM_mve_type_uint32x4x4_t,
    __ARM_mve_type_uint64_t_ptr,
    __ARM_mve_type_uint64x2_t,
    __ARM_mve_type_uint8_t_ptr,
    __ARM_mve_type_uint8x16_t,
    __ARM_mve_type_uint8x16x2_t,
    __ARM_mve_type_uint8x16x4_t,
    __ARM_mve_unsupported_type
};

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */
#define __ARM_mve_typeid(x) _Generic(x, \
    float16_t: __ARM_mve_type_fp_n, \
    float16_t *: __ARM_mve_type_float16_t_ptr, \
    float16_t const *: __ARM_mve_type_float16_t_ptr, \
    float16x8_t: __ARM_mve_type_float16x8_t, \
    float16x8x2_t: __ARM_mve_type_float16x8x2_t, \
    float16x8x4_t: __ARM_mve_type_float16x8x4_t, \
    float32_t: __ARM_mve_type_fp_n, \
    float32_t *: __ARM_mve_type_float32_t_ptr, \
    float32_t const *: __ARM_mve_type_float32_t_ptr, \
    float32x4_t: __ARM_mve_type_float32x4_t, \
    float32x4x2_t: __ARM_mve_type_float32x4x2_t, \
    float32x4x4_t: __ARM_mve_type_float32x4x4_t, \
    int16_t: __ARM_mve_type_int_n, \
    int16_t *: __ARM_mve_type_int16_t_ptr, \
    int16_t const *: __ARM_mve_type_int16_t_ptr, \
    int16x8_t: __ARM_mve_type_int16x8_t, \
    int16x8x2_t: __ARM_mve_type_int16x8x2_t, \
    int16x8x4_t: __ARM_mve_type_int16x8x4_t, \
    int32_t: __ARM_mve_type_int_n, \
    int32_t *: __ARM_mve_type_int32_t_ptr, \
    int32_t const *: __ARM_mve_type_int32_t_ptr, \
    int32x4_t: __ARM_mve_type_int32x4_t, \
    int32x4x2_t: __ARM_mve_type_int32x4x2_t, \
    int32x4x4_t: __ARM_mve_type_int32x4x4_t, \
    int64_t: __ARM_mve_type_int_n, \
    int64_t *: __ARM_mve_type_int64_t_ptr, \
    int64_t const *: __ARM_mve_type_int64_t_ptr, \
    int64x2_t: __ARM_mve_type_int64x2_t, \
    int8_t: __ARM_mve_type_int_n, \
    int8_t *: __ARM_mve_type_int8_t_ptr, \
    int8_t const *: __ARM_mve_type_int8_t_ptr, \
    int8x16_t: __ARM_mve_type_int8x16_t, \
    int8x16x2_t: __ARM_mve_type_int8x16x2_t, \
    int8x16x4_t: __ARM_mve_type_int8x16x4_t, \
    uint16_t: __ARM_mve_type_int_n, \
    uint16_t *: __ARM_mve_type_uint16_t_ptr, \
    uint16_t const *: __ARM_mve_type_uint16_t_ptr, \
    uint16x8_t: __ARM_mve_type_uint16x8_t, \
    uint16x8x2_t: __ARM_mve_type_uint16x8x2_t, \
    uint16x8x4_t: __ARM_mve_type_uint16x8x4_t, \
    uint32_t: __ARM_mve_type_int_n, \
    uint32_t *: __ARM_mve_type_uint32_t_ptr, \
    uint32_t const *: __ARM_mve_type_uint32_t_ptr, \
    uint32x4_t: __ARM_mve_type_uint32x4_t, \
    uint32x4x2_t: __ARM_mve_type_uint32x4x2_t, \
    uint32x4x4_t: __ARM_mve_type_uint32x4x4_t, \
    uint64_t: __ARM_mve_type_int_n, \
    uint64_t *: __ARM_mve_type_uint64_t_ptr, \
    uint64_t const *: __ARM_mve_type_uint64_t_ptr, \
    uint64x2_t: __ARM_mve_type_uint64x2_t, \
    uint8_t: __ARM_mve_type_int_n, \
    uint8_t *: __ARM_mve_type_uint8_t_ptr, \
    uint8_t const *: __ARM_mve_type_uint8_t_ptr, \
    uint8x16_t: __ARM_mve_type_uint8x16_t, \
    uint8x16x2_t: __ARM_mve_type_uint8x16x2_t, \
    uint8x16x4_t: __ARM_mve_type_uint8x16x4_t, \
    default: _Generic(x, \
	signed char: __ARM_mve_type_int_n, \
	short: __ARM_mve_type_int_n, \
	int: __ARM_mve_type_int_n, \
	long: __ARM_mve_type_int_n, \
	long long: __ARM_mve_type_int_n, \
	_Float16: __ARM_mve_type_fp_n, \
	__fp16: __ARM_mve_type_fp_n, \
	float: __ARM_mve_type_fp_n, \
	double: __ARM_mve_type_fp_n, \
	unsigned char: __ARM_mve_type_int_n, \
	unsigned short: __ARM_mve_type_int_n, \
	unsigned int: __ARM_mve_type_int_n, \
	unsigned long: __ARM_mve_type_int_n, \
	unsigned long long: __ARM_mve_type_int_n, \
	signed char*: __ARM_mve_type_int8_t_ptr, \
	short*: __ARM_mve_type_int16_t_ptr, \
	int*: __ARM_mve_type_int32_t_ptr, \
	long*: __ARM_mve_type_int32_t_ptr, \
	long long*: __ARM_mve_type_int64_t_ptr, \
	_Float16*: __ARM_mve_type_float16_t_ptr, \
	__fp16*: __ARM_mve_type_float16_t_ptr, \
	float*: __ARM_mve_type_float32_t_ptr, \
	unsigned char*: __ARM_mve_type_uint8_t_ptr, \
	unsigned short*: __ARM_mve_type_uint16_t_ptr, \
	unsigned int*: __ARM_mve_type_uint32_t_ptr, \
	unsigned long*: __ARM_mve_type_uint32_t_ptr, \
	unsigned long long*: __ARM_mve_type_uint64_t_ptr, \
	default: __ARM_mve_unsupported_type))
#else
#define __ARM_mve_typeid(x) _Generic(x, \
    int16_t: __ARM_mve_type_int_n, \
    int16_t *: __ARM_mve_type_int16_t_ptr, \
    int16_t const *: __ARM_mve_type_int16_t_ptr, \
    int16x8_t: __ARM_mve_type_int16x8_t, \
    int16x8x2_t: __ARM_mve_type_int16x8x2_t, \
    int16x8x4_t: __ARM_mve_type_int16x8x4_t, \
    int32_t: __ARM_mve_type_int_n, \
    int32_t *: __ARM_mve_type_int32_t_ptr, \
    int32_t const *: __ARM_mve_type_int32_t_ptr, \
    int32x4_t: __ARM_mve_type_int32x4_t, \
    int32x4x2_t: __ARM_mve_type_int32x4x2_t, \
    int32x4x4_t: __ARM_mve_type_int32x4x4_t, \
    int64_t: __ARM_mve_type_int_n, \
    int64_t *: __ARM_mve_type_int64_t_ptr, \
    int64_t const *: __ARM_mve_type_int64_t_ptr, \
    int64x2_t: __ARM_mve_type_int64x2_t, \
    int8_t: __ARM_mve_type_int_n, \
    int8_t *: __ARM_mve_type_int8_t_ptr, \
    int8_t const *: __ARM_mve_type_int8_t_ptr, \
    int8x16_t: __ARM_mve_type_int8x16_t, \
    int8x16x2_t: __ARM_mve_type_int8x16x2_t, \
    int8x16x4_t: __ARM_mve_type_int8x16x4_t, \
    uint16_t: __ARM_mve_type_int_n, \
    uint16_t *: __ARM_mve_type_uint16_t_ptr, \
    uint16_t const *: __ARM_mve_type_uint16_t_ptr, \
    uint16x8_t: __ARM_mve_type_uint16x8_t, \
    uint16x8x2_t: __ARM_mve_type_uint16x8x2_t, \
    uint16x8x4_t: __ARM_mve_type_uint16x8x4_t, \
    uint32_t: __ARM_mve_type_int_n, \
    uint32_t *: __ARM_mve_type_uint32_t_ptr, \
    uint32_t const *: __ARM_mve_type_uint32_t_ptr, \
    uint32x4_t: __ARM_mve_type_uint32x4_t, \
    uint32x4x2_t: __ARM_mve_type_uint32x4x2_t, \
    uint32x4x4_t: __ARM_mve_type_uint32x4x4_t, \
    uint64_t: __ARM_mve_type_int_n, \
    uint64_t *: __ARM_mve_type_uint64_t_ptr, \
    uint64_t const *: __ARM_mve_type_uint64_t_ptr, \
    uint64x2_t: __ARM_mve_type_uint64x2_t, \
    uint8_t: __ARM_mve_type_int_n, \
    uint8_t *: __ARM_mve_type_uint8_t_ptr, \
    uint8_t const *: __ARM_mve_type_uint8_t_ptr, \
    uint8x16_t: __ARM_mve_type_uint8x16_t, \
    uint8x16x2_t: __ARM_mve_type_uint8x16x2_t, \
    uint8x16x4_t: __ARM_mve_type_uint8x16x4_t, \
    default: _Generic(x, \
	signed char: __ARM_mve_type_int_n, \
	short: __ARM_mve_type_int_n, \
	int: __ARM_mve_type_int_n, \
	long: __ARM_mve_type_int_n, \
	long long: __ARM_mve_type_int_n, \
	unsigned char: __ARM_mve_type_int_n, \
	unsigned short: __ARM_mve_type_int_n, \
	unsigned int: __ARM_mve_type_int_n, \
	unsigned long: __ARM_mve_type_int_n, \
	unsigned long long: __ARM_mve_type_int_n, \
	signed char*: __ARM_mve_type_int8_t_ptr, \
	short*: __ARM_mve_type_int16_t_ptr, \
	int*: __ARM_mve_type_int32_t_ptr, \
	long*: __ARM_mve_type_int32_t_ptr, \
	long long*: __ARM_mve_type_int64_t_ptr, \
	unsigned char*: __ARM_mve_type_uint8_t_ptr, \
	unsigned short*: __ARM_mve_type_uint16_t_ptr, \
	unsigned int*: __ARM_mve_type_uint32_t_ptr, \
	unsigned long*: __ARM_mve_type_uint32_t_ptr, \
	unsigned long long*: __ARM_mve_type_uint64_t_ptr, \
	default: __ARM_mve_unsupported_type))
#endif /* MVE Floating point.  */

extern void *__ARM_undef;
#define __ARM_mve_coerce(param, type) \
    _Generic(param, type: param, default: *(type *)__ARM_undef)
#define __ARM_mve_coerce_i_scalar(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, int8_t: param, int16_t: param, int32_t: param, int64_t: param, uint8_t: param, uint16_t: param, uint32_t: param, uint64_t: param, default: *(type *)__ARM_undef))

#define __ARM_mve_coerce_s8_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, signed char*: param, default: *(type *)__ARM_undef))
#define __ARM_mve_coerce_u8_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, unsigned char*: param, default: *(type *)__ARM_undef))

#define __ARM_mve_coerce_s16_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, short*: param, default: *(type *)__ARM_undef))
#define __ARM_mve_coerce_u16_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, unsigned short*: param, default: *(type *)__ARM_undef))

#define __ARM_mve_coerce_s32_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, int*: param, long*: param, default: *(type *)__ARM_undef))
#define __ARM_mve_coerce_u32_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, unsigned int*: param, unsigned long*: param, default: *(type *)__ARM_undef))

#define __ARM_mve_coerce_s64_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, long long*: param, default: *(type *)__ARM_undef))
#define __ARM_mve_coerce_u64_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, unsigned long long*: param, default: *(type *)__ARM_undef))

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */
#define __ARM_mve_coerce_f_scalar(param, type) \
    _Generic(param, type: param, const type: param, __fp16: param, default: _Generic (param, _Float16: param, float16_t: param, float32_t: param, default: *(type *)__ARM_undef))
#define __ARM_mve_coerce_f16_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, __fp16*: param, _Float16*: param, default: *(type *)__ARM_undef))
#define __ARM_mve_coerce_f32_ptr(param, type) \
    _Generic(param, type: param, const type: param, default: _Generic (param, float*: param, default: *(type *)__ARM_undef))
#endif

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */

#define __arm_vuninitializedq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vuninitializedq_s8 (), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vuninitializedq_s16 (), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vuninitializedq_s32 (), \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vuninitializedq_s64 (), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vuninitializedq_u8 (), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vuninitializedq_u16 (), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vuninitializedq_u32 (), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vuninitializedq_u64 (), \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vuninitializedq_f16 (), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vuninitializedq_f32 ());})

#define __arm_vgetq_lane(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vgetq_lane_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vgetq_lane_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vgetq_lane_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vgetq_lane_s64 (__ARM_mve_coerce(__p0, int64x2_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vgetq_lane_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vgetq_lane_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vgetq_lane_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vgetq_lane_u64 (__ARM_mve_coerce(__p0, uint64x2_t), p1), \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vgetq_lane_f16 (__ARM_mve_coerce(__p0, float16x8_t), p1), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vgetq_lane_f32 (__ARM_mve_coerce(__p0, float32x4_t), p1));})

#define __arm_vsetq_lane(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int8x16_t]: __arm_vsetq_lane_s8 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t]: __arm_vsetq_lane_s16 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t]: __arm_vsetq_lane_s32 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int64x2_t]: __arm_vsetq_lane_s64 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, int64x2_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint8x16_t]: __arm_vsetq_lane_u8 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t]: __arm_vsetq_lane_u16 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t]: __arm_vsetq_lane_u32 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint64x2_t]: __arm_vsetq_lane_u64 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, uint64x2_t), p2), \
  int (*)[__ARM_mve_type_fp_n][__ARM_mve_type_float16x8_t]: __arm_vsetq_lane_f16 (__ARM_mve_coerce_f_scalar(__p0, double), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_fp_n][__ARM_mve_type_float32x4_t]: __arm_vsetq_lane_f32 (__ARM_mve_coerce_f_scalar(__p0, double), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#else /* MVE Integer.  */

#define __arm_vuninitializedq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vuninitializedq_s8 (), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vuninitializedq_s16 (), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vuninitializedq_s32 (), \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vuninitializedq_s64 (), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vuninitializedq_u8 (), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vuninitializedq_u16 (), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vuninitializedq_u32 (), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vuninitializedq_u64 ());})

#define __arm_vgetq_lane(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vgetq_lane_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vgetq_lane_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vgetq_lane_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vgetq_lane_s64 (__ARM_mve_coerce(__p0, int64x2_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vgetq_lane_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vgetq_lane_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vgetq_lane_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vgetq_lane_u64 (__ARM_mve_coerce(__p0, uint64x2_t), p1));})

#define __arm_vsetq_lane(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int8x16_t]: __arm_vsetq_lane_s8 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t]: __arm_vsetq_lane_s16 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t]: __arm_vsetq_lane_s32 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int64x2_t]: __arm_vsetq_lane_s64 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, int64x2_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint8x16_t]: __arm_vsetq_lane_u8 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t]: __arm_vsetq_lane_u16 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t]: __arm_vsetq_lane_u32 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint64x2_t]: __arm_vsetq_lane_u64 (__ARM_mve_coerce_i_scalar(__p0, int), __ARM_mve_coerce(__p1, uint64x2_t), p2));})

#endif /* MVE Integer.  */

#endif /* __cplusplus  */
#endif /* __ARM_FEATURE_MVE  */
#endif /* _GCC_ARM_MVE_H.  */
