/* ARM NEON intrinsics include file.

   Copyright (C) 2011-2026 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

#ifndef _AARCH64_NEON_H_
#define _AARCH64_NEON_H_

#pragma GCC push_options
#pragma GCC target ("+nothing+simd")

#include <stdint.h>
#include <arm_private_fp8.h>
#include <arm_private_neon_types.h>

#pragma GCC aarch64 "arm_neon.h"

#define __AARCH64_UINT64_C(__C) ((uint64_t) __C)
#define __AARCH64_INT64_C(__C) ((int64_t) __C)

/* __aarch64_vdup_lane internal macros.  */
#define __aarch64_vdup_lane_any(__size, __q, __a, __b) \
  vdup##__q##_n_##__size (__aarch64_vget_lane_any (__a, __b))

#define __aarch64_vdup_lane_f16(__a, __b) \
   __aarch64_vdup_lane_any (f16, , __a, __b)
#define __aarch64_vdup_lane_f32(__a, __b) \
   __aarch64_vdup_lane_any (f32, , __a, __b)
#define __aarch64_vdup_lane_f64(__a, __b) \
   __aarch64_vdup_lane_any (f64, , __a, __b)

/* __aarch64_vdup_laneq internal macros.  */
#define __aarch64_vdup_laneq_f16(__a, __b) \
   __aarch64_vdup_lane_any (f16, , __a, __b)
#define __aarch64_vdup_laneq_f32(__a, __b) \
   __aarch64_vdup_lane_any (f32, , __a, __b)
#define __aarch64_vdup_laneq_f64(__a, __b) \
   __aarch64_vdup_lane_any (f64, , __a, __b)

/* __aarch64_vdupq_lane internal macros.  */
#define __aarch64_vdupq_lane_f16(__a, __b) \
   __aarch64_vdup_lane_any (f16, q, __a, __b)
#define __aarch64_vdupq_lane_f32(__a, __b) \
   __aarch64_vdup_lane_any (f32, q, __a, __b)
#define __aarch64_vdupq_lane_f64(__a, __b) \
   __aarch64_vdup_lane_any (f64, q, __a, __b)

/* __aarch64_vdupq_laneq internal macros.  */
#define __aarch64_vdupq_laneq_f16(__a, __b) \
   __aarch64_vdup_lane_any (f16, q, __a, __b)
#define __aarch64_vdupq_laneq_f32(__a, __b) \
   __aarch64_vdup_lane_any (f32, q, __a, __b)
#define __aarch64_vdupq_laneq_f64(__a, __b) \
   __aarch64_vdup_lane_any (f64, q, __a, __b)

/* Internal macro for lane indices.  */

#define __AARCH64_NUM_LANES(__v) (sizeof (__v) / sizeof (__v[0]))
#define __AARCH64_LANE_CHECK(__vec, __idx)	\
	__builtin_aarch64_im_lane_boundsi (sizeof(__vec), sizeof(__vec[0]), __idx)

/* For big-endian, GCC's vector indices are the opposite way around
   to the architectural lane indices used by Neon intrinsics.  */
#ifdef __AARCH64EB__
#define __aarch64_lane(__vec, __idx) (__AARCH64_NUM_LANES (__vec) - 1 - __idx)
#else
#define __aarch64_lane(__vec, __idx) __idx
#endif

/* vget_lane internal macro.  */
#define __aarch64_vget_lane_any(__vec, __index)				\
  __extension__								\
  ({									\
    __AARCH64_LANE_CHECK (__vec, __index);				\
    __vec[__aarch64_lane (__vec, __index)];				\
  })

/* vset_lane and vld1_lane internal macro.  */
#define __aarch64_vset_lane_any(__elem, __vec, __index)			\
  __extension__								\
  ({									\
    __AARCH64_LANE_CHECK (__vec, __index);				\
    __vec[__aarch64_lane (__vec, __index)] = __elem;			\
    __vec;								\
  })

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_saddlv8qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_saddlv4hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_saddlv2si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_uaddlv8qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_uaddlv4hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __builtin_aarch64_uaddlv2si_uuu (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_high_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_saddl2v16qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_high_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_saddl2v8hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_high_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_saddl2v4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_high_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_uaddl2v16qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_high_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_uaddl2v8hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddl_high_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_uaddl2v4si_uuu (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_s8 (int16x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_saddwv8qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_s16 (int32x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_saddwv4hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_s32 (int64x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_saddwv2si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_u8 (uint16x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_uaddwv8qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_u16 (uint32x4_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_uaddwv4hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_u32 (uint64x2_t __a, uint32x2_t __b)
{
  return __builtin_aarch64_uaddwv2si_uuu (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_high_s8 (int16x8_t __a, int8x16_t __b)
{
  return __builtin_aarch64_saddw2v16qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_high_s16 (int32x4_t __a, int16x8_t __b)
{
  return __builtin_aarch64_saddw2v8hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_high_s32 (int64x2_t __a, int32x4_t __b)
{
  return __builtin_aarch64_saddw2v4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_high_u8 (uint16x8_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_uaddw2v16qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_high_u16 (uint32x4_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_uaddw2v8hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddw_high_u32 (uint64x2_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_uaddw2v4si_uuu (__a, __b);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_addhnv8hi (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_addhnv4si (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_addhnv2di (__a, __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_addhnv8hi_uuu (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_addhnv4si_uuu (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __builtin_aarch64_addhnv2di_uuu (__a, __b);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_raddhnv8hi (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_raddhnv4si (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_raddhnv2di (__a, __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_raddhnv8hi_uuu (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_raddhnv4si_uuu (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __builtin_aarch64_raddhnv2di_uuu (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_high_s16 (int8x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_addhn2v8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_high_s32 (int16x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_addhn2v4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_high_s64 (int32x2_t __a, int64x2_t __b, int64x2_t __c)
{
  return __builtin_aarch64_addhn2v2di (__a, __b, __c);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_high_u16 (uint8x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_aarch64_addhn2v8hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_high_u32 (uint16x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_aarch64_addhn2v4si_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddhn_high_u64 (uint32x2_t __a, uint64x2_t __b, uint64x2_t __c)
{
  return __builtin_aarch64_addhn2v2di_uuuu (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_high_s16 (int8x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_raddhn2v8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_high_s32 (int16x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_raddhn2v4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_high_s64 (int32x2_t __a, int64x2_t __b, int64x2_t __c)
{
  return __builtin_aarch64_raddhn2v2di (__a, __b, __c);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_high_u16 (uint8x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_aarch64_raddhn2v8hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_high_u32 (uint16x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_aarch64_raddhn2v4si_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vraddhn_high_u64 (uint32x2_t __a, uint64x2_t __b, uint64x2_t __c)
{
  return __builtin_aarch64_raddhn2v2di_uuuu (__a, __b, __c);
}

__extension__ extern __inline poly8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmul_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return __builtin_aarch64_pmulv8qi_ppp (__a, __b);
}

__extension__ extern __inline poly8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return __builtin_aarch64_pmulv16qi_ppp (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_ssublv8qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_ssublv4hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_ssublv2si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_usublv8qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_usublv4hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __builtin_aarch64_usublv2si_uuu (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_high_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_ssubl2v16qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_high_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_ssubl2v8hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_high_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_ssubl2v4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_high_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_usubl2v16qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_high_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_usubl2v8hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubl_high_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_usubl2v4si_uuu (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_s8 (int16x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_ssubwv8qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_s16 (int32x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_ssubwv4hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_s32 (int64x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_ssubwv2si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_u8 (uint16x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_usubwv8qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_u16 (uint32x4_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_usubwv4hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_u32 (uint64x2_t __a, uint32x2_t __b)
{
  return __builtin_aarch64_usubwv2si_uuu (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_high_s8 (int16x8_t __a, int8x16_t __b)
{
  return __builtin_aarch64_ssubw2v16qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_high_s16 (int32x4_t __a, int16x8_t __b)
{
  return __builtin_aarch64_ssubw2v8hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_high_s32 (int64x2_t __a, int32x4_t __b)
{
  return __builtin_aarch64_ssubw2v4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_high_u8 (uint16x8_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_usubw2v16qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_high_u16 (uint32x4_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_usubw2v8hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubw_high_u32 (uint64x2_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_usubw2v4si_uuu (__a, __b);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsub_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_shsubv8qi (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsub_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_shsubv4hi (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsub_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_shsubv2si (__a, __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsub_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_uhsubv8qi_uuu (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsub_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_uhsubv4hi_uuu (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsub_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __builtin_aarch64_uhsubv2si_uuu (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsubq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_shsubv16qi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsubq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_shsubv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsubq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_shsubv4si (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsubq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_uhsubv16qi_uuu (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsubq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_uhsubv8hi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vhsubq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_uhsubv4si_uuu (__a, __b);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_subhnv8hi (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_subhnv4si (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_subhnv2di (__a, __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_subhnv8hi_uuu (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_subhnv4si_uuu (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __builtin_aarch64_subhnv2di_uuu (__a, __b);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_rsubhnv8hi (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_rsubhnv4si (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_rsubhnv2di (__a, __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_rsubhnv8hi_uuu (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_rsubhnv4si_uuu (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __builtin_aarch64_rsubhnv2di_uuu (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_high_s16 (int8x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_rsubhn2v8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_high_s32 (int16x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_rsubhn2v4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_high_s64 (int32x2_t __a, int64x2_t __b, int64x2_t __c)
{
  return __builtin_aarch64_rsubhn2v2di (__a, __b, __c);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_high_u16 (uint8x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_aarch64_rsubhn2v8hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_high_u32 (uint16x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_aarch64_rsubhn2v4si_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsubhn_high_u64 (uint32x2_t __a, uint64x2_t __b, uint64x2_t __c)
{
  return __builtin_aarch64_rsubhn2v2di_uuuu (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_high_s16 (int8x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_subhn2v8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_high_s32 (int16x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_subhn2v4si (__a, __b, __c);;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_high_s64 (int32x2_t __a, int64x2_t __b, int64x2_t __c)
{
  return __builtin_aarch64_subhn2v2di (__a, __b, __c);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_high_u16 (uint8x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_aarch64_subhn2v8hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_high_u32 (uint16x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_aarch64_subhn2v4si_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubhn_high_u64 (uint32x2_t __a, uint64x2_t __b, uint64x2_t __c)
{
  return __builtin_aarch64_subhn2v2di_uuuu (__a, __b, __c);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqneg_s8 (int8x8_t __a)
{
  return (int8x8_t) __builtin_aarch64_sqnegv8qi (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqneg_s16 (int16x4_t __a)
{
  return (int16x4_t) __builtin_aarch64_sqnegv4hi (__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqneg_s32 (int32x2_t __a)
{
  return (int32x2_t) __builtin_aarch64_sqnegv2si (__a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqneg_s64 (int64x1_t __a)
{
  return (int64x1_t) {__builtin_aarch64_sqnegdi (__a[0])};
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqnegq_s8 (int8x16_t __a)
{
  return (int8x16_t) __builtin_aarch64_sqnegv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqnegq_s16 (int16x8_t __a)
{
  return (int16x8_t) __builtin_aarch64_sqnegv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqnegq_s32 (int32x4_t __a)
{
  return (int32x4_t) __builtin_aarch64_sqnegv4si (__a);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabs_s8 (int8x8_t __a)
{
  return (int8x8_t) __builtin_aarch64_sqabsv8qi (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabs_s16 (int16x4_t __a)
{
  return (int16x4_t) __builtin_aarch64_sqabsv4hi (__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabs_s32 (int32x2_t __a)
{
  return (int32x2_t) __builtin_aarch64_sqabsv2si (__a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabs_s64 (int64x1_t __a)
{
  return (int64x1_t) {__builtin_aarch64_sqabsdi (__a[0])};
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabsq_s8 (int8x16_t __a)
{
  return (int8x16_t) __builtin_aarch64_sqabsv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabsq_s16 (int16x8_t __a)
{
  return (int16x8_t) __builtin_aarch64_sqabsv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabsq_s32 (int32x4_t __a)
{
  return (int32x4_t) __builtin_aarch64_sqabsv4si (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulh_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_sqdmulhv4hi (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulh_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_sqdmulhv2si (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_sqdmulhv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_sqdmulhv4si (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulh_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_sqrdmulhv4hi (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulh_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_sqrdmulhv2si (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_sqrdmulhv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_sqrdmulhv4si (__a, __b);
}

/* Start of temporary inline asm implementations.  */

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_high_s8 (int16x8_t __a, int8x16_t __b, int8x16_t __c)
{
  return __builtin_aarch64_sabal2v16qi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_high_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_sabal2v8hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_high_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_sabal2v4si (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_high_u8 (uint16x8_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return __builtin_aarch64_uabal2v16qi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_high_u16 (uint32x4_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_aarch64_uabal2v8hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_high_u32 (uint64x2_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_aarch64_uabal2v4si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_s8 (int16x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return __builtin_aarch64_sabalv8qi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_sabalv4hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_sabalv2si (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_u8 (uint16x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return __builtin_aarch64_uabalv8qi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_u16 (uint32x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return __builtin_aarch64_uabalv4hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabal_u32 (uint64x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return __builtin_aarch64_uabalv2si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_high_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_sabdl2v16qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_high_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sabdl2v8hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_high_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sabdl2v4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_high_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_uabdl2v16qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_high_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_uabdl2v8hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_high_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_uabdl2v4si_uuu (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_sabdlv8qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_sabdlv4hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_sabdlv2si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_uabdlv8qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_uabdlv4hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vabdl_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __builtin_aarch64_uabdlv2si_uuu (__a, __b);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlv_s8 (int8x8_t __a)
{
  return __builtin_aarch64_saddlvv8qi (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlv_s16 (int16x4_t __a)
{
  return __builtin_aarch64_saddlvv4hi (__a);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlv_u8 (uint8x8_t __a)
{
  return __builtin_aarch64_uaddlvv8qi_uu (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlv_u16 (uint16x4_t __a)
{
  return __builtin_aarch64_uaddlvv4hi_uu (__a);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlvq_s8 (int8x16_t __a)
{
  return __builtin_aarch64_saddlvv16qi (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlvq_s16 (int16x8_t __a)
{
  return __builtin_aarch64_saddlvv8hi (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlvq_s32 (int32x4_t __a)
{
  return __builtin_aarch64_saddlvv4si (__a);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlvq_u8 (uint8x16_t __a)
{
  return __builtin_aarch64_uaddlvv16qi_uu (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlvq_u16 (uint16x8_t __a)
{
  return __builtin_aarch64_uaddlvv8hi_uu (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlvq_u32 (uint32x4_t __a)
{
  return __builtin_aarch64_uaddlvv4si_uu (__a);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtx_f32_f64 (float64x2_t __a)
{
  return __builtin_aarch64_float_trunc_rodd_lo_v2sf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtx_high_f32_f64 (float32x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_float_trunc_rodd_hi_v4sf (__a, __b);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtxd_f32_f64 (float64_t __a)
{
  return __builtin_aarch64_float_trunc_rodd_df (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_lane_s16(int32x4_t __a, int16x8_t __b, int16x4_t __v,
		    const int __lane)
{
  return __builtin_aarch64_smlal_hi_lanev8hi (__a, __b, __v, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_lane_s32(int64x2_t __a, int32x4_t __b, int32x2_t __v,
		    const int __lane)
{
  return __builtin_aarch64_smlal_hi_lanev4si (__a, __b, __v, __lane);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_lane_u16(uint32x4_t __a, uint16x8_t __b, uint16x4_t __v,
		    const int __lane)
{
  return __builtin_aarch64_umlal_hi_lanev8hi_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_lane_u32(uint64x2_t __a, uint32x4_t __b, uint32x2_t __v,
		    const int __lane)
{
  return __builtin_aarch64_umlal_hi_lanev4si_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_laneq_s16(int32x4_t __a, int16x8_t __b, int16x8_t __v,
		     const int __lane)
{
  return __builtin_aarch64_smlal_hi_laneqv8hi (__a, __b, __v, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_laneq_s32(int64x2_t __a, int32x4_t __b, int32x4_t __v,
		     const int __lane)
{
  return __builtin_aarch64_smlal_hi_laneqv4si (__a, __b, __v, __lane);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_laneq_u16(uint32x4_t __a, uint16x8_t __b, uint16x8_t __v,
		     const int __lane)
{
  return __builtin_aarch64_umlal_hi_laneqv8hi_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_laneq_u32(uint64x2_t __a, uint32x4_t __b, uint32x4_t __v,
		     const int __lane)
{
  return __builtin_aarch64_umlal_hi_laneqv4si_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_n_s16 (int32x4_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_aarch64_smlal_hi_nv8hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_n_s32 (int64x2_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_aarch64_smlal_hi_nv4si (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_n_u16 (uint32x4_t __a, uint16x8_t __b, uint16_t __c)
{
  return __builtin_aarch64_umlal_hi_nv8hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_n_u32 (uint64x2_t __a, uint32x4_t __b, uint32_t __c)
{
  return __builtin_aarch64_umlal_hi_nv4si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_s8 (int16x8_t __a, int8x16_t __b, int8x16_t __c)
{
  return __builtin_aarch64_smlal_hiv16qi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_smlal_hiv8hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_smlal_hiv4si (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_u8 (uint16x8_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return __builtin_aarch64_umlal_hiv16qi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_u16 (uint32x4_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_aarch64_umlal_hiv8hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_high_u32 (uint64x2_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_aarch64_umlal_hiv4si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_lane_s16 (int32x4_t __acc, int16x4_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_vec_smlal_lane_v4hi (__acc, __a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_lane_s32 (int64x2_t __acc, int32x2_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_vec_smlal_lane_v2si (__acc, __a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_lane_u16 (uint32x4_t __acc, uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return __builtin_aarch64_vec_umlal_lane_v4hi_uuuus (__acc, __a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_lane_u32 (uint64x2_t __acc, uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return __builtin_aarch64_vec_umlal_lane_v2si_uuuus (__acc, __a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_laneq_s16 (int32x4_t __acc, int16x4_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_vec_smlal_laneq_v4hi (__acc, __a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_laneq_s32 (int64x2_t __acc, int32x2_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_vec_smlal_laneq_v2si (__acc, __a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_laneq_u16 (uint32x4_t __acc, uint16x4_t __a, uint16x8_t __b, const int __c)
{
  return __builtin_aarch64_vec_umlal_laneq_v4hi_uuuus (__acc, __a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_laneq_u32 (uint64x2_t __acc, uint32x2_t __a, uint32x4_t __b, const int __c)
{
  return __builtin_aarch64_vec_umlal_laneq_v2si_uuuus (__acc, __a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return __builtin_aarch64_smlal_nv4hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return __builtin_aarch64_smlal_nv2si (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_n_u16 (uint32x4_t __a, uint16x4_t __b, uint16_t __c)
{
  return __builtin_aarch64_umlal_nv4hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_n_u32 (uint64x2_t __a, uint32x2_t __b, uint32_t __c)
{
  return __builtin_aarch64_umlal_nv2si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_s8 (int16x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return __builtin_aarch64_smlalv8qi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_smlalv4hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_smlalv2si (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_u8 (uint16x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return __builtin_aarch64_umlalv8qi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_u16 (uint32x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return __builtin_aarch64_umlalv4hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlal_u32 (uint64x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return __builtin_aarch64_umlalv2si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_lane_s16(int32x4_t __a, int16x8_t __b, int16x4_t __v,
		    const int __lane)
{
  return __builtin_aarch64_smlsl_hi_lanev8hi (__a, __b, __v, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_lane_s32(int64x2_t __a, int32x4_t __b, int32x2_t __v,
		    const int __lane)
{
  return __builtin_aarch64_smlsl_hi_lanev4si (__a, __b, __v, __lane);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_lane_u16(uint32x4_t __a, uint16x8_t __b, uint16x4_t __v,
		    const int __lane)
{
  return __builtin_aarch64_umlsl_hi_lanev8hi_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_lane_u32(uint64x2_t __a, uint32x4_t __b, uint32x2_t __v,
		    const int __lane)
{
  return __builtin_aarch64_umlsl_hi_lanev4si_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_laneq_s16(int32x4_t __a, int16x8_t __b, int16x8_t __v,
		     const int __lane)
{
  return __builtin_aarch64_smlsl_hi_laneqv8hi (__a, __b, __v, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_laneq_s32(int64x2_t __a, int32x4_t __b, int32x4_t __v,
		     const int __lane)
{
  return __builtin_aarch64_smlsl_hi_laneqv4si (__a, __b, __v, __lane);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_laneq_u16(uint32x4_t __a, uint16x8_t __b, uint16x8_t __v,
		     const int __lane)
{
  return __builtin_aarch64_umlsl_hi_laneqv8hi_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_laneq_u32(uint64x2_t __a, uint32x4_t __b, uint32x4_t __v,
		     const int __lane)
{
  return __builtin_aarch64_umlsl_hi_laneqv4si_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_n_s16 (int32x4_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_aarch64_smlsl_hi_nv8hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_n_s32 (int64x2_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_aarch64_smlsl_hi_nv4si (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_n_u16 (uint32x4_t __a, uint16x8_t __b, uint16_t __c)
{
  return __builtin_aarch64_umlsl_hi_nv8hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_n_u32 (uint64x2_t __a, uint32x4_t __b, uint32_t __c)
{
  return __builtin_aarch64_umlsl_hi_nv4si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_s8 (int16x8_t __a, int8x16_t __b, int8x16_t __c)
{
  return __builtin_aarch64_smlsl_hiv16qi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_smlsl_hiv8hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_smlsl_hiv4si (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_u8 (uint16x8_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return __builtin_aarch64_umlsl_hiv16qi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_u16 (uint32x4_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_aarch64_umlsl_hiv8hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_high_u32 (uint64x2_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_aarch64_umlsl_hiv4si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_lane_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __v, const int __lane)
{
  return __builtin_aarch64_vec_smlsl_lane_v4hi (__a, __b, __v, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __v, const int __lane)
{
  return __builtin_aarch64_vec_smlsl_lane_v2si (__a, __b, __v, __lane);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_lane_u16 (uint32x4_t __a, uint16x4_t __b, uint16x4_t __v,
                const int __lane)
{
  return __builtin_aarch64_vec_umlsl_lane_v4hi_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_lane_u32 (uint64x2_t __a, uint32x2_t __b, uint32x2_t __v,
                const int __lane)
{
  return __builtin_aarch64_vec_umlsl_lane_v2si_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_laneq_s16 (int32x4_t __a, int16x4_t __b, int16x8_t __v, const int __lane)
{
  return __builtin_aarch64_vec_smlsl_laneq_v4hi (__a, __b, __v, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_laneq_s32 (int64x2_t __a, int32x2_t __b, int32x4_t __v, const int __lane)
{
  return __builtin_aarch64_vec_smlsl_laneq_v2si (__a, __b, __v, __lane);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_laneq_u16 (uint32x4_t __a, uint16x4_t __b, uint16x8_t __v,
                 const int __lane)
{
  return __builtin_aarch64_vec_umlsl_laneq_v4hi_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_laneq_u32 (uint64x2_t __a, uint32x2_t __b, uint32x4_t __v,
                 const int __lane)
{
  return __builtin_aarch64_vec_umlsl_laneq_v2si_uuuus (__a, __b, __v, __lane);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return __builtin_aarch64_smlsl_nv4hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return __builtin_aarch64_smlsl_nv2si (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_n_u16 (uint32x4_t __a, uint16x4_t __b, uint16_t __c)
{
  return __builtin_aarch64_umlsl_nv4hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_n_u32 (uint64x2_t __a, uint32x2_t __b, uint32_t __c)
{
  return __builtin_aarch64_umlsl_nv2si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_s8 (int16x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return __builtin_aarch64_smlslv8qi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_smlslv4hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_smlslv2si (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_u8 (uint16x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return __builtin_aarch64_umlslv8qi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_u16 (uint32x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return __builtin_aarch64_umlslv4hi_uuuu (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmlsl_u32 (uint64x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return __builtin_aarch64_umlslv2si_uuuu (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_high_s8 (int8x16_t __a)
{
  return __builtin_aarch64_vec_unpacks_hi_v16qi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_high_s16 (int16x8_t __a)
{
  return __builtin_aarch64_vec_unpacks_hi_v8hi (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_high_s32 (int32x4_t __a)
{
  return __builtin_aarch64_vec_unpacks_hi_v4si (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_high_u8 (uint8x16_t __a)
{
  return __builtin_aarch64_vec_unpacku_hi_v16qi_uu (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_high_u16 (uint16x8_t __a)
{
  return __builtin_aarch64_vec_unpacku_hi_v8hi_uu (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_high_u32 (uint32x4_t __a)
{
  return __builtin_aarch64_vec_unpacku_hi_v4si_uu (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_s8 (int8x8_t __a)
{
  return __builtin_aarch64_sxtlv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_s16 (int16x4_t __a)
{
  return __builtin_aarch64_sxtlv4si (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_s32 (int32x2_t __a)
{
  return __builtin_aarch64_sxtlv2di (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_u8 (uint8x8_t __a)
{
  return __builtin_aarch64_uxtlv8hi_uu (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_u16 (uint16x4_t __a)
{
  return __builtin_aarch64_uxtlv4si_uu (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovl_u32 (uint32x2_t __a)
{
  return __builtin_aarch64_uxtlv2di_uu (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_high_s16 (int8x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_xtn2v8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_high_s32 (int16x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_xtn2v4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_high_s64 (int32x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_xtn2v2di (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_high_u16 (uint8x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_xtn2v8hi_uuu (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_high_u32 (uint16x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_xtn2v4si_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_high_u64 (uint32x2_t __a, uint64x2_t __b)
{
  return __builtin_aarch64_xtn2v2di_uuu (__a, __b);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_s16 (int16x8_t __a)
{
  return __builtin_aarch64_xtnv8hi (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_s32 (int32x4_t __a)
{
  return __builtin_aarch64_xtnv4si (__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_s64 (int64x2_t __a)
{
  return __builtin_aarch64_xtnv2di (__a);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_u16 (uint16x8_t __a)
{
  return __builtin_aarch64_xtnv8hi_uu (__a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_u32 (uint32x4_t __a)
{
  return __builtin_aarch64_xtnv4si_uu (__a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmovn_u64 (uint64x2_t __a)
{
  return __builtin_aarch64_xtnv2di_uu (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_lane_s16 (int16x8_t __a, int16x4_t __v, const int __lane)
{
  return __builtin_aarch64_smull_hi_lanev8hi (__a, __v, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_lane_s32 (int32x4_t __a, int32x2_t __v, const int __lane)
{
  return __builtin_aarch64_smull_hi_lanev4si (__a, __v, __lane);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_lane_u16 (uint16x8_t __a, uint16x4_t __v, const int __lane)
{
  return __builtin_aarch64_umull_hi_lanev8hi_uuus (__a, __v, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_lane_u32 (uint32x4_t __a, uint32x2_t __v, const int __lane)
{
  return __builtin_aarch64_umull_hi_lanev4si_uuus (__a, __v, __lane);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_laneq_s16 (int16x8_t __a, int16x8_t __v, const int __lane)
{
  return __builtin_aarch64_smull_hi_laneqv8hi (__a, __v, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_laneq_s32 (int32x4_t __a, int32x4_t __v, const int __lane)
{
  return __builtin_aarch64_smull_hi_laneqv4si (__a, __v, __lane);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_laneq_u16 (uint16x8_t __a, uint16x8_t __v, const int __lane)
{
  return __builtin_aarch64_umull_hi_laneqv8hi_uuus (__a, __v, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_laneq_u32 (uint32x4_t __a, uint32x4_t __v, const int __lane)
{
  return __builtin_aarch64_umull_hi_laneqv4si_uuus (__a, __v, __lane);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_aarch64_smull_hi_nv8hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_aarch64_smull_hi_nv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_aarch64_umull_hi_nv8hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_n_u32 (uint32x4_t __a, uint32_t __b)
{
 return __builtin_aarch64_umull_hi_nv4si_uuu (__a, __b);
}

__extension__ extern __inline poly16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return __builtin_aarch64_pmull_hiv16qi_ppp (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_vec_widen_smult_hi_v16qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_vec_widen_smult_hi_v8hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_vec_widen_smult_hi_v4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_vec_widen_umult_hi_v16qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_vec_widen_umult_hi_v8hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_vec_widen_umult_hi_v4si_uuu (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_vec_smult_lane_v4hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_vec_smult_lane_v2si (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_lane_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return __builtin_aarch64_vec_umult_lane_v4hi_uuus (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_lane_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return __builtin_aarch64_vec_umult_lane_v2si_uuus (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_laneq_s16 (int16x4_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_vec_smult_laneq_v4hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_laneq_s32 (int32x2_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_vec_smult_laneq_v2si (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_laneq_u16 (uint16x4_t __a, uint16x8_t __b, const int __c)
{
  return __builtin_aarch64_vec_umult_laneq_v4hi_uuus (__a, __b, __c);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_laneq_u32 (uint32x2_t __a, uint32x4_t __b, const int __c)
{
  return __builtin_aarch64_vec_umult_laneq_v2si_uuus (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_n_s16 (int16x4_t __a, int16_t __b)
{
  return __builtin_aarch64_smull_nv4hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_n_s32 (int32x2_t __a, int32_t __b)
{
  return __builtin_aarch64_smull_nv2si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_n_u16 (uint16x4_t __a, uint16_t __b)
{
  return __builtin_aarch64_umull_nv4hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_n_u32 (uint32x2_t __a, uint32_t __b)
{
  return __builtin_aarch64_umull_nv2si_uuu (__a, __b);
}

__extension__ extern __inline poly16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return __builtin_aarch64_pmullv8qi_ppp (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_intrinsic_vec_smult_lo_v8qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_intrinsic_vec_smult_lo_v4hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_intrinsic_vec_smult_lo_v2si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_intrinsic_vec_umult_lo_v8qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_intrinsic_vec_umult_lo_v4hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __builtin_aarch64_intrinsic_vec_umult_lo_v2si_uuu (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadal_s8 (int16x4_t __a, int8x8_t __b)
{
  return __builtin_aarch64_sadalpv8qi (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadal_s16 (int32x2_t __a, int16x4_t __b)
{
  return __builtin_aarch64_sadalpv4hi (__a, __b);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadal_s32 (int64x1_t __a, int32x2_t __b)
{
  return (int64x1_t) __builtin_aarch64_sadalpv2si (__a[0], __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadal_u8 (uint16x4_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_uadalpv8qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadal_u16 (uint32x2_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_uadalpv4hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadal_u32 (uint64x1_t __a, uint32x2_t __b)
{
  return (uint64x1_t) __builtin_aarch64_uadalpv2si_uuu (__a[0], __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadalq_s8 (int16x8_t __a, int8x16_t __b)
{
  return __builtin_aarch64_sadalpv16qi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadalq_s16 (int32x4_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sadalpv8hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadalq_s32 (int64x2_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sadalpv4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadalq_u8 (uint16x8_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_uadalpv16qi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadalq_u16 (uint32x4_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_uadalpv8hi_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadalq_u32 (uint64x2_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_uadalpv4si_uuu (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddl_s8 (int8x8_t __a)
{
  return __builtin_aarch64_saddlpv8qi (__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddl_s16 (int16x4_t __a)
{
  return __builtin_aarch64_saddlpv4hi (__a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddl_s32 (int32x2_t __a)
{
  return (int64x1_t) __builtin_aarch64_saddlpv2si (__a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddl_u8 (uint8x8_t __a)
{
  return __builtin_aarch64_uaddlpv8qi_uu (__a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddl_u16 (uint16x4_t __a)
{
  return __builtin_aarch64_uaddlpv4hi_uu (__a);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddl_u32 (uint32x2_t __a)
{
  return (uint64x1_t) __builtin_aarch64_uaddlpv2si_uu (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddlq_s8 (int8x16_t __a)
{
  return __builtin_aarch64_saddlpv16qi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddlq_s16 (int16x8_t __a)
{
  return __builtin_aarch64_saddlpv8hi (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddlq_s32 (int32x4_t __a)
{
  return __builtin_aarch64_saddlpv4si (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddlq_u8 (uint8x16_t __a)
{
  return __builtin_aarch64_uaddlpv16qi_uu (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddlq_u16 (uint16x8_t __a)
{
  return __builtin_aarch64_uaddlpv8hi_uu (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddlq_u32 (uint32x4_t __a)
{
  return __builtin_aarch64_uaddlpv4si_uu (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_addpv16qi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_addpv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_addpv4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_addpv2di (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_addpv16qi_uuu (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_addpv8hi_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_addpv4si_uuu (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __builtin_aarch64_addpv2di_uuu (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulh_n_s16 (int16x4_t __a, int16_t __b)
{
  return __builtin_aarch64_sqdmulh_nv4hi (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulh_n_s32 (int32x2_t __a, int32_t __b)
{
  return __builtin_aarch64_sqdmulh_nv2si (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_aarch64_sqdmulh_nv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_aarch64_sqdmulh_nv4si (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_high_s16 (int8x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sqxtn2v8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_high_s32 (int16x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sqxtn2v4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_high_s64 (int32x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_sqxtn2v2di (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_high_u16 (uint8x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_uqxtn2v8hi_uuu (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_high_u32 (uint16x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_uqxtn2v4si_uuu (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_high_u64 (uint32x2_t __a, uint64x2_t __b)
{
  return __builtin_aarch64_uqxtn2v2di_uuu (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovun_high_s16 (uint8x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sqxtun2v8hi_uus (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovun_high_s32 (uint16x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sqxtun2v4si_uus (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovun_high_s64 (uint32x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_sqxtun2v2di_uus (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulh_n_s16 (int16x4_t __a, int16_t __b)
{
  return __builtin_aarch64_sqrdmulh_nv4hi (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulh_n_s32 (int32x2_t __a, int32_t __b)
{
  return __builtin_aarch64_sqrdmulh_nv2si (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_aarch64_sqrdmulh_nv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_aarch64_sqrdmulh_nv4si (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrte_u32 (uint32x2_t __a)
{
  return __builtin_aarch64_ursqrtev2si_uu (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrteq_u32 (uint32x4_t __a)
{
  return __builtin_aarch64_ursqrtev4si_uu (__a);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t) ((((uint8x8_t) __a) & ((uint8x8_t) __b))
		       != 0);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_p16 (poly16x4_t __a, poly16x4_t __b)
{
  return (uint16x4_t) ((((uint16x4_t) __a) & ((uint16x4_t) __b))
		       != 0);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_p64 (poly64x1_t __a, poly64x1_t __b)
{
  return (uint64x1_t) ((__a & __b) != __AARCH64_INT64_C (0));
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t) ((((uint8x16_t) __a) & ((uint8x16_t) __b))
		       != 0);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_p16 (poly16x8_t __a, poly16x8_t __b)
{
  return (uint16x8_t) ((((uint16x8_t) __a) & ((uint16x8_t) __b))
		       != 0);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_p64 (poly64x2_t __a, poly64x2_t __b)
{
  return (uint64x2_t) ((((uint64x2_t) __a) & ((uint64x2_t) __b))
		       != __AARCH64_INT64_C (0));
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_f16 (float16_t *__ptr, float16x4x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev4hf ((__builtin_aarch64_simd_hf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_f32 (float32_t *__ptr, float32x2x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev2sf ((__builtin_aarch64_simd_sf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_f64 (float64_t *__ptr, float64x1x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanedf ((__builtin_aarch64_simd_df *) __ptr, __val,
				__lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_p8 (poly8_t *__ptr, poly8x8x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev8qi_sps ((__builtin_aarch64_simd_qi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_p16 (poly16_t *__ptr, poly16x4x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev4hi_sps ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_p64 (poly64_t *__ptr, poly64x1x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanedi_sps ((__builtin_aarch64_simd_di *) __ptr,
				    __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_s8 (int8_t *__ptr, int8x8x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev8qi ((__builtin_aarch64_simd_qi *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_s16 (int16_t *__ptr, int16x4x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev4hi ((__builtin_aarch64_simd_hi *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_s32 (int32_t *__ptr, int32x2x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev2si ((__builtin_aarch64_simd_si *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_s64 (int64_t *__ptr, int64x1x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanedi ((__builtin_aarch64_simd_di *) __ptr, __val,
				__lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_u8 (uint8_t *__ptr, uint8x8x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev8qi_sus ((__builtin_aarch64_simd_qi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_u16 (uint16_t *__ptr, uint16x4x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev4hi_sus ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_u32 (uint32_t *__ptr, uint32x2x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev2si_sus ((__builtin_aarch64_simd_si *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_u64 (uint64_t *__ptr, uint64x1x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanedi_sus ((__builtin_aarch64_simd_di *) __ptr, __val,
				    __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_f16 (float16_t *__ptr, float16x8x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev8hf ((__builtin_aarch64_simd_hf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_f32 (float32_t *__ptr, float32x4x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev4sf ((__builtin_aarch64_simd_sf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_f64 (float64_t *__ptr, float64x2x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev2df ((__builtin_aarch64_simd_df *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_p8 (poly8_t *__ptr, poly8x16x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev16qi_sps ((__builtin_aarch64_simd_qi *) __ptr,
				       __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_p16 (poly16_t *__ptr, poly16x8x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev8hi_sps ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_p64 (poly64_t *__ptr, poly64x2x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev2di_sps ((__builtin_aarch64_simd_di *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_s8 (int8_t *__ptr, int8x16x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev16qi ((__builtin_aarch64_simd_qi *) __ptr, __val,
				   __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_s16 (int16_t *__ptr, int16x8x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev8hi ((__builtin_aarch64_simd_hi *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_s32 (int32_t *__ptr, int32x4x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev4si ((__builtin_aarch64_simd_si *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_s64 (int64_t *__ptr, int64x2x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev2di ((__builtin_aarch64_simd_di *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_u8 (uint8_t *__ptr, uint8x16x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev16qi_sus ((__builtin_aarch64_simd_qi *) __ptr,
				       __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_u16 (uint16_t *__ptr, uint16x8x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev8hi_sus ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_u32 (uint32_t *__ptr, uint32x4x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev4si_sus ((__builtin_aarch64_simd_si *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_u64 (uint64_t *__ptr, uint64x2x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev2di_sus ((__builtin_aarch64_simd_di *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_f16 (float16_t *__ptr, float16x4x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev4hf ((__builtin_aarch64_simd_hf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_f32 (float32_t *__ptr, float32x2x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev2sf ((__builtin_aarch64_simd_sf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_f64 (float64_t *__ptr, float64x1x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanedf ((__builtin_aarch64_simd_df *) __ptr, __val,
				__lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_p8 (poly8_t *__ptr, poly8x8x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev8qi_sps ((__builtin_aarch64_simd_qi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_p16 (poly16_t *__ptr, poly16x4x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev4hi_sps ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_p64 (poly64_t *__ptr, poly64x1x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanedi_sps ((__builtin_aarch64_simd_di *) __ptr, __val,
				    __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_s8 (int8_t *__ptr, int8x8x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev8qi ((__builtin_aarch64_simd_qi *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_s16 (int16_t *__ptr, int16x4x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev4hi ((__builtin_aarch64_simd_hi *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_s32 (int32_t *__ptr, int32x2x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev2si ((__builtin_aarch64_simd_si *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_s64 (int64_t *__ptr, int64x1x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanedi ((__builtin_aarch64_simd_di *) __ptr, __val,
				__lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_u8 (uint8_t *__ptr, uint8x8x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev8qi_sus ((__builtin_aarch64_simd_qi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_u16 (uint16_t *__ptr, uint16x4x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev4hi_sus ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_u32 (uint32_t *__ptr, uint32x2x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev2si_sus ((__builtin_aarch64_simd_si *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_u64 (uint64_t *__ptr, uint64x1x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanedi_sus ((__builtin_aarch64_simd_di *) __ptr, __val,
				    __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_f16 (float16_t *__ptr, float16x8x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev8hf ((__builtin_aarch64_simd_hf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_f32 (float32_t *__ptr, float32x4x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev4sf ((__builtin_aarch64_simd_sf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_f64 (float64_t *__ptr, float64x2x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev2df ((__builtin_aarch64_simd_df *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_p8 (poly8_t *__ptr, poly8x16x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev16qi_sps ((__builtin_aarch64_simd_qi *) __ptr,
				       __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_p16 (poly16_t *__ptr, poly16x8x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev8hi_sps ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_p64 (poly64_t *__ptr, poly64x2x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev2di_sps ((__builtin_aarch64_simd_di *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_s8 (int8_t *__ptr, int8x16x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev16qi ((__builtin_aarch64_simd_qi *) __ptr, __val,
				   __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_s16 (int16_t *__ptr, int16x8x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev8hi ((__builtin_aarch64_simd_hi *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_s32 (int32_t *__ptr, int32x4x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev4si ((__builtin_aarch64_simd_si *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_s64 (int64_t *__ptr, int64x2x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev2di ((__builtin_aarch64_simd_di *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_u8 (uint8_t *__ptr, uint8x16x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev16qi_sus ((__builtin_aarch64_simd_qi *) __ptr,
				       __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_u16 (uint16_t *__ptr, uint16x8x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev8hi_sus ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_u32 (uint32_t *__ptr, uint32x4x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev4si_sus ((__builtin_aarch64_simd_si *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_u64 (uint64_t *__ptr, uint64x2x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev2di_sus ((__builtin_aarch64_simd_di *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_f16 (float16_t *__ptr, float16x4x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev4hf ((__builtin_aarch64_simd_hf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_f32 (float32_t *__ptr, float32x2x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev2sf ((__builtin_aarch64_simd_sf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_f64 (float64_t *__ptr, float64x1x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanedf ((__builtin_aarch64_simd_df *) __ptr, __val,
				__lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_p8 (poly8_t *__ptr, poly8x8x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev8qi_sps ((__builtin_aarch64_simd_qi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_p16 (poly16_t *__ptr, poly16x4x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev4hi_sps ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_p64 (poly64_t *__ptr, poly64x1x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanedi_sps ((__builtin_aarch64_simd_di *) __ptr, __val,
				    __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_s8 (int8_t *__ptr, int8x8x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev8qi ((__builtin_aarch64_simd_qi *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_s16 (int16_t *__ptr, int16x4x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev4hi ((__builtin_aarch64_simd_hi *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_s32 (int32_t *__ptr, int32x2x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev2si ((__builtin_aarch64_simd_si *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_s64 (int64_t *__ptr, int64x1x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanedi ((__builtin_aarch64_simd_di *) __ptr, __val,
				__lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_u8 (uint8_t *__ptr, uint8x8x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev8qi_sus ((__builtin_aarch64_simd_qi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_u16 (uint16_t *__ptr, uint16x4x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev4hi_sus ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_u32 (uint32_t *__ptr, uint32x2x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev2si_sus ((__builtin_aarch64_simd_si *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_u64 (uint64_t *__ptr, uint64x1x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanedi_sus ((__builtin_aarch64_simd_di *) __ptr, __val,
				    __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_f16 (float16_t *__ptr, float16x8x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev8hf ((__builtin_aarch64_simd_hf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_f32 (float32_t *__ptr, float32x4x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev4sf ((__builtin_aarch64_simd_sf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_f64 (float64_t *__ptr, float64x2x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev2df ((__builtin_aarch64_simd_df *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_p8 (poly8_t *__ptr, poly8x16x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev16qi_sps ((__builtin_aarch64_simd_qi *) __ptr,
				       __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_p16 (poly16_t *__ptr, poly16x8x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev8hi_sps ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_p64 (poly64_t *__ptr, poly64x2x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev2di_sps ((__builtin_aarch64_simd_di *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_s8 (int8_t *__ptr, int8x16x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev16qi ((__builtin_aarch64_simd_qi *) __ptr, __val,
				   __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_s16 (int16_t *__ptr, int16x8x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev8hi ((__builtin_aarch64_simd_hi *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_s32 (int32_t *__ptr, int32x4x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev4si ((__builtin_aarch64_simd_si *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_s64 (int64_t *__ptr, int64x2x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev2di ((__builtin_aarch64_simd_di *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_u8 (uint8_t *__ptr, uint8x16x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev16qi_sus ((__builtin_aarch64_simd_qi *) __ptr,
				       __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_u16 (uint16_t *__ptr, uint16x8x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev8hi_sus ((__builtin_aarch64_simd_hi *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_u32 (uint32_t *__ptr, uint32x4x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev4si_sus ((__builtin_aarch64_simd_si *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_u64 (uint64_t *__ptr, uint64x2x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev2di_sus ((__builtin_aarch64_simd_di *) __ptr,
				      __val, __lane);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlv_s32 (int32x2_t __a)
{
  return __builtin_aarch64_saddlvv2si (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vaddlv_u32 (uint32x2_t __a)
{
  return __builtin_aarch64_uaddlvv2si_uu (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulh_laneq_s16 (int16x4_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqv4hi (__a, __b, __c);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulh_laneq_s32 (int32x2_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqv2si (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhq_laneq_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqv8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhq_laneq_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqv4si (__a, __b, __c);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulh_laneq_s16 (int16x4_t __a, int16x8_t __b, const int __c)
{
  return  __builtin_aarch64_sqrdmulh_laneqv4hi (__a, __b, __c);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulh_laneq_s32 (int32x2_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_laneqv2si (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhq_laneq_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_laneqv8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhq_laneq_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_laneqv4si (__a, __b, __c);
}

/* End of temporary inline asm.  */

/* Start of optimal implementations in approved order.  */

/* ARMv8.1-A intrinsics.  */
#pragma GCC push_options
#pragma GCC target ("+nothing+rdma")

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlah_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_sqrdmlahv4hi (__a, __b, __c);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlah_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_sqrdmlahv2si (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahq_s16 (int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_sqrdmlahv8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahq_s32 (int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_sqrdmlahv4si (__a, __b, __c);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlsh_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_sqrdmlshv4hi (__a, __b, __c);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlsh_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_sqrdmlshv2si (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshq_s16 (int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_sqrdmlshv8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshq_s32 (int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_sqrdmlshv4si (__a, __b, __c);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlah_laneq_s16 (int16x4_t __a, int16x4_t __b, int16x8_t __c, const int __d)
{
  return  __builtin_aarch64_sqrdmlah_laneqv4hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlah_laneq_s32 (int32x2_t __a, int32x2_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_laneqv2si (__a, __b, __c, __d);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahq_laneq_s16 (int16x8_t __a, int16x8_t __b, int16x8_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_laneqv8hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahq_laneq_s32 (int32x4_t __a, int32x4_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_laneqv4si (__a, __b, __c, __d);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlsh_laneq_s16 (int16x4_t __a, int16x4_t __b, int16x8_t __c, const int __d)
{
  return  __builtin_aarch64_sqrdmlsh_laneqv4hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlsh_laneq_s32 (int32x2_t __a, int32x2_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_laneqv2si (__a, __b, __c, __d);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshq_laneq_s16 (int16x8_t __a, int16x8_t __b, int16x8_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_laneqv8hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshq_laneq_s32 (int32x4_t __a, int32x4_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_laneqv4si (__a, __b, __c, __d);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlah_lane_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return  __builtin_aarch64_sqrdmlah_lanev4hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlah_lane_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_lanev2si (__a, __b, __c, __d);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahq_lane_s16 (int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_lanev8hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahq_lane_s32 (int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_lanev4si (__a, __b, __c, __d);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahh_s16 (int16_t __a, int16_t __b, int16_t __c)
{
  return (int16_t) __builtin_aarch64_sqrdmlahhi (__a, __b, __c);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahh_lane_s16 (int16_t __a, int16_t __b, int16x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_lanehi (__a, __b, __c, __d);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahh_laneq_s16 (int16_t __a, int16_t __b, int16x8_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_laneqhi (__a, __b, __c, __d);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahs_s32 (int32_t __a, int32_t __b, int32_t __c)
{
  return (int32_t) __builtin_aarch64_sqrdmlahsi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahs_lane_s32 (int32_t __a, int32_t __b, int32x2_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_lanesi (__a, __b, __c, __d);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlahs_laneq_s32 (int32_t __a, int32_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlah_laneqsi (__a, __b, __c, __d);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlsh_lane_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return  __builtin_aarch64_sqrdmlsh_lanev4hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlsh_lane_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_lanev2si (__a, __b, __c, __d);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshq_lane_s16 (int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_lanev8hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshq_lane_s32 (int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_lanev4si (__a, __b, __c, __d);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshh_s16 (int16_t __a, int16_t __b, int16_t __c)
{
  return (int16_t) __builtin_aarch64_sqrdmlshhi (__a, __b, __c);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshh_lane_s16 (int16_t __a, int16_t __b, int16x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_lanehi (__a, __b, __c, __d);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshh_laneq_s16 (int16_t __a, int16_t __b, int16x8_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_laneqhi (__a, __b, __c, __d);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshs_s32 (int32_t __a, int32_t __b, int32_t __c)
{
  return (int32_t) __builtin_aarch64_sqrdmlshsi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshs_lane_s32 (int32_t __a, int32_t __b, int32x2_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_lanesi (__a, __b, __c, __d);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmlshs_laneq_s32 (int32_t __a, int32_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqrdmlsh_laneqsi (__a, __b, __c, __d);
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("+nothing+aes")
/* vaes  */

__extension__ extern __inline poly128_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_p64 (poly64_t __a, poly64_t __b)
{
  return
    __builtin_aarch64_crypto_pmulldi_ppp (__a, __b);
}

__extension__ extern __inline poly128_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmull_high_p64 (poly64x2_t __a, poly64x2_t __b)
{
  return __builtin_aarch64_crypto_pmullv2di_ppp (__a, __b);
}

#pragma GCC pop_options

/* vcage  */

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcage_f64 (float64x1_t __a, float64x1_t __b)
{
  return vcreate_u64 (__builtin_aarch64_facgedf_uss (__a[0], __b[0]));
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcages_f32 (float32_t __a, float32_t __b)
{
  return __builtin_aarch64_facgesf_uss  (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcage_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_facgev2sf_uss (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcageq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_facgev4sf_uss (__a, __b);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaged_f64 (float64_t __a, float64_t __b)
{
  return __builtin_aarch64_facgedf_uss (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcageq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_facgev2df_uss (__a, __b);
}

/* vcagt  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcagts_f32 (float32_t __a, float32_t __b)
{
  return __builtin_aarch64_facgtsf_uss (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcagt_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_facgtv2sf_uss (__a, __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcagt_f64 (float64x1_t __a, float64x1_t __b)
{
  return vcreate_u64 (__builtin_aarch64_facgtdf_uss (__a[0], __b[0]));
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcagtq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_facgtv4sf_uss (__a, __b);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcagtd_f64 (float64_t __a, float64_t __b)
{
  return __builtin_aarch64_facgtdf_uss (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcagtq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_facgtv2df_uss (__a, __b);
}

/* vcale  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcale_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_faclev2sf_uss (__a, __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcale_f64 (float64x1_t __a, float64x1_t __b)
{
  return vcreate_u64 (__builtin_aarch64_facledf_uss (__a[0], __b[0]));
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaled_f64 (float64_t __a, float64_t __b)
{
  return __builtin_aarch64_facledf_uss (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcales_f32 (float32_t __a, float32_t __b)
{
  return __builtin_aarch64_faclesf_uss (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaleq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_faclev4sf_uss (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaleq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_faclev2df_uss (__a, __b);
}

/* vcalt  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcalt_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_facltv2sf_uss (__a, __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcalt_f64 (float64x1_t __a, float64x1_t __b)
{
  return vcreate_u64 (__builtin_aarch64_facltdf_uss (__a[0], __b[0]));
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaltd_f64 (float64_t __a, float64_t __b)
{
  return __builtin_aarch64_facltdf_uss (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaltq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_facltv4sf_uss (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaltq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_facltv2df_uss (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcalts_f32 (float32_t __a, float32_t __b)
{
  return __builtin_aarch64_facltsf_uss (__a, __b);
}

/* vceq - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) (__a == __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_f64 (float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t) (__a == __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t) (__a == __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_p64 (poly64x1_t __a, poly64x1_t __b)
{
  return (uint64x1_t) (__a == __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) (__a == __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) (__a == __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) (__a == __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_s64 (int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) (__a == __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (__a == __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (__a == __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (__a == __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (__a == __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) (__a == __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) (__a == __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t) (__a == __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) (__a == __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) (__a == __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) (__a == __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) (__a == __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (__a == __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (__a == __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (__a == __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (__a == __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_p64 (poly64x2_t __a, poly64x2_t __b)
{
  return (__a == __b);
}

/* vceq - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqs_f32 (float32_t __a, float32_t __b)
{
  return __a == __b ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqd_s64 (int64_t __a, int64_t __b)
{
  return __a == __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqd_u64 (uint64_t __a, uint64_t __b)
{
  return __a == __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqd_f64 (float64_t __a, float64_t __b)
{
  return __a == __b ? -1ll : 0ll;
}

/* vceqz - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_f32 (float32x2_t __a)
{
  return (uint32x2_t) (__a == 0.0f);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_f64 (float64x1_t __a)
{
  return (uint64x1_t) (__a == (float64x1_t) {0.0});
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_p8 (poly8x8_t __a)
{
  return (uint8x8_t) (__a == 0);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_s8 (int8x8_t __a)
{
  return (uint8x8_t) (__a == 0);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_s16 (int16x4_t __a)
{
  return (uint16x4_t) (__a == 0);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_s32 (int32x2_t __a)
{
  return (uint32x2_t) (__a == 0);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_s64 (int64x1_t __a)
{
  return (uint64x1_t) (__a == __AARCH64_INT64_C (0));
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_u8 (uint8x8_t __a)
{
  return (__a == 0);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_u16 (uint16x4_t __a)
{
  return (__a == 0);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_u32 (uint32x2_t __a)
{
  return (__a == 0);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_u64 (uint64x1_t __a)
{
  return (__a == __AARCH64_UINT64_C (0));
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_p64 (poly64x1_t __a)
{
  return (__a == __AARCH64_UINT64_C (0));
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_f32 (float32x4_t __a)
{
  return (uint32x4_t) (__a == 0.0f);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_f64 (float64x2_t __a)
{
  return (uint64x2_t) (__a == 0.0f);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_p8 (poly8x16_t __a)
{
  return (uint8x16_t) (__a == 0);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_s8 (int8x16_t __a)
{
  return (uint8x16_t) (__a == 0);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_s16 (int16x8_t __a)
{
  return (uint16x8_t) (__a == 0);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_s32 (int32x4_t __a)
{
  return (uint32x4_t) (__a == 0);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_s64 (int64x2_t __a)
{
  return (uint64x2_t) (__a == __AARCH64_INT64_C (0));
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_u8 (uint8x16_t __a)
{
  return (__a == 0);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_u16 (uint16x8_t __a)
{
  return (__a == 0);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_u32 (uint32x4_t __a)
{
  return (__a == 0);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_u64 (uint64x2_t __a)
{
  return (__a == __AARCH64_UINT64_C (0));
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_p64 (poly64x2_t __a)
{
  return (__a == __AARCH64_UINT64_C (0));
}

/* vceqz - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzs_f32 (float32_t __a)
{
  return __a == 0.0f ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzd_s64 (int64_t __a)
{
  return __a == 0 ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzd_u64 (uint64_t __a)
{
  return __a == 0 ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzd_f64 (float64_t __a)
{
  return __a == 0.0 ? -1ll : 0ll;
}

/* vcge - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) (__a >= __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_f64 (float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t) (__a >= __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) (__a >= __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) (__a >= __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) (__a >= __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_s64 (int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) (__a >= __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (__a >= __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (__a >= __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (__a >= __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (__a >= __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) (__a >= __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) (__a >= __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) (__a >= __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) (__a >= __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) (__a >= __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) (__a >= __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (__a >= __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (__a >= __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (__a >= __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (__a >= __b);
}

/* vcge - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcges_f32 (float32_t __a, float32_t __b)
{
  return __a >= __b ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcged_s64 (int64_t __a, int64_t __b)
{
  return __a >= __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcged_u64 (uint64_t __a, uint64_t __b)
{
  return __a >= __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcged_f64 (float64_t __a, float64_t __b)
{
  return __a >= __b ? -1ll : 0ll;
}

/* vcgez - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgez_f32 (float32x2_t __a)
{
  return (uint32x2_t) (__a >= 0.0f);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgez_f64 (float64x1_t __a)
{
  return (uint64x1_t) (__a[0] >= (float64x1_t) {0.0});
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgez_s8 (int8x8_t __a)
{
  return (uint8x8_t) (__a >= 0);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgez_s16 (int16x4_t __a)
{
  return (uint16x4_t) (__a >= 0);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgez_s32 (int32x2_t __a)
{
  return (uint32x2_t) (__a >= 0);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgez_s64 (int64x1_t __a)
{
  return (uint64x1_t) (__a >= __AARCH64_INT64_C (0));
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezq_f32 (float32x4_t __a)
{
  return (uint32x4_t) (__a >= 0.0f);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezq_f64 (float64x2_t __a)
{
  return (uint64x2_t) (__a >= 0.0);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezq_s8 (int8x16_t __a)
{
  return (uint8x16_t) (__a >= 0);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezq_s16 (int16x8_t __a)
{
  return (uint16x8_t) (__a >= 0);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezq_s32 (int32x4_t __a)
{
  return (uint32x4_t) (__a >= 0);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezq_s64 (int64x2_t __a)
{
  return (uint64x2_t) (__a >= __AARCH64_INT64_C (0));
}

/* vcgez - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezs_f32 (float32_t __a)
{
  return __a >= 0.0f ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezd_s64 (int64_t __a)
{
  return __a >= 0 ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezd_f64 (float64_t __a)
{
  return __a >= 0.0 ? -1ll : 0ll;
}

/* vcgt - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) (__a > __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_f64 (float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t) (__a > __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) (__a > __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) (__a > __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) (__a > __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_s64 (int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) (__a > __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (__a > __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (__a > __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (__a > __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (__a > __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) (__a > __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) (__a > __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) (__a > __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) (__a > __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) (__a > __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) (__a > __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (__a > __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (__a > __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (__a > __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (__a > __b);
}

/* vcgt - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgts_f32 (float32_t __a, float32_t __b)
{
  return __a > __b ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtd_s64 (int64_t __a, int64_t __b)
{
  return __a > __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtd_u64 (uint64_t __a, uint64_t __b)
{
  return __a > __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtd_f64 (float64_t __a, float64_t __b)
{
  return __a > __b ? -1ll : 0ll;
}

/* vcgtz - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtz_f32 (float32x2_t __a)
{
  return (uint32x2_t) (__a > 0.0f);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtz_f64 (float64x1_t __a)
{
  return (uint64x1_t) (__a > (float64x1_t) {0.0});
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtz_s8 (int8x8_t __a)
{
  return (uint8x8_t) (__a > 0);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtz_s16 (int16x4_t __a)
{
  return (uint16x4_t) (__a > 0);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtz_s32 (int32x2_t __a)
{
  return (uint32x2_t) (__a > 0);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtz_s64 (int64x1_t __a)
{
  return (uint64x1_t) (__a > __AARCH64_INT64_C (0));
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzq_f32 (float32x4_t __a)
{
  return (uint32x4_t) (__a > 0.0f);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzq_f64 (float64x2_t __a)
{
    return (uint64x2_t) (__a > 0.0);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzq_s8 (int8x16_t __a)
{
  return (uint8x16_t) (__a > 0);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzq_s16 (int16x8_t __a)
{
  return (uint16x8_t) (__a > 0);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzq_s32 (int32x4_t __a)
{
  return (uint32x4_t) (__a > 0);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzq_s64 (int64x2_t __a)
{
  return (uint64x2_t) (__a > __AARCH64_INT64_C (0));
}

/* vcgtz - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzs_f32 (float32_t __a)
{
  return __a > 0.0f ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzd_s64 (int64_t __a)
{
  return __a > 0 ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzd_f64 (float64_t __a)
{
  return __a > 0.0 ? -1ll : 0ll;
}

/* vcle - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) (__a <= __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_f64 (float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t) (__a <= __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) (__a <= __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) (__a <= __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) (__a <= __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_s64 (int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) (__a <= __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (__a <= __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (__a <= __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (__a <= __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (__a <= __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) (__a <= __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) (__a <= __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) (__a <= __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) (__a <= __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) (__a <= __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) (__a <= __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (__a <= __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (__a <= __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (__a <= __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (__a <= __b);
}

/* vcle - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcles_f32 (float32_t __a, float32_t __b)
{
  return __a <= __b ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcled_s64 (int64_t __a, int64_t __b)
{
  return __a <= __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcled_u64 (uint64_t __a, uint64_t __b)
{
  return __a <= __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcled_f64 (float64_t __a, float64_t __b)
{
  return __a <= __b ? -1ll : 0ll;
}

/* vclez - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclez_f32 (float32x2_t __a)
{
  return (uint32x2_t) (__a <= 0.0f);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclez_f64 (float64x1_t __a)
{
  return (uint64x1_t) (__a <= (float64x1_t) {0.0});
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclez_s8 (int8x8_t __a)
{
  return (uint8x8_t) (__a <= 0);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclez_s16 (int16x4_t __a)
{
  return (uint16x4_t) (__a <= 0);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclez_s32 (int32x2_t __a)
{
  return (uint32x2_t) (__a <= 0);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclez_s64 (int64x1_t __a)
{
  return (uint64x1_t) (__a <= __AARCH64_INT64_C (0));
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezq_f32 (float32x4_t __a)
{
  return (uint32x4_t) (__a <= 0.0f);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezq_f64 (float64x2_t __a)
{
  return (uint64x2_t) (__a <= 0.0);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezq_s8 (int8x16_t __a)
{
  return (uint8x16_t) (__a <= 0);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezq_s16 (int16x8_t __a)
{
  return (uint16x8_t) (__a <= 0);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezq_s32 (int32x4_t __a)
{
  return (uint32x4_t) (__a <= 0);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezq_s64 (int64x2_t __a)
{
  return (uint64x2_t) (__a <= __AARCH64_INT64_C (0));
}

/* vclez - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezs_f32 (float32_t __a)
{
  return __a <= 0.0f ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezd_s64 (int64_t __a)
{
  return __a <= 0 ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezd_f64 (float64_t __a)
{
  return __a <= 0.0 ? -1ll : 0ll;
}

/* vclt - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) (__a < __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_f64 (float64x1_t __a, float64x1_t __b)
{
  return (uint64x1_t) (__a < __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) (__a < __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) (__a < __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) (__a < __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_s64 (int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) (__a < __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (__a < __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (__a < __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (__a < __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (__a < __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) (__a < __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) (__a < __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) (__a < __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) (__a < __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) (__a < __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) (__a < __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (__a < __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (__a < __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (__a < __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (__a < __b);
}

/* vclt - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclts_f32 (float32_t __a, float32_t __b)
{
  return __a < __b ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltd_s64 (int64_t __a, int64_t __b)
{
  return __a < __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltd_u64 (uint64_t __a, uint64_t __b)
{
  return __a < __b ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltd_f64 (float64_t __a, float64_t __b)
{
  return __a < __b ? -1ll : 0ll;
}

/* vcltz - vector.  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltz_f32 (float32x2_t __a)
{
  return (uint32x2_t) (__a < 0.0f);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltz_f64 (float64x1_t __a)
{
  return (uint64x1_t) (__a < (float64x1_t) {0.0});
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltz_s8 (int8x8_t __a)
{
  return (uint8x8_t) (__a < 0);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltz_s16 (int16x4_t __a)
{
  return (uint16x4_t) (__a < 0);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltz_s32 (int32x2_t __a)
{
  return (uint32x2_t) (__a < 0);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltz_s64 (int64x1_t __a)
{
  return (uint64x1_t) (__a < __AARCH64_INT64_C (0));
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzq_f32 (float32x4_t __a)
{
  return (uint32x4_t) (__a < 0.0f);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzq_f64 (float64x2_t __a)
{
  return (uint64x2_t) (__a < 0.0);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzq_s8 (int8x16_t __a)
{
  return (uint8x16_t) (__a < 0);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzq_s16 (int16x8_t __a)
{
  return (uint16x8_t) (__a < 0);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzq_s32 (int32x4_t __a)
{
  return (uint32x4_t) (__a < 0);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzq_s64 (int64x2_t __a)
{
  return (uint64x2_t) (__a < __AARCH64_INT64_C (0));
}

/* vcltz - scalar.  */

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzs_f32 (float32_t __a)
{
  return __a < 0.0f ? -1 : 0;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzd_s64 (int64_t __a)
{
  return __a < 0 ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzd_f64 (float64_t __a)
{
  return __a < 0.0 ? -1ll : 0ll;
}

/* vcvt (double -> float).  */

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f16_f32 (float32x4_t __a)
{
  return __builtin_aarch64_float_truncate_lo_v4hf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_high_f16_f32 (float16x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_float_truncate_hi_v8hf (__a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f32_f64 (float64x2_t __a)
{
  return __builtin_aarch64_float_truncate_lo_v2sf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_high_f32_f64 (float32x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_float_truncate_hi_v4sf (__a, __b);
}

/* vcvt (float -> double).  */

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f32_f16 (float16x4_t __a)
{
  return __builtin_aarch64_float_extend_lo_v4sf (__a);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f64_f32 (float32x2_t __a)
{

  return __builtin_aarch64_float_extend_lo_v2df (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_high_f32_f16 (float16x8_t __a)
{
  return __builtin_aarch64_vec_unpacks_hi_v8hf (__a);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_high_f64_f32 (float32x4_t __a)
{
  return __builtin_aarch64_vec_unpacks_hi_v4sf (__a);
}

/* vcvt (<u>fixed-point -> float).  */

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_n_f64_s64 (int64_t __a, const int __b)
{
  return __builtin_aarch64_scvtfdi (__a, __b);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_n_f64_u64 (uint64_t __a, const int __b)
{
  return __builtin_aarch64_ucvtfdi_sus (__a, __b);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_n_f32_s32 (int32_t __a, const int __b)
{
  return __builtin_aarch64_scvtfsi (__a, __b);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_n_f32_u32 (uint32_t __a, const int __b)
{
  return __builtin_aarch64_ucvtfsi_sus (__a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_f32_s32 (int32x2_t __a, const int __b)
{
  return __builtin_aarch64_scvtfv2si (__a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_f32_u32 (uint32x2_t __a, const int __b)
{
  return __builtin_aarch64_ucvtfv2si_sus (__a, __b);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_f64_s64 (int64x1_t __a, const int __b)
{
  return (float64x1_t)
    { __builtin_aarch64_scvtfdi (vget_lane_s64 (__a, 0), __b) };
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_f64_u64 (uint64x1_t __a, const int __b)
{
  return (float64x1_t)
    { __builtin_aarch64_ucvtfdi_sus (vget_lane_u64 (__a, 0), __b) };
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_f32_s32 (int32x4_t __a, const int __b)
{
  return __builtin_aarch64_scvtfv4si (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_f32_u32 (uint32x4_t __a, const int __b)
{
  return __builtin_aarch64_ucvtfv4si_sus (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_f64_s64 (int64x2_t __a, const int __b)
{
  return __builtin_aarch64_scvtfv2di (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_f64_u64 (uint64x2_t __a, const int __b)
{
  return __builtin_aarch64_ucvtfv2di_sus (__a, __b);
}

/* vcvt (float -> <u>fixed-point).  */

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_n_s64_f64 (float64_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzsdf (__a, __b);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_n_u64_f64 (float64_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzudf_uss (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_n_s32_f32 (float32_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzssf (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_n_u32_f32 (float32_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzusf_uss (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_s32_f32 (float32x2_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzsv2sf (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_u32_f32 (float32x2_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzuv2sf_uss (__a, __b);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_s64_f64 (float64x1_t __a, const int __b)
{
  return (int64x1_t)
    { __builtin_aarch64_fcvtzsdf (vget_lane_f64 (__a, 0), __b) };
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_u64_f64 (float64x1_t __a, const int __b)
{
  return (uint64x1_t)
    { __builtin_aarch64_fcvtzudf_uss (vget_lane_f64 (__a, 0), __b) };
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_s32_f32 (float32x4_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzsv4sf (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_u32_f32 (float32x4_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzuv4sf_uss (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_s64_f64 (float64x2_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzsv2df (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_u64_f64 (float64x2_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzuv2df_uss (__a, __b);
}

/* vcvt  (<u>int -> float)  */

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_f64_s64 (int64_t __a)
{
  return (float64_t) __a;
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_f64_u64 (uint64_t __a)
{
  return (float64_t) __a;
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_f32_s32 (int32_t __a)
{
  return (float32_t) __a;
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_f32_u32 (uint32_t __a)
{
  return (float32_t) __a;
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f32_s32 (int32x2_t __a)
{
  return __builtin_aarch64_floatv2siv2sf (__a);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f32_u32 (uint32x2_t __a)
{
  return __builtin_aarch64_floatunsv2siv2sf ((int32x2_t) __a);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f64_s64 (int64x1_t __a)
{
  return (float64x1_t) { vget_lane_s64 (__a, 0) };
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f64_u64 (uint64x1_t __a)
{
  return (float64x1_t) { vget_lane_u64 (__a, 0) };
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_f32_s32 (int32x4_t __a)
{
  return __builtin_aarch64_floatv4siv4sf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_f32_u32 (uint32x4_t __a)
{
  return __builtin_aarch64_floatunsv4siv4sf ((int32x4_t) __a);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_f64_s64 (int64x2_t __a)
{
  return __builtin_aarch64_floatv2div2df (__a);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_f64_u64 (uint64x2_t __a)
{
  return __builtin_aarch64_floatunsv2div2df ((int64x2_t) __a);
}

/* vcvt (float -> <u>int)  */

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_s64_f64 (float64_t __a)
{
  return (int64_t) __a;
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_s32_f64 (float64_t __a)
{
  return (int32_t) __a;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_u64_f64 (float64_t __a)
{
  return (uint64_t) __a;
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtd_u32_f64 (float64_t __a)
{
  return (uint32_t) __a;
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_s32_f32 (float32_t __a)
{
  return (int32_t) __a;
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_s64_f32 (float32_t __a)
{
  return (int64_t) __a;
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_u32_f32 (float32_t __a)
{
  return (uint32_t) __a;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvts_u64_f32 (float32_t __a)
{
  return (uint64_t) __a;
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lbtruncv2sfv2si (__a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_u32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lbtruncuv2sfv2si_us (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lbtruncv4sfv4si (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_u32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lbtruncuv4sfv4si_us (__a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_s64_f64 (float64x1_t __a)
{
  return (int64x1_t) {vcvtd_s64_f64 (__a[0])};
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_u64_f64 (float64x1_t __a)
{
  return (uint64x1_t) {vcvtd_u64_f64 (__a[0])};
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lbtruncv2dfv2di (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_u64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lbtruncuv2dfv2di_us (__a);
}

/* vcvta  */

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtad_s64_f64 (float64_t __a)
{
  return __builtin_aarch64_lrounddfdi (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtad_s32_f64 (float64_t __a)
{
  return __builtin_aarch64_lrounddfsi (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtad_u64_f64 (float64_t __a)
{
  return __builtin_aarch64_lroundudfdi_us (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtad_u32_f64 (float64_t __a)
{
  return __builtin_aarch64_lroundudfsi_us (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtas_s32_f32 (float32_t __a)
{
  return __builtin_aarch64_lroundsfsi (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtas_s64_f32 (float32_t __a)
{
  return __builtin_aarch64_lroundsfdi (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtas_u32_f32 (float32_t __a)
{
  return __builtin_aarch64_lroundusfsi_us (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtas_u64_f32 (float32_t __a)
{
  return __builtin_aarch64_lroundusfdi_us (__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvta_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lroundv2sfv2si (__a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvta_u32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lrounduv2sfv2si_us (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtaq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lroundv4sfv4si (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtaq_u32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lrounduv4sfv4si_us (__a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvta_s64_f64 (float64x1_t __a)
{
  return (int64x1_t) {vcvtad_s64_f64 (__a[0])};
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvta_u64_f64 (float64x1_t __a)
{
  return (uint64x1_t) {vcvtad_u64_f64 (__a[0])};
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtaq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lroundv2dfv2di (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtaq_u64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lrounduv2dfv2di_us (__a);
}

/* vcvtm  */

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmd_s64_f64 (float64_t __a)
{
  return __builtin_llfloor (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmd_s32_f64 (float64_t __a)
{
  return __builtin_aarch64_lfloordfsi (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmd_u64_f64 (float64_t __a)
{
  return __builtin_aarch64_lfloorudfdi_us (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmd_u32_f64 (float64_t __a)
{
  return __builtin_aarch64_lfloorudfsi_us (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtms_s32_f32 (float32_t __a)
{
  return __builtin_ifloorf (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtms_s64_f32 (float32_t __a)
{
  return __builtin_aarch64_lfloorsfdi (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtms_u32_f32 (float32_t __a)
{
  return __builtin_aarch64_lfloorusfsi_us (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtms_u64_f32 (float32_t __a)
{
  return __builtin_aarch64_lfloorusfdi_us (__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtm_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lfloorv2sfv2si (__a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtm_u32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lflooruv2sfv2si_us (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lfloorv4sfv4si (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmq_u32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lflooruv4sfv4si_us (__a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtm_s64_f64 (float64x1_t __a)
{
  return (int64x1_t) {vcvtmd_s64_f64 (__a[0])};
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtm_u64_f64 (float64x1_t __a)
{
  return (uint64x1_t) {vcvtmd_u64_f64 (__a[0])};
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lfloorv2dfv2di (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmq_u64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lflooruv2dfv2di_us (__a);
}

/* vcvtn  */

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnd_s64_f64 (float64_t __a)
{
  return __builtin_aarch64_lfrintndfdi (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnd_s32_f64 (float64_t __a)
{
  return __builtin_aarch64_lfrintndfsi (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnd_u64_f64 (float64_t __a)
{
  return __builtin_aarch64_lfrintnudfdi_us (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnd_u32_f64 (float64_t __a)
{
  return __builtin_aarch64_lfrintnudfsi_us (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtns_s32_f32 (float32_t __a)
{
  return __builtin_aarch64_lfrintnsfsi (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtns_s64_f32 (float32_t __a)
{
  return __builtin_aarch64_lfrintnsfdi (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtns_u32_f32 (float32_t __a)
{
  return __builtin_aarch64_lfrintnusfsi_us (__a);
}
__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtns_u64_f32 (float32_t __a)
{
  return __builtin_aarch64_lfrintnusfdi_us (__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtn_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lfrintnv2sfv2si (__a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtn_u32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lfrintnuv2sfv2si_us (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lfrintnv4sfv4si (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnq_u32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lfrintnuv4sfv4si_us (__a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtn_s64_f64 (float64x1_t __a)
{
  return (int64x1_t) {vcvtnd_s64_f64 (__a[0])};
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtn_u64_f64 (float64x1_t __a)
{
  return (uint64x1_t) {vcvtnd_u64_f64 (__a[0])};
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lfrintnv2dfv2di (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnq_u64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lfrintnuv2dfv2di_us (__a);
}

/* vcvtp  */

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpd_s64_f64 (float64_t __a)
{
  return __builtin_llceil (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpd_s32_f64 (float64_t __a)
{
  return __builtin_aarch64_lceildfsi (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpd_u64_f64 (float64_t __a)
{
  return __builtin_aarch64_lceiludfdi_us (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpd_u32_f64 (float64_t __a)
{
  return __builtin_aarch64_lceiludfsi_us (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtps_s32_f32 (float32_t __a)
{
  return __builtin_iceilf (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtps_s64_f32 (float32_t __a)
{
  return __builtin_aarch64_lceilsfdi (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtps_u32_f32 (float32_t __a)
{
  return __builtin_aarch64_lceilusfsi_us (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtps_u64_f32 (float32_t __a)
{
  return __builtin_aarch64_lceilusfdi_us (__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtp_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lceilv2sfv2si (__a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtp_u32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lceiluv2sfv2si_us (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lceilv4sfv4si (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpq_u32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lceiluv4sfv4si_us (__a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtp_s64_f64 (float64x1_t __a)
{
  return (int64x1_t) {vcvtpd_s64_f64 (__a[0])};
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtp_u64_f64 (float64x1_t __a)
{
  return (uint64x1_t) {vcvtpd_u64_f64 (__a[0])};
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lceilv2dfv2di (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpq_u64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lceiluv2dfv2di_us (__a);
}

/* vld1 */

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f16 (const float16_t *__a)
{
  return __builtin_aarch64_ld1v4hf (__a);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f32 (const float32_t *__a)
{
  return __builtin_aarch64_ld1v2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f64 (const float64_t *__a)
{
  return (float64x1_t) {*__a};
}

__extension__ extern __inline poly8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p8 (const poly8_t *__a)
{
  return __builtin_aarch64_ld1v8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p16 (const poly16_t *__a)
{
  return __builtin_aarch64_ld1v4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p64 (const poly64_t *__a)
{
  return (poly64x1_t) {*__a};
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s8 (const int8_t *__a)
{
  return __builtin_aarch64_ld1v8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s16 (const int16_t *__a)
{
  return __builtin_aarch64_ld1v4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s32 (const int32_t *__a)
{
  return __builtin_aarch64_ld1v2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s64 (const int64_t *__a)
{
  return (int64x1_t) {*__a};
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u8 (const uint8_t *__a)
{
  return __builtin_aarch64_ld1v8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u16 (const uint16_t *__a)
{
  return __builtin_aarch64_ld1v4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u32 (const uint32_t *__a)
{
  return __builtin_aarch64_ld1v2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u64 (const uint64_t *__a)
{
  return (uint64x1_t) {*__a};
}

/* vld1x3  */

__extension__ extern __inline uint8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u8_x3 (const uint8_t *__a)
{
  return __builtin_aarch64_ld1x3v8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s8_x3 (const int8_t *__a)
{
  return __builtin_aarch64_ld1x3v8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u16_x3 (const uint16_t *__a)
{
  return __builtin_aarch64_ld1x3v4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s16_x3 (const int16_t *__a)
{
  return __builtin_aarch64_ld1x3v4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u32_x3 (const uint32_t *__a)
{
  return __builtin_aarch64_ld1x3v2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s32_x3 (const int32_t *__a)
{
  return __builtin_aarch64_ld1x3v2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u64_x3 (const uint64_t *__a)
{
  return __builtin_aarch64_ld1x3di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s64_x3 (const int64_t *__a)
{
  return __builtin_aarch64_ld1x3di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f16_x3 (const float16_t *__a)
{
  return __builtin_aarch64_ld1x3v4hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f32_x3 (const float32_t *__a)
{
  return __builtin_aarch64_ld1x3v2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f64_x3 (const float64_t *__a)
{
  return __builtin_aarch64_ld1x3df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p8_x3 (const poly8_t *__a)
{
  return __builtin_aarch64_ld1x3v8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p16_x3 (const poly16_t *__a)
{
  return __builtin_aarch64_ld1x3v4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p64_x3 (const poly64_t *__a)
{
  return __builtin_aarch64_ld1x3di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u8_x3 (const uint8_t *__a)
{
  return __builtin_aarch64_ld1x3v16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s8_x3 (const int8_t *__a)
{
  return __builtin_aarch64_ld1x3v16qi (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u16_x3 (const uint16_t *__a)
{
  return __builtin_aarch64_ld1x3v8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s16_x3 (const int16_t *__a)
{
  return __builtin_aarch64_ld1x3v8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u32_x3 (const uint32_t *__a)
{
  return __builtin_aarch64_ld1x3v4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s32_x3 (const int32_t *__a)
{
  return __builtin_aarch64_ld1x3v4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u64_x3 (const uint64_t *__a)
{
  return __builtin_aarch64_ld1x3v2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s64_x3 (const int64_t *__a)
{
  return __builtin_aarch64_ld1x3v2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f16_x3 (const float16_t *__a)
{
  return __builtin_aarch64_ld1x3v8hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f32_x3 (const float32_t *__a)
{
  return __builtin_aarch64_ld1x3v4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f64_x3 (const float64_t *__a)
{
  return __builtin_aarch64_ld1x3v2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p8_x3 (const poly8_t *__a)
{
  return __builtin_aarch64_ld1x3v16qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p16_x3 (const poly16_t *__a)
{
  return __builtin_aarch64_ld1x3v8hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p64_x3 (const poly64_t *__a)
{
  return __builtin_aarch64_ld1x3v2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

/* vld1q */

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f16 (const float16_t *__a)
{
  return __builtin_aarch64_ld1v8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f32 (const float32_t *__a)
{
  return __builtin_aarch64_ld1v4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f64 (const float64_t *__a)
{
  return __builtin_aarch64_ld1v2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p8 (const poly8_t *__a)
{
  return __builtin_aarch64_ld1v16qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p16 (const poly16_t *__a)
{
  return __builtin_aarch64_ld1v8hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p64 (const poly64_t *__a)
{
  return __builtin_aarch64_ld1v2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s8 (const int8_t *__a)
{
  return __builtin_aarch64_ld1v16qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s16 (const int16_t *__a)
{
  return __builtin_aarch64_ld1v8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s32 (const int32_t *__a)
{
  return __builtin_aarch64_ld1v4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s64 (const int64_t *__a)
{
  return __builtin_aarch64_ld1v2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u8 (const uint8_t *__a)
{
  return __builtin_aarch64_ld1v16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u8_x2 (const uint8_t *__a)
{
  return __builtin_aarch64_ld1x2v8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s8_x2 (const int8_t *__a)
{
  return __builtin_aarch64_ld1x2v8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u16_x2 (const uint16_t *__a)
{
  return __builtin_aarch64_ld1x2v4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s16_x2 (const int16_t *__a)
{
  return __builtin_aarch64_ld1x2v4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u32_x2 (const uint32_t *__a)
{
  return __builtin_aarch64_ld1x2v2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s32_x2 (const int32_t *__a)
{
  return __builtin_aarch64_ld1x2v2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u64_x2 (const uint64_t *__a)
{
  return __builtin_aarch64_ld1x2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s64_x2 (const int64_t *__a)
{
  return __builtin_aarch64_ld1x2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f16_x2 (const float16_t *__a)
{
  return __builtin_aarch64_ld1x2v4hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f32_x2 (const float32_t *__a)
{
  return __builtin_aarch64_ld1x2v2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f64_x2 (const float64_t *__a)
{
  return __builtin_aarch64_ld1x2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p8_x2 (const poly8_t *__a)
{
  return __builtin_aarch64_ld1x2v8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p16_x2 (const poly16_t *__a)
{
  return __builtin_aarch64_ld1x2v4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p64_x2 (const poly64_t *__a)
{
  return __builtin_aarch64_ld1x2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u8_x2 (const uint8_t *__a)
{
  return __builtin_aarch64_ld1x2v16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s8_x2 (const int8_t *__a)
{
  return __builtin_aarch64_ld1x2v16qi (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u16_x2 (const uint16_t *__a)
{
  return __builtin_aarch64_ld1x2v8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s16_x2 (const int16_t *__a)
{
  return __builtin_aarch64_ld1x2v8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u32_x2 (const uint32_t *__a)
{
  return __builtin_aarch64_ld1x2v4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s32_x2 (const int32_t *__a)
{
  return __builtin_aarch64_ld1x2v4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u64_x2 (const uint64_t *__a)
{
  return __builtin_aarch64_ld1x2v2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s64_x2 (const int64_t *__a)
{
  return __builtin_aarch64_ld1x2v2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f16_x2 (const float16_t *__a)
{
  return __builtin_aarch64_ld1x2v8hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f32_x2 (const float32_t *__a)
{
  return __builtin_aarch64_ld1x2v4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f64_x2 (const float64_t *__a)
{
  return __builtin_aarch64_ld1x2v2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p8_x2 (const poly8_t *__a)
{
  return __builtin_aarch64_ld1x2v16qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p16_x2 (const poly16_t *__a)
{
  return __builtin_aarch64_ld1x2v8hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p64_x2 (const poly64_t *__a)
{
  return __builtin_aarch64_ld1x2v2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u16 (const uint16_t *__a)
{
  return __builtin_aarch64_ld1v8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u32 (const uint32_t *__a)
{
  return __builtin_aarch64_ld1v4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u64 (const uint64_t *__a)
{
  return __builtin_aarch64_ld1v2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

/* vld1(q)_x4.  */

__extension__ extern __inline int8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s8_x4 (const int8_t *__a)
{
  return __builtin_aarch64_ld1x4v8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s8_x4 (const int8_t *__a)
{
  return __builtin_aarch64_ld1x4v16qi (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s16_x4 (const int16_t *__a)
{
  return __builtin_aarch64_ld1x4v4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s16_x4 (const int16_t *__a)
{
  return __builtin_aarch64_ld1x4v8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s32_x4 (const int32_t *__a)
{
  return __builtin_aarch64_ld1x4v2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s32_x4 (const int32_t *__a)
{
  return __builtin_aarch64_ld1x4v4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u8_x4 (const uint8_t *__a)
{
  return __builtin_aarch64_ld1x4v8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u8_x4 (const uint8_t *__a)
{
  return __builtin_aarch64_ld1x4v16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u16_x4 (const uint16_t *__a)
{
  return __builtin_aarch64_ld1x4v4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u16_x4 (const uint16_t *__a)
{
  return __builtin_aarch64_ld1x4v8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u32_x4 (const uint32_t *__a)
{
  return __builtin_aarch64_ld1x4v2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u32_x4 (const uint32_t *__a)
{
  return __builtin_aarch64_ld1x4v4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline float16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f16_x4 (const float16_t *__a)
{
  return __builtin_aarch64_ld1x4v4hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f16_x4 (const float16_t *__a)
{
  return __builtin_aarch64_ld1x4v8hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f32_x4 (const float32_t *__a)
{
  return __builtin_aarch64_ld1x4v2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f32_x4 (const float32_t *__a)
{
  return __builtin_aarch64_ld1x4v4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline poly8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p8_x4 (const poly8_t *__a)
{
  return __builtin_aarch64_ld1x4v8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p8_x4 (const poly8_t *__a)
{
  return __builtin_aarch64_ld1x4v16qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p16_x4 (const poly16_t *__a)
{
  return __builtin_aarch64_ld1x4v4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p16_x4 (const poly16_t *__a)
{
  return __builtin_aarch64_ld1x4v8hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_s64_x4 (const int64_t *__a)
{
  return __builtin_aarch64_ld1x4di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_u64_x4 (const uint64_t *__a)
{
  return __builtin_aarch64_ld1x4di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline poly64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_p64_x4 (const poly64_t *__a)
{
  return __builtin_aarch64_ld1x4di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_s64_x4 (const int64_t *__a)
{
  return __builtin_aarch64_ld1x4v2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_u64_x4 (const uint64_t *__a)
{
  return __builtin_aarch64_ld1x4v2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline poly64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_p64_x4 (const poly64_t *__a)
{
  return __builtin_aarch64_ld1x4v2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_f64_x4 (const float64_t *__a)
{
  return __builtin_aarch64_ld1x4df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline float64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_f64_x4 (const float64_t *__a)
{
  return __builtin_aarch64_ld1x4v2df ((const __builtin_aarch64_simd_df *) __a);
}

/* vld1_dup  */

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_f16 (const float16_t* __a)
{
  return vdup_n_f16 (*__a);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_f32 (const float32_t* __a)
{
  return vdup_n_f32 (*__a);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_f64 (const float64_t* __a)
{
  return vdup_n_f64 (*__a);
}

__extension__ extern __inline poly8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_p8 (const poly8_t* __a)
{
  return vdup_n_p8 (*__a);
}

__extension__ extern __inline poly16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_p16 (const poly16_t* __a)
{
  return vdup_n_p16 (*__a);
}

__extension__ extern __inline poly64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_p64 (const poly64_t* __a)
{
  return vdup_n_p64 (*__a);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_s8 (const int8_t* __a)
{
  return vdup_n_s8 (*__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_s16 (const int16_t* __a)
{
  return vdup_n_s16 (*__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_s32 (const int32_t* __a)
{
  return vdup_n_s32 (*__a);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_s64 (const int64_t* __a)
{
  return vdup_n_s64 (*__a);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_u8 (const uint8_t* __a)
{
  return vdup_n_u8 (*__a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_u16 (const uint16_t* __a)
{
  return vdup_n_u16 (*__a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_u32 (const uint32_t* __a)
{
  return vdup_n_u32 (*__a);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_u64 (const uint64_t* __a)
{
  return vdup_n_u64 (*__a);
}

/* vld1q_dup  */

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_f16 (const float16_t* __a)
{
  return vdupq_n_f16 (*__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_f32 (const float32_t* __a)
{
  return vdupq_n_f32 (*__a);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_f64 (const float64_t* __a)
{
  return vdupq_n_f64 (*__a);
}

__extension__ extern __inline poly8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_p8 (const poly8_t* __a)
{
  return vdupq_n_p8 (*__a);
}

__extension__ extern __inline poly16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_p16 (const poly16_t* __a)
{
  return vdupq_n_p16 (*__a);
}

__extension__ extern __inline poly64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_p64 (const poly64_t* __a)
{
  return vdupq_n_p64 (*__a);
}

 __extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_s8 (const int8_t* __a)
{
  return vdupq_n_s8 (*__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_s16 (const int16_t* __a)
{
  return vdupq_n_s16 (*__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_s32 (const int32_t* __a)
{
  return vdupq_n_s32 (*__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_s64 (const int64_t* __a)
{
  return vdupq_n_s64 (*__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_u8 (const uint8_t* __a)
{
  return vdupq_n_u8 (*__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_u16 (const uint16_t* __a)
{
  return vdupq_n_u16 (*__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_u32 (const uint32_t* __a)
{
  return vdupq_n_u32 (*__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_u64 (const uint64_t* __a)
{
  return vdupq_n_u64 (*__a);
}

/* vld1_lane  */

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_f16 (const float16_t *__src, float16x4_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_f32 (const float32_t *__src, float32x2_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_f64 (const float64_t *__src, float64x1_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline poly8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_p8 (const poly8_t *__src, poly8x8_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline poly16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_p16 (const poly16_t *__src, poly16x4_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline poly64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_p64 (const poly64_t *__src, poly64x1_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_s8 (const int8_t *__src, int8x8_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_s16 (const int16_t *__src, int16x4_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_s32 (const int32_t *__src, int32x2_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_s64 (const int64_t *__src, int64x1_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_u8 (const uint8_t *__src, uint8x8_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_u16 (const uint16_t *__src, uint16x4_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_u32 (const uint32_t *__src, uint32x2_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_u64 (const uint64_t *__src, uint64x1_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

/* vld1q_lane  */

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_f16 (const float16_t *__src, float16x8_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_f32 (const float32_t *__src, float32x4_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_f64 (const float64_t *__src, float64x2_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline poly8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_p8 (const poly8_t *__src, poly8x16_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline poly16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_p16 (const poly16_t *__src, poly16x8_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline poly64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_p64 (const poly64_t *__src, poly64x2_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_s8 (const int8_t *__src, int8x16_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_s16 (const int16_t *__src, int16x8_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_s32 (const int32_t *__src, int32x4_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_s64 (const int64_t *__src, int64x2_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_u8 (const uint8_t *__src, uint8x16_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_u16 (const uint16_t *__src, uint16x8_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_u32 (const uint32_t *__src, uint32x4_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_u64 (const uint64_t *__src, uint64x2_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

#pragma GCC push_options
#pragma GCC target ("+nothing+rcpc3+simd")

/* vldap1_lane.  */

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vldap1_lane_u64 (const uint64_t *__src, uint64x1_t __vec, const int __lane)
{
  return __builtin_aarch64_vec_ldap1_lanev1di_usus (
	  (const __builtin_aarch64_simd_di *) __src, __vec, __lane);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vldap1q_lane_u64 (const uint64_t *__src, uint64x2_t __vec, const int __lane)
{
  return __builtin_aarch64_vec_ldap1_lanev2di_usus (
	  (const __builtin_aarch64_simd_di *) __src, __vec, __lane);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vldap1_lane_s64 (const int64_t *__src, int64x1_t __vec, const int __lane)
{
  return __builtin_aarch64_vec_ldap1_lanev1di (
	  (const __builtin_aarch64_simd_di *) __src, __vec, __lane);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vldap1q_lane_s64 (const int64_t *__src, int64x2_t __vec, const int __lane)
{
  return __builtin_aarch64_vec_ldap1_lanev2di (
	  (const __builtin_aarch64_simd_di *) __src, __vec, __lane);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vldap1_lane_f64 (const float64_t *__src, float64x1_t __vec, const int __lane)
{
  return __builtin_aarch64_vec_ldap1_lanev1df (
	  (const __builtin_aarch64_simd_df *) __src, __vec, __lane);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vldap1q_lane_f64 (const float64_t *__src, float64x2_t __vec, const int __lane)
{
  return __builtin_aarch64_vec_ldap1_lanev2df (
	  (const __builtin_aarch64_simd_df *) __src, __vec, __lane);
}

__extension__ extern __inline poly64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vldap1_lane_p64 (const poly64_t *__src, poly64x1_t __vec, const int __lane)
{
  return __builtin_aarch64_vec_ldap1_lanev1di_psps (
	  (const __builtin_aarch64_simd_di *) __src, __vec, __lane);
}

__extension__ extern __inline poly64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vldap1q_lane_p64 (const poly64_t *__src, poly64x2_t __vec, const int __lane)
{
  return __builtin_aarch64_vec_ldap1_lanev2di_psps (
	  (const __builtin_aarch64_simd_di *) __src, __vec, __lane);
}

/* vstl1_lane.  */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vstl1_lane_u64 (uint64_t *__src, uint64x1_t __vec, const int __lane)
{
  __builtin_aarch64_vec_stl1_lanev1di_sus ((__builtin_aarch64_simd_di *) __src,
					   __vec, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vstl1q_lane_u64 (uint64_t *__src, uint64x2_t __vec, const int __lane)
{
  __builtin_aarch64_vec_stl1_lanev2di_sus ((__builtin_aarch64_simd_di *) __src,
					   __vec, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vstl1_lane_s64 (int64_t *__src, int64x1_t __vec, const int __lane)
{
  __builtin_aarch64_vec_stl1_lanev1di ((__builtin_aarch64_simd_di *) __src,
				       __vec, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vstl1q_lane_s64 (int64_t *__src, int64x2_t __vec, const int __lane)
{
  __builtin_aarch64_vec_stl1_lanev2di ((__builtin_aarch64_simd_di *) __src,
				       __vec, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vstl1_lane_f64 (float64_t *__src, float64x1_t __vec, const int __lane)
{
  __builtin_aarch64_vec_stl1_lanev1df ((__builtin_aarch64_simd_df *) __src,
				       __vec, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vstl1q_lane_f64 (float64_t *__src, float64x2_t __vec, const int __lane)
{
  __builtin_aarch64_vec_stl1_lanev2df ((__builtin_aarch64_simd_df *) __src,
				       __vec, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vstl1_lane_p64 (poly64_t *__src, poly64x1_t __vec, const int __lane)
{
  __builtin_aarch64_vec_stl1_lanev1di_sps ((__builtin_aarch64_simd_di *) __src,
					   __vec, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vstl1q_lane_p64 (poly64_t *__src, poly64x2_t __vec, const int __lane)
{
  __builtin_aarch64_vec_stl1_lanev2di_sps ((__builtin_aarch64_simd_di *) __src,
					   __vec, __lane);
}

#pragma GCC pop_options

/* vldn */

__extension__ extern __inline int64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld2di_us ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline int8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld2v8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld2v8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld2di_ps ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld2v4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld2v4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld2v2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld2v8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld2v4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld2v2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline float16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld2v4hf (__a);
}

__extension__ extern __inline float32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld2v2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline int8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld2v16qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld2v16qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld2v8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld2v8hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld2v2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld2v4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld2v2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld2v16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld2v8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld2v4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld2v2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld2v8hf (__a);
}

__extension__ extern __inline float32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld2v4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld2v2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline int64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld3di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld3di_us ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld3df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline int8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld3v8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld3v8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld3v4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld3v4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld3v2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld3v8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld3v4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld3v2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline float16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld3v4hf (__a);
}

__extension__ extern __inline float32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld3v2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline poly64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld3di_ps ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld3v16qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld3v16qi_ps ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld3v8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld3v8hi_ps ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld3v4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld3v2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld3v16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld3v8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld3v4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld3v2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld3v8hf (__a);
}

__extension__ extern __inline float32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld3v4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld3v2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld3v2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld4di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld4di_us ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld4df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline int8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld4v8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld4v8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld4v4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld4v4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld4v2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld4v8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld4v4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld4v2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline float16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld4v4hf (__a);
}

__extension__ extern __inline float32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld4v2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline poly64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld4di_ps ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld4v16qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld4v16qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld4v8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld4v8hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld4v4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld4v2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld4v16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld4v8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld4v4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld4v2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld4v8hf (__a);
}

__extension__ extern __inline float32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld4v4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld4v2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld4v2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline poly128_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vldrq_p128 (const poly128_t * __ptr)
{
  return *__ptr;
}

/* vldn_dup */

__extension__ extern __inline int8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld2rv8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld2rv4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld2rv2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline float16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld2rv4hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld2rv2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld2rdf ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline uint8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld2rv8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld2rv4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld2rv2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline poly8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld2rv8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld2rv4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld2rdi_ps ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld2rdi ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld2rdi_us ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld2rv16qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld2rv16qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld2rv8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld2rv8hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld2rv4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld2rv2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld2rv16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld2rv8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld2rv4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld2rv2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld2rv8hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld2rv4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld2rv2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld2rv2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld3rdi ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld3rdi_us ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld3rdf ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline int8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld3rv8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld3rv8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld3rv4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld3rv4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld3rv2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld3rv8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld3rv4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld3rv2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline float16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld3rv4hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld3rv2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline poly64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld3rdi_ps ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld3rv16qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld3rv16qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld3rv8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld3rv8hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld3rv4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld3rv2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld3rv16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld3rv8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld3rv4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld3rv2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld3rv8hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld3rv4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld3rv2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld3rv2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld4rdi ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld4rdi_us ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld4rdf ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline int8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld4rv8qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld4rv8qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld4rv4hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld4rv4hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld4rv2si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld4rv8qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld4rv4hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld4rv2si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline float16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld4rv4hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld4rv2sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline poly64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld4rdi_ps ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline int8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_s8 (const int8_t * __a)
{
  return __builtin_aarch64_ld4rv16qi ((const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline poly8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_p8 (const poly8_t * __a)
{
  return __builtin_aarch64_ld4rv16qi_ps (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline int16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_s16 (const int16_t * __a)
{
  return __builtin_aarch64_ld4rv8hi ((const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline poly16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_p16 (const poly16_t * __a)
{
  return __builtin_aarch64_ld4rv8hi_ps (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline int32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_s32 (const int32_t * __a)
{
  return __builtin_aarch64_ld4rv4si ((const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline int64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_s64 (const int64_t * __a)
{
  return __builtin_aarch64_ld4rv2di ((const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline uint8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_u8 (const uint8_t * __a)
{
  return __builtin_aarch64_ld4rv16qi_us (
				(const __builtin_aarch64_simd_qi *) __a);
}

__extension__ extern __inline uint16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_u16 (const uint16_t * __a)
{
  return __builtin_aarch64_ld4rv8hi_us (
				(const __builtin_aarch64_simd_hi *) __a);
}

__extension__ extern __inline uint32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_u32 (const uint32_t * __a)
{
  return __builtin_aarch64_ld4rv4si_us (
				(const __builtin_aarch64_simd_si *) __a);
}

__extension__ extern __inline uint64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_u64 (const uint64_t * __a)
{
  return __builtin_aarch64_ld4rv2di_us (
				(const __builtin_aarch64_simd_di *) __a);
}

__extension__ extern __inline float16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_f16 (const float16_t * __a)
{
  return __builtin_aarch64_ld4rv8hf ((const __builtin_aarch64_simd_hf *) __a);
}

__extension__ extern __inline float32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_f32 (const float32_t * __a)
{
  return __builtin_aarch64_ld4rv4sf ((const __builtin_aarch64_simd_sf *) __a);
}

__extension__ extern __inline float64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_f64 (const float64_t * __a)
{
  return __builtin_aarch64_ld4rv2df ((const __builtin_aarch64_simd_df *) __a);
}

__extension__ extern __inline poly64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_p64 (const poly64_t * __a)
{
  return __builtin_aarch64_ld4rv2di_ps (
				(const __builtin_aarch64_simd_di *) __a);
}

/* vld2_lane */

__extension__ extern __inline uint8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_u8 (const uint8_t * __ptr, uint8x8x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev8qi_usus (
	  (__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline uint16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_u16 (const uint16_t * __ptr, uint16x4x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev4hi_usus (
	  (__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline uint32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_u32 (const uint32_t * __ptr, uint32x2x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev2si_usus (
	  (__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline uint64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_u64 (const uint64_t * __ptr, uint64x1x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanedi_usus (
	  (__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline int8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_s8 (const int8_t * __ptr, int8x8x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev8qi (
	  (__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline int16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_s16 (const int16_t * __ptr, int16x4x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev4hi (
	  (__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline int32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_s32 (const int32_t * __ptr, int32x2x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev2si (
	  (__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline int64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_s64 (const int64_t * __ptr, int64x1x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanedi (
	  (__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline float16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_f16 (const float16_t * __ptr, float16x4x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev4hf (
	  (__builtin_aarch64_simd_hf *) __ptr, __b, __c);
}

__extension__ extern __inline float32x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_f32 (const float32_t * __ptr, float32x2x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev2sf (
	  (__builtin_aarch64_simd_sf *) __ptr, __b, __c);
}

__extension__ extern __inline float64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_f64 (const float64_t * __ptr, float64x1x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanedf (
	  (__builtin_aarch64_simd_df *) __ptr, __b, __c);
}

__extension__ extern __inline poly8x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_p8 (const poly8_t * __ptr, poly8x8x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev8qi_psps (
	  (__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline poly16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_p16 (const poly16_t * __ptr, poly16x4x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev4hi_psps (
	  (__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline poly64x1x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_p64 (const poly64_t * __ptr, poly64x1x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanedi_psps (
	  (__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

/* vld2q_lane */

__extension__ extern __inline uint8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_u8 (const uint8_t * __ptr, uint8x16x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev16qi_usus (
	(__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline uint16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_u16 (const uint16_t * __ptr, uint16x8x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev8hi_usus (
	(__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline uint32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_u32 (const uint32_t * __ptr, uint32x4x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev4si_usus (
	(__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline uint64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_u64 (const uint64_t * __ptr, uint64x2x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev2di_usus (
	(__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline int8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_s8 (const int8_t * __ptr, int8x16x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev16qi (
	(__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline int16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_s16 (const int16_t * __ptr, int16x8x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev8hi (
	(__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline int32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_s32 (const int32_t * __ptr, int32x4x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev4si (
	(__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline int64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_s64 (const int64_t * __ptr, int64x2x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev2di (
	(__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline float16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_f16 (const float16_t * __ptr, float16x8x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev8hf (
	(__builtin_aarch64_simd_hf *) __ptr, __b, __c);
}

__extension__ extern __inline float32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_f32 (const float32_t * __ptr, float32x4x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev4sf (
	(__builtin_aarch64_simd_sf *) __ptr, __b, __c);
}

__extension__ extern __inline float64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_f64 (const float64_t * __ptr, float64x2x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev2df (
	(__builtin_aarch64_simd_df *) __ptr, __b, __c);
}

__extension__ extern __inline poly8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_p8 (const poly8_t * __ptr, poly8x16x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev16qi_psps (
	(__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline poly16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_p16 (const poly16_t * __ptr, poly16x8x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev8hi_psps (
	(__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline poly64x2x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_p64 (const poly64_t * __ptr, poly64x2x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev2di_psps (
	(__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

/* vld3_lane */

__extension__ extern __inline uint8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_u8 (const uint8_t * __ptr, uint8x8x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev8qi_usus (
	  (__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline uint16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_u16 (const uint16_t * __ptr, uint16x4x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev4hi_usus (
	  (__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline uint32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_u32 (const uint32_t * __ptr, uint32x2x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev2si_usus (
	  (__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline uint64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_u64 (const uint64_t * __ptr, uint64x1x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanedi_usus (
	  (__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline int8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_s8 (const int8_t * __ptr, int8x8x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev8qi (
	  (__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline int16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_s16 (const int16_t * __ptr, int16x4x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev4hi (
	  (__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline int32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_s32 (const int32_t * __ptr, int32x2x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev2si (
	  (__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline int64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_s64 (const int64_t * __ptr, int64x1x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanedi (
	  (__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline float16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_f16 (const float16_t * __ptr, float16x4x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev4hf (
	  (__builtin_aarch64_simd_hf *) __ptr, __b, __c);
}

__extension__ extern __inline float32x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_f32 (const float32_t * __ptr, float32x2x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev2sf (
	  (__builtin_aarch64_simd_sf *) __ptr, __b, __c);
}

__extension__ extern __inline float64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_f64 (const float64_t * __ptr, float64x1x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanedf (
	  (__builtin_aarch64_simd_df *) __ptr, __b, __c);
}

__extension__ extern __inline poly8x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_p8 (const poly8_t * __ptr, poly8x8x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev8qi_psps (
	  (__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline poly16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_p16 (const poly16_t * __ptr, poly16x4x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev4hi_psps (
	  (__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline poly64x1x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_p64 (const poly64_t * __ptr, poly64x1x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanedi_psps (
	  (__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

/* vld3q_lane */

__extension__ extern __inline uint8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_u8 (const uint8_t * __ptr, uint8x16x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev16qi_usus (
	(__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline uint16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_u16 (const uint16_t * __ptr, uint16x8x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev8hi_usus (
	(__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline uint32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_u32 (const uint32_t * __ptr, uint32x4x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev4si_usus (
	(__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline uint64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_u64 (const uint64_t * __ptr, uint64x2x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev2di_usus (
	(__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline int8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_s8 (const int8_t * __ptr, int8x16x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev16qi (
	(__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline int16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_s16 (const int16_t * __ptr, int16x8x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev8hi (
	(__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline int32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_s32 (const int32_t * __ptr, int32x4x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev4si (
	(__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline int64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_s64 (const int64_t * __ptr, int64x2x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev2di (
	(__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline float16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_f16 (const float16_t * __ptr, float16x8x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev8hf (
	(__builtin_aarch64_simd_hf *) __ptr, __b, __c);
}

__extension__ extern __inline float32x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_f32 (const float32_t * __ptr, float32x4x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev4sf (
	(__builtin_aarch64_simd_sf *) __ptr, __b, __c);
}

__extension__ extern __inline float64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_f64 (const float64_t * __ptr, float64x2x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev2df (
	(__builtin_aarch64_simd_df *) __ptr, __b, __c);
}

__extension__ extern __inline poly8x16x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_p8 (const poly8_t * __ptr, poly8x16x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev16qi_psps (
	(__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline poly16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_p16 (const poly16_t * __ptr, poly16x8x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev8hi_psps (
	(__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline poly64x2x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_p64 (const poly64_t * __ptr, poly64x2x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev2di_psps (
	(__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

/* vld4_lane */

__extension__ extern __inline uint8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_u8 (const uint8_t * __ptr, uint8x8x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev8qi_usus (
	  (__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline uint16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_u16 (const uint16_t * __ptr, uint16x4x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev4hi_usus (
	  (__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline uint32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_u32 (const uint32_t * __ptr, uint32x2x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev2si_usus (
	  (__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline uint64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_u64 (const uint64_t * __ptr, uint64x1x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanedi_usus (
	  (__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline int8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_s8 (const int8_t * __ptr, int8x8x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev8qi (
	  (__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline int16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_s16 (const int16_t * __ptr, int16x4x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev4hi (
	  (__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline int32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_s32 (const int32_t * __ptr, int32x2x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev2si (
	  (__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline int64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_s64 (const int64_t * __ptr, int64x1x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanedi (
	  (__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline float16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_f16 (const float16_t * __ptr, float16x4x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev4hf (
	  (__builtin_aarch64_simd_hf *) __ptr, __b, __c);
}

__extension__ extern __inline float32x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_f32 (const float32_t * __ptr, float32x2x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev2sf (
	  (__builtin_aarch64_simd_sf *) __ptr, __b, __c);
}

__extension__ extern __inline float64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_f64 (const float64_t * __ptr, float64x1x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanedf (
	  (__builtin_aarch64_simd_df *) __ptr, __b, __c);
}

__extension__ extern __inline poly8x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_p8 (const poly8_t * __ptr, poly8x8x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev8qi_psps (
	  (__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline poly16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_p16 (const poly16_t * __ptr, poly16x4x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev4hi_psps (
	  (__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline poly64x1x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_p64 (const poly64_t * __ptr, poly64x1x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanedi_psps (
	  (__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

/* vld4q_lane */

__extension__ extern __inline uint8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_u8 (const uint8_t * __ptr, uint8x16x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev16qi_usus (
	(__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline uint16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_u16 (const uint16_t * __ptr, uint16x8x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev8hi_usus (
	(__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline uint32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_u32 (const uint32_t * __ptr, uint32x4x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev4si_usus (
	(__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline uint64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_u64 (const uint64_t * __ptr, uint64x2x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev2di_usus (
	(__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline int8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_s8 (const int8_t * __ptr, int8x16x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev16qi (
	(__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline int16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_s16 (const int16_t * __ptr, int16x8x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev8hi (
	(__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline int32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_s32 (const int32_t * __ptr, int32x4x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev4si (
	(__builtin_aarch64_simd_si *) __ptr, __b, __c);
}

__extension__ extern __inline int64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_s64 (const int64_t * __ptr, int64x2x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev2di (
	(__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

__extension__ extern __inline float16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_f16 (const float16_t * __ptr, float16x8x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev8hf (
	(__builtin_aarch64_simd_hf *) __ptr, __b, __c);
}

__extension__ extern __inline float32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_f32 (const float32_t * __ptr, float32x4x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev4sf (
	(__builtin_aarch64_simd_sf *) __ptr, __b, __c);
}

__extension__ extern __inline float64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_f64 (const float64_t * __ptr, float64x2x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev2df (
	(__builtin_aarch64_simd_df *) __ptr, __b, __c);
}

__extension__ extern __inline poly8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_p8 (const poly8_t * __ptr, poly8x16x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev16qi_psps (
	(__builtin_aarch64_simd_qi *) __ptr, __b, __c);
}

__extension__ extern __inline poly16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_p16 (const poly16_t * __ptr, poly16x8x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev8hi_psps (
	(__builtin_aarch64_simd_hi *) __ptr, __b, __c);
}

__extension__ extern __inline poly64x2x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_p64 (const poly64_t * __ptr, poly64x2x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev2di_psps (
	(__builtin_aarch64_simd_di *) __ptr, __b, __c);
}

/* vmulx */

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_fmulxv2sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_fmulxv4sf (__a, __b);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_f64 (float64x1_t __a, float64x1_t __b)
{
  return (float64x1_t) {__builtin_aarch64_fmulxdf (__a[0], __b[0])};
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_fmulxv2df (__a, __b);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxs_f32 (float32_t __a, float32_t __b)
{
  return __builtin_aarch64_fmulxsf (__a, __b);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxd_f64 (float64_t __a, float64_t __b)
{
  return __builtin_aarch64_fmulxdf (__a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_lane_f32 (float32x2_t __a, float32x2_t __v, const int __lane)
{
  return vmulx_f32 (__a, __aarch64_vdup_lane_f32 (__v, __lane));
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_lane_f64 (float64x1_t __a, float64x1_t __v, const int __lane)
{
  return vmulx_f64 (__a, __aarch64_vdup_lane_f64 (__v, __lane));
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_lane_f32 (float32x4_t __a, float32x2_t __v, const int __lane)
{
  return vmulxq_f32 (__a, __aarch64_vdupq_lane_f32 (__v, __lane));
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_lane_f64 (float64x2_t __a, float64x1_t __v, const int __lane)
{
  return vmulxq_f64 (__a, __aarch64_vdupq_lane_f64 (__v, __lane));
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_laneq_f32 (float32x2_t __a, float32x4_t __v, const int __lane)
{
  return vmulx_f32 (__a, __aarch64_vdup_laneq_f32 (__v, __lane));
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_laneq_f64 (float64x1_t __a, float64x2_t __v, const int __lane)
{
  return vmulx_f64 (__a, __aarch64_vdup_laneq_f64 (__v, __lane));
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_laneq_f32 (float32x4_t __a, float32x4_t __v, const int __lane)
{
  return vmulxq_f32 (__a, __aarch64_vdupq_laneq_f32 (__v, __lane));
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_laneq_f64 (float64x2_t __a, float64x2_t __v, const int __lane)
{
  return vmulxq_f64 (__a, __aarch64_vdupq_laneq_f64 (__v, __lane));
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxs_lane_f32 (float32_t __a, float32x2_t __v, const int __lane)
{
  return vmulxs_f32 (__a, __aarch64_vget_lane_any (__v, __lane));
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxs_laneq_f32 (float32_t __a, float32x4_t __v, const int __lane)
{
  return vmulxs_f32 (__a, __aarch64_vget_lane_any (__v, __lane));
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxd_lane_f64 (float64_t __a, float64x1_t __v, const int __lane)
{
  return vmulxd_f64 (__a, __aarch64_vget_lane_any (__v, __lane));
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxd_laneq_f64 (float64_t __a, float64x2_t __v, const int __lane)
{
  return vmulxd_f64 (__a, __aarch64_vget_lane_any (__v, __lane));
}

/* vpmax  */

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmax_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_smaxpv8qi (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmax_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_smaxpv4hi (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmax_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_smaxpv2si (__a, __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmax_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_umaxpv8qi ((int8x8_t) __a,
						  (int8x8_t) __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmax_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_umaxpv4hi ((int16x4_t) __a,
						   (int16x4_t) __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmax_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_umaxpv2si ((int32x2_t) __a,
						   (int32x2_t) __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_smaxpv16qi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_smaxpv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_smaxpv4si (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_umaxpv16qi ((int8x16_t) __a,
						    (int8x16_t) __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_umaxpv8hi ((int16x8_t) __a,
						   (int16x8_t) __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_umaxpv4si ((int32x4_t) __a,
						   (int32x4_t) __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmax_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_smax_nanpv2sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_smax_nanpv4sf (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_smax_nanpv2df (__a, __b);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxqd_f64 (float64x2_t __a)
{
  return vmaxvq_f64 (__a);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxs_f32 (float32x2_t __a)
{
  return vmaxv_f32 (__a);
}

/* vpmaxnm  */

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxnm_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_smaxpv2sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxnmq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_smaxpv4sf (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxnmq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_smaxpv2df (__a, __b);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxnmqd_f64 (float64x2_t __a)
{
  return vmaxnmvq_f64 (__a);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxnms_f32 (float32x2_t __a)
{
  return vmaxnmv_f32 (__a);
}

/* vpmin  */

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmin_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_sminpv8qi (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmin_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_sminpv4hi (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmin_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_sminpv2si (__a, __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmin_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_uminpv8qi ((int8x8_t) __a,
						  (int8x8_t) __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmin_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_uminpv4hi ((int16x4_t) __a,
						   (int16x4_t) __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmin_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_uminpv2si ((int32x2_t) __a,
						   (int32x2_t) __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_sminpv16qi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sminpv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sminpv4si (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_uminpv16qi ((int8x16_t) __a,
						    (int8x16_t) __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uminpv8hi ((int16x8_t) __a,
						   (int16x8_t) __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uminpv4si ((int32x4_t) __a,
						   (int32x4_t) __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmin_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_smin_nanpv2sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_smin_nanpv4sf (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_smin_nanpv2df (__a, __b);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminqd_f64 (float64x2_t __a)
{
  return vminvq_f64 (__a);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmins_f32 (float32x2_t __a)
{
  return vminv_f32 (__a);
}

/* vpminnm  */

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminnm_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_sminpv2sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminnmq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_sminpv4sf (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminnmq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_sminpv2df (__a, __b);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminnmqd_f64 (float64x2_t __a)
{
  return vminnmvq_f64 (__a);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminnms_f32 (float32x2_t __a)
{
  return vminnmv_f32 (__a);
}

/* vpadd  */

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadd_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_faddpv2sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_faddpv4sf (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_faddpv2df (__a, __b);
}

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_addpv8qi (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_addpv4hi (__a, __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_addpv2si (__a, __b);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_addpv8qi_uuu (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_addpv4hi_uuu (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __builtin_aarch64_addpv2si_uuu (__a, __b);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadds_f32 (float32x2_t __a)
{
  return __builtin_aarch64_reduc_plus_scal_v2sf (__a);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddd_f64 (float64x2_t __a)
{
  return __builtin_aarch64_reduc_plus_scal_v2df (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddd_s64 (int64x2_t __a)
{
  return __builtin_aarch64_reduc_plus_scal_v2di (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddd_u64 (uint64x2_t __a)
{
  return __builtin_aarch64_reduc_plus_scal_v2di_uu (__a);
}

/* vqabs */

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabsq_s64 (int64x2_t __a)
{
  return (int64x2_t) __builtin_aarch64_sqabsv2di (__a);
}

__extension__ extern __inline int8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabsb_s8 (int8_t __a)
{
  return (int8_t) __builtin_aarch64_sqabsqi (__a);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabsh_s16 (int16_t __a)
{
  return (int16_t) __builtin_aarch64_sqabshi (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabss_s32 (int32_t __a)
{
  return (int32_t) __builtin_aarch64_sqabssi (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqabsd_s64 (int64_t __a)
{
  return __builtin_aarch64_sqabsdi (__a);
}

/* vqdmlal */

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_sqdmlalv4hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_high_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_sqdmlal2v8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_high_lane_s16 (int32x4_t __a, int16x8_t __b, int16x4_t __c,
		       int const __d)
{
  return __builtin_aarch64_sqdmlal2_lanev8hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_high_laneq_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c,
			int const __d)
{
  return __builtin_aarch64_sqdmlal2_laneqv8hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_high_n_s16 (int32x4_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlal2_nv8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_lane_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlal_lanev4hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_laneq_s16 (int32x4_t __a, int16x4_t __b, int16x8_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlal_laneqv4hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlal_nv4hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_sqdmlalv2si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_high_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_sqdmlal2v4si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_high_lane_s32 (int64x2_t __a, int32x4_t __b, int32x2_t __c,
		       int const __d)
{
  return __builtin_aarch64_sqdmlal2_lanev4si (__a, __b, __c, __d);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_high_laneq_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c,
			int const __d)
{
  return __builtin_aarch64_sqdmlal2_laneqv4si (__a, __b, __c, __d);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_high_n_s32 (int64x2_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlal2_nv4si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlal_lanev2si (__a, __b, __c, __d);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_laneq_s32 (int64x2_t __a, int32x2_t __b, int32x4_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlal_laneqv2si (__a, __b, __c, __d);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlal_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlal_nv2si (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlalh_s16 (int32_t __a, int16_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlalhi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlalh_lane_s16 (int32_t __a, int16_t __b, int16x4_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlal_lanehi (__a, __b, __c, __d);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlalh_laneq_s16 (int32_t __a, int16_t __b, int16x8_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlal_laneqhi (__a, __b, __c, __d);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlals_s32 (int64_t __a, int32_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlalsi (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlals_lane_s32 (int64_t __a, int32_t __b, int32x2_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlal_lanesi (__a, __b, __c, __d);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlals_laneq_s32 (int64_t __a, int32_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlal_laneqsi (__a, __b, __c, __d);
}

/* vqdmlsl */

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_sqdmlslv4hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_high_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_sqdmlsl2v8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_high_lane_s16 (int32x4_t __a, int16x8_t __b, int16x4_t __c,
		       int const __d)
{
  return __builtin_aarch64_sqdmlsl2_lanev8hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_high_laneq_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c,
			int const __d)
{
  return __builtin_aarch64_sqdmlsl2_laneqv8hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_high_n_s16 (int32x4_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlsl2_nv8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_lane_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlsl_lanev4hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_laneq_s16 (int32x4_t __a, int16x4_t __b, int16x8_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlsl_laneqv4hi (__a, __b, __c, __d);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlsl_nv4hi (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_sqdmlslv2si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_high_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_sqdmlsl2v4si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_high_lane_s32 (int64x2_t __a, int32x4_t __b, int32x2_t __c,
		       int const __d)
{
  return __builtin_aarch64_sqdmlsl2_lanev4si (__a, __b, __c, __d);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_high_laneq_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c,
			int const __d)
{
  return __builtin_aarch64_sqdmlsl2_laneqv4si (__a, __b, __c, __d);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_high_n_s32 (int64x2_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlsl2_nv4si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlsl_lanev2si (__a, __b, __c, __d);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_laneq_s32 (int64x2_t __a, int32x2_t __b, int32x4_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlsl_laneqv2si (__a, __b, __c, __d);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsl_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlsl_nv2si (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlslh_s16 (int32_t __a, int16_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlslhi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlslh_lane_s16 (int32_t __a, int16_t __b, int16x4_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlsl_lanehi (__a, __b, __c, __d);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlslh_laneq_s16 (int32_t __a, int16_t __b, int16x8_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlsl_laneqhi (__a, __b, __c, __d);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsls_s32 (int64_t __a, int32_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlslsi (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsls_lane_s32 (int64_t __a, int32_t __b, int32x2_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlsl_lanesi (__a, __b, __c, __d);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmlsls_laneq_s32 (int64_t __a, int32_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlsl_laneqsi (__a, __b, __c, __d);
}

/* vqdmulh */

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulh_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanev4hi (__a, __b, __c);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulh_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanev2si (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhq_lane_s16 (int16x8_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanev8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhq_lane_s32 (int32x4_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanev4si (__a, __b, __c);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhh_s16 (int16_t __a, int16_t __b)
{
  return (int16_t) __builtin_aarch64_sqdmulhhi (__a, __b);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhh_lane_s16 (int16_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanehi (__a, __b, __c);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhh_laneq_s16 (int16_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqhi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhs_s32 (int32_t __a, int32_t __b)
{
  return (int32_t) __builtin_aarch64_sqdmulhsi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhs_lane_s32 (int32_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanesi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulhs_laneq_s32 (int32_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqsi (__a, __b, __c);
}

/* vqdmull */

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_sqdmullv4hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_high_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sqdmull2v8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_high_lane_s16 (int16x8_t __a, int16x4_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull2_lanev8hi (__a, __b,__c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_high_laneq_s16 (int16x8_t __a, int16x8_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull2_laneqv8hi (__a, __b,__c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_high_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_aarch64_sqdmull2_nv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_lane_s16 (int16x4_t __a, int16x4_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull_lanev4hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_laneq_s16 (int16x4_t __a, int16x8_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull_laneqv4hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_n_s16 (int16x4_t __a, int16_t __b)
{
  return __builtin_aarch64_sqdmull_nv4hi (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_sqdmullv2si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_high_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sqdmull2v4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_high_lane_s32 (int32x4_t __a, int32x2_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull2_lanev4si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_high_laneq_s32 (int32x4_t __a, int32x4_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull2_laneqv4si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_high_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_aarch64_sqdmull2_nv4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_lane_s32 (int32x2_t __a, int32x2_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull_lanev2si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_laneq_s32 (int32x2_t __a, int32x4_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull_laneqv2si (__a, __b, __c);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmull_n_s32 (int32x2_t __a, int32_t __b)
{
  return __builtin_aarch64_sqdmull_nv2si (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmullh_s16 (int16_t __a, int16_t __b)
{
  return (int32_t) __builtin_aarch64_sqdmullhi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmullh_lane_s16 (int16_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmull_lanehi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmullh_laneq_s16 (int16_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqdmull_laneqhi (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulls_s32 (int32_t __a, int32_t __b)
{
  return __builtin_aarch64_sqdmullsi (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulls_lane_s32 (int32_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqdmull_lanesi (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqdmulls_laneq_s32 (int32_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmull_laneqsi (__a, __b, __c);
}

/* vqmovn */

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_s16 (int16x8_t __a)
{
  return (int8x8_t) __builtin_aarch64_sqmovnv8hi (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_s32 (int32x4_t __a)
{
  return (int16x4_t) __builtin_aarch64_sqmovnv4si (__a);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_s64 (int64x2_t __a)
{
  return (int32x2_t) __builtin_aarch64_sqmovnv2di (__a);
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_u16 (uint16x8_t __a)
{
  return (uint8x8_t) __builtin_aarch64_uqmovnv8hi ((int16x8_t) __a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_u32 (uint32x4_t __a)
{
  return (uint16x4_t) __builtin_aarch64_uqmovnv4si ((int32x4_t) __a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovn_u64 (uint64x2_t __a)
{
  return (uint32x2_t) __builtin_aarch64_uqmovnv2di ((int64x2_t) __a);
}

__extension__ extern __inline int8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovnh_s16 (int16_t __a)
{
  return (int8_t) __builtin_aarch64_sqmovnhi (__a);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovns_s32 (int32_t __a)
{
  return (int16_t) __builtin_aarch64_sqmovnsi (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovnd_s64 (int64_t __a)
{
  return (int32_t) __builtin_aarch64_sqmovndi (__a);
}

__extension__ extern __inline uint8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovnh_u16 (uint16_t __a)
{
  return (uint8_t) __builtin_aarch64_uqmovnhi (__a);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovns_u32 (uint32_t __a)
{
  return (uint16_t) __builtin_aarch64_uqmovnsi (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovnd_u64 (uint64_t __a)
{
  return (uint32_t) __builtin_aarch64_uqmovndi (__a);
}

/* vqmovun */

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovun_s16 (int16x8_t __a)
{
  return __builtin_aarch64_sqmovunv8hi_us (__a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovun_s32 (int32x4_t __a)
{
  return __builtin_aarch64_sqmovunv4si_us (__a);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovun_s64 (int64x2_t __a)
{
  return __builtin_aarch64_sqmovunv2di_us (__a);
}

__extension__ extern __inline uint8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovunh_s16 (int16_t __a)
{
  return __builtin_aarch64_sqmovunhi_us (__a);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovuns_s32 (int32_t __a)
{
  return __builtin_aarch64_sqmovunsi_us (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqmovund_s64 (int64_t __a)
{
  return __builtin_aarch64_sqmovundi_us (__a);
}

/* vqneg */

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqnegq_s64 (int64x2_t __a)
{
  return (int64x2_t) __builtin_aarch64_sqnegv2di (__a);
}

__extension__ extern __inline int8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqnegb_s8 (int8_t __a)
{
  return (int8_t) __builtin_aarch64_sqnegqi (__a);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqnegh_s16 (int16_t __a)
{
  return (int16_t) __builtin_aarch64_sqneghi (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqnegs_s32 (int32_t __a)
{
  return (int32_t) __builtin_aarch64_sqnegsi (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqnegd_s64 (int64_t __a)
{
  return __builtin_aarch64_sqnegdi (__a);
}

/* vqrdmulh */

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulh_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return  __builtin_aarch64_sqrdmulh_lanev4hi (__a, __b, __c);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulh_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanev2si (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhq_lane_s16 (int16x8_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanev8hi (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhq_lane_s32 (int32x4_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanev4si (__a, __b, __c);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhh_s16 (int16_t __a, int16_t __b)
{
  return (int16_t) __builtin_aarch64_sqrdmulhhi (__a, __b);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhh_lane_s16 (int16_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanehi (__a, __b, __c);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhh_laneq_s16 (int16_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_laneqhi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhs_s32 (int32_t __a, int32_t __b)
{
  return (int32_t) __builtin_aarch64_sqrdmulhsi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhs_lane_s32 (int32_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanesi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vqrdmulhs_laneq_s32 (int32_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_laneqsi (__a, __b, __c);
}

/* vrecpe  */

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpe_u32 (uint32x2_t __a)
{
  return (uint32x2_t) __builtin_aarch64_urecpev2si ((int32x2_t) __a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpeq_u32 (uint32x4_t __a)
{
  return (uint32x4_t) __builtin_aarch64_urecpev4si ((int32x4_t) __a);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpes_f32 (float32_t __a)
{
  return __builtin_aarch64_frecpesf (__a);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecped_f64 (float64_t __a)
{
  return __builtin_aarch64_frecpedf (__a);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpe_f32 (float32x2_t __a)
{
  return __builtin_aarch64_frecpev2sf (__a);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpe_f64 (float64x1_t __a)
{
  return (float64x1_t) { vrecped_f64 (vget_lane_f64 (__a, 0)) };
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpeq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_frecpev4sf (__a);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpeq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_frecpev2df (__a);
}

/* vrecps  */

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpss_f32 (float32_t __a, float32_t __b)
{
  return __builtin_aarch64_frecpssf (__a, __b);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpsd_f64 (float64_t __a, float64_t __b)
{
  return __builtin_aarch64_frecpsdf (__a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecps_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_frecpsv2sf (__a, __b);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecps_f64 (float64x1_t __a, float64x1_t __b)
{
  return (float64x1_t) { vrecpsd_f64 (vget_lane_f64 (__a, 0),
				      vget_lane_f64 (__b, 0)) };
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpsq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_frecpsv4sf (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpsq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_frecpsv2df (__a, __b);
}

/* vrecpx  */

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpxs_f32 (float32_t __a)
{
  return __builtin_aarch64_frecpxsf (__a);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpxd_f64 (float64_t __a)
{
  return __builtin_aarch64_frecpxdf (__a);
}

/* vrsqrte.  */

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrtes_f32 (float32_t __a)
{
  return __builtin_aarch64_rsqrtesf (__a);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrted_f64 (float64_t __a)
{
  return __builtin_aarch64_rsqrtedf (__a);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrte_f32 (float32x2_t __a)
{
  return __builtin_aarch64_rsqrtev2sf (__a);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrte_f64 (float64x1_t __a)
{
  return (float64x1_t) {vrsqrted_f64 (vget_lane_f64 (__a, 0))};
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrteq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_rsqrtev4sf (__a);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrteq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_rsqrtev2df (__a);
}

/* vrsqrts.  */

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrtss_f32 (float32_t __a, float32_t __b)
{
  return __builtin_aarch64_rsqrtssf (__a, __b);
}

__extension__ extern __inline float64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrtsd_f64 (float64_t __a, float64_t __b)
{
  return __builtin_aarch64_rsqrtsdf (__a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrts_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_rsqrtsv2sf (__a, __b);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrts_f64 (float64x1_t __a, float64x1_t __b)
{
  return (float64x1_t) {vrsqrtsd_f64 (vget_lane_f64 (__a, 0),
				      vget_lane_f64 (__b, 0))};
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrtsq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_rsqrtsv4sf (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrtsq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_rsqrtsv2df (__a, __b);
}

/* vsqadd */

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqadd_u8 (uint8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_usqaddv8qi_uus (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqadd_u16 (uint16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_usqaddv4hi_uus (__a, __b);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqadd_u32 (uint32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_usqaddv2si_uus (__a, __b);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqadd_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) {__builtin_aarch64_usqadddi_uus (__a[0], __b[0])};
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqaddq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_usqaddv16qi_uus (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqaddq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_usqaddv8hi_uus (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqaddq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_usqaddv4si_uus (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqaddq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_usqaddv2di_uus (__a, __b);
}

__extension__ extern __inline uint8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqaddb_u8 (uint8_t __a, int8_t __b)
{
  return __builtin_aarch64_usqaddqi_uus (__a, __b);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqaddh_u16 (uint16_t __a, int16_t __b)
{
  return __builtin_aarch64_usqaddhi_uus (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqadds_u32 (uint32_t __a, int32_t __b)
{
  return __builtin_aarch64_usqaddsi_uus (__a, __b);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsqaddd_u64 (uint64_t __a, int64_t __b)
{
  return __builtin_aarch64_usqadddi_uus (__a, __b);
}

/* vst1 */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f16 (float16_t *__a, float16x4_t __b)
{
  __builtin_aarch64_st1v4hf (__a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f32 (float32_t *__a, float32x2_t __b)
{
  __builtin_aarch64_st1v2sf ((__builtin_aarch64_simd_sf *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f64 (float64_t *__a, float64x1_t __b)
{
  *__a = __b[0];
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p8 (poly8_t *__a, poly8x8_t __b)
{
  __builtin_aarch64_st1v8qi_sp ((__builtin_aarch64_simd_qi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p16 (poly16_t *__a, poly16x4_t __b)
{
  __builtin_aarch64_st1v4hi_sp ((__builtin_aarch64_simd_hi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p64 (poly64_t *__a, poly64x1_t __b)
{
  *__a = __b[0];
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s8 (int8_t *__a, int8x8_t __b)
{
  __builtin_aarch64_st1v8qi ((__builtin_aarch64_simd_qi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s16 (int16_t *__a, int16x4_t __b)
{
  __builtin_aarch64_st1v4hi ((__builtin_aarch64_simd_hi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s32 (int32_t *__a, int32x2_t __b)
{
  __builtin_aarch64_st1v2si ((__builtin_aarch64_simd_si *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s64 (int64_t *__a, int64x1_t __b)
{
  *__a = __b[0];
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u8 (uint8_t *__a, uint8x8_t __b)
{
  __builtin_aarch64_st1v8qi_su ((__builtin_aarch64_simd_qi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u16 (uint16_t *__a, uint16x4_t __b)
{
  __builtin_aarch64_st1v4hi_su ((__builtin_aarch64_simd_hi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u32 (uint32_t *__a, uint32x2_t __b)
{
  __builtin_aarch64_st1v2si_su ((__builtin_aarch64_simd_si *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u64 (uint64_t *__a, uint64x1_t __b)
{
  *__a = __b[0];
}

/* vst1q */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f16 (float16_t *__a, float16x8_t __b)
{
  __builtin_aarch64_st1v8hf (__a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f32 (float32_t *__a, float32x4_t __b)
{
  __builtin_aarch64_st1v4sf ((__builtin_aarch64_simd_sf *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f64 (float64_t *__a, float64x2_t __b)
{
  __builtin_aarch64_st1v2df ((__builtin_aarch64_simd_df *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p8 (poly8_t *__a, poly8x16_t __b)
{
  __builtin_aarch64_st1v16qi_sp ((__builtin_aarch64_simd_qi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p16 (poly16_t *__a, poly16x8_t __b)
{
  __builtin_aarch64_st1v8hi_sp ((__builtin_aarch64_simd_hi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p64 (poly64_t *__a, poly64x2_t __b)
{
  __builtin_aarch64_st1v2di_sp ((__builtin_aarch64_simd_di *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s8 (int8_t *__a, int8x16_t __b)
{
  __builtin_aarch64_st1v16qi ((__builtin_aarch64_simd_qi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s16 (int16_t *__a, int16x8_t __b)
{
  __builtin_aarch64_st1v8hi ((__builtin_aarch64_simd_hi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s32 (int32_t *__a, int32x4_t __b)
{
  __builtin_aarch64_st1v4si ((__builtin_aarch64_simd_si *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s64 (int64_t *__a, int64x2_t __b)
{
  __builtin_aarch64_st1v2di ((__builtin_aarch64_simd_di *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u8 (uint8_t *__a, uint8x16_t __b)
{
  __builtin_aarch64_st1v16qi_su ((__builtin_aarch64_simd_qi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u16 (uint16_t *__a, uint16x8_t __b)
{
  __builtin_aarch64_st1v8hi_su ((__builtin_aarch64_simd_hi *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u32 (uint32_t *__a, uint32x4_t __b)
{
  __builtin_aarch64_st1v4si_su ((__builtin_aarch64_simd_si *) __a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u64 (uint64_t *__a, uint64x2_t __b)
{
  __builtin_aarch64_st1v2di_su ((__builtin_aarch64_simd_di *) __a, __b);
}

/* vst1_lane */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_f16 (float16_t *__a, float16x4_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_f32 (float32_t *__a, float32x2_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_f64 (float64_t *__a, float64x1_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_p8 (poly8_t *__a, poly8x8_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_p16 (poly16_t *__a, poly16x4_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_p64 (poly64_t *__a, poly64x1_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_s8 (int8_t *__a, int8x8_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_s16 (int16_t *__a, int16x4_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_s32 (int32_t *__a, int32x2_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_s64 (int64_t *__a, int64x1_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_u8 (uint8_t *__a, uint8x8_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_u16 (uint16_t *__a, uint16x4_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_u32 (uint32_t *__a, uint32x2_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_u64 (uint64_t *__a, uint64x1_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

/* vst1q_lane */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_f16 (float16_t *__a, float16x8_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_f32 (float32_t *__a, float32x4_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_f64 (float64_t *__a, float64x2_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_p8 (poly8_t *__a, poly8x16_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_p16 (poly16_t *__a, poly16x8_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_p64 (poly64_t *__a, poly64x2_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_s8 (int8_t *__a, int8x16_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_s16 (int16_t *__a, int16x8_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_s32 (int32_t *__a, int32x4_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_s64 (int64_t *__a, int64x2_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_u8 (uint8_t *__a, uint8x16_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_u16 (uint16_t *__a, uint16x8_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_u32 (uint32_t *__a, uint32x4_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_u64 (uint64_t *__a, uint64x2_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

/* vst1x2 */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s64_x2 (int64_t * __a, int64x1x2_t __val)
{
  __builtin_aarch64_st1x2di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u64_x2 (uint64_t * __a, uint64x1x2_t __val)
{
  __builtin_aarch64_st1x2di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f64_x2 (float64_t * __a, float64x1x2_t __val)
{
  __builtin_aarch64_st1x2df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s8_x2 (int8_t * __a, int8x8x2_t __val)
{
  __builtin_aarch64_st1x2v8qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p8_x2 (poly8_t * __a, poly8x8x2_t __val)
{
  __builtin_aarch64_st1x2v8qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s16_x2 (int16_t * __a, int16x4x2_t __val)
{
  __builtin_aarch64_st1x2v4hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p16_x2 (poly16_t * __a, poly16x4x2_t __val)
{
  __builtin_aarch64_st1x2v4hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s32_x2 (int32_t * __a, int32x2x2_t __val)
{
  __builtin_aarch64_st1x2v2si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u8_x2 (uint8_t * __a, uint8x8x2_t __val)
{
  __builtin_aarch64_st1x2v8qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u16_x2 (uint16_t * __a, uint16x4x2_t __val)
{
  __builtin_aarch64_st1x2v4hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u32_x2 (uint32_t * __a, uint32x2x2_t __val)
{
  __builtin_aarch64_st1x2v2si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f16_x2 (float16_t * __a, float16x4x2_t __val)
{
  __builtin_aarch64_st1x2v4hf (__a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f32_x2 (float32_t * __a, float32x2x2_t __val)
{
  __builtin_aarch64_st1x2v2sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p64_x2 (poly64_t * __a, poly64x1x2_t __val)
{
  __builtin_aarch64_st1x2di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s8_x2 (int8_t * __a, int8x16x2_t __val)
{
  __builtin_aarch64_st1x2v16qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p8_x2 (poly8_t * __a, poly8x16x2_t __val)
{
  __builtin_aarch64_st1x2v16qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s16_x2 (int16_t * __a, int16x8x2_t __val)
{
  __builtin_aarch64_st1x2v8hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p16_x2 (poly16_t * __a, poly16x8x2_t __val)
{
  __builtin_aarch64_st1x2v8hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s32_x2 (int32_t * __a, int32x4x2_t __val)
{
  __builtin_aarch64_st1x2v4si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s64_x2 (int64_t * __a, int64x2x2_t __val)
{
  __builtin_aarch64_st1x2v2di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u8_x2 (uint8_t * __a, uint8x16x2_t __val)
{
  __builtin_aarch64_st1x2v16qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u16_x2 (uint16_t * __a, uint16x8x2_t __val)
{
  __builtin_aarch64_st1x2v8hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u32_x2 (uint32_t * __a, uint32x4x2_t __val)
{
  __builtin_aarch64_st1x2v4si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u64_x2 (uint64_t * __a, uint64x2x2_t __val)
{
  __builtin_aarch64_st1x2v2di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f16_x2 (float16_t * __a, float16x8x2_t __val)
{
  __builtin_aarch64_st1x2v8hf (__a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f32_x2 (float32_t * __a, float32x4x2_t __val)
{
  __builtin_aarch64_st1x2v4sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f64_x2 (float64_t * __a, float64x2x2_t __val)
{
  __builtin_aarch64_st1x2v2df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p64_x2 (poly64_t * __a, poly64x2x2_t __val)
{
  __builtin_aarch64_st1x2v2di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

/* vst1x3 */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s64_x3 (int64_t * __a, int64x1x3_t __val)
{
  __builtin_aarch64_st1x3di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u64_x3 (uint64_t * __a, uint64x1x3_t __val)
{
  __builtin_aarch64_st1x3di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f64_x3 (float64_t * __a, float64x1x3_t __val)
{
  __builtin_aarch64_st1x3df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s8_x3 (int8_t * __a, int8x8x3_t __val)
{
  __builtin_aarch64_st1x3v8qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p8_x3 (poly8_t * __a, poly8x8x3_t __val)
{
  __builtin_aarch64_st1x3v8qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s16_x3 (int16_t * __a, int16x4x3_t __val)
{
  __builtin_aarch64_st1x3v4hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p16_x3 (poly16_t * __a, poly16x4x3_t __val)
{
  __builtin_aarch64_st1x3v4hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s32_x3 (int32_t * __a, int32x2x3_t __val)
{
  __builtin_aarch64_st1x3v2si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u8_x3 (uint8_t * __a, uint8x8x3_t __val)
{
  __builtin_aarch64_st1x3v8qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u16_x3 (uint16_t * __a, uint16x4x3_t __val)
{
  __builtin_aarch64_st1x3v4hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u32_x3 (uint32_t * __a, uint32x2x3_t __val)
{
  __builtin_aarch64_st1x3v2si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f16_x3 (float16_t * __a, float16x4x3_t __val)
{
  __builtin_aarch64_st1x3v4hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f32_x3 (float32_t * __a, float32x2x3_t __val)
{
  __builtin_aarch64_st1x3v2sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p64_x3 (poly64_t * __a, poly64x1x3_t __val)
{
  __builtin_aarch64_st1x3di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s8_x3 (int8_t * __a, int8x16x3_t __val)
{
  __builtin_aarch64_st1x3v16qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p8_x3 (poly8_t * __a, poly8x16x3_t __val)
{
  __builtin_aarch64_st1x3v16qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s16_x3 (int16_t * __a, int16x8x3_t __val)
{
  __builtin_aarch64_st1x3v8hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p16_x3 (poly16_t * __a, poly16x8x3_t __val)
{
  __builtin_aarch64_st1x3v8hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s32_x3 (int32_t * __a, int32x4x3_t __val)
{
  __builtin_aarch64_st1x3v4si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s64_x3 (int64_t * __a, int64x2x3_t __val)
{
  __builtin_aarch64_st1x3v2di ((__builtin_aarch64_simd_di *) __a,
			       (int64x2x3_t) __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u8_x3 (uint8_t * __a, uint8x16x3_t __val)
{
  __builtin_aarch64_st1x3v16qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u16_x3 (uint16_t * __a, uint16x8x3_t __val)
{
  __builtin_aarch64_st1x3v8hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u32_x3 (uint32_t * __a, uint32x4x3_t __val)
{
  __builtin_aarch64_st1x3v4si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u64_x3 (uint64_t * __a, uint64x2x3_t __val)
{
  __builtin_aarch64_st1x3v2di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f16_x3 (float16_t * __a, float16x8x3_t __val)
{
  __builtin_aarch64_st1x3v8hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f32_x3 (float32_t * __a, float32x4x3_t __val)
{
  __builtin_aarch64_st1x3v4sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f64_x3 (float64_t * __a, float64x2x3_t __val)
{
  __builtin_aarch64_st1x3v2df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p64_x3 (poly64_t * __a, poly64x2x3_t __val)
{
  __builtin_aarch64_st1x3v2di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

/* vst1(q)_x4.  */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s8_x4 (int8_t * __a, int8x8x4_t __val)
{
  __builtin_aarch64_st1x4v8qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s8_x4 (int8_t * __a, int8x16x4_t __val)
{
  __builtin_aarch64_st1x4v16qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s16_x4 (int16_t * __a, int16x4x4_t __val)
{
  __builtin_aarch64_st1x4v4hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s16_x4 (int16_t * __a, int16x8x4_t __val)
{
  __builtin_aarch64_st1x4v8hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s32_x4 (int32_t * __a, int32x2x4_t __val)
{
  __builtin_aarch64_st1x4v2si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s32_x4 (int32_t * __a, int32x4x4_t __val)
{
  __builtin_aarch64_st1x4v4si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u8_x4 (uint8_t * __a, uint8x8x4_t __val)
{
  __builtin_aarch64_st1x4v8qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u8_x4 (uint8_t * __a, uint8x16x4_t __val)
{
  __builtin_aarch64_st1x4v16qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u16_x4 (uint16_t * __a, uint16x4x4_t __val)
{
  __builtin_aarch64_st1x4v4hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u16_x4 (uint16_t * __a, uint16x8x4_t __val)
{
  __builtin_aarch64_st1x4v8hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u32_x4 (uint32_t * __a, uint32x2x4_t __val)
{
  __builtin_aarch64_st1x4v2si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u32_x4 (uint32_t * __a, uint32x4x4_t __val)
{
  __builtin_aarch64_st1x4v4si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f16_x4 (float16_t * __a, float16x4x4_t __val)
{
  __builtin_aarch64_st1x4v4hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f16_x4 (float16_t * __a, float16x8x4_t __val)
{
  __builtin_aarch64_st1x4v8hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f32_x4 (float32_t * __a, float32x2x4_t __val)
{
  __builtin_aarch64_st1x4v2sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f32_x4 (float32_t * __a, float32x4x4_t __val)
{
  __builtin_aarch64_st1x4v4sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p8_x4 (poly8_t * __a, poly8x8x4_t __val)
{
  __builtin_aarch64_st1x4v8qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p8_x4 (poly8_t * __a, poly8x16x4_t __val)
{
  __builtin_aarch64_st1x4v16qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p16_x4 (poly16_t * __a, poly16x4x4_t __val)
{
  __builtin_aarch64_st1x4v4hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p16_x4 (poly16_t * __a, poly16x8x4_t __val)
{
  __builtin_aarch64_st1x4v8hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_s64_x4 (int64_t * __a, int64x1x4_t __val)
{
  __builtin_aarch64_st1x4di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_u64_x4 (uint64_t * __a, uint64x1x4_t __val)
{
  __builtin_aarch64_st1x4di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_p64_x4 (poly64_t * __a, poly64x1x4_t __val)
{
  __builtin_aarch64_st1x4di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_s64_x4 (int64_t * __a, int64x2x4_t __val)
{
  __builtin_aarch64_st1x4v2di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_u64_x4 (uint64_t * __a, uint64x2x4_t __val)
{
  __builtin_aarch64_st1x4v2di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_p64_x4 (poly64_t * __a, poly64x2x4_t __val)
{
  __builtin_aarch64_st1x4v2di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_f64_x4 (float64_t * __a, float64x1x4_t __val)
{
  __builtin_aarch64_st1x4df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_f64_x4 (float64_t * __a, float64x2x4_t __val)
{
  __builtin_aarch64_st1x4v2df ((__builtin_aarch64_simd_df *) __a, __val);
}

/* vstn */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_s64 (int64_t * __a, int64x1x2_t __val)
{
  __builtin_aarch64_st2di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_u64 (uint64_t * __a, uint64x1x2_t __val)
{
  __builtin_aarch64_st2di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_f64 (float64_t * __a, float64x1x2_t __val)
{
  __builtin_aarch64_st2df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_s8 (int8_t * __a, int8x8x2_t __val)
{
  __builtin_aarch64_st2v8qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_p8 (poly8_t * __a, poly8x8x2_t __val)
{
  __builtin_aarch64_st2v8qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_s16 (int16_t * __a, int16x4x2_t __val)
{
  __builtin_aarch64_st2v4hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_p16 (poly16_t * __a, poly16x4x2_t __val)
{
  __builtin_aarch64_st2v4hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_s32 (int32_t * __a, int32x2x2_t __val)
{
  __builtin_aarch64_st2v2si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_u8 (uint8_t * __a, uint8x8x2_t __val)
{
  __builtin_aarch64_st2v8qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_u16 (uint16_t * __a, uint16x4x2_t __val)
{
  __builtin_aarch64_st2v4hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_u32 (uint32_t * __a, uint32x2x2_t __val)
{
  __builtin_aarch64_st2v2si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_f16 (float16_t * __a, float16x4x2_t __val)
{
  __builtin_aarch64_st2v4hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_f32 (float32_t * __a, float32x2x2_t __val)
{
  __builtin_aarch64_st2v2sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_p64 (poly64_t * __a, poly64x1x2_t __val)
{
  __builtin_aarch64_st2di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_s8 (int8_t * __a, int8x16x2_t __val)
{
  __builtin_aarch64_st2v16qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_p8 (poly8_t * __a, poly8x16x2_t __val)
{
  __builtin_aarch64_st2v16qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_s16 (int16_t * __a, int16x8x2_t __val)
{
  __builtin_aarch64_st2v8hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_p16 (poly16_t * __a, poly16x8x2_t __val)
{
  __builtin_aarch64_st2v8hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_s32 (int32_t * __a, int32x4x2_t __val)
{
  __builtin_aarch64_st2v4si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_s64 (int64_t * __a, int64x2x2_t __val)
{
  __builtin_aarch64_st2v2di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_u8 (uint8_t * __a, uint8x16x2_t __val)
{
  __builtin_aarch64_st2v16qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_u16 (uint16_t * __a, uint16x8x2_t __val)
{
  __builtin_aarch64_st2v8hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_u32 (uint32_t * __a, uint32x4x2_t __val)
{
  __builtin_aarch64_st2v4si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_u64 (uint64_t * __a, uint64x2x2_t __val)
{
  __builtin_aarch64_st2v2di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_f16 (float16_t * __a, float16x8x2_t __val)
{
  __builtin_aarch64_st2v8hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_f32 (float32_t * __a, float32x4x2_t __val)
{
  __builtin_aarch64_st2v4sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_f64 (float64_t * __a, float64x2x2_t __val)
{
  __builtin_aarch64_st2v2df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_p64 (poly64_t * __a, poly64x2x2_t __val)
{
  __builtin_aarch64_st2v2di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_s64 (int64_t * __a, int64x1x3_t __val)
{
  __builtin_aarch64_st3di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_u64 (uint64_t * __a, uint64x1x3_t __val)
{
  __builtin_aarch64_st3di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_f64 (float64_t * __a, float64x1x3_t __val)
{
  __builtin_aarch64_st3df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_s8 (int8_t * __a, int8x8x3_t __val)
{
  __builtin_aarch64_st3v8qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_p8 (poly8_t * __a, poly8x8x3_t __val)
{
  __builtin_aarch64_st3v8qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_s16 (int16_t * __a, int16x4x3_t __val)
{
  __builtin_aarch64_st3v4hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_p16 (poly16_t * __a, poly16x4x3_t __val)
{
  __builtin_aarch64_st3v4hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_s32 (int32_t * __a, int32x2x3_t __val)
{
  __builtin_aarch64_st3v2si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_u8 (uint8_t * __a, uint8x8x3_t __val)
{
  __builtin_aarch64_st3v8qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_u16 (uint16_t * __a, uint16x4x3_t __val)
{
  __builtin_aarch64_st3v4hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_u32 (uint32_t * __a, uint32x2x3_t __val)
{
  __builtin_aarch64_st3v2si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_f16 (float16_t * __a, float16x4x3_t __val)
{
  __builtin_aarch64_st3v4hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_f32 (float32_t * __a, float32x2x3_t __val)
{
  __builtin_aarch64_st3v2sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_p64 (poly64_t * __a, poly64x1x3_t __val)
{
  __builtin_aarch64_st3di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_s8 (int8_t * __a, int8x16x3_t __val)
{
  __builtin_aarch64_st3v16qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_p8 (poly8_t * __a, poly8x16x3_t __val)
{
  __builtin_aarch64_st3v16qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_s16 (int16_t * __a, int16x8x3_t __val)
{
  __builtin_aarch64_st3v8hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_p16 (poly16_t * __a, poly16x8x3_t __val)
{
  __builtin_aarch64_st3v8hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_s32 (int32_t * __a, int32x4x3_t __val)
{
  __builtin_aarch64_st3v4si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_s64 (int64_t * __a, int64x2x3_t __val)
{
  __builtin_aarch64_st3v2di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_u8 (uint8_t * __a, uint8x16x3_t __val)
{
  __builtin_aarch64_st3v16qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_u16 (uint16_t * __a, uint16x8x3_t __val)
{
  __builtin_aarch64_st3v8hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_u32 (uint32_t * __a, uint32x4x3_t __val)
{
  __builtin_aarch64_st3v4si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_u64 (uint64_t * __a, uint64x2x3_t __val)
{
  __builtin_aarch64_st3v2di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_f16 (float16_t * __a, float16x8x3_t __val)
{
  __builtin_aarch64_st3v8hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_f32 (float32_t * __a, float32x4x3_t __val)
{
  __builtin_aarch64_st3v4sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_f64 (float64_t * __a, float64x2x3_t __val)
{
  __builtin_aarch64_st3v2df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_p64 (poly64_t * __a, poly64x2x3_t __val)
{
  __builtin_aarch64_st3v2di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_s64 (int64_t * __a, int64x1x4_t __val)
{
  __builtin_aarch64_st4di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_u64 (uint64_t * __a, uint64x1x4_t __val)
{
  __builtin_aarch64_st4di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_f64 (float64_t * __a, float64x1x4_t __val)
{
  __builtin_aarch64_st4df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_s8 (int8_t * __a, int8x8x4_t __val)
{
  __builtin_aarch64_st4v8qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_p8 (poly8_t * __a, poly8x8x4_t __val)
{
  __builtin_aarch64_st4v8qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_s16 (int16_t * __a, int16x4x4_t __val)
{
  __builtin_aarch64_st4v4hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_p16 (poly16_t * __a, poly16x4x4_t __val)
{
  __builtin_aarch64_st4v4hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_s32 (int32_t * __a, int32x2x4_t __val)
{
  __builtin_aarch64_st4v2si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_u8 (uint8_t * __a, uint8x8x4_t __val)
{
  __builtin_aarch64_st4v8qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_u16 (uint16_t * __a, uint16x4x4_t __val)
{
  __builtin_aarch64_st4v4hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_u32 (uint32_t * __a, uint32x2x4_t __val)
{
  __builtin_aarch64_st4v2si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_f16 (float16_t * __a, float16x4x4_t __val)
{
  __builtin_aarch64_st4v4hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_f32 (float32_t * __a, float32x2x4_t __val)
{
  __builtin_aarch64_st4v2sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_p64 (poly64_t * __a, poly64x1x4_t __val)
{
  __builtin_aarch64_st4di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_s8 (int8_t * __a, int8x16x4_t __val)
{
  __builtin_aarch64_st4v16qi ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_p8 (poly8_t * __a, poly8x16x4_t __val)
{
  __builtin_aarch64_st4v16qi_sp ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_s16 (int16_t * __a, int16x8x4_t __val)
{
  __builtin_aarch64_st4v8hi ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_p16 (poly16_t * __a, poly16x8x4_t __val)
{
  __builtin_aarch64_st4v8hi_sp ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_s32 (int32_t * __a, int32x4x4_t __val)
{
  __builtin_aarch64_st4v4si ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_s64 (int64_t * __a, int64x2x4_t __val)
{
  __builtin_aarch64_st4v2di ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_u8 (uint8_t * __a, uint8x16x4_t __val)
{
  __builtin_aarch64_st4v16qi_su ((__builtin_aarch64_simd_qi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_u16 (uint16_t * __a, uint16x8x4_t __val)
{
  __builtin_aarch64_st4v8hi_su ((__builtin_aarch64_simd_hi *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_u32 (uint32_t * __a, uint32x4x4_t __val)
{
  __builtin_aarch64_st4v4si_su ((__builtin_aarch64_simd_si *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_u64 (uint64_t * __a, uint64x2x4_t __val)
{
  __builtin_aarch64_st4v2di_su ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_f16 (float16_t * __a, float16x8x4_t __val)
{
  __builtin_aarch64_st4v8hf ((__builtin_aarch64_simd_hf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_f32 (float32_t * __a, float32x4x4_t __val)
{
  __builtin_aarch64_st4v4sf ((__builtin_aarch64_simd_sf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_f64 (float64_t * __a, float64x2x4_t __val)
{
  __builtin_aarch64_st4v2df ((__builtin_aarch64_simd_df *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_p64 (poly64_t * __a, poly64x2x4_t __val)
{
  __builtin_aarch64_st4v2di_sp ((__builtin_aarch64_simd_di *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vstrq_p128 (poly128_t * __ptr, poly128_t __val)
{
  *__ptr = __val;
}

/* vsub */

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubd_s64 (int64_t __a, int64_t __b)
{
  return __a - __b;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsubd_u64 (uint64_t __a, uint64_t __b)
{
  return __a - __b;
}

/* vtst */

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) ((__a & __b) != 0);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) ((__a & __b) != 0);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) ((__a & __b) != 0);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_s64 (int64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) ((__a & __b) != __AARCH64_INT64_C (0));
}

__extension__ extern __inline uint8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return ((__a & __b) != 0);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return ((__a & __b) != 0);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return ((__a & __b) != 0);
}

__extension__ extern __inline uint64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtst_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return ((__a & __b) != __AARCH64_UINT64_C (0));
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) ((__a & __b) != 0);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) ((__a & __b) != 0);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) ((__a & __b) != 0);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) ((__a & __b) != __AARCH64_INT64_C (0));
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return ((__a & __b) != 0);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return ((__a & __b) != 0);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return ((__a & __b) != 0);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return ((__a & __b) != __AARCH64_UINT64_C (0));
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstd_s64 (int64_t __a, int64_t __b)
{
  return (__a & __b) ? -1ll : 0ll;
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vtstd_u64 (uint64_t __a, uint64_t __b)
{
  return (__a & __b) ? -1ll : 0ll;
}

/* vuqadd */

__extension__ extern __inline int8x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqadd_s8 (int8x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_suqaddv8qi_ssu (__a,  __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqadd_s16 (int16x4_t __a, uint16x4_t __b)
{
  return __builtin_aarch64_suqaddv4hi_ssu (__a,  __b);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqadd_s32 (int32x2_t __a, uint32x2_t __b)
{
  return __builtin_aarch64_suqaddv2si_ssu (__a,  __b);
}

__extension__ extern __inline int64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqadd_s64 (int64x1_t __a, uint64x1_t __b)
{
  return (int64x1_t) {__builtin_aarch64_suqadddi_ssu (__a[0], __b[0])};
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqaddq_s8 (int8x16_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_suqaddv16qi_ssu (__a,  __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqaddq_s16 (int16x8_t __a, uint16x8_t __b)
{
  return __builtin_aarch64_suqaddv8hi_ssu (__a,  __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqaddq_s32 (int32x4_t __a, uint32x4_t __b)
{
  return __builtin_aarch64_suqaddv4si_ssu (__a,  __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqaddq_s64 (int64x2_t __a, uint64x2_t __b)
{
  return __builtin_aarch64_suqaddv2di_ssu (__a,  __b);
}

__extension__ extern __inline int8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqaddb_s8 (int8_t __a, uint8_t __b)
{
  return __builtin_aarch64_suqaddqi_ssu (__a,  __b);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqaddh_s16 (int16_t __a, uint16_t __b)
{
  return __builtin_aarch64_suqaddhi_ssu (__a,  __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqadds_s32 (int32_t __a, uint32_t __b)
{
  return __builtin_aarch64_suqaddsi_ssu (__a,  __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vuqaddd_s64 (int64_t __a, uint64_t __b)
{
  return __builtin_aarch64_suqadddi_ssu (__a,  __b);
}

#pragma GCC pop_options

/* ARMv8.2-A FP16 intrinsics.  */

#include "arm_fp16.h"

#pragma GCC push_options
#pragma GCC target ("+nothing+simd+fp16")

/* ARMv8.2-A FP16 one operand vector intrinsics.  */

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqz_f16 (float16x4_t __a)
{
  return __builtin_aarch64_cmeqv4hf_uss (__a, vdup_n_f16 (0.0f));
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqzq_f16 (float16x8_t __a)
{
  return __builtin_aarch64_cmeqv8hf_uss (__a, vdupq_n_f16 (0.0f));
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgez_f16 (float16x4_t __a)
{
  return __builtin_aarch64_cmgev4hf_uss (__a, vdup_n_f16 (0.0f));
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgezq_f16 (float16x8_t __a)
{
  return __builtin_aarch64_cmgev8hf_uss (__a, vdupq_n_f16 (0.0f));
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtz_f16 (float16x4_t __a)
{
  return __builtin_aarch64_cmgtv4hf_uss (__a, vdup_n_f16 (0.0f));
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtzq_f16 (float16x8_t __a)
{
  return __builtin_aarch64_cmgtv8hf_uss (__a, vdupq_n_f16 (0.0f));
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclez_f16 (float16x4_t __a)
{
  return __builtin_aarch64_cmlev4hf_uss (__a, vdup_n_f16 (0.0f));
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclezq_f16 (float16x8_t __a)
{
  return __builtin_aarch64_cmlev8hf_uss (__a, vdupq_n_f16 (0.0f));
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltz_f16 (float16x4_t __a)
{
  return __builtin_aarch64_cmltv4hf_uss (__a, vdup_n_f16 (0.0f));
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltzq_f16 (float16x8_t __a)
{
  return __builtin_aarch64_cmltv8hf_uss (__a, vdupq_n_f16 (0.0f));
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f16_s16 (int16x4_t __a)
{
  return __builtin_aarch64_floatv4hiv4hf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_f16_s16 (int16x8_t __a)
{
  return __builtin_aarch64_floatv8hiv8hf (__a);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f16_u16 (uint16x4_t __a)
{
  return __builtin_aarch64_floatunsv4hiv4hf ((int16x4_t) __a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_f16_u16 (uint16x8_t __a)
{
  return __builtin_aarch64_floatunsv8hiv8hf ((int16x8_t) __a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_s16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lbtruncv4hfv4hi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_s16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lbtruncv8hfv8hi (__a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_u16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lbtruncuv4hfv4hi_us (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_u16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lbtruncuv8hfv8hi_us (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvta_s16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lroundv4hfv4hi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtaq_s16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lroundv8hfv8hi (__a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvta_u16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lrounduv4hfv4hi_us (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtaq_u16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lrounduv8hfv8hi_us (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtm_s16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lfloorv4hfv4hi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmq_s16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lfloorv8hfv8hi (__a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtm_u16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lflooruv4hfv4hi_us (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtmq_u16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lflooruv8hfv8hi_us (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtn_s16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lfrintnv4hfv4hi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnq_s16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lfrintnv8hfv8hi (__a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtn_u16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lfrintnuv4hfv4hi_us (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtnq_u16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lfrintnuv8hfv8hi_us (__a);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtp_s16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lceilv4hfv4hi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpq_s16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lceilv8hfv8hi (__a);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtp_u16_f16 (float16x4_t __a)
{
  return __builtin_aarch64_lceiluv4hfv4hi_us (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtpq_u16_f16 (float16x8_t __a)
{
  return __builtin_aarch64_lceiluv8hfv8hi_us (__a);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpe_f16 (float16x4_t __a)
{
  return __builtin_aarch64_frecpev4hf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpeq_f16 (float16x8_t __a)
{
  return __builtin_aarch64_frecpev8hf (__a);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrte_f16 (float16x4_t __a)
{
  return __builtin_aarch64_rsqrtev4hf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrteq_f16 (float16x8_t __a)
{
  return __builtin_aarch64_rsqrtev8hf (__a);
}

/* ARMv8.2-A FP16 two operands vector intrinsics.  */

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcage_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_facgev4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcageq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_facgev8hf_uss (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcagt_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_facgtv4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcagtq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_facgtv8hf_uss (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcale_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_faclev4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaleq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_faclev8hf_uss (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcalt_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_facltv4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaltq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_facltv8hf_uss (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceq_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_cmeqv4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vceqq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_cmeqv8hf_uss (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcge_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_cmgev4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgeq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_cmgev8hf_uss (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgt_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_cmgtv4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcgtq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_cmgtv8hf_uss (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcle_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_cmlev4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcleq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_cmlev8hf_uss (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vclt_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_cmltv4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcltq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_cmltv8hf_uss (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_f16_s16 (int16x4_t __a, const int __b)
{
  return __builtin_aarch64_scvtfv4hi (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_f16_s16 (int16x8_t __a, const int __b)
{
  return __builtin_aarch64_scvtfv8hi (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_f16_u16 (uint16x4_t __a, const int __b)
{
  return __builtin_aarch64_ucvtfv4hi_sus (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_f16_u16 (uint16x8_t __a, const int __b)
{
  return __builtin_aarch64_ucvtfv8hi_sus (__a, __b);
}

__extension__ extern __inline int16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_s16_f16 (float16x4_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzsv4hf (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_s16_f16 (float16x8_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzsv8hf (__a, __b);
}

__extension__ extern __inline uint16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_n_u16_f16 (float16x4_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzuv4hf_uss (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_n_u16_f16 (float16x8_t __a, const int __b)
{
  return __builtin_aarch64_fcvtzuv8hf_uss (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fmulxv4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fmulxv8hf (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpadd_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_faddpv4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpaddq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_faddpv8hf (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmax_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_smax_nanpv4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_smax_nanpv8hf (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxnm_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_smaxpv4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmaxnmq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_smaxpv8hf (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpmin_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_smin_nanpv4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_smin_nanpv8hf (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminnm_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_sminpv4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vpminnmq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_sminpv8hf (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecps_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_frecpsv4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrecpsq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_frecpsv8hf (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrts_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_rsqrtsv4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrsqrtsq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_rsqrtsv8hf (__a, __b);
}

/* ARMv8.2-A FP16 lane vector intrinsics.  */

__extension__ extern __inline float16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxh_lane_f16 (float16_t __a, float16x4_t __b, const int __lane)
{
  return vmulxh_f16 (__a, __aarch64_vget_lane_any (__b, __lane));
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_lane_f16 (float16x4_t __a, float16x4_t __b, const int __lane)
{
  return vmulx_f16 (__a, __aarch64_vdup_lane_f16 (__b, __lane));
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_lane_f16 (float16x8_t __a, float16x4_t __b, const int __lane)
{
  return vmulxq_f16 (__a, __aarch64_vdupq_lane_f16 (__b, __lane));
}

__extension__ extern __inline float16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxh_laneq_f16 (float16_t __a, float16x8_t __b, const int __lane)
{
  return vmulxh_f16 (__a, __aarch64_vget_lane_any (__b, __lane));
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_laneq_f16 (float16x4_t __a, float16x8_t __b, const int __lane)
{
  return vmulx_f16 (__a, __aarch64_vdup_laneq_f16 (__b, __lane));
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_laneq_f16 (float16x8_t __a, float16x8_t __b, const int __lane)
{
  return vmulxq_f16 (__a, __aarch64_vdupq_laneq_f16 (__b, __lane));
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulx_n_f16 (float16x4_t __a, float16_t __b)
{
  return vmulx_f16 (__a, vdup_n_f16 (__b));
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmulxq_n_f16 (float16x8_t __a, float16_t __b)
{
  return vmulxq_f16 (__a, vdupq_n_f16 (__b));
}

#pragma GCC pop_options

/* AdvSIMD Dot Product intrinsics.  */

#pragma GCC push_options
#pragma GCC target ("+nothing+dotprod")

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdot_u32 (uint32x2_t __r, uint8x8_t __a, uint8x8_t __b)
{
  return __builtin_aarch64_udot_prodv8qi_uuuu (__a, __b, __r);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdotq_u32 (uint32x4_t __r, uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_udot_prodv16qi_uuuu (__a, __b, __r);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdot_s32 (int32x2_t __r, int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_sdot_prodv8qi (__a, __b, __r);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdotq_s32 (int32x4_t __r, int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_sdot_prodv16qi (__a, __b, __r);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdot_lane_u32 (uint32x2_t __r, uint8x8_t __a, uint8x8_t __b, const int __index)
{
  return __builtin_aarch64_udot_lanev8qi_uuuus (__r, __a, __b, __index);
}

__extension__ extern __inline uint32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdot_laneq_u32 (uint32x2_t __r, uint8x8_t __a, uint8x16_t __b,
		const int __index)
{
  return __builtin_aarch64_udot_laneqv8qi_uuuus (__r, __a, __b, __index);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdotq_lane_u32 (uint32x4_t __r, uint8x16_t __a, uint8x8_t __b,
		const int __index)
{
  return __builtin_aarch64_udot_lanev16qi_uuuus (__r, __a, __b, __index);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdotq_laneq_u32 (uint32x4_t __r, uint8x16_t __a, uint8x16_t __b,
		 const int __index)
{
  return __builtin_aarch64_udot_laneqv16qi_uuuus (__r, __a, __b, __index);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdot_lane_s32 (int32x2_t __r, int8x8_t __a, int8x8_t __b, const int __index)
{
  return __builtin_aarch64_sdot_lanev8qi (__r, __a, __b, __index);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdot_laneq_s32 (int32x2_t __r, int8x8_t __a, int8x16_t __b, const int __index)
{
  return __builtin_aarch64_sdot_laneqv8qi (__r, __a, __b, __index);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdotq_lane_s32 (int32x4_t __r, int8x16_t __a, int8x8_t __b, const int __index)
{
  return __builtin_aarch64_sdot_lanev16qi (__r, __a, __b, __index);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdotq_laneq_s32 (int32x4_t __r, int8x16_t __a, int8x16_t __b, const int __index)
{
  return __builtin_aarch64_sdot_laneqv16qi (__r, __a, __b, __index);
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("+nothing+sha3")

#pragma GCC pop_options

/* AdvSIMD Complex numbers intrinsics.  */

#pragma GCC push_options
#pragma GCC target ("+nothing+fcma")

#pragma GCC push_options
#pragma GCC target ("+fp16")
__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcadd_rot90_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fcadd90v4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaddq_rot90_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fcadd90v8hf (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcadd_rot270_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fcadd270v4hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaddq_rot270_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fcadd270v8hf (__a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fcmla0v4hf (__r, __a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fcmla0v8hf (__r, __a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_lane_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b,
		const int __index)
{
  return __builtin_aarch64_fcmla_lane0v4hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_laneq_f16 (float16x4_t __r, float16x4_t __a, float16x8_t __b,
		 const int __index)
{
  return __builtin_aarch64_fcmla_laneq0v4hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_lane_f16 (float16x8_t __r, float16x8_t __a, float16x4_t __b,
		 const int __index)
{
  return __builtin_aarch64_fcmlaq_lane0v8hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot90_lane_f16 (float16x8_t __r, float16x8_t __a, float16x4_t __b,
		       const int __index)
{
  return __builtin_aarch64_fcmlaq_lane90v8hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot90_laneq_f16 (float16x4_t __r, float16x4_t __a, float16x8_t __b,
		       const int __index)
{
  return __builtin_aarch64_fcmla_laneq90v4hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot90_lane_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b,
		      const int __index)
{
  return __builtin_aarch64_fcmla_lane90v4hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot90_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fcmla90v8hf (__r, __a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot90_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fcmla90v4hf (__r, __a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_laneq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b,
		  const int __index)
{
  return __builtin_aarch64_fcmla_lane0v8hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot180_laneq_f16 (float16x4_t __r, float16x4_t __a, float16x8_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmla_laneq180v4hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot180_lane_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b,
		       const int __index)
{
  return __builtin_aarch64_fcmla_lane180v4hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot180_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fcmla180v8hf (__r, __a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot180_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fcmla180v4hf (__r, __a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot90_laneq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmla_lane90v8hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot270_laneq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b,
			 const int __index)
{
  return __builtin_aarch64_fcmla_lane270v8hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot270_lane_f16 (float16x8_t __r, float16x8_t __a, float16x4_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmlaq_lane270v8hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot270_laneq_f16 (float16x4_t __r, float16x4_t __a, float16x8_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmla_laneq270v4hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot270_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fcmla270v8hf (__r, __a, __b);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot270_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fcmla270v4hf (__r, __a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot180_laneq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b,
			 const int __index)
{
  return __builtin_aarch64_fcmla_lane180v8hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot180_lane_f16 (float16x8_t __r, float16x8_t __a, float16x4_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmlaq_lane180v8hf (__r, __a, __b, __index);
}

__extension__ extern __inline float16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot270_lane_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b,
		       const int __index)
{
  return __builtin_aarch64_fcmla_lane270v4hf (__r, __a, __b, __index);
}
#pragma GCC pop_options

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcadd_rot90_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_fcadd90v2sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaddq_rot90_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_fcadd90v4sf (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaddq_rot90_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_fcadd90v2df (__a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcadd_rot270_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_fcadd270v2sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaddq_rot270_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_fcadd270v4sf (__a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcaddq_rot270_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_fcadd270v2df (__a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_fcmla0v2sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_fcmla0v4sf (__r, __a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_f64 (float64x2_t __r, float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_fcmla0v2df (__r, __a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_lane_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b,
		const int __index)
{
  return __builtin_aarch64_fcmla_lane0v2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_laneq_f32 (float32x2_t __r, float32x2_t __a, float32x4_t __b,
		 const int __index)
{
  return __builtin_aarch64_fcmla_laneq0v2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_lane_f32 (float32x4_t __r, float32x4_t __a, float32x2_t __b,
		 const int __index)
{
  return __builtin_aarch64_fcmlaq_lane0v4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_laneq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b,
		  const int __index)
{
  return __builtin_aarch64_fcmla_lane0v4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot90_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_fcmla90v2sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot90_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_fcmla90v4sf (__r, __a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot90_f64 (float64x2_t __r, float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_fcmla90v2df (__r, __a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot90_lane_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b,
		      const int __index)
{
  return __builtin_aarch64_fcmla_lane90v2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot90_laneq_f32 (float32x2_t __r, float32x2_t __a, float32x4_t __b,
		       const int __index)
{
  return __builtin_aarch64_fcmla_laneq90v2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot90_lane_f32 (float32x4_t __r, float32x4_t __a, float32x2_t __b,
		       const int __index)
{
  return __builtin_aarch64_fcmlaq_lane90v4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot90_laneq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmla_lane90v4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot180_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_fcmla180v2sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot180_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_fcmla180v4sf (__r, __a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot180_f64 (float64x2_t __r, float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_fcmla180v2df (__r, __a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot180_lane_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b,
		       const int __index)
{
  return __builtin_aarch64_fcmla_lane180v2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot180_laneq_f32 (float32x2_t __r, float32x2_t __a, float32x4_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmla_laneq180v2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot180_lane_f32 (float32x4_t __r, float32x4_t __a, float32x2_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmlaq_lane180v4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot180_laneq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b,
			 const int __index)
{
  return __builtin_aarch64_fcmla_lane180v4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot270_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_fcmla270v2sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot270_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_fcmla270v4sf (__r, __a, __b);
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot270_f64 (float64x2_t __r, float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_fcmla270v2df (__r, __a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot270_lane_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b,
		       const int __index)
{
  return __builtin_aarch64_fcmla_lane270v2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmla_rot270_laneq_f32 (float32x2_t __r, float32x2_t __a, float32x4_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmla_laneq270v2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot270_lane_f32 (float32x4_t __r, float32x4_t __a, float32x2_t __b,
			const int __index)
{
  return __builtin_aarch64_fcmlaq_lane270v4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcmlaq_rot270_laneq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b,
			 const int __index)
{
  return __builtin_aarch64_fcmla_lane270v4sf (__r, __a, __b, __index);
}

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("+nothing+fp16fml")

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlal_low_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fmlal_lowv2sf (__r, __a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlsl_low_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fmlsl_lowv2sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlalq_low_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fmlalq_lowv4sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlslq_low_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fmlslq_lowv4sf (__r, __a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlal_high_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fmlal_highv2sf (__r, __a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlsl_high_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_fmlsl_highv2sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlalq_high_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fmlalq_highv4sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlslq_high_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_fmlslq_highv4sf (__r, __a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlal_lane_low_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b,
		     const int __lane)
{
  return __builtin_aarch64_fmlal_lane_lowv2sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlsl_lane_low_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b,
		     const int __lane)
{
  return __builtin_aarch64_fmlsl_lane_lowv2sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlal_laneq_low_f16 (float32x2_t __r, float16x4_t __a, float16x8_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlal_laneq_lowv2sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlsl_laneq_low_f16 (float32x2_t __r, float16x4_t __a, float16x8_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlsl_laneq_lowv2sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlalq_lane_low_f16 (float32x4_t __r, float16x8_t __a, float16x4_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlalq_lane_lowv4sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlslq_lane_low_f16 (float32x4_t __r, float16x8_t __a, float16x4_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlslq_lane_lowv4sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlalq_laneq_low_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b,
		       const int __lane)
{
  return __builtin_aarch64_fmlalq_laneq_lowv4sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlslq_laneq_low_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlslq_laneq_lowv4sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlal_lane_high_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b,
		     const int __lane)
{
  return __builtin_aarch64_fmlal_lane_highv2sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlsl_lane_high_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b,
		     const int __lane)
{
  return __builtin_aarch64_fmlsl_lane_highv2sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlal_laneq_high_f16 (float32x2_t __r, float16x4_t __a, float16x8_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlal_laneq_highv2sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlsl_laneq_high_f16 (float32x2_t __r, float16x4_t __a, float16x8_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlsl_laneq_highv2sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlalq_lane_high_f16 (float32x4_t __r, float16x8_t __a, float16x4_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlalq_lane_highv4sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlslq_lane_high_f16 (float32x4_t __r, float16x8_t __a, float16x4_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlslq_lane_highv4sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlalq_laneq_high_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b,
		       const int __lane)
{
  return __builtin_aarch64_fmlalq_laneq_highv4sf (__r, __a, __b, __lane);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vfmlslq_laneq_high_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b,
		      const int __lane)
{
  return __builtin_aarch64_fmlslq_laneq_highv4sf (__r, __a, __b, __lane);
}

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("+nothing+simd+frintts")

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd32z_f32 (float32x2_t __a)
{
  return __builtin_aarch64_frint32zv2sf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd32zq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_frint32zv4sf (__a);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd32z_f64 (float64x1_t __a)
{
  return (float64x1_t)
	   {__builtin_aarch64_frint32zdf (vget_lane_f64 (__a, 0))};
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd32zq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_frint32zv2df (__a);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd32x_f32 (float32x2_t __a)
{
  return __builtin_aarch64_frint32xv2sf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd32xq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_frint32xv4sf (__a);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd32x_f64 (float64x1_t __a)
{
  return (float64x1_t) {__builtin_aarch64_frint32xdf (vget_lane_f64 (__a, 0))};
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd32xq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_frint32xv2df (__a);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd64z_f32 (float32x2_t __a)
{
  return __builtin_aarch64_frint64zv2sf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd64zq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_frint64zv4sf (__a);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd64z_f64 (float64x1_t __a)
{
  return (float64x1_t) {__builtin_aarch64_frint64zdf (vget_lane_f64 (__a, 0))};
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd64zq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_frint64zv2df (__a);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd64x_f32 (float32x2_t __a)
{
  return __builtin_aarch64_frint64xv2sf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd64xq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_frint64xv4sf (__a);
}

__extension__ extern __inline float64x1_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd64x_f64 (float64x1_t __a)
{
  return (float64x1_t) {__builtin_aarch64_frint64xdf (vget_lane_f64 (__a, 0))};
}

__extension__ extern __inline float64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vrnd64xq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_frint64xv2df (__a);
}

#pragma GCC pop_options

#include "arm_bf16.h"

#pragma GCC push_options
#pragma GCC target ("+nothing+bf16")

/* vld */

__extension__ extern __inline bfloat16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_bf16 (const bfloat16_t *__a)
{
  return (bfloat16x4_t) __builtin_aarch64_ld1v4bf (__a);
}

__extension__ extern __inline bfloat16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_bf16 (const bfloat16_t *__a)
{
  return __builtin_aarch64_ld1v8bf (__a);
}

__extension__ extern __inline bfloat16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_bf16_x2 (const bfloat16_t *__a)
{
  return __builtin_aarch64_ld1x2v4bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_bf16_x2 (const bfloat16_t *__a)
{
  return  __builtin_aarch64_ld1x2v8bf (
				(const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_bf16_x3 (const bfloat16_t *__a)
{
  return __builtin_aarch64_ld1x3v4bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_bf16_x3 (const bfloat16_t *__a)
{
  return __builtin_aarch64_ld1x3v8bf ((const __builtin_aarch64_simd_bf *) __a);
}
__extension__ extern __inline bfloat16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_bf16_x4 (const bfloat16_t *__a)
{
  return __builtin_aarch64_ld1x4v4bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_bf16_x4 (const bfloat16_t *__a)
{
  return __builtin_aarch64_ld1x4v8bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_lane_bf16 (const bfloat16_t *__src, bfloat16x4_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline bfloat16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_lane_bf16 (const bfloat16_t *__src, bfloat16x8_t __vec, const int __lane)
{
  return __aarch64_vset_lane_any (*__src, __vec, __lane);
}

__extension__ extern __inline bfloat16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1_dup_bf16 (const bfloat16_t* __a)
{
  return vdup_n_bf16 (*__a);
}

__extension__ extern __inline bfloat16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld1q_dup_bf16 (const bfloat16_t* __a)
{
  return vdupq_n_bf16 (*__a);
}

__extension__ extern __inline bfloat16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld2v4bf (__a);
}

__extension__ extern __inline bfloat16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld2v8bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2_dup_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld2rv4bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld2q_dup_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld2rv8bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld3v4bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld3v8bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3_dup_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld3rv4bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld3q_dup_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld3rv8bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld4v4bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld4v8bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4_dup_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld4rv4bf ((const __builtin_aarch64_simd_bf *) __a);
}

__extension__ extern __inline bfloat16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vld4q_dup_bf16 (const bfloat16_t * __a)
{
  return __builtin_aarch64_ld4rv8bf ((const __builtin_aarch64_simd_bf *) __a);
}

/* vst */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_bf16 (bfloat16_t *__a, bfloat16x4_t __b)
{
  __builtin_aarch64_st1v4bf (__a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_bf16_x2 (bfloat16_t * __a, bfloat16x4x2_t __val)
{
  __builtin_aarch64_st1x2v4bf (__a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_bf16_x2 (bfloat16_t * __a, bfloat16x8x2_t __val)
{
  __builtin_aarch64_st1x2v8bf (__a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_bf16_x3 (bfloat16_t * __a, bfloat16x4x3_t __val)
{
  __builtin_aarch64_st1x3v4bf ((__builtin_aarch64_simd_bf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_bf16_x3 (bfloat16_t * __a, bfloat16x8x3_t __val)
{
  __builtin_aarch64_st1x3v8bf ((__builtin_aarch64_simd_bf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_bf16_x4 (bfloat16_t * __a, bfloat16x4x4_t __val)
{
  __builtin_aarch64_st1x4v4bf ((__builtin_aarch64_simd_bf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_bf16_x4 (bfloat16_t * __a, bfloat16x8x4_t __val)
{
  __builtin_aarch64_st1x4v8bf ((__builtin_aarch64_simd_bf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_bf16 (bfloat16_t *__a, bfloat16x8_t __b)
{
  __builtin_aarch64_st1v8bf (__a, __b);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1_lane_bf16 (bfloat16_t *__a, bfloat16x4_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst1q_lane_bf16 (bfloat16_t *__a, bfloat16x8_t __b, const int __lane)
{
  *__a = __aarch64_vget_lane_any (__b, __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_bf16 (bfloat16_t * __a, bfloat16x4x2_t __val)
{
  __builtin_aarch64_st2v4bf (__a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_bf16 (bfloat16_t * __a, bfloat16x8x2_t __val)
{
  __builtin_aarch64_st2v8bf (__a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_bf16 (bfloat16_t * __a, bfloat16x4x3_t __val)
{
  __builtin_aarch64_st3v4bf ((__builtin_aarch64_simd_bf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_bf16 (bfloat16_t * __a, bfloat16x8x3_t __val)
{
  __builtin_aarch64_st3v8bf ((__builtin_aarch64_simd_bf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_bf16 (bfloat16_t * __a, bfloat16x4x4_t __val)
{
  __builtin_aarch64_st4v4bf ((__builtin_aarch64_simd_bf *) __a, __val);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_bf16 (bfloat16_t * __a, bfloat16x8x4_t __val)
{
  __builtin_aarch64_st4v8bf ((__builtin_aarch64_simd_bf *) __a, __val);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfdot_f32 (float32x2_t __r, bfloat16x4_t __a, bfloat16x4_t __b)
{
  return __builtin_aarch64_bfdotv2sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfdotq_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x8_t __b)
{
  return __builtin_aarch64_bfdotv4sf (__r, __a, __b);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfdot_lane_f32 (float32x2_t __r, bfloat16x4_t __a, bfloat16x4_t __b,
		 const int __index)
{
  return __builtin_aarch64_bfdot_lanev2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfdotq_lane_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x4_t __b,
		  const int __index)
{
  return __builtin_aarch64_bfdot_lanev4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfdot_laneq_f32 (float32x2_t __r, bfloat16x4_t __a, bfloat16x8_t __b,
		  const int __index)
{
  return __builtin_aarch64_bfdot_laneqv2sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfdotq_laneq_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x8_t __b,
		   const int __index)
{
  return __builtin_aarch64_bfdot_laneqv4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfmmlaq_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x8_t __b)

{
  return __builtin_aarch64_bfmmlaqv4sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfmlalbq_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x8_t __b)
{
  return __builtin_aarch64_bfmlalbv4sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfmlaltq_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x8_t __b)
{
  return __builtin_aarch64_bfmlaltv4sf (__r, __a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfmlalbq_lane_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x4_t __b,
		    const int __index)
{
  return __builtin_aarch64_bfmlalb_lanev4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfmlaltq_lane_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x4_t __b,
		    const int __index)
{
  return __builtin_aarch64_bfmlalt_lanev4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfmlalbq_laneq_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x8_t __b,
		     const int __index)
{
  return __builtin_aarch64_bfmlalb_lane_qv4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vbfmlaltq_laneq_f32 (float32x4_t __r, bfloat16x8_t __a, bfloat16x8_t __b,
		     const int __index)
{
  return __builtin_aarch64_bfmlalt_lane_qv4sf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_f32_bf16 (bfloat16x4_t __a)
{
  return __builtin_aarch64_vbfcvtv4bf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_low_f32_bf16 (bfloat16x8_t __a)
{
  return __builtin_aarch64_vbfcvtv8bf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_high_f32_bf16 (bfloat16x8_t __a)
{
  return __builtin_aarch64_vbfcvt_highv8bf (__a);
}

__extension__ extern __inline bfloat16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvt_bf16_f32 (float32x4_t __a)
{
  return __builtin_aarch64_bfcvtnv4bf (__a);
}

__extension__ extern __inline bfloat16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_low_bf16_f32 (float32x4_t __a)
{
  return __builtin_aarch64_bfcvtn_qv8bf (__a);
}

__extension__ extern __inline bfloat16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtq_high_bf16_f32 (bfloat16x8_t __inactive, float32x4_t __a)
{
  return __builtin_aarch64_bfcvtn2v8bf (__inactive, __a);
}

__extension__ extern __inline bfloat16x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2_lane_bf16 (const bfloat16_t * __ptr, bfloat16x4x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev4bf (
	  (__builtin_aarch64_simd_bf *) __ptr, __b, __c);
}

__extension__ extern __inline bfloat16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld2q_lane_bf16 (const bfloat16_t * __ptr, bfloat16x8x2_t __b, const int __c)
{
  return __builtin_aarch64_ld2_lanev8bf (
	  (__builtin_aarch64_simd_bf *) __ptr, __b, __c);
}

__extension__ extern __inline bfloat16x4x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3_lane_bf16 (const bfloat16_t * __ptr, bfloat16x4x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev4bf (
	  (__builtin_aarch64_simd_bf *) __ptr, __b, __c);
}

__extension__ extern __inline bfloat16x8x3_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld3q_lane_bf16 (const bfloat16_t * __ptr, bfloat16x8x3_t __b, const int __c)
{
  return __builtin_aarch64_ld3_lanev8bf (
	  (__builtin_aarch64_simd_bf *) __ptr, __b, __c);
}

__extension__ extern __inline bfloat16x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4_lane_bf16 (const bfloat16_t * __ptr, bfloat16x4x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev4bf (
	  (__builtin_aarch64_simd_bf *) __ptr, __b, __c);
}

__extension__ extern __inline bfloat16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__,__artificial__))
vld4q_lane_bf16 (const bfloat16_t * __ptr, bfloat16x8x4_t __b, const int __c)
{
  return __builtin_aarch64_ld4_lanev8bf (
	  (__builtin_aarch64_simd_bf *) __ptr, __b, __c);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2_lane_bf16 (bfloat16_t *__ptr, bfloat16x4x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev4bf ((__builtin_aarch64_simd_bf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst2q_lane_bf16 (bfloat16_t *__ptr, bfloat16x8x2_t __val, const int __lane)
{
  __builtin_aarch64_st2_lanev8bf ((__builtin_aarch64_simd_bf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3_lane_bf16 (bfloat16_t *__ptr, bfloat16x4x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev4bf ((__builtin_aarch64_simd_bf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst3q_lane_bf16 (bfloat16_t *__ptr, bfloat16x8x3_t __val, const int __lane)
{
  __builtin_aarch64_st3_lanev8bf ((__builtin_aarch64_simd_bf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4_lane_bf16 (bfloat16_t *__ptr, bfloat16x4x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev4bf ((__builtin_aarch64_simd_bf *) __ptr, __val,
				  __lane);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vst4q_lane_bf16 (bfloat16_t *__ptr, bfloat16x8x4_t __val, const int __lane)
{
  __builtin_aarch64_st4_lanev8bf ((__builtin_aarch64_simd_bf *) __ptr, __val,
				  __lane);
}

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("+nothing+f16f32dot")

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdot_f32_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b)
{
  return __builtin_aarch64_sdot_prodv4hf (__a, __b, __r);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdotq_f32_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b)
{
  return __builtin_aarch64_sdot_prodv8hf (__a, __b, __r);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdot_lane_f32_f16 (float32x2_t __r, float16x4_t __a, float16x4_t __b,
		 const int __index)
{
  return __builtin_aarch64_sdot_lanev4hf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdot_laneq_f32_f16 (float32x2_t __r, float16x4_t __a, float16x8_t __b,
		  const int __index)
{
  return __builtin_aarch64_sdot_laneqv4hf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdotq_lane_f32_f16 (float32x4_t __r, float16x8_t __a, float16x4_t __b,
		  const int __index)
{
  return __builtin_aarch64_sdot_lanev8hf (__r, __a, __b, __index);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vdotq_laneq_f32_f16 (float32x4_t __r, float16x8_t __a, float16x8_t __b,
		  const int __index)
{
  return __builtin_aarch64_sdot_laneqv8hf (__r, __a, __b, __index);
}

#pragma GCC pop_options
/* AdvSIMD 8-bit Integer Matrix Multiply (I8MM) intrinsics.  */

#pragma GCC push_options
#pragma GCC target ("+nothing+i8mm")

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vusdot_s32 (int32x2_t __r, uint8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_usdot_prodv8qi_suss (__a, __b, __r);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vusdotq_s32 (int32x4_t __r, uint8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_usdot_prodv16qi_suss (__a, __b, __r);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vusdot_lane_s32 (int32x2_t __r, uint8x8_t __a, int8x8_t __b, const int __index)
{
  return __builtin_aarch64_usdot_lanev8qi_ssuss (__r, __a, __b, __index);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vusdot_laneq_s32 (int32x2_t __r, uint8x8_t __a, int8x16_t __b,
		  const int __index)
{
  return __builtin_aarch64_usdot_laneqv8qi_ssuss (__r, __a, __b, __index);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vusdotq_lane_s32 (int32x4_t __r, uint8x16_t __a, int8x8_t __b,
		  const int __index)
{
  return __builtin_aarch64_usdot_lanev16qi_ssuss (__r, __a, __b, __index);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vusdotq_laneq_s32 (int32x4_t __r, uint8x16_t __a, int8x16_t __b,
		   const int __index)
{
  return __builtin_aarch64_usdot_laneqv16qi_ssuss (__r, __a, __b, __index);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsudot_lane_s32 (int32x2_t __r, int8x8_t __a, uint8x8_t __b, const int __index)
{
  return __builtin_aarch64_sudot_lanev8qi_sssus (__r, __a, __b, __index);
}

__extension__ extern __inline int32x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsudot_laneq_s32 (int32x2_t __r, int8x8_t __a, uint8x16_t __b,
		  const int __index)
{
  return __builtin_aarch64_sudot_laneqv8qi_sssus (__r, __a, __b, __index);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsudotq_lane_s32 (int32x4_t __r, int8x16_t __a, uint8x8_t __b,
		  const int __index)
{
  return __builtin_aarch64_sudot_lanev16qi_sssus (__r, __a, __b, __index);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vsudotq_laneq_s32 (int32x4_t __r, int8x16_t __a, uint8x16_t __b,
		   const int __index)
{
  return __builtin_aarch64_sudot_laneqv16qi_sssus (__r, __a, __b, __index);
}

/* Matrix Multiply-Accumulate.  */

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmmlaq_s32 (int32x4_t __r, int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_simd_smmlav16qi (__r, __a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vmmlaq_u32 (uint32x4_t __r, uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_aarch64_simd_ummlav16qi_uuuu (__r, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vusmmlaq_s32 (int32x4_t __r, uint8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_simd_usmmlav16qi_ssus (__r, __a, __b);
}

#pragma GCC pop_options

#undef __aarch64_vget_lane_any

#undef __aarch64_vdup_lane_any
#undef __aarch64_vdup_lane_f16
#undef __aarch64_vdup_lane_f32
#undef __aarch64_vdup_lane_f64
#undef __aarch64_vdup_laneq_f16
#undef __aarch64_vdup_laneq_f32
#undef __aarch64_vdup_laneq_f64
#undef __aarch64_vdupq_lane_f16
#undef __aarch64_vdupq_lane_f32
#undef __aarch64_vdupq_lane_f64
#undef __aarch64_vdupq_laneq_f16
#undef __aarch64_vdupq_laneq_f32
#undef __aarch64_vdupq_laneq_f64

#endif
