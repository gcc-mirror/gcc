/* ARM NEON intrinsics include file.

   Copyright (C) 2011-2013 Free Software Foundation, Inc.
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

#include <stdint.h>

#define __AARCH64_UINT64_C(__C) ((uint64_t) __C)
#define __AARCH64_INT64_C(__C) ((int64_t) __C)

typedef __builtin_aarch64_simd_qi int8x8_t
  __attribute__ ((__vector_size__ (8)));
typedef __builtin_aarch64_simd_hi int16x4_t
  __attribute__ ((__vector_size__ (8)));
typedef __builtin_aarch64_simd_si int32x2_t
  __attribute__ ((__vector_size__ (8)));
typedef int64_t int64x1_t;
typedef int32_t int32x1_t;
typedef int16_t int16x1_t;
typedef int8_t int8x1_t;
typedef double float64x1_t;
typedef __builtin_aarch64_simd_sf float32x2_t
  __attribute__ ((__vector_size__ (8)));
typedef __builtin_aarch64_simd_poly8 poly8x8_t
  __attribute__ ((__vector_size__ (8)));
typedef __builtin_aarch64_simd_poly16 poly16x4_t
  __attribute__ ((__vector_size__ (8)));
typedef __builtin_aarch64_simd_uqi uint8x8_t
  __attribute__ ((__vector_size__ (8)));
typedef __builtin_aarch64_simd_uhi uint16x4_t
  __attribute__ ((__vector_size__ (8)));
typedef __builtin_aarch64_simd_usi uint32x2_t
  __attribute__ ((__vector_size__ (8)));
typedef uint64_t uint64x1_t;
typedef uint32_t uint32x1_t;
typedef uint16_t uint16x1_t;
typedef uint8_t uint8x1_t;
typedef __builtin_aarch64_simd_qi int8x16_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_hi int16x8_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_si int32x4_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_di int64x2_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_sf float32x4_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_df float64x2_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_poly8 poly8x16_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_poly16 poly16x8_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_uqi uint8x16_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_uhi uint16x8_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_usi uint32x4_t
  __attribute__ ((__vector_size__ (16)));
typedef __builtin_aarch64_simd_udi uint64x2_t
  __attribute__ ((__vector_size__ (16)));

typedef float float32_t;
typedef double float64_t;
typedef __builtin_aarch64_simd_poly8 poly8_t;
typedef __builtin_aarch64_simd_poly16 poly16_t;

typedef struct int8x8x2_t
{
  int8x8_t val[2];
} int8x8x2_t;

typedef struct int8x16x2_t
{
  int8x16_t val[2];
} int8x16x2_t;

typedef struct int16x4x2_t
{
  int16x4_t val[2];
} int16x4x2_t;

typedef struct int16x8x2_t
{
  int16x8_t val[2];
} int16x8x2_t;

typedef struct int32x2x2_t
{
  int32x2_t val[2];
} int32x2x2_t;

typedef struct int32x4x2_t
{
  int32x4_t val[2];
} int32x4x2_t;

typedef struct int64x1x2_t
{
  int64x1_t val[2];
} int64x1x2_t;

typedef struct int64x2x2_t
{
  int64x2_t val[2];
} int64x2x2_t;

typedef struct uint8x8x2_t
{
  uint8x8_t val[2];
} uint8x8x2_t;

typedef struct uint8x16x2_t
{
  uint8x16_t val[2];
} uint8x16x2_t;

typedef struct uint16x4x2_t
{
  uint16x4_t val[2];
} uint16x4x2_t;

typedef struct uint16x8x2_t
{
  uint16x8_t val[2];
} uint16x8x2_t;

typedef struct uint32x2x2_t
{
  uint32x2_t val[2];
} uint32x2x2_t;

typedef struct uint32x4x2_t
{
  uint32x4_t val[2];
} uint32x4x2_t;

typedef struct uint64x1x2_t
{
  uint64x1_t val[2];
} uint64x1x2_t;

typedef struct uint64x2x2_t
{
  uint64x2_t val[2];
} uint64x2x2_t;

typedef struct float32x2x2_t
{
  float32x2_t val[2];
} float32x2x2_t;

typedef struct float32x4x2_t
{
  float32x4_t val[2];
} float32x4x2_t;

typedef struct float64x2x2_t
{
  float64x2_t val[2];
} float64x2x2_t;

typedef struct float64x1x2_t
{
  float64x1_t val[2];
} float64x1x2_t;

typedef struct poly8x8x2_t
{
  poly8x8_t val[2];
} poly8x8x2_t;

typedef struct poly8x16x2_t
{
  poly8x16_t val[2];
} poly8x16x2_t;

typedef struct poly16x4x2_t
{
  poly16x4_t val[2];
} poly16x4x2_t;

typedef struct poly16x8x2_t
{
  poly16x8_t val[2];
} poly16x8x2_t;

typedef struct int8x8x3_t
{
  int8x8_t val[3];
} int8x8x3_t;

typedef struct int8x16x3_t
{
  int8x16_t val[3];
} int8x16x3_t;

typedef struct int16x4x3_t
{
  int16x4_t val[3];
} int16x4x3_t;

typedef struct int16x8x3_t
{
  int16x8_t val[3];
} int16x8x3_t;

typedef struct int32x2x3_t
{
  int32x2_t val[3];
} int32x2x3_t;

typedef struct int32x4x3_t
{
  int32x4_t val[3];
} int32x4x3_t;

typedef struct int64x1x3_t
{
  int64x1_t val[3];
} int64x1x3_t;

typedef struct int64x2x3_t
{
  int64x2_t val[3];
} int64x2x3_t;

typedef struct uint8x8x3_t
{
  uint8x8_t val[3];
} uint8x8x3_t;

typedef struct uint8x16x3_t
{
  uint8x16_t val[3];
} uint8x16x3_t;

typedef struct uint16x4x3_t
{
  uint16x4_t val[3];
} uint16x4x3_t;

typedef struct uint16x8x3_t
{
  uint16x8_t val[3];
} uint16x8x3_t;

typedef struct uint32x2x3_t
{
  uint32x2_t val[3];
} uint32x2x3_t;

typedef struct uint32x4x3_t
{
  uint32x4_t val[3];
} uint32x4x3_t;

typedef struct uint64x1x3_t
{
  uint64x1_t val[3];
} uint64x1x3_t;

typedef struct uint64x2x3_t
{
  uint64x2_t val[3];
} uint64x2x3_t;

typedef struct float32x2x3_t
{
  float32x2_t val[3];
} float32x2x3_t;

typedef struct float32x4x3_t
{
  float32x4_t val[3];
} float32x4x3_t;

typedef struct float64x2x3_t
{
  float64x2_t val[3];
} float64x2x3_t;

typedef struct float64x1x3_t
{
  float64x1_t val[3];
} float64x1x3_t;

typedef struct poly8x8x3_t
{
  poly8x8_t val[3];
} poly8x8x3_t;

typedef struct poly8x16x3_t
{
  poly8x16_t val[3];
} poly8x16x3_t;

typedef struct poly16x4x3_t
{
  poly16x4_t val[3];
} poly16x4x3_t;

typedef struct poly16x8x3_t
{
  poly16x8_t val[3];
} poly16x8x3_t;

typedef struct int8x8x4_t
{
  int8x8_t val[4];
} int8x8x4_t;

typedef struct int8x16x4_t
{
  int8x16_t val[4];
} int8x16x4_t;

typedef struct int16x4x4_t
{
  int16x4_t val[4];
} int16x4x4_t;

typedef struct int16x8x4_t
{
  int16x8_t val[4];
} int16x8x4_t;

typedef struct int32x2x4_t
{
  int32x2_t val[4];
} int32x2x4_t;

typedef struct int32x4x4_t
{
  int32x4_t val[4];
} int32x4x4_t;

typedef struct int64x1x4_t
{
  int64x1_t val[4];
} int64x1x4_t;

typedef struct int64x2x4_t
{
  int64x2_t val[4];
} int64x2x4_t;

typedef struct uint8x8x4_t
{
  uint8x8_t val[4];
} uint8x8x4_t;

typedef struct uint8x16x4_t
{
  uint8x16_t val[4];
} uint8x16x4_t;

typedef struct uint16x4x4_t
{
  uint16x4_t val[4];
} uint16x4x4_t;

typedef struct uint16x8x4_t
{
  uint16x8_t val[4];
} uint16x8x4_t;

typedef struct uint32x2x4_t
{
  uint32x2_t val[4];
} uint32x2x4_t;

typedef struct uint32x4x4_t
{
  uint32x4_t val[4];
} uint32x4x4_t;

typedef struct uint64x1x4_t
{
  uint64x1_t val[4];
} uint64x1x4_t;

typedef struct uint64x2x4_t
{
  uint64x2_t val[4];
} uint64x2x4_t;

typedef struct float32x2x4_t
{
  float32x2_t val[4];
} float32x2x4_t;

typedef struct float32x4x4_t
{
  float32x4_t val[4];
} float32x4x4_t;

typedef struct float64x2x4_t
{
  float64x2_t val[4];
} float64x2x4_t;

typedef struct float64x1x4_t
{
  float64x1_t val[4];
} float64x1x4_t;

typedef struct poly8x8x4_t
{
  poly8x8_t val[4];
} poly8x8x4_t;

typedef struct poly8x16x4_t
{
  poly8x16_t val[4];
} poly8x16x4_t;

typedef struct poly16x4x4_t
{
  poly16x4_t val[4];
} poly16x4x4_t;

typedef struct poly16x8x4_t
{
  poly16x8_t val[4];
} poly16x8x4_t;

/* vget_lane internal macros.  */

#define __aarch64_vget_lane_any(__size, __cast_ret, __cast_a, __a, __b) \
  (__cast_ret								\
     __builtin_aarch64_get_lane##__size (__cast_a __a, __b))

#define __aarch64_vget_lane_f32(__a, __b) \
  __aarch64_vget_lane_any (v2sf, , , __a, __b)
#define __aarch64_vget_lane_f64(__a, __b) (__a)

#define __aarch64_vget_lane_p8(__a, __b) \
  __aarch64_vget_lane_any (v8qi, (poly8_t), (int8x8_t), __a, __b)
#define __aarch64_vget_lane_p16(__a, __b) \
  __aarch64_vget_lane_any (v4hi, (poly16_t), (int16x4_t), __a, __b)

#define __aarch64_vget_lane_s8(__a, __b) \
  __aarch64_vget_lane_any (v8qi, , ,__a, __b)
#define __aarch64_vget_lane_s16(__a, __b) \
  __aarch64_vget_lane_any (v4hi, , ,__a, __b)
#define __aarch64_vget_lane_s32(__a, __b) \
  __aarch64_vget_lane_any (v2si, , ,__a, __b)
#define __aarch64_vget_lane_s64(__a, __b) (__a)

#define __aarch64_vget_lane_u8(__a, __b) \
  __aarch64_vget_lane_any (v8qi, (uint8_t), (int8x8_t), __a, __b)
#define __aarch64_vget_lane_u16(__a, __b) \
  __aarch64_vget_lane_any (v4hi, (uint16_t), (int16x4_t), __a, __b)
#define __aarch64_vget_lane_u32(__a, __b) \
  __aarch64_vget_lane_any (v2si, (uint32_t), (int32x2_t), __a, __b)
#define __aarch64_vget_lane_u64(__a, __b) (__a)

#define __aarch64_vgetq_lane_f32(__a, __b) \
  __aarch64_vget_lane_any (v4sf, , , __a, __b)
#define __aarch64_vgetq_lane_f64(__a, __b) \
  __aarch64_vget_lane_any (v2df, , , __a, __b)

#define __aarch64_vgetq_lane_p8(__a, __b) \
  __aarch64_vget_lane_any (v16qi, (poly8_t), (int8x16_t), __a, __b)
#define __aarch64_vgetq_lane_p16(__a, __b) \
  __aarch64_vget_lane_any (v8hi, (poly16_t), (int16x8_t), __a, __b)

#define __aarch64_vgetq_lane_s8(__a, __b) \
  __aarch64_vget_lane_any (v16qi, , ,__a, __b)
#define __aarch64_vgetq_lane_s16(__a, __b) \
  __aarch64_vget_lane_any (v8hi, , ,__a, __b)
#define __aarch64_vgetq_lane_s32(__a, __b) \
  __aarch64_vget_lane_any (v4si, , ,__a, __b)
#define __aarch64_vgetq_lane_s64(__a, __b) \
  __aarch64_vget_lane_any (v2di, , ,__a, __b)

#define __aarch64_vgetq_lane_u8(__a, __b) \
  __aarch64_vget_lane_any (v16qi, (uint8_t), (int8x16_t), __a, __b)
#define __aarch64_vgetq_lane_u16(__a, __b) \
  __aarch64_vget_lane_any (v8hi, (uint16_t), (int16x8_t), __a, __b)
#define __aarch64_vgetq_lane_u32(__a, __b) \
  __aarch64_vget_lane_any (v4si, (uint32_t), (int32x4_t), __a, __b)
#define __aarch64_vgetq_lane_u64(__a, __b) \
  __aarch64_vget_lane_any (v2di, (uint64_t), (int64x2_t), __a, __b)

/* __aarch64_vdup_lane internal macros.  */
#define __aarch64_vdup_lane_any(__size, __q1, __q2, __a, __b) \
  vdup##__q1##_n_##__size (__aarch64_vget##__q2##_lane_##__size (__a, __b))

#define __aarch64_vdup_lane_f32(__a, __b) \
   __aarch64_vdup_lane_any (f32, , , __a, __b)
#define __aarch64_vdup_lane_f64(__a, __b) (__a)
#define __aarch64_vdup_lane_p8(__a, __b) \
   __aarch64_vdup_lane_any (p8, , , __a, __b)
#define __aarch64_vdup_lane_p16(__a, __b) \
   __aarch64_vdup_lane_any (p16, , , __a, __b)
#define __aarch64_vdup_lane_s8(__a, __b) \
   __aarch64_vdup_lane_any (s8, , , __a, __b)
#define __aarch64_vdup_lane_s16(__a, __b) \
   __aarch64_vdup_lane_any (s16, , , __a, __b)
#define __aarch64_vdup_lane_s32(__a, __b) \
   __aarch64_vdup_lane_any (s32, , , __a, __b)
#define __aarch64_vdup_lane_s64(__a, __b) (__a)
#define __aarch64_vdup_lane_u8(__a, __b) \
   __aarch64_vdup_lane_any (u8, , , __a, __b)
#define __aarch64_vdup_lane_u16(__a, __b) \
   __aarch64_vdup_lane_any (u16, , , __a, __b)
#define __aarch64_vdup_lane_u32(__a, __b) \
   __aarch64_vdup_lane_any (u32, , , __a, __b)
#define __aarch64_vdup_lane_u64(__a, __b) (__a)

/* __aarch64_vdup_laneq internal macros.  */
#define __aarch64_vdup_laneq_f32(__a, __b) \
   __aarch64_vdup_lane_any (f32, , q, __a, __b)
#define __aarch64_vdup_laneq_f64(__a, __b) \
   __aarch64_vdup_lane_any (f64, , q, __a, __b)
#define __aarch64_vdup_laneq_p8(__a, __b) \
   __aarch64_vdup_lane_any (p8, , q, __a, __b)
#define __aarch64_vdup_laneq_p16(__a, __b) \
   __aarch64_vdup_lane_any (p16, , q, __a, __b)
#define __aarch64_vdup_laneq_s8(__a, __b) \
   __aarch64_vdup_lane_any (s8, , q, __a, __b)
#define __aarch64_vdup_laneq_s16(__a, __b) \
   __aarch64_vdup_lane_any (s16, , q, __a, __b)
#define __aarch64_vdup_laneq_s32(__a, __b) \
   __aarch64_vdup_lane_any (s32, , q, __a, __b)
#define __aarch64_vdup_laneq_s64(__a, __b) \
   __aarch64_vdup_lane_any (s64, , q, __a, __b)
#define __aarch64_vdup_laneq_u8(__a, __b) \
   __aarch64_vdup_lane_any (u8, , q, __a, __b)
#define __aarch64_vdup_laneq_u16(__a, __b) \
   __aarch64_vdup_lane_any (u16, , q, __a, __b)
#define __aarch64_vdup_laneq_u32(__a, __b) \
   __aarch64_vdup_lane_any (u32, , q, __a, __b)
#define __aarch64_vdup_laneq_u64(__a, __b) \
   __aarch64_vdup_lane_any (u64, , q, __a, __b)

/* __aarch64_vdupq_lane internal macros.  */
#define __aarch64_vdupq_lane_f32(__a, __b) \
   __aarch64_vdup_lane_any (f32, q, , __a, __b)
#define __aarch64_vdupq_lane_f64(__a, __b) (vdupq_n_f64 (__a))
#define __aarch64_vdupq_lane_p8(__a, __b) \
   __aarch64_vdup_lane_any (p8, q, , __a, __b)
#define __aarch64_vdupq_lane_p16(__a, __b) \
   __aarch64_vdup_lane_any (p16, q, , __a, __b)
#define __aarch64_vdupq_lane_s8(__a, __b) \
   __aarch64_vdup_lane_any (s8, q, , __a, __b)
#define __aarch64_vdupq_lane_s16(__a, __b) \
   __aarch64_vdup_lane_any (s16, q, , __a, __b)
#define __aarch64_vdupq_lane_s32(__a, __b) \
   __aarch64_vdup_lane_any (s32, q, , __a, __b)
#define __aarch64_vdupq_lane_s64(__a, __b) (vdupq_n_s64 (__a))
#define __aarch64_vdupq_lane_u8(__a, __b) \
   __aarch64_vdup_lane_any (u8, q, , __a, __b)
#define __aarch64_vdupq_lane_u16(__a, __b) \
   __aarch64_vdup_lane_any (u16, q, , __a, __b)
#define __aarch64_vdupq_lane_u32(__a, __b) \
   __aarch64_vdup_lane_any (u32, q, , __a, __b)
#define __aarch64_vdupq_lane_u64(__a, __b) (vdupq_n_u64 (__a))

/* __aarch64_vdupq_laneq internal macros.  */
#define __aarch64_vdupq_laneq_f32(__a, __b) \
   __aarch64_vdup_lane_any (f32, q, q, __a, __b)
#define __aarch64_vdupq_laneq_f64(__a, __b) \
   __aarch64_vdup_lane_any (f64, q, q, __a, __b)
#define __aarch64_vdupq_laneq_p8(__a, __b) \
   __aarch64_vdup_lane_any (p8, q, q, __a, __b)
#define __aarch64_vdupq_laneq_p16(__a, __b) \
   __aarch64_vdup_lane_any (p16, q, q, __a, __b)
#define __aarch64_vdupq_laneq_s8(__a, __b) \
   __aarch64_vdup_lane_any (s8, q, q, __a, __b)
#define __aarch64_vdupq_laneq_s16(__a, __b) \
   __aarch64_vdup_lane_any (s16, q, q, __a, __b)
#define __aarch64_vdupq_laneq_s32(__a, __b) \
   __aarch64_vdup_lane_any (s32, q, q, __a, __b)
#define __aarch64_vdupq_laneq_s64(__a, __b) \
   __aarch64_vdup_lane_any (s64, q, q, __a, __b)
#define __aarch64_vdupq_laneq_u8(__a, __b) \
   __aarch64_vdup_lane_any (u8, q, q, __a, __b)
#define __aarch64_vdupq_laneq_u16(__a, __b) \
   __aarch64_vdup_lane_any (u16, q, q, __a, __b)
#define __aarch64_vdupq_laneq_u32(__a, __b) \
   __aarch64_vdup_lane_any (u32, q, q, __a, __b)
#define __aarch64_vdupq_laneq_u64(__a, __b) \
   __aarch64_vdup_lane_any (u64, q, q, __a, __b)

/* vadd  */
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return __a + __b;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return __a + __b;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return __a + __b;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vadd_f32 (float32x2_t __a, float32x2_t __b)
{
  return __a + __b;
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vadd_f64 (float64x1_t __a, float64x1_t __b)
{
  return __a + __b;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __a + __b;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __a + __b;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __a + __b;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vadd_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a + __b;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vadd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a + __b;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __a + __b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __a + __b;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __a + __b;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vaddq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __a + __b;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vaddq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __a + __b;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vaddq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __a + __b;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __a + __b;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __a + __b;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __a + __b;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vaddq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __a + __b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vaddl_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_saddlv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vaddl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_saddlv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vaddl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int64x2_t) __builtin_aarch64_saddlv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vaddl_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uaddlv8qi ((int8x8_t) __a,
						   (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vaddl_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uaddlv4hi ((int16x4_t) __a,
						   (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vaddl_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_uaddlv2si ((int32x2_t) __a,
						   (int32x2_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vaddl_high_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int16x8_t) __builtin_aarch64_saddl2v16qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vaddl_high_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int32x4_t) __builtin_aarch64_saddl2v8hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vaddl_high_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int64x2_t) __builtin_aarch64_saddl2v4si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vaddl_high_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uaddl2v16qi ((int8x16_t) __a,
						     (int8x16_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vaddl_high_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uaddl2v8hi ((int16x8_t) __a,
						    (int16x8_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vaddl_high_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint64x2_t) __builtin_aarch64_uaddl2v4si ((int32x4_t) __a,
						    (int32x4_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vaddw_s8 (int16x8_t __a, int8x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_saddwv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vaddw_s16 (int32x4_t __a, int16x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_saddwv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vaddw_s32 (int64x2_t __a, int32x2_t __b)
{
  return (int64x2_t) __builtin_aarch64_saddwv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vaddw_u8 (uint16x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uaddwv8qi ((int16x8_t) __a,
						   (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vaddw_u16 (uint32x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uaddwv4hi ((int32x4_t) __a,
						   (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vaddw_u32 (uint64x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_uaddwv2si ((int64x2_t) __a,
						   (int32x2_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vaddw_high_s8 (int16x8_t __a, int8x16_t __b)
{
  return (int16x8_t) __builtin_aarch64_saddw2v16qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vaddw_high_s16 (int32x4_t __a, int16x8_t __b)
{
  return (int32x4_t) __builtin_aarch64_saddw2v8hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vaddw_high_s32 (int64x2_t __a, int32x4_t __b)
{
  return (int64x2_t) __builtin_aarch64_saddw2v4si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vaddw_high_u8 (uint16x8_t __a, uint8x16_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uaddw2v16qi ((int16x8_t) __a,
						     (int8x16_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vaddw_high_u16 (uint32x4_t __a, uint16x8_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uaddw2v8hi ((int32x4_t) __a,
						    (int16x8_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vaddw_high_u32 (uint64x2_t __a, uint32x4_t __b)
{
  return (uint64x2_t) __builtin_aarch64_uaddw2v4si ((int64x2_t) __a,
						    (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vhadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t) __builtin_aarch64_shaddv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vhadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_shaddv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vhadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_shaddv2si (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vhadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_uhaddv8qi ((int8x8_t) __a,
						  (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vhadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_uhaddv4hi ((int16x4_t) __a,
						   (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vhadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_uhaddv2si ((int32x2_t) __a,
						   (int32x2_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vhaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t) __builtin_aarch64_shaddv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vhaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_shaddv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vhaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_shaddv4si (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vhaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_uhaddv16qi ((int8x16_t) __a,
						    (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vhaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uhaddv8hi ((int16x8_t) __a,
						   (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vhaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uhaddv4si ((int32x4_t) __a,
						   (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrhadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t) __builtin_aarch64_srhaddv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrhadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_srhaddv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrhadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_srhaddv2si (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrhadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_urhaddv8qi ((int8x8_t) __a,
						   (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrhadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_urhaddv4hi ((int16x4_t) __a,
						    (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrhadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_urhaddv2si ((int32x2_t) __a,
						    (int32x2_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrhaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t) __builtin_aarch64_srhaddv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrhaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_srhaddv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrhaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_srhaddv4si (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrhaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_urhaddv16qi ((int8x16_t) __a,
						     (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrhaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_urhaddv8hi ((int16x8_t) __a,
						    (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrhaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_urhaddv4si ((int32x4_t) __a,
						    (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vaddhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int8x8_t) __builtin_aarch64_addhnv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vaddhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_addhnv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vaddhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_addhnv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vaddhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_addhnv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vaddhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_addhnv4si ((int32x4_t) __a,
						   (int32x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vaddhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_addhnv2di ((int64x2_t) __a,
						   (int64x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vraddhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int8x8_t) __builtin_aarch64_raddhnv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vraddhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_raddhnv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vraddhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_raddhnv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vraddhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_raddhnv8hi ((int16x8_t) __a,
						   (int16x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vraddhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_raddhnv4si ((int32x4_t) __a,
						    (int32x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vraddhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_raddhnv2di ((int64x2_t) __a,
						    (int64x2_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vaddhn_high_s16 (int8x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int8x16_t) __builtin_aarch64_addhn2v8hi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vaddhn_high_s32 (int16x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int16x8_t) __builtin_aarch64_addhn2v4si (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vaddhn_high_s64 (int32x2_t __a, int64x2_t __b, int64x2_t __c)
{
  return (int32x4_t) __builtin_aarch64_addhn2v2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaddhn_high_u16 (uint8x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return (uint8x16_t) __builtin_aarch64_addhn2v8hi ((int8x8_t) __a,
						    (int16x8_t) __b,
						    (int16x8_t) __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vaddhn_high_u32 (uint16x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return (uint16x8_t) __builtin_aarch64_addhn2v4si ((int16x4_t) __a,
						    (int32x4_t) __b,
						    (int32x4_t) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vaddhn_high_u64 (uint32x2_t __a, uint64x2_t __b, uint64x2_t __c)
{
  return (uint32x4_t) __builtin_aarch64_addhn2v2di ((int32x2_t) __a,
						    (int64x2_t) __b,
						    (int64x2_t) __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vraddhn_high_s16 (int8x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int8x16_t) __builtin_aarch64_raddhn2v8hi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vraddhn_high_s32 (int16x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int16x8_t) __builtin_aarch64_raddhn2v4si (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vraddhn_high_s64 (int32x2_t __a, int64x2_t __b, int64x2_t __c)
{
  return (int32x4_t) __builtin_aarch64_raddhn2v2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vraddhn_high_u16 (uint8x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return (uint8x16_t) __builtin_aarch64_raddhn2v8hi ((int8x8_t) __a,
						     (int16x8_t) __b,
						     (int16x8_t) __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vraddhn_high_u32 (uint16x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return (uint16x8_t) __builtin_aarch64_raddhn2v4si ((int16x4_t) __a,
						     (int32x4_t) __b,
						     (int32x4_t) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vraddhn_high_u64 (uint32x2_t __a, uint64x2_t __b, uint64x2_t __c)
{
  return (uint32x4_t) __builtin_aarch64_raddhn2v2di ((int32x2_t) __a,
						     (int64x2_t) __b,
						     (int64x2_t) __c);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vdiv_f32 (float32x2_t __a, float32x2_t __b)
{
  return __a / __b;
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vdiv_f64 (float64x1_t __a, float64x1_t __b)
{
  return __a / __b;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vdivq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __a / __b;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vdivq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __a / __b;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmul_s8 (int8x8_t __a, int8x8_t __b)
{
  return __a * __b;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmul_s16 (int16x4_t __a, int16x4_t __b)
{
  return __a * __b;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmul_s32 (int32x2_t __a, int32x2_t __b)
{
  return __a * __b;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmul_f32 (float32x2_t __a, float32x2_t __b)
{
  return __a * __b;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmul_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __a * __b;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmul_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __a * __b;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmul_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __a * __b;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vmul_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (poly8x8_t) __builtin_aarch64_pmulv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmulq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __a * __b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmulq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __a * __b;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmulq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __a * __b;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmulq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __a * __b;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmulq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __a * __b;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmulq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __a * __b;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmulq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __a * __b;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmulq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __a * __b;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vmulq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (poly8x16_t) __builtin_aarch64_pmulv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vand_s8 (int8x8_t __a, int8x8_t __b)
{
  return __a & __b;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vand_s16 (int16x4_t __a, int16x4_t __b)
{
  return __a & __b;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vand_s32 (int32x2_t __a, int32x2_t __b)
{
  return __a & __b;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vand_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __a & __b;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vand_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __a & __b;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vand_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __a & __b;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vand_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a & __b;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vand_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a & __b;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vandq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __a & __b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vandq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __a & __b;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vandq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __a & __b;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vandq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __a & __b;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vandq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __a & __b;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vandq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __a & __b;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vandq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __a & __b;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vandq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __a & __b;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vorr_s8 (int8x8_t __a, int8x8_t __b)
{
  return __a | __b;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vorr_s16 (int16x4_t __a, int16x4_t __b)
{
  return __a | __b;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vorr_s32 (int32x2_t __a, int32x2_t __b)
{
  return __a | __b;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vorr_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __a | __b;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vorr_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __a | __b;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vorr_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __a | __b;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vorr_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a | __b;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vorr_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a | __b;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vorrq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __a | __b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vorrq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __a | __b;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vorrq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __a | __b;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vorrq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __a | __b;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vorrq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __a | __b;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vorrq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __a | __b;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vorrq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __a | __b;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vorrq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __a | __b;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
veor_s8 (int8x8_t __a, int8x8_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
veor_s16 (int16x4_t __a, int16x4_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
veor_s32 (int32x2_t __a, int32x2_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
veor_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
veor_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
veor_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
veor_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
veor_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
veorq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
veorq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
veorq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
veorq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
veorq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
veorq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
veorq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
veorq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __a ^ __b;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vbic_s8 (int8x8_t __a, int8x8_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vbic_s16 (int16x4_t __a, int16x4_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vbic_s32 (int32x2_t __a, int32x2_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vbic_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vbic_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vbic_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vbic_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vbic_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vbicq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vbicq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vbicq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vbicq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vbicq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vbicq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vbicq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vbicq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __a & ~__b;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vorn_s8 (int8x8_t __a, int8x8_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vorn_s16 (int16x4_t __a, int16x4_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vorn_s32 (int32x2_t __a, int32x2_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vorn_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vorn_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vorn_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vorn_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vorn_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vornq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vornq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vornq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vornq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vornq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vornq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vornq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vornq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __a | ~__b;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vsub_s8 (int8x8_t __a, int8x8_t __b)
{
  return __a - __b;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vsub_s16 (int16x4_t __a, int16x4_t __b)
{
  return __a - __b;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vsub_s32 (int32x2_t __a, int32x2_t __b)
{
  return __a - __b;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vsub_f32 (float32x2_t __a, float32x2_t __b)
{
  return __a - __b;
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vsub_f64 (float64x1_t __a, float64x1_t __b)
{
  return __a - __b;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsub_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return __a - __b;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsub_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return __a - __b;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsub_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return __a - __b;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsub_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a - __b;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsub_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a - __b;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vsubq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __a - __b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsubq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __a - __b;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsubq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __a - __b;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsubq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __a - __b;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vsubq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __a - __b;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vsubq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __a - __b;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsubq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __a - __b;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsubq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __a - __b;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsubq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __a - __b;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsubq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return __a - __b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsubl_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_ssublv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsubl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_ssublv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsubl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int64x2_t) __builtin_aarch64_ssublv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsubl_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_usublv8qi ((int8x8_t) __a,
						   (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsubl_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_usublv4hi ((int16x4_t) __a,
						   (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsubl_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_usublv2si ((int32x2_t) __a,
						   (int32x2_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsubl_high_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int16x8_t) __builtin_aarch64_ssubl2v16qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsubl_high_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int32x4_t) __builtin_aarch64_ssubl2v8hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsubl_high_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int64x2_t) __builtin_aarch64_ssubl2v4si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsubl_high_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint16x8_t) __builtin_aarch64_usubl2v16qi ((int8x16_t) __a,
						     (int8x16_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsubl_high_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint32x4_t) __builtin_aarch64_usubl2v8hi ((int16x8_t) __a,
						    (int16x8_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsubl_high_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint64x2_t) __builtin_aarch64_usubl2v4si ((int32x4_t) __a,
						    (int32x4_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsubw_s8 (int16x8_t __a, int8x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_ssubwv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsubw_s16 (int32x4_t __a, int16x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_ssubwv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsubw_s32 (int64x2_t __a, int32x2_t __b)
{
  return (int64x2_t) __builtin_aarch64_ssubwv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsubw_u8 (uint16x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_usubwv8qi ((int16x8_t) __a,
						   (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsubw_u16 (uint32x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_usubwv4hi ((int32x4_t) __a,
						   (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsubw_u32 (uint64x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_usubwv2si ((int64x2_t) __a,
						   (int32x2_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsubw_high_s8 (int16x8_t __a, int8x16_t __b)
{
  return (int16x8_t) __builtin_aarch64_ssubw2v16qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsubw_high_s16 (int32x4_t __a, int16x8_t __b)
{
  return (int32x4_t) __builtin_aarch64_ssubw2v8hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsubw_high_s32 (int64x2_t __a, int32x4_t __b)
{
  return (int64x2_t) __builtin_aarch64_ssubw2v4si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsubw_high_u8 (uint16x8_t __a, uint8x16_t __b)
{
  return (uint16x8_t) __builtin_aarch64_usubw2v16qi ((int16x8_t) __a,
						     (int8x16_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsubw_high_u16 (uint32x4_t __a, uint16x8_t __b)
{
  return (uint32x4_t) __builtin_aarch64_usubw2v8hi ((int32x4_t) __a,
						    (int16x8_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsubw_high_u32 (uint64x2_t __a, uint32x4_t __b)
{
  return (uint64x2_t) __builtin_aarch64_usubw2v4si ((int64x2_t) __a,
						    (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t) __builtin_aarch64_sqaddv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_sqaddv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_sqaddv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqadd_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_sqadddi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_uqaddv8qi ((int8x8_t) __a,
						  (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_uqaddv4hi ((int16x4_t) __a,
						   (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_uqaddv2si ((int32x2_t) __a,
						   (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqadd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_uqadddi ((int64x1_t) __a,
						 (int64x1_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t) __builtin_aarch64_sqaddv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_sqaddv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_sqaddv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqaddq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t) __builtin_aarch64_sqaddv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_uqaddv16qi ((int8x16_t) __a,
						    (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uqaddv8hi ((int16x8_t) __a,
						   (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uqaddv4si ((int32x4_t) __a,
						   (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqaddq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_uqaddv2di ((int64x2_t) __a,
						   (int64x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqsub_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t) __builtin_aarch64_sqsubv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqsub_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_sqsubv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqsub_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_sqsubv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqsub_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_sqsubdi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqsub_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_uqsubv8qi ((int8x8_t) __a,
						  (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqsub_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_uqsubv4hi ((int16x4_t) __a,
						   (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqsub_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_uqsubv2si ((int32x2_t) __a,
						   (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqsub_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_uqsubdi ((int64x1_t) __a,
						 (int64x1_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqsubq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t) __builtin_aarch64_sqsubv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqsubq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_sqsubv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqsubq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_sqsubv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqsubq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t) __builtin_aarch64_sqsubv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqsubq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_uqsubv16qi ((int8x16_t) __a,
						    (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqsubq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uqsubv8hi ((int16x8_t) __a,
						   (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqsubq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uqsubv4si ((int32x4_t) __a,
						   (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqsubq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_uqsubv2di ((int64x2_t) __a,
						   (int64x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqneg_s8 (int8x8_t __a)
{
  return (int8x8_t) __builtin_aarch64_sqnegv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqneg_s16 (int16x4_t __a)
{
  return (int16x4_t) __builtin_aarch64_sqnegv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqneg_s32 (int32x2_t __a)
{
  return (int32x2_t) __builtin_aarch64_sqnegv2si (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqnegq_s8 (int8x16_t __a)
{
  return (int8x16_t) __builtin_aarch64_sqnegv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqnegq_s16 (int16x8_t __a)
{
  return (int16x8_t) __builtin_aarch64_sqnegv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqnegq_s32 (int32x4_t __a)
{
  return (int32x4_t) __builtin_aarch64_sqnegv4si (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqabs_s8 (int8x8_t __a)
{
  return (int8x8_t) __builtin_aarch64_sqabsv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqabs_s16 (int16x4_t __a)
{
  return (int16x4_t) __builtin_aarch64_sqabsv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqabs_s32 (int32x2_t __a)
{
  return (int32x2_t) __builtin_aarch64_sqabsv2si (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqabsq_s8 (int8x16_t __a)
{
  return (int8x16_t) __builtin_aarch64_sqabsv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqabsq_s16 (int16x8_t __a)
{
  return (int16x8_t) __builtin_aarch64_sqabsv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqabsq_s32 (int32x4_t __a)
{
  return (int32x4_t) __builtin_aarch64_sqabsv4si (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqdmulh_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_sqdmulhv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqdmulh_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_sqdmulhv2si (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqdmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_sqdmulhv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_sqdmulhv4si (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmulh_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_sqrdmulhv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmulh_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_sqrdmulhv2si (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_sqrdmulhv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_sqrdmulhv4si (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vcreate_s8 (uint64_t __a)
{
  return (int8x8_t) __a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcreate_s16 (uint64_t __a)
{
  return (int16x4_t) __a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcreate_s32 (uint64_t __a)
{
  return (int32x2_t) __a;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vcreate_s64 (uint64_t __a)
{
  return (int64x1_t) __a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcreate_f32 (uint64_t __a)
{
  return (float32x2_t) __a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcreate_u8 (uint64_t __a)
{
  return (uint8x8_t) __a;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcreate_u16 (uint64_t __a)
{
  return (uint16x4_t) __a;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcreate_u32 (uint64_t __a)
{
  return (uint32x2_t) __a;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcreate_u64 (uint64_t __a)
{
  return (uint64x1_t) __a;
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vcreate_f64 (uint64_t __a)
{
  return (float64x1_t) __builtin_aarch64_createdf (__a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vcreate_p8 (uint64_t __a)
{
  return (poly8x8_t) __a;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vcreate_p16 (uint64_t __a)
{
  return (poly16x4_t) __a;
}

/* vget_lane  */

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vget_lane_f32 (float32x2_t __a, const int __b)
{
  return __aarch64_vget_lane_f32 (__a, __b);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vget_lane_f64 (float64x1_t __a, const int __b)
{
  return __aarch64_vget_lane_f64 (__a, __b);
}

__extension__ static __inline poly8_t __attribute__ ((__always_inline__))
vget_lane_p8 (poly8x8_t __a, const int __b)
{
  return __aarch64_vget_lane_p8 (__a, __b);
}

__extension__ static __inline poly16_t __attribute__ ((__always_inline__))
vget_lane_p16 (poly16x4_t __a, const int __b)
{
  return __aarch64_vget_lane_p16 (__a, __b);
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vget_lane_s8 (int8x8_t __a, const int __b)
{
  return __aarch64_vget_lane_s8 (__a, __b);
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vget_lane_s16 (int16x4_t __a, const int __b)
{
  return __aarch64_vget_lane_s16 (__a, __b);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vget_lane_s32 (int32x2_t __a, const int __b)
{
  return __aarch64_vget_lane_s32 (__a, __b);
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vget_lane_s64 (int64x1_t __a, const int __b)
{
  return __aarch64_vget_lane_s64 (__a, __b);
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vget_lane_u8 (uint8x8_t __a, const int __b)
{
  return __aarch64_vget_lane_u8 (__a, __b);
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vget_lane_u16 (uint16x4_t __a, const int __b)
{
  return __aarch64_vget_lane_u16 (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vget_lane_u32 (uint32x2_t __a, const int __b)
{
  return __aarch64_vget_lane_u32 (__a, __b);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vget_lane_u64 (uint64x1_t __a, const int __b)
{
  return __aarch64_vget_lane_u64 (__a, __b);
}

/* vgetq_lane  */

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vgetq_lane_f32 (float32x4_t __a, const int __b)
{
  return __aarch64_vgetq_lane_f32 (__a, __b);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vgetq_lane_f64 (float64x2_t __a, const int __b)
{
  return __aarch64_vgetq_lane_f64 (__a, __b);
}

__extension__ static __inline poly8_t __attribute__ ((__always_inline__))
vgetq_lane_p8 (poly8x16_t __a, const int __b)
{
  return __aarch64_vgetq_lane_p8 (__a, __b);
}

__extension__ static __inline poly16_t __attribute__ ((__always_inline__))
vgetq_lane_p16 (poly16x8_t __a, const int __b)
{
  return __aarch64_vgetq_lane_p16 (__a, __b);
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vgetq_lane_s8 (int8x16_t __a, const int __b)
{
  return __aarch64_vgetq_lane_s8 (__a, __b);
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vgetq_lane_s16 (int16x8_t __a, const int __b)
{
  return __aarch64_vgetq_lane_s16 (__a, __b);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vgetq_lane_s32 (int32x4_t __a, const int __b)
{
  return __aarch64_vgetq_lane_s32 (__a, __b);
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vgetq_lane_s64 (int64x2_t __a, const int __b)
{
  return __aarch64_vgetq_lane_s64 (__a, __b);
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vgetq_lane_u8 (uint8x16_t __a, const int __b)
{
  return __aarch64_vgetq_lane_u8 (__a, __b);
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vgetq_lane_u16 (uint16x8_t __a, const int __b)
{
  return __aarch64_vgetq_lane_u16 (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vgetq_lane_u32 (uint32x4_t __a, const int __b)
{
  return __aarch64_vgetq_lane_u32 (__a, __b);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vgetq_lane_u64 (uint64x2_t __a, const int __b)
{
  return __aarch64_vgetq_lane_u64 (__a, __b);
}

/* vreinterpret  */

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_s8 (int8x8_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qiv8qi (__a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_s16 (int16x4_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qiv4hi (__a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_s32 (int32x2_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qiv2si (__a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_s64 (int64x1_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qidi (__a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_f32 (float32x2_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qiv2sf (__a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_u8 (uint8x8_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qiv8qi ((int8x8_t) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_u16 (uint16x4_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qiv4hi ((int16x4_t) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_u32 (uint32x2_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qiv2si ((int32x2_t) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_u64 (uint64x1_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qidi ((int64x1_t) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_p16 (poly16x4_t __a)
{
  return (poly8x8_t) __builtin_aarch64_reinterpretv8qiv4hi ((int16x4_t) __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_s8 (int8x16_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv16qi (__a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_s16 (int16x8_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv8hi (__a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_s32 (int32x4_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv4si (__a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_s64 (int64x2_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv2di (__a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_f32 (float32x4_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv4sf (__a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_u8 (uint8x16_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv16qi ((int8x16_t)
							       __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_u16 (uint16x8_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv8hi ((int16x8_t)
							      __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_u32 (uint32x4_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv4si ((int32x4_t)
							      __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_u64 (uint64x2_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv2di ((int64x2_t)
							      __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_p16 (poly16x8_t __a)
{
  return (poly8x16_t) __builtin_aarch64_reinterpretv16qiv8hi ((int16x8_t)
							      __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_s8 (int8x8_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hiv8qi (__a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_s16 (int16x4_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hiv4hi (__a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_s32 (int32x2_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hiv2si (__a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_s64 (int64x1_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hidi (__a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_f32 (float32x2_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hiv2sf (__a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_u8 (uint8x8_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hiv8qi ((int8x8_t) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_u16 (uint16x4_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hiv4hi ((int16x4_t) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_u32 (uint32x2_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hiv2si ((int32x2_t) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_u64 (uint64x1_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hidi ((int64x1_t) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_p8 (poly8x8_t __a)
{
  return (poly16x4_t) __builtin_aarch64_reinterpretv4hiv8qi ((int8x8_t) __a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_s8 (int8x16_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv16qi (__a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_s16 (int16x8_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv8hi (__a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_s32 (int32x4_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv4si (__a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_s64 (int64x2_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv2di (__a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_f32 (float32x4_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv4sf (__a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_u8 (uint8x16_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_u16 (uint16x8_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv8hi ((int16x8_t) __a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_u32 (uint32x4_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv4si ((int32x4_t) __a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_u64 (uint64x2_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv2di ((int64x2_t) __a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_p8 (poly8x16_t __a)
{
  return (poly16x8_t) __builtin_aarch64_reinterpretv8hiv16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_s8 (int8x8_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfv8qi (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_s16 (int16x4_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfv4hi (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_s32 (int32x2_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfv2si (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_s64 (int64x1_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfdi (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_u8 (uint8x8_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfv8qi ((int8x8_t) __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_u16 (uint16x4_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfv4hi ((int16x4_t)
							      __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_u32 (uint32x2_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfv2si ((int32x2_t)
							      __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_u64 (uint64x1_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfdi ((int64x1_t) __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_p8 (poly8x8_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfv8qi ((int8x8_t) __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_p16 (poly16x4_t __a)
{
  return (float32x2_t) __builtin_aarch64_reinterpretv2sfv4hi ((int16x4_t)
							      __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_s8 (int8x16_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv16qi (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_s16 (int16x8_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv8hi (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_s32 (int32x4_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv4si (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_s64 (int64x2_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv2di (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_u8 (uint8x16_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv16qi ((int8x16_t)
							       __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_u16 (uint16x8_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv8hi ((int16x8_t)
							      __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_u32 (uint32x4_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv4si ((int32x4_t)
							      __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_u64 (uint64x2_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv2di ((int64x2_t)
							      __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_p8 (poly8x16_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv16qi ((int8x16_t)
							       __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_p16 (poly16x8_t __a)
{
  return (float32x4_t) __builtin_aarch64_reinterpretv4sfv8hi ((int16x8_t)
							      __a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_s8 (int8x8_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdiv8qi (__a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_s16 (int16x4_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdiv4hi (__a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_s32 (int32x2_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdiv2si (__a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_f32 (float32x2_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdiv2sf (__a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_u8 (uint8x8_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdiv8qi ((int8x8_t) __a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_u16 (uint16x4_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdiv4hi ((int16x4_t) __a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_u32 (uint32x2_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdiv2si ((int32x2_t) __a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_u64 (uint64x1_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdidi ((int64x1_t) __a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_p8 (poly8x8_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdiv8qi ((int8x8_t) __a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_p16 (poly16x4_t __a)
{
  return (int64x1_t) __builtin_aarch64_reinterpretdiv4hi ((int16x4_t) __a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_s8 (int8x16_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div16qi (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_s16 (int16x8_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div8hi (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_s32 (int32x4_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div4si (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_f32 (float32x4_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div4sf (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_u8 (uint8x16_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div16qi ((int8x16_t) __a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_u16 (uint16x8_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div8hi ((int16x8_t) __a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_u32 (uint32x4_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div4si ((int32x4_t) __a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_u64 (uint64x2_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div2di ((int64x2_t) __a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_p8 (poly8x16_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div16qi ((int8x16_t) __a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_p16 (poly16x8_t __a)
{
  return (int64x2_t) __builtin_aarch64_reinterpretv2div8hi ((int16x8_t) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_s8 (int8x8_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdiv8qi (__a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_s16 (int16x4_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdiv4hi (__a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_s32 (int32x2_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdiv2si (__a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_s64 (int64x1_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdidi (__a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_f32 (float32x2_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdiv2sf (__a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_u8 (uint8x8_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdiv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_u16 (uint16x4_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdiv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_u32 (uint32x2_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdiv2si ((int32x2_t) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_p8 (poly8x8_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdiv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_p16 (poly16x4_t __a)
{
  return (uint64x1_t) __builtin_aarch64_reinterpretdiv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_s8 (int8x16_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div16qi (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_s16 (int16x8_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div8hi (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_s32 (int32x4_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div4si (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_s64 (int64x2_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div2di (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_f32 (float32x4_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div4sf (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_u8 (uint8x16_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_u16 (uint16x8_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div8hi ((int16x8_t) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_u32 (uint32x4_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div4si ((int32x4_t) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_p8 (poly8x16_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_p16 (poly16x8_t __a)
{
  return (uint64x2_t) __builtin_aarch64_reinterpretv2div8hi ((int16x8_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_s16 (int16x4_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qiv4hi (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_s32 (int32x2_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qiv2si (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_s64 (int64x1_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qidi (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_f32 (float32x2_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qiv2sf (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_u8 (uint8x8_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qiv8qi ((int8x8_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_u16 (uint16x4_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qiv4hi ((int16x4_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_u32 (uint32x2_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qiv2si ((int32x2_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_u64 (uint64x1_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qidi ((int64x1_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_p8 (poly8x8_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qiv8qi ((int8x8_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_p16 (poly16x4_t __a)
{
  return (int8x8_t) __builtin_aarch64_reinterpretv8qiv4hi ((int16x4_t) __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_s16 (int16x8_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv8hi (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_s32 (int32x4_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv4si (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_s64 (int64x2_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv2di (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_f32 (float32x4_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv4sf (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_u8 (uint8x16_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_u16 (uint16x8_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv8hi ((int16x8_t) __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_u32 (uint32x4_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv4si ((int32x4_t) __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_u64 (uint64x2_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv2di ((int64x2_t) __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_p8 (poly8x16_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_p16 (poly16x8_t __a)
{
  return (int8x16_t) __builtin_aarch64_reinterpretv16qiv8hi ((int16x8_t) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_s8 (int8x8_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hiv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_s32 (int32x2_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hiv2si (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_s64 (int64x1_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hidi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_f32 (float32x2_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hiv2sf (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_u8 (uint8x8_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hiv8qi ((int8x8_t) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_u16 (uint16x4_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hiv4hi ((int16x4_t) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_u32 (uint32x2_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hiv2si ((int32x2_t) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_u64 (uint64x1_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hidi ((int64x1_t) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_p8 (poly8x8_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hiv8qi ((int8x8_t) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_p16 (poly16x4_t __a)
{
  return (int16x4_t) __builtin_aarch64_reinterpretv4hiv4hi ((int16x4_t) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_s8 (int8x16_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_s32 (int32x4_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv4si (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_s64 (int64x2_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv2di (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_f32 (float32x4_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv4sf (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_u8 (uint8x16_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv16qi ((int8x16_t) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_u16 (uint16x8_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv8hi ((int16x8_t) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_u32 (uint32x4_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv4si ((int32x4_t) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_u64 (uint64x2_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv2di ((int64x2_t) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_p8 (poly8x16_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv16qi ((int8x16_t) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_p16 (poly16x8_t __a)
{
  return (int16x8_t) __builtin_aarch64_reinterpretv8hiv8hi ((int16x8_t) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_s8 (int8x8_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2siv8qi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_s16 (int16x4_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2siv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_s64 (int64x1_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2sidi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_f32 (float32x2_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2siv2sf (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_u8 (uint8x8_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2siv8qi ((int8x8_t) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_u16 (uint16x4_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2siv4hi ((int16x4_t) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_u32 (uint32x2_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2siv2si ((int32x2_t) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_u64 (uint64x1_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2sidi ((int64x1_t) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_p8 (poly8x8_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2siv8qi ((int8x8_t) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_p16 (poly16x4_t __a)
{
  return (int32x2_t) __builtin_aarch64_reinterpretv2siv4hi ((int16x4_t) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_s8 (int8x16_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv16qi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_s16 (int16x8_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_s64 (int64x2_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv2di (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_f32 (float32x4_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv4sf (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_u8 (uint8x16_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv16qi ((int8x16_t) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_u16 (uint16x8_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv8hi ((int16x8_t) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_u32 (uint32x4_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv4si ((int32x4_t) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_u64 (uint64x2_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv2di ((int64x2_t) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_p8 (poly8x16_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv16qi ((int8x16_t) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_p16 (poly16x8_t __a)
{
  return (int32x4_t) __builtin_aarch64_reinterpretv4siv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_s8 (int8x8_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qiv8qi (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_s16 (int16x4_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qiv4hi (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_s32 (int32x2_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qiv2si (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_s64 (int64x1_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qidi (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_f32 (float32x2_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qiv2sf (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_u16 (uint16x4_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qiv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_u32 (uint32x2_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qiv2si ((int32x2_t) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_u64 (uint64x1_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qidi ((int64x1_t) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_p8 (poly8x8_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qiv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_p16 (poly16x4_t __a)
{
  return (uint8x8_t) __builtin_aarch64_reinterpretv8qiv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_s8 (int8x16_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv16qi (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_s16 (int16x8_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv8hi (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_s32 (int32x4_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv4si (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_s64 (int64x2_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv2di (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_f32 (float32x4_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv4sf (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_u16 (uint16x8_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv8hi ((int16x8_t)
							      __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_u32 (uint32x4_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv4si ((int32x4_t)
							      __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_u64 (uint64x2_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv2di ((int64x2_t)
							      __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_p8 (poly8x16_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv16qi ((int8x16_t)
							       __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_p16 (poly16x8_t __a)
{
  return (uint8x16_t) __builtin_aarch64_reinterpretv16qiv8hi ((int16x8_t)
							      __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_s8 (int8x8_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hiv8qi (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_s16 (int16x4_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hiv4hi (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_s32 (int32x2_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hiv2si (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_s64 (int64x1_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hidi (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_f32 (float32x2_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hiv2sf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_u8 (uint8x8_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hiv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_u32 (uint32x2_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hiv2si ((int32x2_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_u64 (uint64x1_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hidi ((int64x1_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_p8 (poly8x8_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hiv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_p16 (poly16x4_t __a)
{
  return (uint16x4_t) __builtin_aarch64_reinterpretv4hiv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_s8 (int8x16_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv16qi (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_s16 (int16x8_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv8hi (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_s32 (int32x4_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv4si (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_s64 (int64x2_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv2di (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_f32 (float32x4_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv4sf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_u8 (uint8x16_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_u32 (uint32x4_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv4si ((int32x4_t) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_u64 (uint64x2_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv2di ((int64x2_t) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_p8 (poly8x16_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_p16 (poly16x8_t __a)
{
  return (uint16x8_t) __builtin_aarch64_reinterpretv8hiv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_s8 (int8x8_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2siv8qi (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_s16 (int16x4_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2siv4hi (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_s32 (int32x2_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2siv2si (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_s64 (int64x1_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2sidi (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_f32 (float32x2_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2siv2sf (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_u8 (uint8x8_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2siv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_u16 (uint16x4_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2siv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_u64 (uint64x1_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2sidi ((int64x1_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_p8 (poly8x8_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2siv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_p16 (poly16x4_t __a)
{
  return (uint32x2_t) __builtin_aarch64_reinterpretv2siv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_s8 (int8x16_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv16qi (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_s16 (int16x8_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv8hi (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_s32 (int32x4_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv4si (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_s64 (int64x2_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv2di (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_f32 (float32x4_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv4sf (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_u8 (uint8x16_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_u16 (uint16x8_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_u64 (uint64x2_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv2di ((int64x2_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_p8 (poly8x16_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv16qi ((int8x16_t)
							      __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_p16 (poly16x8_t __a)
{
  return (uint32x4_t) __builtin_aarch64_reinterpretv4siv8hi ((int16x8_t) __a);
}

#define __GET_LOW(__TYPE) \
  uint64x2_t tmp = vreinterpretq_u64_##__TYPE (__a);  \
  uint64_t lo = vgetq_lane_u64 (tmp, 0);  \
  return vreinterpret_##__TYPE##_u64 (lo);

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vget_low_f32 (float32x4_t __a)
{
  __GET_LOW (f32);
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vget_low_f64 (float64x2_t __a)
{
  return vgetq_lane_f64 (__a, 0);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vget_low_p8 (poly8x16_t __a)
{
  __GET_LOW (p8);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vget_low_p16 (poly16x8_t __a)
{
  __GET_LOW (p16);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vget_low_s8 (int8x16_t __a)
{
  __GET_LOW (s8);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vget_low_s16 (int16x8_t __a)
{
  __GET_LOW (s16);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vget_low_s32 (int32x4_t __a)
{
  __GET_LOW (s32);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vget_low_s64 (int64x2_t __a)
{
  return vgetq_lane_s64 (__a, 0);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vget_low_u8 (uint8x16_t __a)
{
  __GET_LOW (u8);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vget_low_u16 (uint16x8_t __a)
{
  __GET_LOW (u16);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vget_low_u32 (uint32x4_t __a)
{
  __GET_LOW (u32);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vget_low_u64 (uint64x2_t __a)
{
  return vgetq_lane_u64 (__a, 0);
}

#undef __GET_LOW

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vcombine_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x16_t) __builtin_aarch64_combinev8qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vcombine_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x8_t) __builtin_aarch64_combinev4hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vcombine_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x4_t) __builtin_aarch64_combinev2si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vcombine_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x2_t) __builtin_aarch64_combinedi (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcombine_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x4_t) __builtin_aarch64_combinev2sf (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcombine_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x16_t) __builtin_aarch64_combinev8qi ((int8x8_t) __a,
						     (int8x8_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcombine_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x8_t) __builtin_aarch64_combinev4hi ((int16x4_t) __a,
						     (int16x4_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcombine_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x4_t) __builtin_aarch64_combinev2si ((int32x2_t) __a,
						     (int32x2_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcombine_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x2_t) __builtin_aarch64_combinedi ((int64x1_t) __a,
						   (int64x1_t) __b);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vcombine_f64 (float64x1_t __a, float64x1_t __b)
{
  return (float64x2_t) __builtin_aarch64_combinedf (__a, __b);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vcombine_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (poly8x16_t) __builtin_aarch64_combinev8qi ((int8x8_t) __a,
						     (int8x8_t) __b);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vcombine_p16 (poly16x4_t __a, poly16x4_t __b)
{
  return (poly16x8_t) __builtin_aarch64_combinev4hi ((int16x4_t) __a,
						     (int16x4_t) __b);
}

/* Start of temporary inline asm implementations.  */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vaba_s8 (int8x8_t a, int8x8_t b, int8x8_t c)
{
  int8x8_t result;
  __asm__ ("saba %0.8b,%2.8b,%3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vaba_s16 (int16x4_t a, int16x4_t b, int16x4_t c)
{
  int16x4_t result;
  __asm__ ("saba %0.4h,%2.4h,%3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vaba_s32 (int32x2_t a, int32x2_t b, int32x2_t c)
{
  int32x2_t result;
  __asm__ ("saba %0.2s,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vaba_u8 (uint8x8_t a, uint8x8_t b, uint8x8_t c)
{
  uint8x8_t result;
  __asm__ ("uaba %0.8b,%2.8b,%3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vaba_u16 (uint16x4_t a, uint16x4_t b, uint16x4_t c)
{
  uint16x4_t result;
  __asm__ ("uaba %0.4h,%2.4h,%3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vaba_u32 (uint32x2_t a, uint32x2_t b, uint32x2_t c)
{
  uint32x2_t result;
  __asm__ ("uaba %0.2s,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabal_high_s8 (int16x8_t a, int8x16_t b, int8x16_t c)
{
  int16x8_t result;
  __asm__ ("sabal2 %0.8h,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabal_high_s16 (int32x4_t a, int16x8_t b, int16x8_t c)
{
  int32x4_t result;
  __asm__ ("sabal2 %0.4s,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vabal_high_s32 (int64x2_t a, int32x4_t b, int32x4_t c)
{
  int64x2_t result;
  __asm__ ("sabal2 %0.2d,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabal_high_u8 (uint16x8_t a, uint8x16_t b, uint8x16_t c)
{
  uint16x8_t result;
  __asm__ ("uabal2 %0.8h,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabal_high_u16 (uint32x4_t a, uint16x8_t b, uint16x8_t c)
{
  uint32x4_t result;
  __asm__ ("uabal2 %0.4s,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vabal_high_u32 (uint64x2_t a, uint32x4_t b, uint32x4_t c)
{
  uint64x2_t result;
  __asm__ ("uabal2 %0.2d,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabal_s8 (int16x8_t a, int8x8_t b, int8x8_t c)
{
  int16x8_t result;
  __asm__ ("sabal %0.8h,%2.8b,%3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabal_s16 (int32x4_t a, int16x4_t b, int16x4_t c)
{
  int32x4_t result;
  __asm__ ("sabal %0.4s,%2.4h,%3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vabal_s32 (int64x2_t a, int32x2_t b, int32x2_t c)
{
  int64x2_t result;
  __asm__ ("sabal %0.2d,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabal_u8 (uint16x8_t a, uint8x8_t b, uint8x8_t c)
{
  uint16x8_t result;
  __asm__ ("uabal %0.8h,%2.8b,%3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabal_u16 (uint32x4_t a, uint16x4_t b, uint16x4_t c)
{
  uint32x4_t result;
  __asm__ ("uabal %0.4s,%2.4h,%3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vabal_u32 (uint64x2_t a, uint32x2_t b, uint32x2_t c)
{
  uint64x2_t result;
  __asm__ ("uabal %0.2d,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vabaq_s8 (int8x16_t a, int8x16_t b, int8x16_t c)
{
  int8x16_t result;
  __asm__ ("saba %0.16b,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabaq_s16 (int16x8_t a, int16x8_t b, int16x8_t c)
{
  int16x8_t result;
  __asm__ ("saba %0.8h,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabaq_s32 (int32x4_t a, int32x4_t b, int32x4_t c)
{
  int32x4_t result;
  __asm__ ("saba %0.4s,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vabaq_u8 (uint8x16_t a, uint8x16_t b, uint8x16_t c)
{
  uint8x16_t result;
  __asm__ ("uaba %0.16b,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabaq_u16 (uint16x8_t a, uint16x8_t b, uint16x8_t c)
{
  uint16x8_t result;
  __asm__ ("uaba %0.8h,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabaq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  uint32x4_t result;
  __asm__ ("uaba %0.4s,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vabd_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("fabd %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vabd_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("sabd %0.8b, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vabd_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("sabd %0.4h, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vabd_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("sabd %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vabd_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("uabd %0.8b, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vabd_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("uabd %0.4h, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vabd_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("uabd %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vabdd_f64 (float64_t a, float64_t b)
{
  float64_t result;
  __asm__ ("fabd %d0, %d1, %d2"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabdl_high_s8 (int8x16_t a, int8x16_t b)
{
  int16x8_t result;
  __asm__ ("sabdl2 %0.8h,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabdl_high_s16 (int16x8_t a, int16x8_t b)
{
  int32x4_t result;
  __asm__ ("sabdl2 %0.4s,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vabdl_high_s32 (int32x4_t a, int32x4_t b)
{
  int64x2_t result;
  __asm__ ("sabdl2 %0.2d,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabdl_high_u8 (uint8x16_t a, uint8x16_t b)
{
  uint16x8_t result;
  __asm__ ("uabdl2 %0.8h,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabdl_high_u16 (uint16x8_t a, uint16x8_t b)
{
  uint32x4_t result;
  __asm__ ("uabdl2 %0.4s,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vabdl_high_u32 (uint32x4_t a, uint32x4_t b)
{
  uint64x2_t result;
  __asm__ ("uabdl2 %0.2d,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabdl_s8 (int8x8_t a, int8x8_t b)
{
  int16x8_t result;
  __asm__ ("sabdl %0.8h, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabdl_s16 (int16x4_t a, int16x4_t b)
{
  int32x4_t result;
  __asm__ ("sabdl %0.4s, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vabdl_s32 (int32x2_t a, int32x2_t b)
{
  int64x2_t result;
  __asm__ ("sabdl %0.2d, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabdl_u8 (uint8x8_t a, uint8x8_t b)
{
  uint16x8_t result;
  __asm__ ("uabdl %0.8h, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabdl_u16 (uint16x4_t a, uint16x4_t b)
{
  uint32x4_t result;
  __asm__ ("uabdl %0.4s, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vabdl_u32 (uint32x2_t a, uint32x2_t b)
{
  uint64x2_t result;
  __asm__ ("uabdl %0.2d, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vabdq_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("fabd %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vabdq_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("fabd %0.2d, %1.2d, %2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vabdq_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("sabd %0.16b, %1.16b, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabdq_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("sabd %0.8h, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabdq_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("sabd %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vabdq_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("uabd %0.16b, %1.16b, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabdq_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("uabd %0.8h, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabdq_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("uabd %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vabds_f32 (float32_t a, float32_t b)
{
  float32_t result;
  __asm__ ("fabd %s0, %s1, %s2"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vaddlv_s8 (int8x8_t a)
{
  int16_t result;
  __asm__ ("saddlv %h0,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vaddlv_s16 (int16x4_t a)
{
  int32_t result;
  __asm__ ("saddlv %s0,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vaddlv_u8 (uint8x8_t a)
{
  uint16_t result;
  __asm__ ("uaddlv %h0,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vaddlv_u16 (uint16x4_t a)
{
  uint32_t result;
  __asm__ ("uaddlv %s0,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vaddlvq_s8 (int8x16_t a)
{
  int16_t result;
  __asm__ ("saddlv %h0,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vaddlvq_s16 (int16x8_t a)
{
  int32_t result;
  __asm__ ("saddlv %s0,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vaddlvq_s32 (int32x4_t a)
{
  int64_t result;
  __asm__ ("saddlv %d0,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vaddlvq_u8 (uint8x16_t a)
{
  uint16_t result;
  __asm__ ("uaddlv %h0,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vaddlvq_u16 (uint16x8_t a)
{
  uint32_t result;
  __asm__ ("uaddlv %s0,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vaddlvq_u32 (uint32x4_t a)
{
  uint64_t result;
  __asm__ ("uaddlv %d0,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vcls_s8 (int8x8_t a)
{
  int8x8_t result;
  __asm__ ("cls %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcls_s16 (int16x4_t a)
{
  int16x4_t result;
  __asm__ ("cls %0.4h,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcls_s32 (int32x2_t a)
{
  int32x2_t result;
  __asm__ ("cls %0.2s,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vclsq_s8 (int8x16_t a)
{
  int8x16_t result;
  __asm__ ("cls %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vclsq_s16 (int16x8_t a)
{
  int16x8_t result;
  __asm__ ("cls %0.8h,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vclsq_s32 (int32x4_t a)
{
  int32x4_t result;
  __asm__ ("cls %0.4s,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vcnt_p8 (poly8x8_t a)
{
  poly8x8_t result;
  __asm__ ("cnt %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vcnt_s8 (int8x8_t a)
{
  int8x8_t result;
  __asm__ ("cnt %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcnt_u8 (uint8x8_t a)
{
  uint8x8_t result;
  __asm__ ("cnt %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vcntq_p8 (poly8x16_t a)
{
  poly8x16_t result;
  __asm__ ("cnt %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vcntq_s8 (int8x16_t a)
{
  int8x16_t result;
  __asm__ ("cnt %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcntq_u8 (uint8x16_t a)
{
  uint8x16_t result;
  __asm__ ("cnt %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

#define vcopyq_lane_f32(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t c_ = (c);                                            \
       float32x4_t a_ = (a);                                            \
       float32x4_t result;                                              \
       __asm__ ("ins %0.s[%2], %3.s[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_f64(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       float64x2_t c_ = (c);                                            \
       float64x2_t a_ = (a);                                            \
       float64x2_t result;                                              \
       __asm__ ("ins %0.d[%2], %3.d[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_p8(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       poly8x16_t c_ = (c);                                             \
       poly8x16_t a_ = (a);                                             \
       poly8x16_t result;                                               \
       __asm__ ("ins %0.b[%2], %3.b[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_p16(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       poly16x8_t c_ = (c);                                             \
       poly16x8_t a_ = (a);                                             \
       poly16x8_t result;                                               \
       __asm__ ("ins %0.h[%2], %3.h[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_s8(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       int8x16_t c_ = (c);                                              \
       int8x16_t a_ = (a);                                              \
       int8x16_t result;                                                \
       __asm__ ("ins %0.b[%2], %3.b[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_s16(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t c_ = (c);                                              \
       int16x8_t a_ = (a);                                              \
       int16x8_t result;                                                \
       __asm__ ("ins %0.h[%2], %3.h[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_s32(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t c_ = (c);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("ins %0.s[%2], %3.s[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_s64(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t c_ = (c);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("ins %0.d[%2], %3.d[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_u8(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint8x16_t c_ = (c);                                             \
       uint8x16_t a_ = (a);                                             \
       uint8x16_t result;                                               \
       __asm__ ("ins %0.b[%2], %3.b[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_u16(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t c_ = (c);                                             \
       uint16x8_t a_ = (a);                                             \
       uint16x8_t result;                                               \
       __asm__ ("ins %0.h[%2], %3.h[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_u32(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t c_ = (c);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("ins %0.s[%2], %3.s[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcopyq_lane_u64(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t c_ = (c);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("ins %0.d[%2], %3.d[%4]"                                \
                : "=w"(result)                                          \
                : "0"(a_), "i"(b), "w"(c_), "i"(d)                      \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

/* vcvt_f16_f32 not supported */

/* vcvt_f32_f16 not supported */

/* vcvt_high_f16_f32 not supported */

/* vcvt_high_f32_f16 not supported */

static float32x2_t vdup_n_f32 (float32_t);

#define vcvt_n_f32_s32(a, b)                                            \
  __extension__                                                         \
    ({                                                                  \
       int32x2_t a_ = (a);                                              \
       float32x2_t result;                                              \
       __asm__ ("scvtf %0.2s, %1.2s, #%2"                               \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvt_n_f32_u32(a, b)                                            \
  __extension__                                                         \
    ({                                                                  \
       uint32x2_t a_ = (a);                                             \
       float32x2_t result;                                              \
       __asm__ ("ucvtf %0.2s, %1.2s, #%2"                               \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvt_n_s32_f32(a, b)                                            \
  __extension__                                                         \
    ({                                                                  \
       float32x2_t a_ = (a);                                            \
       int32x2_t result;                                                \
       __asm__ ("fcvtzs %0.2s, %1.2s, #%2"                              \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvt_n_u32_f32(a, b)                                            \
  __extension__                                                         \
    ({                                                                  \
       float32x2_t a_ = (a);                                            \
       uint32x2_t result;                                               \
       __asm__ ("fcvtzu %0.2s, %1.2s, #%2"                              \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtd_n_f64_s64(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       int64_t a_ = (a);                                                \
       float64_t result;                                                \
       __asm__ ("scvtf %d0,%d1,%2"                                      \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtd_n_f64_u64(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       uint64_t a_ = (a);                                               \
       float64_t result;                                                \
       __asm__ ("ucvtf %d0,%d1,%2"                                      \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtd_n_s64_f64(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       float64_t a_ = (a);                                              \
       int64_t result;                                                  \
       __asm__ ("fcvtzs %d0,%d1,%2"                                     \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtd_n_u64_f64(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       float64_t a_ = (a);                                              \
       uint64_t result;                                                 \
       __asm__ ("fcvtzu %d0,%d1,%2"                                     \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtq_n_f32_s32(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t a_ = (a);                                              \
       float32x4_t result;                                              \
       __asm__ ("scvtf %0.4s, %1.4s, #%2"                               \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtq_n_f32_u32(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t a_ = (a);                                             \
       float32x4_t result;                                              \
       __asm__ ("ucvtf %0.4s, %1.4s, #%2"                               \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtq_n_f64_s64(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t a_ = (a);                                              \
       float64x2_t result;                                              \
       __asm__ ("scvtf %0.2d, %1.2d, #%2"                               \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtq_n_f64_u64(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t a_ = (a);                                             \
       float64x2_t result;                                              \
       __asm__ ("ucvtf %0.2d, %1.2d, #%2"                               \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtq_n_s32_f32(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t a_ = (a);                                            \
       int32x4_t result;                                                \
       __asm__ ("fcvtzs %0.4s, %1.4s, #%2"                              \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtq_n_s64_f64(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       float64x2_t a_ = (a);                                            \
       int64x2_t result;                                                \
       __asm__ ("fcvtzs %0.2d, %1.2d, #%2"                              \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtq_n_u32_f32(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t a_ = (a);                                            \
       uint32x4_t result;                                               \
       __asm__ ("fcvtzu %0.4s, %1.4s, #%2"                              \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvtq_n_u64_f64(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       float64x2_t a_ = (a);                                            \
       uint64x2_t result;                                               \
       __asm__ ("fcvtzu %0.2d, %1.2d, #%2"                              \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvts_n_f32_s32(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       int32_t a_ = (a);                                                \
       float32_t result;                                                \
       __asm__ ("scvtf %s0,%s1,%2"                                      \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvts_n_f32_u32(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       uint32_t a_ = (a);                                               \
       float32_t result;                                                \
       __asm__ ("ucvtf %s0,%s1,%2"                                      \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvts_n_s32_f32(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       float32_t a_ = (a);                                              \
       int32_t result;                                                  \
       __asm__ ("fcvtzs %s0,%s1,%2"                                     \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vcvts_n_u32_f32(a, b)                                           \
  __extension__                                                         \
    ({                                                                  \
       float32_t a_ = (a);                                              \
       uint32_t result;                                                 \
       __asm__ ("fcvtzu %s0,%s1,%2"                                     \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcvtx_f32_f64 (float64x2_t a)
{
  float32x2_t result;
  __asm__ ("fcvtxn %0.2s,%1.2d"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcvtx_high_f32_f64 (float32x2_t a, float64x2_t b)
{
  float32x4_t result;
  __asm__ ("fcvtxn2 %0.4s,%1.2d"
           : "=w"(result)
           : "w" (b), "0"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vcvtxd_f32_f64 (float64_t a)
{
  float32_t result;
  __asm__ ("fcvtxn %s0,%d1"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

#define vext_f32(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       float32x2_t b_ = (b);                                            \
       float32x2_t a_ = (a);                                            \
       float32x2_t result;                                              \
       __asm__ ("ext %0.8b, %1.8b, %2.8b, #%3*4"                        \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_f64(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       float64x1_t b_ = (b);                                            \
       float64x1_t a_ = (a);                                            \
       float64x1_t result;                                              \
       __asm__ ("ext %0.8b, %1.8b, %2.8b, #%3*8"                        \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_p8(a, b, c)                                                \
  __extension__                                                         \
    ({                                                                  \
       poly8x8_t b_ = (b);                                              \
       poly8x8_t a_ = (a);                                              \
       poly8x8_t result;                                                \
       __asm__ ("ext %0.8b,%1.8b,%2.8b,%3"                              \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_p16(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       poly16x4_t b_ = (b);                                             \
       poly16x4_t a_ = (a);                                             \
       poly16x4_t result;                                               \
       __asm__ ("ext %0.8b, %1.8b, %2.8b, #%3*2"                        \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_s8(a, b, c)                                                \
  __extension__                                                         \
    ({                                                                  \
       int8x8_t b_ = (b);                                               \
       int8x8_t a_ = (a);                                               \
       int8x8_t result;                                                 \
       __asm__ ("ext %0.8b,%1.8b,%2.8b,%3"                              \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_s16(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       int16x4_t b_ = (b);                                              \
       int16x4_t a_ = (a);                                              \
       int16x4_t result;                                                \
       __asm__ ("ext %0.8b, %1.8b, %2.8b, #%3*2"                        \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_s32(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       int32x2_t b_ = (b);                                              \
       int32x2_t a_ = (a);                                              \
       int32x2_t result;                                                \
       __asm__ ("ext %0.8b, %1.8b, %2.8b, #%3*4"                        \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_s64(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       int64x1_t b_ = (b);                                              \
       int64x1_t a_ = (a);                                              \
       int64x1_t result;                                                \
       __asm__ ("ext %0.8b, %1.8b, %2.8b, #%3*8"                        \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_u8(a, b, c)                                                \
  __extension__                                                         \
    ({                                                                  \
       uint8x8_t b_ = (b);                                              \
       uint8x8_t a_ = (a);                                              \
       uint8x8_t result;                                                \
       __asm__ ("ext %0.8b,%1.8b,%2.8b,%3"                              \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_u16(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       uint16x4_t b_ = (b);                                             \
       uint16x4_t a_ = (a);                                             \
       uint16x4_t result;                                               \
       __asm__ ("ext %0.8b, %1.8b, %2.8b, #%3*2"                        \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_u32(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       uint32x2_t b_ = (b);                                             \
       uint32x2_t a_ = (a);                                             \
       uint32x2_t result;                                               \
       __asm__ ("ext %0.8b, %1.8b, %2.8b, #%3*4"                        \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vext_u64(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       uint64x1_t b_ = (b);                                             \
       uint64x1_t a_ = (a);                                             \
       uint64x1_t result;                                               \
       __asm__ ("ext %0.8b, %1.8b, %2.8b, #%3*8"                        \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_f32(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t b_ = (b);                                            \
       float32x4_t a_ = (a);                                            \
       float32x4_t result;                                              \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3*4"                     \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_f64(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       float64x2_t b_ = (b);                                            \
       float64x2_t a_ = (a);                                            \
       float64x2_t result;                                              \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3*8"                     \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_p8(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       poly8x16_t b_ = (b);                                             \
       poly8x16_t a_ = (a);                                             \
       poly8x16_t result;                                               \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3"                       \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_p16(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       poly16x8_t b_ = (b);                                             \
       poly16x8_t a_ = (a);                                             \
       poly16x8_t result;                                               \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3*2"                     \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_s8(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       int8x16_t b_ = (b);                                              \
       int8x16_t a_ = (a);                                              \
       int8x16_t result;                                                \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3"                       \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_s16(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int16x8_t a_ = (a);                                              \
       int16x8_t result;                                                \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3*2"                     \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_s32(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3*4"                     \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_s64(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3*8"                     \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_u8(a, b, c)                                               \
  __extension__                                                         \
    ({                                                                  \
       uint8x16_t b_ = (b);                                             \
       uint8x16_t a_ = (a);                                             \
       uint8x16_t result;                                               \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3"                       \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_u16(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint16x8_t a_ = (a);                                             \
       uint16x8_t result;                                               \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3*2"                     \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_u32(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3*4"                     \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vextq_u64(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t b_ = (b);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("ext %0.16b, %1.16b, %2.16b, #%3*8"                     \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vfma_f32 (float32x2_t a, float32x2_t b, float32x2_t c)
{
  float32x2_t result;
  __asm__ ("fmla %0.2s,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vfmaq_f32 (float32x4_t a, float32x4_t b, float32x4_t c)
{
  float32x4_t result;
  __asm__ ("fmla %0.4s,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vfmaq_f64 (float64x2_t a, float64x2_t b, float64x2_t c)
{
  float64x2_t result;
  __asm__ ("fmla %0.2d,%2.2d,%3.2d"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vfma_n_f32 (float32x2_t a, float32x2_t b, float32_t c)
{
  float32x2_t result;
  __asm__ ("fmla %0.2s, %2.2s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vfmaq_n_f32 (float32x4_t a, float32x4_t b, float32_t c)
{
  float32x4_t result;
  __asm__ ("fmla %0.4s, %2.4s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vfmaq_n_f64 (float64x2_t a, float64x2_t b, float64_t c)
{
  float64x2_t result;
  __asm__ ("fmla %0.2d, %2.2d, %3.d[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vfms_f32 (float32x2_t a, float32x2_t b, float32x2_t c)
{
  float32x2_t result;
  __asm__ ("fmls %0.2s,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vfmsq_f32 (float32x4_t a, float32x4_t b, float32x4_t c)
{
  float32x4_t result;
  __asm__ ("fmls %0.4s,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vfmsq_f64 (float64x2_t a, float64x2_t b, float64x2_t c)
{
  float64x2_t result;
  __asm__ ("fmls %0.2d,%2.2d,%3.2d"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vget_high_f32 (float32x4_t a)
{
  float32x2_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vget_high_f64 (float64x2_t a)
{
  float64x1_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vget_high_p8 (poly8x16_t a)
{
  poly8x8_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vget_high_p16 (poly16x8_t a)
{
  poly16x4_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vget_high_s8 (int8x16_t a)
{
  int8x8_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vget_high_s16 (int16x8_t a)
{
  int16x4_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vget_high_s32 (int32x4_t a)
{
  int32x2_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vget_high_s64 (int64x2_t a)
{
  int64x1_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vget_high_u8 (uint8x16_t a)
{
  uint8x8_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vget_high_u16 (uint16x8_t a)
{
  uint16x4_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vget_high_u32 (uint32x4_t a)
{
  uint32x2_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vget_high_u64 (uint64x2_t a)
{
  uint64x1_t result;
  __asm__ ("ins %0.d[0], %1.d[1]"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vhsub_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("shsub %0.8b, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vhsub_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("shsub %0.4h, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vhsub_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("shsub %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vhsub_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("uhsub %0.8b, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vhsub_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("uhsub %0.4h, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vhsub_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("uhsub %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vhsubq_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("shsub %0.16b, %1.16b, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vhsubq_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("shsub %0.8h, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vhsubq_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("shsub %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vhsubq_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("uhsub %0.16b, %1.16b, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vhsubq_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("uhsub %0.8h, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vhsubq_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("uhsub %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vld1_dup_f32 (const float32_t * a)
{
  float32x2_t result;
  __asm__ ("ld1r {%0.2s}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vld1_dup_f64 (const float64_t * a)
{
  float64x1_t result;
  __asm__ ("ld1r {%0.1d}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vld1_dup_p8 (const poly8_t * a)
{
  poly8x8_t result;
  __asm__ ("ld1r {%0.8b}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vld1_dup_p16 (const poly16_t * a)
{
  poly16x4_t result;
  __asm__ ("ld1r {%0.4h}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vld1_dup_s8 (const int8_t * a)
{
  int8x8_t result;
  __asm__ ("ld1r {%0.8b}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vld1_dup_s16 (const int16_t * a)
{
  int16x4_t result;
  __asm__ ("ld1r {%0.4h}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vld1_dup_s32 (const int32_t * a)
{
  int32x2_t result;
  __asm__ ("ld1r {%0.2s}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vld1_dup_s64 (const int64_t * a)
{
  int64x1_t result;
  __asm__ ("ld1r {%0.1d}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vld1_dup_u8 (const uint8_t * a)
{
  uint8x8_t result;
  __asm__ ("ld1r {%0.8b}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vld1_dup_u16 (const uint16_t * a)
{
  uint16x4_t result;
  __asm__ ("ld1r {%0.4h}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vld1_dup_u32 (const uint32_t * a)
{
  uint32x2_t result;
  __asm__ ("ld1r {%0.2s}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vld1_dup_u64 (const uint64_t * a)
{
  uint64x1_t result;
  __asm__ ("ld1r {%0.1d}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

#define vld1_lane_f32(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       float32x2_t b_ = (b);                                            \
       const float32_t * a_ = (a);                                      \
       float32x2_t result;                                              \
       __asm__ ("ld1 {%0.s}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_f64(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       float64x1_t b_ = (b);                                            \
       const float64_t * a_ = (a);                                      \
       float64x1_t result;                                              \
       __asm__ ("ld1 {%0.d}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_p8(a, b, c)                                           \
  __extension__                                                         \
    ({                                                                  \
       poly8x8_t b_ = (b);                                              \
       const poly8_t * a_ = (a);                                        \
       poly8x8_t result;                                                \
       __asm__ ("ld1 {%0.b}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_p16(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       poly16x4_t b_ = (b);                                             \
       const poly16_t * a_ = (a);                                       \
       poly16x4_t result;                                               \
       __asm__ ("ld1 {%0.h}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_s8(a, b, c)                                           \
  __extension__                                                         \
    ({                                                                  \
       int8x8_t b_ = (b);                                               \
       const int8_t * a_ = (a);                                         \
       int8x8_t result;                                                 \
       __asm__ ("ld1 {%0.b}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_s16(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int16x4_t b_ = (b);                                              \
       const int16_t * a_ = (a);                                        \
       int16x4_t result;                                                \
       __asm__ ("ld1 {%0.h}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_s32(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int32x2_t b_ = (b);                                              \
       const int32_t * a_ = (a);                                        \
       int32x2_t result;                                                \
       __asm__ ("ld1 {%0.s}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_s64(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int64x1_t b_ = (b);                                              \
       const int64_t * a_ = (a);                                        \
       int64x1_t result;                                                \
       __asm__ ("ld1 {%0.d}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_u8(a, b, c)                                           \
  __extension__                                                         \
    ({                                                                  \
       uint8x8_t b_ = (b);                                              \
       const uint8_t * a_ = (a);                                        \
       uint8x8_t result;                                                \
       __asm__ ("ld1 {%0.b}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_u16(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint16x4_t b_ = (b);                                             \
       const uint16_t * a_ = (a);                                       \
       uint16x4_t result;                                               \
       __asm__ ("ld1 {%0.h}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_u32(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint32x2_t b_ = (b);                                             \
       const uint32_t * a_ = (a);                                       \
       uint32x2_t result;                                               \
       __asm__ ("ld1 {%0.s}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1_lane_u64(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint64x1_t b_ = (b);                                             \
       const uint64_t * a_ = (a);                                       \
       uint64x1_t result;                                               \
       __asm__ ("ld1 {%0.d}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i" (c), "Utv"(*a_), "0"(b_)                          \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vld1q_dup_f32 (const float32_t * a)
{
  float32x4_t result;
  __asm__ ("ld1r {%0.4s}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vld1q_dup_f64 (const float64_t * a)
{
  float64x2_t result;
  __asm__ ("ld1r {%0.2d}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vld1q_dup_p8 (const poly8_t * a)
{
  poly8x16_t result;
  __asm__ ("ld1r {%0.16b}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vld1q_dup_p16 (const poly16_t * a)
{
  poly16x8_t result;
  __asm__ ("ld1r {%0.8h}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vld1q_dup_s8 (const int8_t * a)
{
  int8x16_t result;
  __asm__ ("ld1r {%0.16b}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vld1q_dup_s16 (const int16_t * a)
{
  int16x8_t result;
  __asm__ ("ld1r {%0.8h}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vld1q_dup_s32 (const int32_t * a)
{
  int32x4_t result;
  __asm__ ("ld1r {%0.4s}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vld1q_dup_s64 (const int64_t * a)
{
  int64x2_t result;
  __asm__ ("ld1r {%0.2d}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vld1q_dup_u8 (const uint8_t * a)
{
  uint8x16_t result;
  __asm__ ("ld1r {%0.16b}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vld1q_dup_u16 (const uint16_t * a)
{
  uint16x8_t result;
  __asm__ ("ld1r {%0.8h}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vld1q_dup_u32 (const uint32_t * a)
{
  uint32x4_t result;
  __asm__ ("ld1r {%0.4s}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vld1q_dup_u64 (const uint64_t * a)
{
  uint64x2_t result;
  __asm__ ("ld1r {%0.2d}, %1"
	   : "=w"(result)
	   : "Utv"(*a)
	   : /* No clobbers */);
  return result;
}

#define vld1q_lane_f32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t b_ = (b);                                            \
       const float32_t * a_ = (a);                                      \
       float32x4_t result;                                              \
       __asm__ ("ld1 {%0.s}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_f64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       float64x2_t b_ = (b);                                            \
       const float64_t * a_ = (a);                                      \
       float64x2_t result;                                              \
       __asm__ ("ld1 {%0.d}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_p8(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       poly8x16_t b_ = (b);                                             \
       const poly8_t * a_ = (a);                                        \
       poly8x16_t result;                                               \
       __asm__ ("ld1 {%0.b}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_p16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       poly16x8_t b_ = (b);                                             \
       const poly16_t * a_ = (a);                                       \
       poly16x8_t result;                                               \
       __asm__ ("ld1 {%0.h}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_s8(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int8x16_t b_ = (b);                                              \
       const int8_t * a_ = (a);                                         \
       int8x16_t result;                                                \
       __asm__ ("ld1 {%0.b}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_s16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       const int16_t * a_ = (a);                                        \
       int16x8_t result;                                                \
       __asm__ ("ld1 {%0.h}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_s32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       const int32_t * a_ = (a);                                        \
       int32x4_t result;                                                \
       __asm__ ("ld1 {%0.s}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_s64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       const int64_t * a_ = (a);                                        \
       int64x2_t result;                                                \
       __asm__ ("ld1 {%0.d}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_u8(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint8x16_t b_ = (b);                                             \
       const uint8_t * a_ = (a);                                        \
       uint8x16_t result;                                               \
       __asm__ ("ld1 {%0.b}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_u16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       const uint16_t * a_ = (a);                                       \
       uint16x8_t result;                                               \
       __asm__ ("ld1 {%0.h}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_u32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       const uint32_t * a_ = (a);                                       \
       uint32x4_t result;                                               \
       __asm__ ("ld1 {%0.s}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vld1q_lane_u64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t b_ = (b);                                             \
       const uint64_t * a_ = (a);                                       \
       uint64x2_t result;                                               \
       __asm__ ("ld1 {%0.d}[%1], %2"                                    \
                : "=w"(result)                                          \
                : "i"(c), "Utv"(*a_), "0"(b_)                           \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmla_n_f32 (float32x2_t a, float32x2_t b, float32_t c)
{
  float32x2_t result;
  float32x2_t t1;
  __asm__ ("fmul %1.2s, %3.2s, %4.s[0]; fadd %0.2s, %0.2s, %1.2s"
           : "=w"(result), "=w"(t1)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmla_n_s16 (int16x4_t a, int16x4_t b, int16_t c)
{
  int16x4_t result;
  __asm__ ("mla %0.4h,%2.4h,%3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmla_n_s32 (int32x2_t a, int32x2_t b, int32_t c)
{
  int32x2_t result;
  __asm__ ("mla %0.2s,%2.2s,%3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmla_n_u16 (uint16x4_t a, uint16x4_t b, uint16_t c)
{
  uint16x4_t result;
  __asm__ ("mla %0.4h,%2.4h,%3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmla_n_u32 (uint32x2_t a, uint32x2_t b, uint32_t c)
{
  uint32x2_t result;
  __asm__ ("mla %0.2s,%2.2s,%3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmla_s8 (int8x8_t a, int8x8_t b, int8x8_t c)
{
  int8x8_t result;
  __asm__ ("mla %0.8b, %2.8b, %3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmla_s16 (int16x4_t a, int16x4_t b, int16x4_t c)
{
  int16x4_t result;
  __asm__ ("mla %0.4h, %2.4h, %3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmla_s32 (int32x2_t a, int32x2_t b, int32x2_t c)
{
  int32x2_t result;
  __asm__ ("mla %0.2s, %2.2s, %3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmla_u8 (uint8x8_t a, uint8x8_t b, uint8x8_t c)
{
  uint8x8_t result;
  __asm__ ("mla %0.8b, %2.8b, %3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmla_u16 (uint16x4_t a, uint16x4_t b, uint16x4_t c)
{
  uint16x4_t result;
  __asm__ ("mla %0.4h, %2.4h, %3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmla_u32 (uint32x2_t a, uint32x2_t b, uint32x2_t c)
{
  uint32x2_t result;
  __asm__ ("mla %0.2s, %2.2s, %3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

#define vmlal_high_lane_s16(a, b, c, d)                                 \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t c_ = (c);                                              \
       int16x8_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smlal2 %0.4s, %2.8h, %3.h[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_high_lane_s32(a, b, c, d)                                 \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t c_ = (c);                                              \
       int32x4_t b_ = (b);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smlal2 %0.2d, %2.4s, %3.s[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_high_lane_u16(a, b, c, d)                                 \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t c_ = (c);                                             \
       uint16x8_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umlal2 %0.4s, %2.8h, %3.h[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_high_lane_u32(a, b, c, d)                                 \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t c_ = (c);                                             \
       uint32x4_t b_ = (b);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umlal2 %0.2d, %2.4s, %3.s[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_high_laneq_s16(a, b, c, d)                                \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t c_ = (c);                                              \
       int16x8_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smlal2 %0.4s, %2.8h, %3.h[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_high_laneq_s32(a, b, c, d)                                \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t c_ = (c);                                              \
       int32x4_t b_ = (b);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smlal2 %0.2d, %2.4s, %3.s[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_high_laneq_u16(a, b, c, d)                                \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t c_ = (c);                                             \
       uint16x8_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umlal2 %0.4s, %2.8h, %3.h[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_high_laneq_u32(a, b, c, d)                                \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t c_ = (c);                                             \
       uint32x4_t b_ = (b);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umlal2 %0.2d, %2.4s, %3.s[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlal_high_n_s16 (int32x4_t a, int16x8_t b, int16_t c)
{
  int32x4_t result;
  __asm__ ("smlal2 %0.4s,%2.8h,%3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlal_high_n_s32 (int64x2_t a, int32x4_t b, int32_t c)
{
  int64x2_t result;
  __asm__ ("smlal2 %0.2d,%2.4s,%3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlal_high_n_u16 (uint32x4_t a, uint16x8_t b, uint16_t c)
{
  uint32x4_t result;
  __asm__ ("umlal2 %0.4s,%2.8h,%3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlal_high_n_u32 (uint64x2_t a, uint32x4_t b, uint32_t c)
{
  uint64x2_t result;
  __asm__ ("umlal2 %0.2d,%2.4s,%3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlal_high_s8 (int16x8_t a, int8x16_t b, int8x16_t c)
{
  int16x8_t result;
  __asm__ ("smlal2 %0.8h,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlal_high_s16 (int32x4_t a, int16x8_t b, int16x8_t c)
{
  int32x4_t result;
  __asm__ ("smlal2 %0.4s,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlal_high_s32 (int64x2_t a, int32x4_t b, int32x4_t c)
{
  int64x2_t result;
  __asm__ ("smlal2 %0.2d,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlal_high_u8 (uint16x8_t a, uint8x16_t b, uint8x16_t c)
{
  uint16x8_t result;
  __asm__ ("umlal2 %0.8h,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlal_high_u16 (uint32x4_t a, uint16x8_t b, uint16x8_t c)
{
  uint32x4_t result;
  __asm__ ("umlal2 %0.4s,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlal_high_u32 (uint64x2_t a, uint32x4_t b, uint32x4_t c)
{
  uint64x2_t result;
  __asm__ ("umlal2 %0.2d,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

#define vmlal_lane_s16(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       int16x4_t c_ = (c);                                              \
       int16x4_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smlal %0.4s,%2.4h,%3.h[%4]"                            \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_lane_s32(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       int32x2_t c_ = (c);                                              \
       int32x2_t b_ = (b);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smlal %0.2d,%2.2s,%3.s[%4]"                            \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_lane_u16(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint16x4_t c_ = (c);                                             \
       uint16x4_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umlal %0.4s,%2.4h,%3.h[%4]"                            \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_lane_u32(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint32x2_t c_ = (c);                                             \
       uint32x2_t b_ = (b);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umlal %0.2d, %2.2s, %3.s[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_laneq_s16(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t c_ = (c);                                              \
       int16x4_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smlal %0.4s, %2.4h, %3.h[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_laneq_s32(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t c_ = (c);                                              \
       int32x2_t b_ = (b);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smlal %0.2d, %2.2s, %3.s[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_laneq_u16(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t c_ = (c);                                             \
       uint16x4_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umlal %0.4s, %2.4h, %3.h[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlal_laneq_u32(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t c_ = (c);                                             \
       uint32x2_t b_ = (b);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umlal %0.2d, %2.2s, %3.s[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlal_n_s16 (int32x4_t a, int16x4_t b, int16_t c)
{
  int32x4_t result;
  __asm__ ("smlal %0.4s,%2.4h,%3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlal_n_s32 (int64x2_t a, int32x2_t b, int32_t c)
{
  int64x2_t result;
  __asm__ ("smlal %0.2d,%2.2s,%3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlal_n_u16 (uint32x4_t a, uint16x4_t b, uint16_t c)
{
  uint32x4_t result;
  __asm__ ("umlal %0.4s,%2.4h,%3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlal_n_u32 (uint64x2_t a, uint32x2_t b, uint32_t c)
{
  uint64x2_t result;
  __asm__ ("umlal %0.2d,%2.2s,%3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlal_s8 (int16x8_t a, int8x8_t b, int8x8_t c)
{
  int16x8_t result;
  __asm__ ("smlal %0.8h,%2.8b,%3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlal_s16 (int32x4_t a, int16x4_t b, int16x4_t c)
{
  int32x4_t result;
  __asm__ ("smlal %0.4s,%2.4h,%3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlal_s32 (int64x2_t a, int32x2_t b, int32x2_t c)
{
  int64x2_t result;
  __asm__ ("smlal %0.2d,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlal_u8 (uint16x8_t a, uint8x8_t b, uint8x8_t c)
{
  uint16x8_t result;
  __asm__ ("umlal %0.8h,%2.8b,%3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlal_u16 (uint32x4_t a, uint16x4_t b, uint16x4_t c)
{
  uint32x4_t result;
  __asm__ ("umlal %0.4s,%2.4h,%3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlal_u32 (uint64x2_t a, uint32x2_t b, uint32x2_t c)
{
  uint64x2_t result;
  __asm__ ("umlal %0.2d,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlaq_n_f32 (float32x4_t a, float32x4_t b, float32_t c)
{
  float32x4_t result;
  float32x4_t t1;
  __asm__ ("fmul %1.4s, %3.4s, %4.s[0]; fadd %0.4s, %0.4s, %1.4s"
           : "=w"(result), "=w"(t1)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmlaq_n_f64 (float64x2_t a, float64x2_t b, float64_t c)
{
  float64x2_t result;
  float64x2_t t1;
  __asm__ ("fmul %1.2d, %3.2d, %4.d[0]; fadd %0.2d, %0.2d, %1.2d"
           : "=w"(result), "=w"(t1)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlaq_n_s16 (int16x8_t a, int16x8_t b, int16_t c)
{
  int16x8_t result;
  __asm__ ("mla %0.8h,%2.8h,%3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlaq_n_s32 (int32x4_t a, int32x4_t b, int32_t c)
{
  int32x4_t result;
  __asm__ ("mla %0.4s,%2.4s,%3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlaq_n_u16 (uint16x8_t a, uint16x8_t b, uint16_t c)
{
  uint16x8_t result;
  __asm__ ("mla %0.8h,%2.8h,%3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlaq_n_u32 (uint32x4_t a, uint32x4_t b, uint32_t c)
{
  uint32x4_t result;
  __asm__ ("mla %0.4s,%2.4s,%3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmlaq_s8 (int8x16_t a, int8x16_t b, int8x16_t c)
{
  int8x16_t result;
  __asm__ ("mla %0.16b, %2.16b, %3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlaq_s16 (int16x8_t a, int16x8_t b, int16x8_t c)
{
  int16x8_t result;
  __asm__ ("mla %0.8h, %2.8h, %3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlaq_s32 (int32x4_t a, int32x4_t b, int32x4_t c)
{
  int32x4_t result;
  __asm__ ("mla %0.4s, %2.4s, %3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmlaq_u8 (uint8x16_t a, uint8x16_t b, uint8x16_t c)
{
  uint8x16_t result;
  __asm__ ("mla %0.16b, %2.16b, %3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlaq_u16 (uint16x8_t a, uint16x8_t b, uint16x8_t c)
{
  uint16x8_t result;
  __asm__ ("mla %0.8h, %2.8h, %3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlaq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  uint32x4_t result;
  __asm__ ("mla %0.4s, %2.4s, %3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmls_n_f32 (float32x2_t a, float32x2_t b, float32_t c)
{
  float32x2_t result;
  float32x2_t t1;
  __asm__ ("fmul %1.2s, %3.2s, %4.s[0]; fsub %0.2s, %0.2s, %1.2s"
           : "=w"(result), "=w"(t1)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmls_n_s16 (int16x4_t a, int16x4_t b, int16_t c)
{
  int16x4_t result;
  __asm__ ("mls %0.4h, %2.4h, %3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmls_n_s32 (int32x2_t a, int32x2_t b, int32_t c)
{
  int32x2_t result;
  __asm__ ("mls %0.2s, %2.2s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmls_n_u16 (uint16x4_t a, uint16x4_t b, uint16_t c)
{
  uint16x4_t result;
  __asm__ ("mls %0.4h, %2.4h, %3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmls_n_u32 (uint32x2_t a, uint32x2_t b, uint32_t c)
{
  uint32x2_t result;
  __asm__ ("mls %0.2s, %2.2s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmls_s8 (int8x8_t a, int8x8_t b, int8x8_t c)
{
  int8x8_t result;
  __asm__ ("mls %0.8b,%2.8b,%3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmls_s16 (int16x4_t a, int16x4_t b, int16x4_t c)
{
  int16x4_t result;
  __asm__ ("mls %0.4h,%2.4h,%3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmls_s32 (int32x2_t a, int32x2_t b, int32x2_t c)
{
  int32x2_t result;
  __asm__ ("mls %0.2s,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmls_u8 (uint8x8_t a, uint8x8_t b, uint8x8_t c)
{
  uint8x8_t result;
  __asm__ ("mls %0.8b,%2.8b,%3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmls_u16 (uint16x4_t a, uint16x4_t b, uint16x4_t c)
{
  uint16x4_t result;
  __asm__ ("mls %0.4h,%2.4h,%3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmls_u32 (uint32x2_t a, uint32x2_t b, uint32x2_t c)
{
  uint32x2_t result;
  __asm__ ("mls %0.2s,%2.2s,%3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

#define vmlsl_high_lane_s16(a, b, c, d)                                 \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t c_ = (c);                                              \
       int16x8_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smlsl2 %0.4s, %2.8h, %3.h[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_high_lane_s32(a, b, c, d)                                 \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t c_ = (c);                                              \
       int32x4_t b_ = (b);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smlsl2 %0.2d, %2.4s, %3.s[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_high_lane_u16(a, b, c, d)                                 \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t c_ = (c);                                             \
       uint16x8_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umlsl2 %0.4s, %2.8h, %3.h[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_high_lane_u32(a, b, c, d)                                 \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t c_ = (c);                                             \
       uint32x4_t b_ = (b);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umlsl2 %0.2d, %2.4s, %3.s[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_high_laneq_s16(a, b, c, d)                                \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t c_ = (c);                                              \
       int16x8_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smlsl2 %0.4s, %2.8h, %3.h[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_high_laneq_s32(a, b, c, d)                                \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t c_ = (c);                                              \
       int32x4_t b_ = (b);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smlsl2 %0.2d, %2.4s, %3.s[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_high_laneq_u16(a, b, c, d)                                \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t c_ = (c);                                             \
       uint16x8_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umlsl2 %0.4s, %2.8h, %3.h[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_high_laneq_u32(a, b, c, d)                                \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t c_ = (c);                                             \
       uint32x4_t b_ = (b);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umlsl2 %0.2d, %2.4s, %3.s[%4]"                         \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsl_high_n_s16 (int32x4_t a, int16x8_t b, int16_t c)
{
  int32x4_t result;
  __asm__ ("smlsl2 %0.4s, %2.8h, %3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlsl_high_n_s32 (int64x2_t a, int32x4_t b, int32_t c)
{
  int64x2_t result;
  __asm__ ("smlsl2 %0.2d, %2.4s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsl_high_n_u16 (uint32x4_t a, uint16x8_t b, uint16_t c)
{
  uint32x4_t result;
  __asm__ ("umlsl2 %0.4s, %2.8h, %3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlsl_high_n_u32 (uint64x2_t a, uint32x4_t b, uint32_t c)
{
  uint64x2_t result;
  __asm__ ("umlsl2 %0.2d, %2.4s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsl_high_s8 (int16x8_t a, int8x16_t b, int8x16_t c)
{
  int16x8_t result;
  __asm__ ("smlsl2 %0.8h,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsl_high_s16 (int32x4_t a, int16x8_t b, int16x8_t c)
{
  int32x4_t result;
  __asm__ ("smlsl2 %0.4s,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlsl_high_s32 (int64x2_t a, int32x4_t b, int32x4_t c)
{
  int64x2_t result;
  __asm__ ("smlsl2 %0.2d,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsl_high_u8 (uint16x8_t a, uint8x16_t b, uint8x16_t c)
{
  uint16x8_t result;
  __asm__ ("umlsl2 %0.8h,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsl_high_u16 (uint32x4_t a, uint16x8_t b, uint16x8_t c)
{
  uint32x4_t result;
  __asm__ ("umlsl2 %0.4s,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlsl_high_u32 (uint64x2_t a, uint32x4_t b, uint32x4_t c)
{
  uint64x2_t result;
  __asm__ ("umlsl2 %0.2d,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

#define vmlsl_lane_s16(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       int16x4_t c_ = (c);                                              \
       int16x4_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smlsl %0.4s, %2.4h, %3.h[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_lane_s32(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       int32x2_t c_ = (c);                                              \
       int32x2_t b_ = (b);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smlsl %0.2d, %2.2s, %3.s[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_lane_u16(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint16x4_t c_ = (c);                                             \
       uint16x4_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umlsl %0.4s, %2.4h, %3.h[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_lane_u32(a, b, c, d)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint32x2_t c_ = (c);                                             \
       uint32x2_t b_ = (b);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umlsl %0.2d, %2.2s, %3.s[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_laneq_s16(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t c_ = (c);                                              \
       int16x4_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smlsl %0.4s, %2.4h, %3.h[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_laneq_s32(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t c_ = (c);                                              \
       int32x2_t b_ = (b);                                              \
       int64x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smlsl %0.2d, %2.2s, %3.s[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_laneq_u16(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t c_ = (c);                                             \
       uint16x4_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umlsl %0.4s, %2.4h, %3.h[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "x"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmlsl_laneq_u32(a, b, c, d)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t c_ = (c);                                             \
       uint32x2_t b_ = (b);                                             \
       uint64x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umlsl %0.2d, %2.2s, %3.s[%4]"                          \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "w"(c_), "i"(d)                     \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsl_n_s16 (int32x4_t a, int16x4_t b, int16_t c)
{
  int32x4_t result;
  __asm__ ("smlsl %0.4s, %2.4h, %3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlsl_n_s32 (int64x2_t a, int32x2_t b, int32_t c)
{
  int64x2_t result;
  __asm__ ("smlsl %0.2d, %2.2s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsl_n_u16 (uint32x4_t a, uint16x4_t b, uint16_t c)
{
  uint32x4_t result;
  __asm__ ("umlsl %0.4s, %2.4h, %3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlsl_n_u32 (uint64x2_t a, uint32x2_t b, uint32_t c)
{
  uint64x2_t result;
  __asm__ ("umlsl %0.2d, %2.2s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsl_s8 (int16x8_t a, int8x8_t b, int8x8_t c)
{
  int16x8_t result;
  __asm__ ("smlsl %0.8h, %2.8b, %3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsl_s16 (int32x4_t a, int16x4_t b, int16x4_t c)
{
  int32x4_t result;
  __asm__ ("smlsl %0.4s, %2.4h, %3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlsl_s32 (int64x2_t a, int32x2_t b, int32x2_t c)
{
  int64x2_t result;
  __asm__ ("smlsl %0.2d, %2.2s, %3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsl_u8 (uint16x8_t a, uint8x8_t b, uint8x8_t c)
{
  uint16x8_t result;
  __asm__ ("umlsl %0.8h, %2.8b, %3.8b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsl_u16 (uint32x4_t a, uint16x4_t b, uint16x4_t c)
{
  uint32x4_t result;
  __asm__ ("umlsl %0.4s, %2.4h, %3.4h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlsl_u32 (uint64x2_t a, uint32x2_t b, uint32x2_t c)
{
  uint64x2_t result;
  __asm__ ("umlsl %0.2d, %2.2s, %3.2s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlsq_n_f32 (float32x4_t a, float32x4_t b, float32_t c)
{
  float32x4_t result;
  float32x4_t t1;
  __asm__ ("fmul %1.4s, %3.4s, %4.s[0]; fsub %0.4s, %0.4s, %1.4s"
           : "=w"(result), "=w"(t1)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmlsq_n_f64 (float64x2_t a, float64x2_t b, float64_t c)
{
  float64x2_t result;
  float64x2_t t1;
  __asm__ ("fmul %1.2d, %3.2d, %4.d[0]; fsub %0.2d, %0.2d, %1.2d"
           : "=w"(result), "=w"(t1)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsq_n_s16 (int16x8_t a, int16x8_t b, int16_t c)
{
  int16x8_t result;
  __asm__ ("mls %0.8h, %2.8h, %3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsq_n_s32 (int32x4_t a, int32x4_t b, int32_t c)
{
  int32x4_t result;
  __asm__ ("mls %0.4s, %2.4s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsq_n_u16 (uint16x8_t a, uint16x8_t b, uint16_t c)
{
  uint16x8_t result;
  __asm__ ("mls %0.8h, %2.8h, %3.h[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "x"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsq_n_u32 (uint32x4_t a, uint32x4_t b, uint32_t c)
{
  uint32x4_t result;
  __asm__ ("mls %0.4s, %2.4s, %3.s[0]"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmlsq_s8 (int8x16_t a, int8x16_t b, int8x16_t c)
{
  int8x16_t result;
  __asm__ ("mls %0.16b,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsq_s16 (int16x8_t a, int16x8_t b, int16x8_t c)
{
  int16x8_t result;
  __asm__ ("mls %0.8h,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsq_s32 (int32x4_t a, int32x4_t b, int32x4_t c)
{
  int32x4_t result;
  __asm__ ("mls %0.4s,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmlsq_u8 (uint8x16_t a, uint8x16_t b, uint8x16_t c)
{
  uint8x16_t result;
  __asm__ ("mls %0.16b,%2.16b,%3.16b"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsq_u16 (uint16x8_t a, uint16x8_t b, uint16x8_t c)
{
  uint16x8_t result;
  __asm__ ("mls %0.8h,%2.8h,%3.8h"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  uint32x4_t result;
  __asm__ ("mls %0.4s,%2.4s,%3.4s"
           : "=w"(result)
           : "0"(a), "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmovl_high_s8 (int8x16_t a)
{
  int16x8_t result;
  __asm__ ("sshll2 %0.8h,%1.16b,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmovl_high_s16 (int16x8_t a)
{
  int32x4_t result;
  __asm__ ("sshll2 %0.4s,%1.8h,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmovl_high_s32 (int32x4_t a)
{
  int64x2_t result;
  __asm__ ("sshll2 %0.2d,%1.4s,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmovl_high_u8 (uint8x16_t a)
{
  uint16x8_t result;
  __asm__ ("ushll2 %0.8h,%1.16b,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmovl_high_u16 (uint16x8_t a)
{
  uint32x4_t result;
  __asm__ ("ushll2 %0.4s,%1.8h,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmovl_high_u32 (uint32x4_t a)
{
  uint64x2_t result;
  __asm__ ("ushll2 %0.2d,%1.4s,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmovl_s8 (int8x8_t a)
{
  int16x8_t result;
  __asm__ ("sshll %0.8h,%1.8b,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmovl_s16 (int16x4_t a)
{
  int32x4_t result;
  __asm__ ("sshll %0.4s,%1.4h,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmovl_s32 (int32x2_t a)
{
  int64x2_t result;
  __asm__ ("sshll %0.2d,%1.2s,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmovl_u8 (uint8x8_t a)
{
  uint16x8_t result;
  __asm__ ("ushll %0.8h,%1.8b,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmovl_u16 (uint16x4_t a)
{
  uint32x4_t result;
  __asm__ ("ushll %0.4s,%1.4h,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmovl_u32 (uint32x2_t a)
{
  uint64x2_t result;
  __asm__ ("ushll %0.2d,%1.2s,#0"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmovn_high_s16 (int8x8_t a, int16x8_t b)
{
  int8x16_t result = vcombine_s8 (a, vcreate_s8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("xtn2 %0.16b,%1.8h"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmovn_high_s32 (int16x4_t a, int32x4_t b)
{
  int16x8_t result = vcombine_s16 (a, vcreate_s16 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("xtn2 %0.8h,%1.4s"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmovn_high_s64 (int32x2_t a, int64x2_t b)
{
  int32x4_t result = vcombine_s32 (a, vcreate_s32 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("xtn2 %0.4s,%1.2d"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmovn_high_u16 (uint8x8_t a, uint16x8_t b)
{
  uint8x16_t result = vcombine_u8 (a, vcreate_u8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("xtn2 %0.16b,%1.8h"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmovn_high_u32 (uint16x4_t a, uint32x4_t b)
{
  uint16x8_t result = vcombine_u16 (a, vcreate_u16 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("xtn2 %0.8h,%1.4s"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmovn_high_u64 (uint32x2_t a, uint64x2_t b)
{
  uint32x4_t result = vcombine_u32 (a, vcreate_u32 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("xtn2 %0.4s,%1.2d"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmovn_s16 (int16x8_t a)
{
  int8x8_t result;
  __asm__ ("xtn %0.8b,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmovn_s32 (int32x4_t a)
{
  int16x4_t result;
  __asm__ ("xtn %0.4h,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmovn_s64 (int64x2_t a)
{
  int32x2_t result;
  __asm__ ("xtn %0.2s,%1.2d"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmovn_u16 (uint16x8_t a)
{
  uint8x8_t result;
  __asm__ ("xtn %0.8b,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmovn_u32 (uint32x4_t a)
{
  uint16x4_t result;
  __asm__ ("xtn %0.4h,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmovn_u64 (uint64x2_t a)
{
  uint32x2_t result;
  __asm__ ("xtn %0.2s,%1.2d"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmul_n_f32 (float32x2_t a, float32_t b)
{
  float32x2_t result;
  __asm__ ("fmul %0.2s,%1.2s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmul_n_s16 (int16x4_t a, int16_t b)
{
  int16x4_t result;
  __asm__ ("mul %0.4h,%1.4h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmul_n_s32 (int32x2_t a, int32_t b)
{
  int32x2_t result;
  __asm__ ("mul %0.2s,%1.2s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmul_n_u16 (uint16x4_t a, uint16_t b)
{
  uint16x4_t result;
  __asm__ ("mul %0.4h,%1.4h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmul_n_u32 (uint32x2_t a, uint32_t b)
{
  uint32x2_t result;
  __asm__ ("mul %0.2s,%1.2s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

#define vmuld_lane_f64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       float64x2_t b_ = (b);                                            \
       float64_t a_ = (a);                                              \
       float64_t result;                                                \
       __asm__ ("fmul %d0,%d1,%2.d[%3]"                                 \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_high_lane_s16(a, b, c)                                    \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int16x8_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smull2 %0.4s, %1.8h, %2.h[%3]"                         \
                : "=w"(result)                                          \
                : "w"(a_), "x"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_high_lane_s32(a, b, c)                                    \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smull2 %0.2d, %1.4s, %2.s[%3]"                         \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_high_lane_u16(a, b, c)                                    \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint16x8_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umull2 %0.4s, %1.8h, %2.h[%3]"                         \
                : "=w"(result)                                          \
                : "w"(a_), "x"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_high_lane_u32(a, b, c)                                    \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umull2 %0.2d, %1.4s, %2.s[%3]"                         \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_high_laneq_s16(a, b, c)                                   \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int16x8_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smull2 %0.4s, %1.8h, %2.h[%3]"                         \
                : "=w"(result)                                          \
                : "w"(a_), "x"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_high_laneq_s32(a, b, c)                                   \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int32x4_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smull2 %0.2d, %1.4s, %2.s[%3]"                         \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_high_laneq_u16(a, b, c)                                   \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint16x8_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umull2 %0.4s, %1.8h, %2.h[%3]"                         \
                : "=w"(result)                                          \
                : "w"(a_), "x"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_high_laneq_u32(a, b, c)                                   \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint32x4_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umull2 %0.2d, %1.4s, %2.s[%3]"                         \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmull_high_n_s16 (int16x8_t a, int16_t b)
{
  int32x4_t result;
  __asm__ ("smull2 %0.4s,%1.8h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmull_high_n_s32 (int32x4_t a, int32_t b)
{
  int64x2_t result;
  __asm__ ("smull2 %0.2d,%1.4s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmull_high_n_u16 (uint16x8_t a, uint16_t b)
{
  uint32x4_t result;
  __asm__ ("umull2 %0.4s,%1.8h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmull_high_n_u32 (uint32x4_t a, uint32_t b)
{
  uint64x2_t result;
  __asm__ ("umull2 %0.2d,%1.4s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vmull_high_p8 (poly8x16_t a, poly8x16_t b)
{
  poly16x8_t result;
  __asm__ ("pmull2 %0.8h,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmull_high_s8 (int8x16_t a, int8x16_t b)
{
  int16x8_t result;
  __asm__ ("smull2 %0.8h,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmull_high_s16 (int16x8_t a, int16x8_t b)
{
  int32x4_t result;
  __asm__ ("smull2 %0.4s,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmull_high_s32 (int32x4_t a, int32x4_t b)
{
  int64x2_t result;
  __asm__ ("smull2 %0.2d,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmull_high_u8 (uint8x16_t a, uint8x16_t b)
{
  uint16x8_t result;
  __asm__ ("umull2 %0.8h,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmull_high_u16 (uint16x8_t a, uint16x8_t b)
{
  uint32x4_t result;
  __asm__ ("umull2 %0.4s,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmull_high_u32 (uint32x4_t a, uint32x4_t b)
{
  uint64x2_t result;
  __asm__ ("umull2 %0.2d,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

#define vmull_lane_s16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int16x4_t b_ = (b);                                              \
       int16x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smull %0.4s,%1.4h,%2.h[%3]"                            \
                : "=w"(result)                                          \
                : "w"(a_), "x"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_lane_s32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int32x2_t b_ = (b);                                              \
       int32x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smull %0.2d,%1.2s,%2.s[%3]"                            \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_lane_u16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint16x4_t b_ = (b);                                             \
       uint16x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umull %0.4s,%1.4h,%2.h[%3]"                            \
                : "=w"(result)                                          \
                : "w"(a_), "x"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_lane_u32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint32x2_t b_ = (b);                                             \
       uint32x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umull %0.2d, %1.2s, %2.s[%3]"                          \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_laneq_s16(a, b, c)                                        \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int16x4_t a_ = (a);                                              \
       int32x4_t result;                                                \
       __asm__ ("smull %0.4s, %1.4h, %2.h[%3]"                          \
                : "=w"(result)                                          \
                : "w"(a_), "x"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_laneq_s32(a, b, c)                                        \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int32x2_t a_ = (a);                                              \
       int64x2_t result;                                                \
       __asm__ ("smull %0.2d, %1.2s, %2.s[%3]"                          \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_laneq_u16(a, b, c)                                        \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint16x4_t a_ = (a);                                             \
       uint32x4_t result;                                               \
       __asm__ ("umull %0.4s, %1.4h, %2.h[%3]"                          \
                : "=w"(result)                                          \
                : "w"(a_), "x"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmull_laneq_u32(a, b, c)                                        \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint32x2_t a_ = (a);                                             \
       uint64x2_t result;                                               \
       __asm__ ("umull %0.2d, %1.2s, %2.s[%3]"                          \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmull_n_s16 (int16x4_t a, int16_t b)
{
  int32x4_t result;
  __asm__ ("smull %0.4s,%1.4h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmull_n_s32 (int32x2_t a, int32_t b)
{
  int64x2_t result;
  __asm__ ("smull %0.2d,%1.2s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmull_n_u16 (uint16x4_t a, uint16_t b)
{
  uint32x4_t result;
  __asm__ ("umull %0.4s,%1.4h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmull_n_u32 (uint32x2_t a, uint32_t b)
{
  uint64x2_t result;
  __asm__ ("umull %0.2d,%1.2s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vmull_p8 (poly8x8_t a, poly8x8_t b)
{
  poly16x8_t result;
  __asm__ ("pmull %0.8h, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmull_s8 (int8x8_t a, int8x8_t b)
{
  int16x8_t result;
  __asm__ ("smull %0.8h, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmull_s16 (int16x4_t a, int16x4_t b)
{
  int32x4_t result;
  __asm__ ("smull %0.4s, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmull_s32 (int32x2_t a, int32x2_t b)
{
  int64x2_t result;
  __asm__ ("smull %0.2d, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmull_u8 (uint8x8_t a, uint8x8_t b)
{
  uint16x8_t result;
  __asm__ ("umull %0.8h, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmull_u16 (uint16x4_t a, uint16x4_t b)
{
  uint32x4_t result;
  __asm__ ("umull %0.4s, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmull_u32 (uint32x2_t a, uint32x2_t b)
{
  uint64x2_t result;
  __asm__ ("umull %0.2d, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmulq_n_f32 (float32x4_t a, float32_t b)
{
  float32x4_t result;
  __asm__ ("fmul %0.4s,%1.4s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmulq_n_f64 (float64x2_t a, float64_t b)
{
  float64x2_t result;
  __asm__ ("fmul %0.2d,%1.2d,%2.d[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmulq_n_s16 (int16x8_t a, int16_t b)
{
  int16x8_t result;
  __asm__ ("mul %0.8h,%1.8h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmulq_n_s32 (int32x4_t a, int32_t b)
{
  int32x4_t result;
  __asm__ ("mul %0.4s,%1.4s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmulq_n_u16 (uint16x8_t a, uint16_t b)
{
  uint16x8_t result;
  __asm__ ("mul %0.8h,%1.8h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmulq_n_u32 (uint32x4_t a, uint32_t b)
{
  uint32x4_t result;
  __asm__ ("mul %0.4s,%1.4s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

#define vmuls_lane_f32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t b_ = (b);                                            \
       float32_t a_ = (a);                                              \
       float32_t result;                                                \
       __asm__ ("fmul %s0,%s1,%2.s[%3]"                                 \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmulx_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("fmulx %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

#define vmulx_lane_f32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t b_ = (b);                                            \
       float32x2_t a_ = (a);                                            \
       float32x2_t result;                                              \
       __asm__ ("fmulx %0.2s,%1.2s,%2.s[%3]"                            \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vmulxd_f64 (float64_t a, float64_t b)
{
  float64_t result;
  __asm__ ("fmulx %d0, %d1, %d2"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmulxq_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("fmulx %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmulxq_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("fmulx %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

#define vmulxq_lane_f32(a, b, c)                                        \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t b_ = (b);                                            \
       float32x4_t a_ = (a);                                            \
       float32x4_t result;                                              \
       __asm__ ("fmulx %0.4s,%1.4s,%2.s[%3]"                            \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vmulxq_lane_f64(a, b, c)                                        \
  __extension__                                                         \
    ({                                                                  \
       float64x2_t b_ = (b);                                            \
       float64x2_t a_ = (a);                                            \
       float64x2_t result;                                              \
       __asm__ ("fmulx %0.2d,%1.2d,%2.d[%3]"                            \
                : "=w"(result)                                          \
                : "w"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vmulxs_f32 (float32_t a, float32_t b)
{
  float32_t result;
  __asm__ ("fmulx %s0, %s1, %s2"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vmvn_p8 (poly8x8_t a)
{
  poly8x8_t result;
  __asm__ ("mvn %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmvn_s8 (int8x8_t a)
{
  int8x8_t result;
  __asm__ ("mvn %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmvn_s16 (int16x4_t a)
{
  int16x4_t result;
  __asm__ ("mvn %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmvn_s32 (int32x2_t a)
{
  int32x2_t result;
  __asm__ ("mvn %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmvn_u8 (uint8x8_t a)
{
  uint8x8_t result;
  __asm__ ("mvn %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmvn_u16 (uint16x4_t a)
{
  uint16x4_t result;
  __asm__ ("mvn %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmvn_u32 (uint32x2_t a)
{
  uint32x2_t result;
  __asm__ ("mvn %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vmvnq_p8 (poly8x16_t a)
{
  poly8x16_t result;
  __asm__ ("mvn %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmvnq_s8 (int8x16_t a)
{
  int8x16_t result;
  __asm__ ("mvn %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmvnq_s16 (int16x8_t a)
{
  int16x8_t result;
  __asm__ ("mvn %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmvnq_s32 (int32x4_t a)
{
  int32x4_t result;
  __asm__ ("mvn %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmvnq_u8 (uint8x16_t a)
{
  uint8x16_t result;
  __asm__ ("mvn %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmvnq_u16 (uint16x8_t a)
{
  uint16x8_t result;
  __asm__ ("mvn %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmvnq_u32 (uint32x4_t a)
{
  uint32x4_t result;
  __asm__ ("mvn %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}


__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpadal_s8 (int16x4_t a, int8x8_t b)
{
  int16x4_t result;
  __asm__ ("sadalp %0.4h,%2.8b"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpadal_s16 (int32x2_t a, int16x4_t b)
{
  int32x2_t result;
  __asm__ ("sadalp %0.2s,%2.4h"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vpadal_s32 (int64x1_t a, int32x2_t b)
{
  int64x1_t result;
  __asm__ ("sadalp %0.1d,%2.2s"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpadal_u8 (uint16x4_t a, uint8x8_t b)
{
  uint16x4_t result;
  __asm__ ("uadalp %0.4h,%2.8b"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpadal_u16 (uint32x2_t a, uint16x4_t b)
{
  uint32x2_t result;
  __asm__ ("uadalp %0.2s,%2.4h"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vpadal_u32 (uint64x1_t a, uint32x2_t b)
{
  uint64x1_t result;
  __asm__ ("uadalp %0.1d,%2.2s"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vpadalq_s8 (int16x8_t a, int8x16_t b)
{
  int16x8_t result;
  __asm__ ("sadalp %0.8h,%2.16b"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vpadalq_s16 (int32x4_t a, int16x8_t b)
{
  int32x4_t result;
  __asm__ ("sadalp %0.4s,%2.8h"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vpadalq_s32 (int64x2_t a, int32x4_t b)
{
  int64x2_t result;
  __asm__ ("sadalp %0.2d,%2.4s"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vpadalq_u8 (uint16x8_t a, uint8x16_t b)
{
  uint16x8_t result;
  __asm__ ("uadalp %0.8h,%2.16b"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vpadalq_u16 (uint32x4_t a, uint16x8_t b)
{
  uint32x4_t result;
  __asm__ ("uadalp %0.4s,%2.8h"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vpadalq_u32 (uint64x2_t a, uint32x4_t b)
{
  uint64x2_t result;
  __asm__ ("uadalp %0.2d,%2.4s"
           : "=w"(result)
           : "0"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vpadd_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("faddp %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vpadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_addpv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_addpv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_addpv2si (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vpadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_addpv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_addpv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_addpv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vpaddd_f64 (float64x2_t a)
{
  float64_t result;
  __asm__ ("faddp %d0,%1.2d"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpaddl_s8 (int8x8_t a)
{
  int16x4_t result;
  __asm__ ("saddlp %0.4h,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpaddl_s16 (int16x4_t a)
{
  int32x2_t result;
  __asm__ ("saddlp %0.2s,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vpaddl_s32 (int32x2_t a)
{
  int64x1_t result;
  __asm__ ("saddlp %0.1d,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpaddl_u8 (uint8x8_t a)
{
  uint16x4_t result;
  __asm__ ("uaddlp %0.4h,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpaddl_u16 (uint16x4_t a)
{
  uint32x2_t result;
  __asm__ ("uaddlp %0.2s,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vpaddl_u32 (uint32x2_t a)
{
  uint64x1_t result;
  __asm__ ("uaddlp %0.1d,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vpaddlq_s8 (int8x16_t a)
{
  int16x8_t result;
  __asm__ ("saddlp %0.8h,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vpaddlq_s16 (int16x8_t a)
{
  int32x4_t result;
  __asm__ ("saddlp %0.4s,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vpaddlq_s32 (int32x4_t a)
{
  int64x2_t result;
  __asm__ ("saddlp %0.2d,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vpaddlq_u8 (uint8x16_t a)
{
  uint16x8_t result;
  __asm__ ("uaddlp %0.8h,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vpaddlq_u16 (uint16x8_t a)
{
  uint32x4_t result;
  __asm__ ("uaddlp %0.4s,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vpaddlq_u32 (uint32x4_t a)
{
  uint64x2_t result;
  __asm__ ("uaddlp %0.2d,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vpaddq_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("faddp %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vpaddq_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("faddp %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vpaddq_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("addp %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vpaddq_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("addp %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vpaddq_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("addp %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vpaddq_s64 (int64x2_t a, int64x2_t b)
{
  int64x2_t result;
  __asm__ ("addp %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vpaddq_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("addp %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vpaddq_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("addp %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vpaddq_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("addp %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vpaddq_u64 (uint64x2_t a, uint64x2_t b)
{
  uint64x2_t result;
  __asm__ ("addp %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vpadds_f32 (float32x2_t a)
{
  float32_t result;
  __asm__ ("faddp %s0,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vpmax_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("fmaxp %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vpmax_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("smaxp %0.8b, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpmax_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("smaxp %0.4h, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpmax_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("smaxp %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vpmax_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("umaxp %0.8b, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpmax_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("umaxp %0.4h, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpmax_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("umaxp %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vpmaxnm_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("fmaxnmp %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vpmaxnmq_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("fmaxnmp %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vpmaxnmq_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("fmaxnmp %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vpmaxnmqd_f64 (float64x2_t a)
{
  float64_t result;
  __asm__ ("fmaxnmp %d0,%1.2d"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vpmaxnms_f32 (float32x2_t a)
{
  float32_t result;
  __asm__ ("fmaxnmp %s0,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vpmaxq_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("fmaxp %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vpmaxq_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("fmaxp %0.2d, %1.2d, %2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vpmaxq_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("smaxp %0.16b, %1.16b, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vpmaxq_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("smaxp %0.8h, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vpmaxq_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("smaxp %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vpmaxq_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("umaxp %0.16b, %1.16b, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vpmaxq_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("umaxp %0.8h, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vpmaxq_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("umaxp %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vpmaxqd_f64 (float64x2_t a)
{
  float64_t result;
  __asm__ ("fmaxp %d0,%1.2d"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vpmaxs_f32 (float32x2_t a)
{
  float32_t result;
  __asm__ ("fmaxp %s0,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vpmin_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("fminp %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vpmin_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("sminp %0.8b, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpmin_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("sminp %0.4h, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpmin_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("sminp %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vpmin_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("uminp %0.8b, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpmin_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("uminp %0.4h, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpmin_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("uminp %0.2s, %1.2s, %2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vpminnm_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("fminnmp %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vpminnmq_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("fminnmp %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vpminnmq_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("fminnmp %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vpminnmqd_f64 (float64x2_t a)
{
  float64_t result;
  __asm__ ("fminnmp %d0,%1.2d"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vpminnms_f32 (float32x2_t a)
{
  float32_t result;
  __asm__ ("fminnmp %s0,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vpminq_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("fminp %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vpminq_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("fminp %0.2d, %1.2d, %2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vpminq_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("sminp %0.16b, %1.16b, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vpminq_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("sminp %0.8h, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vpminq_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("sminp %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vpminq_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("uminp %0.16b, %1.16b, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vpminq_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("uminp %0.8h, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vpminq_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("uminp %0.4s, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vpminqd_f64 (float64x2_t a)
{
  float64_t result;
  __asm__ ("fminp %d0,%1.2d"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vpmins_f32 (float32x2_t a)
{
  float32_t result;
  __asm__ ("fminp %s0,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqdmulh_n_s16 (int16x4_t a, int16_t b)
{
  int16x4_t result;
  __asm__ ("sqdmulh %0.4h,%1.4h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqdmulh_n_s32 (int32x2_t a, int32_t b)
{
  int32x2_t result;
  __asm__ ("sqdmulh %0.2s,%1.2s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqdmulhq_n_s16 (int16x8_t a, int16_t b)
{
  int16x8_t result;
  __asm__ ("sqdmulh %0.8h,%1.8h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmulhq_n_s32 (int32x4_t a, int32_t b)
{
  int32x4_t result;
  __asm__ ("sqdmulh %0.4s,%1.4s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqmovn_high_s16 (int8x8_t a, int16x8_t b)
{
  int8x16_t result = vcombine_s8 (a, vcreate_s8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("sqxtn2 %0.16b, %1.8h"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqmovn_high_s32 (int16x4_t a, int32x4_t b)
{
  int16x8_t result = vcombine_s16 (a, vcreate_s16 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("sqxtn2 %0.8h, %1.4s"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqmovn_high_s64 (int32x2_t a, int64x2_t b)
{
  int32x4_t result = vcombine_s32 (a, vcreate_s32 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("sqxtn2 %0.4s, %1.2d"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqmovn_high_u16 (uint8x8_t a, uint16x8_t b)
{
  uint8x16_t result = vcombine_u8 (a, vcreate_u8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("uqxtn2 %0.16b, %1.8h"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqmovn_high_u32 (uint16x4_t a, uint32x4_t b)
{
  uint16x8_t result = vcombine_u16 (a, vcreate_u16 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("uqxtn2 %0.8h, %1.4s"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqmovn_high_u64 (uint32x2_t a, uint64x2_t b)
{
  uint32x4_t result = vcombine_u32 (a, vcreate_u32 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("uqxtn2 %0.4s, %1.2d"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqmovun_high_s16 (uint8x8_t a, int16x8_t b)
{
  uint8x16_t result = vcombine_u8 (a, vcreate_u8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("sqxtun2 %0.16b, %1.8h"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqmovun_high_s32 (uint16x4_t a, int32x4_t b)
{
  uint16x8_t result = vcombine_u16 (a, vcreate_u16 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("sqxtun2 %0.8h, %1.4s"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqmovun_high_s64 (uint32x2_t a, int64x2_t b)
{
  uint32x4_t result = vcombine_u32 (a, vcreate_u32 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("sqxtun2 %0.4s, %1.2d"
           : "+w"(result)
           : "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmulh_n_s16 (int16x4_t a, int16_t b)
{
  int16x4_t result;
  __asm__ ("sqrdmulh %0.4h,%1.4h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmulh_n_s32 (int32x2_t a, int32_t b)
{
  int32x2_t result;
  __asm__ ("sqrdmulh %0.2s,%1.2s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmulhq_n_s16 (int16x8_t a, int16_t b)
{
  int16x8_t result;
  __asm__ ("sqrdmulh %0.8h,%1.8h,%2.h[0]"
           : "=w"(result)
           : "w"(a), "x"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmulhq_n_s32 (int32x4_t a, int32_t b)
{
  int32x4_t result;
  __asm__ ("sqrdmulh %0.4s,%1.4s,%2.s[0]"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

#define vqrshrn_high_n_s16(a, b, c)                                     \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int8x8_t a_ = (a);                                               \
       int8x16_t result = vcombine_s8                                   \
                            (a_, vcreate_s8                             \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("sqrshrn2 %0.16b, %1.8h, #%2"                           \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqrshrn_high_n_s32(a, b, c)                                     \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int16x4_t a_ = (a);                                              \
       int16x8_t result = vcombine_s16                                  \
                            (a_, vcreate_s16                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("sqrshrn2 %0.8h, %1.4s, #%2"                            \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqrshrn_high_n_s64(a, b, c)                                     \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       int32x2_t a_ = (a);                                              \
       int32x4_t result = vcombine_s32                                  \
                            (a_, vcreate_s32                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("sqrshrn2 %0.4s, %1.2d, #%2"                            \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqrshrn_high_n_u16(a, b, c)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint8x8_t a_ = (a);                                              \
       uint8x16_t result = vcombine_u8                                  \
                             (a_, vcreate_u8                            \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("uqrshrn2 %0.16b, %1.8h, #%2"                           \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqrshrn_high_n_u32(a, b, c)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint16x4_t a_ = (a);                                             \
       uint16x8_t result = vcombine_u16                                 \
                             (a_, vcreate_u16                           \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("uqrshrn2 %0.8h, %1.4s, #%2"                            \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqrshrn_high_n_u64(a, b, c)                                     \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t b_ = (b);                                             \
       uint32x2_t a_ = (a);                                             \
       uint32x4_t result = vcombine_u32                                 \
                             (a_, vcreate_u32                           \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("uqrshrn2 %0.4s, %1.2d, #%2"                            \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqrshrun_high_n_s16(a, b, c)                                    \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       uint8x8_t a_ = (a);                                              \
       uint8x16_t result = vcombine_u8                                  \
                             (a_, vcreate_u8                            \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("sqrshrun2 %0.16b, %1.8h, #%2"                          \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqrshrun_high_n_s32(a, b, c)                                    \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       uint16x4_t a_ = (a);                                             \
       uint16x8_t result = vcombine_u16                                 \
                             (a_, vcreate_u16                           \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("sqrshrun2 %0.8h, %1.4s, #%2"                           \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqrshrun_high_n_s64(a, b, c)                                    \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       uint32x2_t a_ = (a);                                             \
       uint32x4_t result = vcombine_u32                                 \
                             (a_, vcreate_u32                           \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("sqrshrun2 %0.4s, %1.2d, #%2"                           \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqshrn_high_n_s16(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int8x8_t a_ = (a);                                               \
       int8x16_t result = vcombine_s8                                   \
                            (a_, vcreate_s8                             \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("sqshrn2 %0.16b, %1.8h, #%2"                            \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqshrn_high_n_s32(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int16x4_t a_ = (a);                                              \
       int16x8_t result = vcombine_s16                                  \
                            (a_, vcreate_s16                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("sqshrn2 %0.8h, %1.4s, #%2"                             \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqshrn_high_n_s64(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       int32x2_t a_ = (a);                                              \
       int32x4_t result = vcombine_s32                                  \
                            (a_, vcreate_s32                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("sqshrn2 %0.4s, %1.2d, #%2"                             \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqshrn_high_n_u16(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint8x8_t a_ = (a);                                              \
       uint8x16_t result = vcombine_u8                                  \
                             (a_, vcreate_u8                            \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("uqshrn2 %0.16b, %1.8h, #%2"                            \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqshrn_high_n_u32(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint16x4_t a_ = (a);                                             \
       uint16x8_t result = vcombine_u16                                 \
                             (a_, vcreate_u16                           \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("uqshrn2 %0.8h, %1.4s, #%2"                             \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqshrn_high_n_u64(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t b_ = (b);                                             \
       uint32x2_t a_ = (a);                                             \
       uint32x4_t result = vcombine_u32                                 \
                             (a_, vcreate_u32                           \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("uqshrn2 %0.4s, %1.2d, #%2"                             \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqshrun_high_n_s16(a, b, c)                                     \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       uint8x8_t a_ = (a);                                              \
       uint8x16_t result = vcombine_u8                                  \
                             (a_, vcreate_u8                            \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("sqshrun2 %0.16b, %1.8h, #%2"                           \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqshrun_high_n_s32(a, b, c)                                     \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       uint16x4_t a_ = (a);                                             \
       uint16x8_t result = vcombine_u16                                 \
                             (a_, vcreate_u16                           \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("sqshrun2 %0.8h, %1.4s, #%2"                            \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vqshrun_high_n_s64(a, b, c)                                     \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       uint32x2_t a_ = (a);                                             \
       uint32x4_t result = vcombine_u32                                 \
                             (a_, vcreate_u32                           \
                                    (__AARCH64_UINT64_C (0x0)));        \
       __asm__ ("sqshrun2 %0.4s, %1.2d, #%2"                            \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrbit_s8 (int8x8_t a)
{
  int8x8_t result;
  __asm__ ("rbit %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrbit_u8 (uint8x8_t a)
{
  uint8x8_t result;
  __asm__ ("rbit %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrbitq_s8 (int8x16_t a)
{
  int8x16_t result;
  __asm__ ("rbit %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrbitq_u8 (uint8x16_t a)
{
  uint8x16_t result;
  __asm__ ("rbit %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrecpe_u32 (uint32x2_t a)
{
  uint32x2_t result;
  __asm__ ("urecpe %0.2s,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrecpeq_u32 (uint32x4_t a)
{
  uint32x4_t result;
  __asm__ ("urecpe %0.4s,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vrev16_p8 (poly8x8_t a)
{
  poly8x8_t result;
  __asm__ ("rev16 %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrev16_s8 (int8x8_t a)
{
  int8x8_t result;
  __asm__ ("rev16 %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrev16_u8 (uint8x8_t a)
{
  uint8x8_t result;
  __asm__ ("rev16 %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vrev16q_p8 (poly8x16_t a)
{
  poly8x16_t result;
  __asm__ ("rev16 %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrev16q_s8 (int8x16_t a)
{
  int8x16_t result;
  __asm__ ("rev16 %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrev16q_u8 (uint8x16_t a)
{
  uint8x16_t result;
  __asm__ ("rev16 %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vrev32_p8 (poly8x8_t a)
{
  poly8x8_t result;
  __asm__ ("rev32 %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vrev32_p16 (poly16x4_t a)
{
  poly16x4_t result;
  __asm__ ("rev32 %0.4h,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrev32_s8 (int8x8_t a)
{
  int8x8_t result;
  __asm__ ("rev32 %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrev32_s16 (int16x4_t a)
{
  int16x4_t result;
  __asm__ ("rev32 %0.4h,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrev32_u8 (uint8x8_t a)
{
  uint8x8_t result;
  __asm__ ("rev32 %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrev32_u16 (uint16x4_t a)
{
  uint16x4_t result;
  __asm__ ("rev32 %0.4h,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vrev32q_p8 (poly8x16_t a)
{
  poly8x16_t result;
  __asm__ ("rev32 %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vrev32q_p16 (poly16x8_t a)
{
  poly16x8_t result;
  __asm__ ("rev32 %0.8h,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrev32q_s8 (int8x16_t a)
{
  int8x16_t result;
  __asm__ ("rev32 %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrev32q_s16 (int16x8_t a)
{
  int16x8_t result;
  __asm__ ("rev32 %0.8h,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrev32q_u8 (uint8x16_t a)
{
  uint8x16_t result;
  __asm__ ("rev32 %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrev32q_u16 (uint16x8_t a)
{
  uint16x8_t result;
  __asm__ ("rev32 %0.8h,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrev64_f32 (float32x2_t a)
{
  float32x2_t result;
  __asm__ ("rev64 %0.2s,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vrev64_p8 (poly8x8_t a)
{
  poly8x8_t result;
  __asm__ ("rev64 %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vrev64_p16 (poly16x4_t a)
{
  poly16x4_t result;
  __asm__ ("rev64 %0.4h,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrev64_s8 (int8x8_t a)
{
  int8x8_t result;
  __asm__ ("rev64 %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrev64_s16 (int16x4_t a)
{
  int16x4_t result;
  __asm__ ("rev64 %0.4h,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrev64_s32 (int32x2_t a)
{
  int32x2_t result;
  __asm__ ("rev64 %0.2s,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrev64_u8 (uint8x8_t a)
{
  uint8x8_t result;
  __asm__ ("rev64 %0.8b,%1.8b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrev64_u16 (uint16x4_t a)
{
  uint16x4_t result;
  __asm__ ("rev64 %0.4h,%1.4h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrev64_u32 (uint32x2_t a)
{
  uint32x2_t result;
  __asm__ ("rev64 %0.2s,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrev64q_f32 (float32x4_t a)
{
  float32x4_t result;
  __asm__ ("rev64 %0.4s,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vrev64q_p8 (poly8x16_t a)
{
  poly8x16_t result;
  __asm__ ("rev64 %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vrev64q_p16 (poly16x8_t a)
{
  poly16x8_t result;
  __asm__ ("rev64 %0.8h,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrev64q_s8 (int8x16_t a)
{
  int8x16_t result;
  __asm__ ("rev64 %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrev64q_s16 (int16x8_t a)
{
  int16x8_t result;
  __asm__ ("rev64 %0.8h,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrev64q_s32 (int32x4_t a)
{
  int32x4_t result;
  __asm__ ("rev64 %0.4s,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrev64q_u8 (uint8x16_t a)
{
  uint8x16_t result;
  __asm__ ("rev64 %0.16b,%1.16b"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrev64q_u16 (uint16x8_t a)
{
  uint16x8_t result;
  __asm__ ("rev64 %0.8h,%1.8h"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrev64q_u32 (uint32x4_t a)
{
  uint32x4_t result;
  __asm__ ("rev64 %0.4s,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

#define vrshrn_high_n_s16(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int8x8_t a_ = (a);                                               \
       int8x16_t result = vcombine_s8                                   \
                            (a_, vcreate_s8                             \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("rshrn2 %0.16b,%1.8h,#%2"                               \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_high_n_s32(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int16x4_t a_ = (a);                                              \
       int16x8_t result = vcombine_s16                                  \
                            (a_, vcreate_s16                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("rshrn2 %0.8h,%1.4s,#%2"                                \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_high_n_s64(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       int32x2_t a_ = (a);                                              \
       int32x4_t result = vcombine_s32                                  \
                            (a_, vcreate_s32                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("rshrn2 %0.4s,%1.2d,#%2"                                \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_high_n_u16(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint8x8_t a_ = (a);                                              \
       uint8x16_t result = vcombine_u8                                  \
                            (a_, vcreate_u8                             \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("rshrn2 %0.16b,%1.8h,#%2"                               \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_high_n_u32(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint16x4_t a_ = (a);                                             \
       uint16x8_t result = vcombine_u16                                 \
                            (a_, vcreate_u16                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("rshrn2 %0.8h,%1.4s,#%2"                                \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_high_n_u64(a, b, c)                                      \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t b_ = (b);                                             \
       uint32x2_t a_ = (a);                                             \
       uint32x4_t result = vcombine_u32                                 \
                            (a_, vcreate_u32                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("rshrn2 %0.4s,%1.2d,#%2"                                \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_n_s16(a, b)                                              \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t a_ = (a);                                              \
       int8x8_t result;                                                 \
       __asm__ ("rshrn %0.8b,%1.8h,%2"                                  \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_n_s32(a, b)                                              \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t a_ = (a);                                              \
       int16x4_t result;                                                \
       __asm__ ("rshrn %0.4h,%1.4s,%2"                                  \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_n_s64(a, b)                                              \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t a_ = (a);                                              \
       int32x2_t result;                                                \
       __asm__ ("rshrn %0.2s,%1.2d,%2"                                  \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_n_u16(a, b)                                              \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t a_ = (a);                                             \
       uint8x8_t result;                                                \
       __asm__ ("rshrn %0.8b,%1.8h,%2"                                  \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_n_u32(a, b)                                              \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t a_ = (a);                                             \
       uint16x4_t result;                                               \
       __asm__ ("rshrn %0.4h,%1.4s,%2"                                  \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vrshrn_n_u64(a, b)                                              \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t a_ = (a);                                             \
       uint32x2_t result;                                               \
       __asm__ ("rshrn %0.2s,%1.2d,%2"                                  \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrsqrte_f32 (float32x2_t a)
{
  float32x2_t result;
  __asm__ ("frsqrte %0.2s,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vrsqrte_f64 (float64x1_t a)
{
  float64x1_t result;
  __asm__ ("frsqrte %d0,%d1"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrsqrte_u32 (uint32x2_t a)
{
  uint32x2_t result;
  __asm__ ("ursqrte %0.2s,%1.2s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vrsqrted_f64 (float64_t a)
{
  float64_t result;
  __asm__ ("frsqrte %d0,%d1"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrsqrteq_f32 (float32x4_t a)
{
  float32x4_t result;
  __asm__ ("frsqrte %0.4s,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrsqrteq_f64 (float64x2_t a)
{
  float64x2_t result;
  __asm__ ("frsqrte %0.2d,%1.2d"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrsqrteq_u32 (uint32x4_t a)
{
  uint32x4_t result;
  __asm__ ("ursqrte %0.4s,%1.4s"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vrsqrtes_f32 (float32_t a)
{
  float32_t result;
  __asm__ ("frsqrte %s0,%s1"
           : "=w"(result)
           : "w"(a)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrsqrts_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("frsqrts %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vrsqrtsd_f64 (float64_t a, float64_t b)
{
  float64_t result;
  __asm__ ("frsqrts %d0,%d1,%d2"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrsqrtsq_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("frsqrts %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrsqrtsq_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("frsqrts %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vrsqrtss_f32 (float32_t a, float32_t b)
{
  float32_t result;
  __asm__ ("frsqrts %s0,%s1,%s2"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrsrtsq_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("frsqrts %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrsubhn_high_s16 (int8x8_t a, int16x8_t b, int16x8_t c)
{
  int8x16_t result = vcombine_s8 (a, vcreate_s8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("rsubhn2 %0.16b, %1.8h, %2.8h"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrsubhn_high_s32 (int16x4_t a, int32x4_t b, int32x4_t c)
{
  int16x8_t result = vcombine_s16 (a, vcreate_s16 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("rsubhn2 %0.8h, %1.4s, %2.4s"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrsubhn_high_s64 (int32x2_t a, int64x2_t b, int64x2_t c)
{
  int32x4_t result = vcombine_s32 (a, vcreate_s32 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("rsubhn2 %0.4s, %1.2d, %2.2d"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrsubhn_high_u16 (uint8x8_t a, uint16x8_t b, uint16x8_t c)
{
  uint8x16_t result = vcombine_u8 (a, vcreate_u8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("rsubhn2 %0.16b, %1.8h, %2.8h"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrsubhn_high_u32 (uint16x4_t a, uint32x4_t b, uint32x4_t c)
{
  uint16x8_t result = vcombine_u16 (a, vcreate_u16 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("rsubhn2 %0.8h, %1.4s, %2.4s"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrsubhn_high_u64 (uint32x2_t a, uint64x2_t b, uint64x2_t c)
{
  uint32x4_t result = vcombine_u32 (a, vcreate_u32 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("rsubhn2 %0.4s, %1.2d, %2.2d"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrsubhn_s16 (int16x8_t a, int16x8_t b)
{
  int8x8_t result;
  __asm__ ("rsubhn %0.8b, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrsubhn_s32 (int32x4_t a, int32x4_t b)
{
  int16x4_t result;
  __asm__ ("rsubhn %0.4h, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrsubhn_s64 (int64x2_t a, int64x2_t b)
{
  int32x2_t result;
  __asm__ ("rsubhn %0.2s, %1.2d, %2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrsubhn_u16 (uint16x8_t a, uint16x8_t b)
{
  uint8x8_t result;
  __asm__ ("rsubhn %0.8b, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrsubhn_u32 (uint32x4_t a, uint32x4_t b)
{
  uint16x4_t result;
  __asm__ ("rsubhn %0.4h, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrsubhn_u64 (uint64x2_t a, uint64x2_t b)
{
  uint32x2_t result;
  __asm__ ("rsubhn %0.2s, %1.2d, %2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

#define vset_lane_f32(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       float32x2_t b_ = (b);                                            \
       float32_t a_ = (a);                                              \
       float32x2_t result;                                              \
       __asm__ ("ins %0.s[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_f64(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       float64x1_t b_ = (b);                                            \
       float64_t a_ = (a);                                              \
       float64x1_t result;                                              \
       __asm__ ("ins %0.d[%3], %x1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_p8(a, b, c)                                           \
  __extension__                                                         \
    ({                                                                  \
       poly8x8_t b_ = (b);                                              \
       poly8_t a_ = (a);                                                \
       poly8x8_t result;                                                \
       __asm__ ("ins %0.b[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_p16(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       poly16x4_t b_ = (b);                                             \
       poly16_t a_ = (a);                                               \
       poly16x4_t result;                                               \
       __asm__ ("ins %0.h[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_s8(a, b, c)                                           \
  __extension__                                                         \
    ({                                                                  \
       int8x8_t b_ = (b);                                               \
       int8_t a_ = (a);                                                 \
       int8x8_t result;                                                 \
       __asm__ ("ins %0.b[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_s16(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int16x4_t b_ = (b);                                              \
       int16_t a_ = (a);                                                \
       int16x4_t result;                                                \
       __asm__ ("ins %0.h[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_s32(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int32x2_t b_ = (b);                                              \
       int32_t a_ = (a);                                                \
       int32x2_t result;                                                \
       __asm__ ("ins %0.s[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_s64(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int64x1_t b_ = (b);                                              \
       int64_t a_ = (a);                                                \
       int64x1_t result;                                                \
       __asm__ ("ins %0.d[%3], %x1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_u8(a, b, c)                                           \
  __extension__                                                         \
    ({                                                                  \
       uint8x8_t b_ = (b);                                              \
       uint8_t a_ = (a);                                                \
       uint8x8_t result;                                                \
       __asm__ ("ins %0.b[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_u16(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint16x4_t b_ = (b);                                             \
       uint16_t a_ = (a);                                               \
       uint16x4_t result;                                               \
       __asm__ ("ins %0.h[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_u32(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint32x2_t b_ = (b);                                             \
       uint32_t a_ = (a);                                               \
       uint32x2_t result;                                               \
       __asm__ ("ins %0.s[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vset_lane_u64(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint64x1_t b_ = (b);                                             \
       uint64_t a_ = (a);                                               \
       uint64x1_t result;                                               \
       __asm__ ("ins %0.d[%3], %x1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_f32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t b_ = (b);                                            \
       float32_t a_ = (a);                                              \
       float32x4_t result;                                              \
       __asm__ ("ins %0.s[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_f64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       float64x2_t b_ = (b);                                            \
       float64_t a_ = (a);                                              \
       float64x2_t result;                                              \
       __asm__ ("ins %0.d[%3], %x1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_p8(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       poly8x16_t b_ = (b);                                             \
       poly8_t a_ = (a);                                                \
       poly8x16_t result;                                               \
       __asm__ ("ins %0.b[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_p16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       poly16x8_t b_ = (b);                                             \
       poly16_t a_ = (a);                                               \
       poly16x8_t result;                                               \
       __asm__ ("ins %0.h[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_s8(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int8x16_t b_ = (b);                                              \
       int8_t a_ = (a);                                                 \
       int8x16_t result;                                                \
       __asm__ ("ins %0.b[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_s16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int16_t a_ = (a);                                                \
       int16x8_t result;                                                \
       __asm__ ("ins %0.h[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_s32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int32_t a_ = (a);                                                \
       int32x4_t result;                                                \
       __asm__ ("ins %0.s[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_s64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       int64_t a_ = (a);                                                \
       int64x2_t result;                                                \
       __asm__ ("ins %0.d[%3], %x1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_u8(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint8x16_t b_ = (b);                                             \
       uint8_t a_ = (a);                                                \
       uint8x16_t result;                                               \
       __asm__ ("ins %0.b[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_u16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint16_t a_ = (a);                                               \
       uint16x8_t result;                                               \
       __asm__ ("ins %0.h[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_u32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint32_t a_ = (a);                                               \
       uint32x4_t result;                                               \
       __asm__ ("ins %0.s[%3], %w1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsetq_lane_u64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t b_ = (b);                                             \
       uint64_t a_ = (a);                                               \
       uint64x2_t result;                                               \
       __asm__ ("ins %0.d[%3], %x1"                                     \
                : "=w"(result)                                          \
                : "r"(a_), "0"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_high_n_s16(a, b, c)                                       \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int8x8_t a_ = (a);                                               \
       int8x16_t result = vcombine_s8                                   \
                            (a_, vcreate_s8                             \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("shrn2 %0.16b,%1.8h,#%2"                                \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_high_n_s32(a, b, c)                                       \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int16x4_t a_ = (a);                                              \
       int16x8_t result = vcombine_s16                                  \
                            (a_, vcreate_s16                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("shrn2 %0.8h,%1.4s,#%2"                                 \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_high_n_s64(a, b, c)                                       \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       int32x2_t a_ = (a);                                              \
       int32x4_t result = vcombine_s32                                  \
                            (a_, vcreate_s32                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("shrn2 %0.4s,%1.2d,#%2"                                 \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_high_n_u16(a, b, c)                                       \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint8x8_t a_ = (a);                                              \
       uint8x16_t result = vcombine_u8                                  \
                            (a_, vcreate_u8                             \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("shrn2 %0.16b,%1.8h,#%2"                                \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_high_n_u32(a, b, c)                                       \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint16x4_t a_ = (a);                                             \
       uint16x8_t result = vcombine_u16                                 \
                            (a_, vcreate_u16                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("shrn2 %0.8h,%1.4s,#%2"                                 \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_high_n_u64(a, b, c)                                       \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t b_ = (b);                                             \
       uint32x2_t a_ = (a);                                             \
       uint32x4_t result = vcombine_u32                                 \
                            (a_, vcreate_u32                            \
                                   (__AARCH64_UINT64_C (0x0)));         \
       __asm__ ("shrn2 %0.4s,%1.2d,#%2"                                 \
                : "+w"(result)                                          \
                : "w"(b_), "i"(c)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_n_s16(a, b)                                               \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t a_ = (a);                                              \
       int8x8_t result;                                                 \
       __asm__ ("shrn %0.8b,%1.8h,%2"                                   \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_n_s32(a, b)                                               \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t a_ = (a);                                              \
       int16x4_t result;                                                \
       __asm__ ("shrn %0.4h,%1.4s,%2"                                   \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_n_s64(a, b)                                               \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t a_ = (a);                                              \
       int32x2_t result;                                                \
       __asm__ ("shrn %0.2s,%1.2d,%2"                                   \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_n_u16(a, b)                                               \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t a_ = (a);                                             \
       uint8x8_t result;                                                \
       __asm__ ("shrn %0.8b,%1.8h,%2"                                   \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_n_u32(a, b)                                               \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t a_ = (a);                                             \
       uint16x4_t result;                                               \
       __asm__ ("shrn %0.4h,%1.4s,%2"                                   \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vshrn_n_u64(a, b)                                               \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t a_ = (a);                                             \
       uint32x2_t result;                                               \
       __asm__ ("shrn %0.2s,%1.2d,%2"                                   \
                : "=w"(result)                                          \
                : "w"(a_), "i"(b)                                       \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsli_n_p8(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       poly8x8_t b_ = (b);                                              \
       poly8x8_t a_ = (a);                                              \
       poly8x8_t result;                                                \
       __asm__ ("sli %0.8b,%2.8b,%3"                                    \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsli_n_p16(a, b, c)                                             \
  __extension__                                                         \
    ({                                                                  \
       poly16x4_t b_ = (b);                                             \
       poly16x4_t a_ = (a);                                             \
       poly16x4_t result;                                               \
       __asm__ ("sli %0.4h,%2.4h,%3"                                    \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsliq_n_p8(a, b, c)                                             \
  __extension__                                                         \
    ({                                                                  \
       poly8x16_t b_ = (b);                                             \
       poly8x16_t a_ = (a);                                             \
       poly8x16_t result;                                               \
       __asm__ ("sli %0.16b,%2.16b,%3"                                  \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsliq_n_p16(a, b, c)                                            \
  __extension__                                                         \
    ({                                                                  \
       poly16x8_t b_ = (b);                                             \
       poly16x8_t a_ = (a);                                             \
       poly16x8_t result;                                               \
       __asm__ ("sli %0.8h,%2.8h,%3"                                    \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsri_n_p8(a, b, c)                                              \
  __extension__                                                         \
    ({                                                                  \
       poly8x8_t b_ = (b);                                              \
       poly8x8_t a_ = (a);                                              \
       poly8x8_t result;                                                \
       __asm__ ("sri %0.8b,%2.8b,%3"                                    \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsri_n_p16(a, b, c)                                             \
  __extension__                                                         \
    ({                                                                  \
       poly16x4_t b_ = (b);                                             \
       poly16x4_t a_ = (a);                                             \
       poly16x4_t result;                                               \
       __asm__ ("sri %0.4h,%2.4h,%3"                                    \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsriq_n_p8(a, b, c)                                             \
  __extension__                                                         \
    ({                                                                  \
       poly8x16_t b_ = (b);                                             \
       poly8x16_t a_ = (a);                                             \
       poly8x16_t result;                                               \
       __asm__ ("sri %0.16b,%2.16b,%3"                                  \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vsriq_n_p16(a, b, c)                                            \
  __extension__                                                         \
    ({                                                                  \
       poly16x8_t b_ = (b);                                             \
       poly16x8_t a_ = (a);                                             \
       poly16x8_t result;                                               \
       __asm__ ("sri %0.8h,%2.8h,%3"                                    \
                : "=w"(result)                                          \
                : "0"(a_), "w"(b_), "i"(c)                              \
                : /* No clobbers */);                                   \
       result;                                                          \
     })

#define vst1_lane_f32(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       float32x2_t b_ = (b);                                            \
       float32_t * a_ = (a);                                            \
       __asm__ ("st1 {%1.s}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_f64(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       float64x1_t b_ = (b);                                            \
       float64_t * a_ = (a);                                            \
       __asm__ ("st1 {%1.d}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_p8(a, b, c)                                           \
  __extension__                                                         \
    ({                                                                  \
       poly8x8_t b_ = (b);                                              \
       poly8_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.b}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_p16(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       poly16x4_t b_ = (b);                                             \
       poly16_t * a_ = (a);                                             \
       __asm__ ("st1 {%1.h}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_s8(a, b, c)                                           \
  __extension__                                                         \
    ({                                                                  \
       int8x8_t b_ = (b);                                               \
       int8_t * a_ = (a);                                               \
       __asm__ ("st1 {%1.b}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_s16(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int16x4_t b_ = (b);                                              \
       int16_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.h}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_s32(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int32x2_t b_ = (b);                                              \
       int32_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.s}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_s64(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int64x1_t b_ = (b);                                              \
       int64_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.d}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_u8(a, b, c)                                           \
  __extension__                                                         \
    ({                                                                  \
       uint8x8_t b_ = (b);                                              \
       uint8_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.b}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_u16(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint16x4_t b_ = (b);                                             \
       uint16_t * a_ = (a);                                             \
       __asm__ ("st1 {%1.h}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_u32(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint32x2_t b_ = (b);                                             \
       uint32_t * a_ = (a);                                             \
       __asm__ ("st1 {%1.s}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1_lane_u64(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint64x1_t b_ = (b);                                             \
       uint64_t * a_ = (a);                                             \
       __asm__ ("st1 {%1.d}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })


#define vst1q_lane_f32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       float32x4_t b_ = (b);                                            \
       float32_t * a_ = (a);                                            \
       __asm__ ("st1 {%1.s}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_f64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       float64x2_t b_ = (b);                                            \
       float64_t * a_ = (a);                                            \
       __asm__ ("st1 {%1.d}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_p8(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       poly8x16_t b_ = (b);                                             \
       poly8_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.b}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_p16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       poly16x8_t b_ = (b);                                             \
       poly16_t * a_ = (a);                                             \
       __asm__ ("st1 {%1.h}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_s8(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       int8x16_t b_ = (b);                                              \
       int8_t * a_ = (a);                                               \
       __asm__ ("st1 {%1.b}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_s16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int16x8_t b_ = (b);                                              \
       int16_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.h}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_s32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int32x4_t b_ = (b);                                              \
       int32_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.s}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_s64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       int64x2_t b_ = (b);                                              \
       int64_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.d}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_u8(a, b, c)                                          \
  __extension__                                                         \
    ({                                                                  \
       uint8x16_t b_ = (b);                                             \
       uint8_t * a_ = (a);                                              \
       __asm__ ("st1 {%1.b}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_u16(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint16x8_t b_ = (b);                                             \
       uint16_t * a_ = (a);                                             \
       __asm__ ("st1 {%1.h}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_u32(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint32x4_t b_ = (b);                                             \
       uint32_t * a_ = (a);                                             \
       __asm__ ("st1 {%1.s}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

#define vst1q_lane_u64(a, b, c)                                         \
  __extension__                                                         \
    ({                                                                  \
       uint64x2_t b_ = (b);                                             \
       uint64_t * a_ = (a);                                             \
       __asm__ ("st1 {%1.d}[%2],[%0]"                                   \
                :                                                       \
                : "r"(a_), "w"(b_), "i"(c)                              \
                : "memory");                                            \
     })

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vsubhn_high_s16 (int8x8_t a, int16x8_t b, int16x8_t c)
{
  int8x16_t result = vcombine_s8 (a, vcreate_s8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("subhn2 %0.16b, %1.8h, %2.8h"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsubhn_high_s32 (int16x4_t a, int32x4_t b, int32x4_t c)
{
  int16x8_t result = vcombine_s16 (a, vcreate_s16 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("subhn2 %0.8h, %1.4s, %2.4s"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsubhn_high_s64 (int32x2_t a, int64x2_t b, int64x2_t c)
{
  int32x4_t result = vcombine_s32 (a, vcreate_s32 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("subhn2 %0.4s, %1.2d, %2.2d"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsubhn_high_u16 (uint8x8_t a, uint16x8_t b, uint16x8_t c)
{
  uint8x16_t result = vcombine_u8 (a, vcreate_u8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("subhn2 %0.16b, %1.8h, %2.8h"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsubhn_high_u32 (uint16x4_t a, uint32x4_t b, uint32x4_t c)
{
  uint16x8_t result = vcombine_u16 (a, vcreate_u16 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("subhn2 %0.8h, %1.4s, %2.4s"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsubhn_high_u64 (uint32x2_t a, uint64x2_t b, uint64x2_t c)
{
  uint32x4_t result = vcombine_u32 (a, vcreate_u32 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("subhn2 %0.4s, %1.2d, %2.2d"
           : "+w"(result)
           : "w"(b), "w"(c)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vsubhn_s16 (int16x8_t a, int16x8_t b)
{
  int8x8_t result;
  __asm__ ("subhn %0.8b, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vsubhn_s32 (int32x4_t a, int32x4_t b)
{
  int16x4_t result;
  __asm__ ("subhn %0.4h, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vsubhn_s64 (int64x2_t a, int64x2_t b)
{
  int32x2_t result;
  __asm__ ("subhn %0.2s, %1.2d, %2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsubhn_u16 (uint16x8_t a, uint16x8_t b)
{
  uint8x8_t result;
  __asm__ ("subhn %0.8b, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsubhn_u32 (uint32x4_t a, uint32x4_t b)
{
  uint16x4_t result;
  __asm__ ("subhn %0.4h, %1.4s, %2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsubhn_u64 (uint64x2_t a, uint64x2_t b)
{
  uint32x2_t result;
  __asm__ ("subhn %0.2s, %1.2d, %2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vtrn1_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("trn1 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtrn1_p8 (poly8x8_t a, poly8x8_t b)
{
  poly8x8_t result;
  __asm__ ("trn1 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vtrn1_p16 (poly16x4_t a, poly16x4_t b)
{
  poly16x4_t result;
  __asm__ ("trn1 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtrn1_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("trn1 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vtrn1_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("trn1 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vtrn1_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("trn1 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtrn1_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("trn1 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vtrn1_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("trn1 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vtrn1_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("trn1 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vtrn1q_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("trn1 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vtrn1q_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("trn1 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vtrn1q_p8 (poly8x16_t a, poly8x16_t b)
{
  poly8x16_t result;
  __asm__ ("trn1 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vtrn1q_p16 (poly16x8_t a, poly16x8_t b)
{
  poly16x8_t result;
  __asm__ ("trn1 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vtrn1q_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("trn1 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vtrn1q_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("trn1 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vtrn1q_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("trn1 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vtrn1q_s64 (int64x2_t a, int64x2_t b)
{
  int64x2_t result;
  __asm__ ("trn1 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vtrn1q_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("trn1 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vtrn1q_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("trn1 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vtrn1q_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("trn1 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vtrn1q_u64 (uint64x2_t a, uint64x2_t b)
{
  uint64x2_t result;
  __asm__ ("trn1 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vtrn2_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("trn2 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtrn2_p8 (poly8x8_t a, poly8x8_t b)
{
  poly8x8_t result;
  __asm__ ("trn2 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vtrn2_p16 (poly16x4_t a, poly16x4_t b)
{
  poly16x4_t result;
  __asm__ ("trn2 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtrn2_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("trn2 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vtrn2_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("trn2 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vtrn2_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("trn2 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtrn2_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("trn2 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vtrn2_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("trn2 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vtrn2_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("trn2 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vtrn2q_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("trn2 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vtrn2q_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("trn2 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vtrn2q_p8 (poly8x16_t a, poly8x16_t b)
{
  poly8x16_t result;
  __asm__ ("trn2 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vtrn2q_p16 (poly16x8_t a, poly16x8_t b)
{
  poly16x8_t result;
  __asm__ ("trn2 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vtrn2q_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("trn2 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vtrn2q_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("trn2 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vtrn2q_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("trn2 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vtrn2q_s64 (int64x2_t a, int64x2_t b)
{
  int64x2_t result;
  __asm__ ("trn2 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vtrn2q_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("trn2 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vtrn2q_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("trn2 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vtrn2q_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("trn2 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vtrn2q_u64 (uint64x2_t a, uint64x2_t b)
{
  uint64x2_t result;
  __asm__ ("trn2 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtst_p8 (poly8x8_t a, poly8x8_t b)
{
  uint8x8_t result;
  __asm__ ("cmtst %0.8b, %1.8b, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vtst_p16 (poly16x4_t a, poly16x4_t b)
{
  uint16x4_t result;
  __asm__ ("cmtst %0.4h, %1.4h, %2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vtstq_p8 (poly8x16_t a, poly8x16_t b)
{
  uint8x16_t result;
  __asm__ ("cmtst %0.16b, %1.16b, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vtstq_p16 (poly16x8_t a, poly16x8_t b)
{
  uint16x8_t result;
  __asm__ ("cmtst %0.8h, %1.8h, %2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vuzp1_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("uzp1 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vuzp1_p8 (poly8x8_t a, poly8x8_t b)
{
  poly8x8_t result;
  __asm__ ("uzp1 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vuzp1_p16 (poly16x4_t a, poly16x4_t b)
{
  poly16x4_t result;
  __asm__ ("uzp1 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vuzp1_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("uzp1 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vuzp1_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("uzp1 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vuzp1_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("uzp1 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vuzp1_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("uzp1 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vuzp1_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("uzp1 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vuzp1_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("uzp1 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vuzp1q_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("uzp1 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vuzp1q_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("uzp1 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vuzp1q_p8 (poly8x16_t a, poly8x16_t b)
{
  poly8x16_t result;
  __asm__ ("uzp1 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vuzp1q_p16 (poly16x8_t a, poly16x8_t b)
{
  poly16x8_t result;
  __asm__ ("uzp1 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vuzp1q_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("uzp1 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vuzp1q_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("uzp1 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vuzp1q_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("uzp1 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vuzp1q_s64 (int64x2_t a, int64x2_t b)
{
  int64x2_t result;
  __asm__ ("uzp1 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vuzp1q_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("uzp1 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vuzp1q_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("uzp1 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vuzp1q_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("uzp1 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vuzp1q_u64 (uint64x2_t a, uint64x2_t b)
{
  uint64x2_t result;
  __asm__ ("uzp1 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vuzp2_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("uzp2 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vuzp2_p8 (poly8x8_t a, poly8x8_t b)
{
  poly8x8_t result;
  __asm__ ("uzp2 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vuzp2_p16 (poly16x4_t a, poly16x4_t b)
{
  poly16x4_t result;
  __asm__ ("uzp2 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vuzp2_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("uzp2 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vuzp2_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("uzp2 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vuzp2_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("uzp2 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vuzp2_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("uzp2 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vuzp2_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("uzp2 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vuzp2_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("uzp2 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vuzp2q_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("uzp2 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vuzp2q_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("uzp2 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vuzp2q_p8 (poly8x16_t a, poly8x16_t b)
{
  poly8x16_t result;
  __asm__ ("uzp2 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vuzp2q_p16 (poly16x8_t a, poly16x8_t b)
{
  poly16x8_t result;
  __asm__ ("uzp2 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vuzp2q_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("uzp2 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vuzp2q_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("uzp2 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vuzp2q_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("uzp2 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vuzp2q_s64 (int64x2_t a, int64x2_t b)
{
  int64x2_t result;
  __asm__ ("uzp2 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vuzp2q_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("uzp2 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vuzp2q_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("uzp2 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vuzp2q_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("uzp2 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vuzp2q_u64 (uint64x2_t a, uint64x2_t b)
{
  uint64x2_t result;
  __asm__ ("uzp2 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vzip1_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("zip1 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vzip1_p8 (poly8x8_t a, poly8x8_t b)
{
  poly8x8_t result;
  __asm__ ("zip1 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vzip1_p16 (poly16x4_t a, poly16x4_t b)
{
  poly16x4_t result;
  __asm__ ("zip1 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vzip1_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("zip1 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vzip1_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("zip1 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vzip1_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("zip1 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vzip1_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("zip1 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vzip1_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("zip1 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vzip1_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("zip1 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vzip1q_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("zip1 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vzip1q_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("zip1 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vzip1q_p8 (poly8x16_t a, poly8x16_t b)
{
  poly8x16_t result;
  __asm__ ("zip1 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vzip1q_p16 (poly16x8_t a, poly16x8_t b)
{
  poly16x8_t result;
  __asm__ ("zip1 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vzip1q_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("zip1 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vzip1q_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("zip1 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vzip1q_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("zip1 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vzip1q_s64 (int64x2_t a, int64x2_t b)
{
  int64x2_t result;
  __asm__ ("zip1 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vzip1q_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("zip1 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vzip1q_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("zip1 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vzip1q_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("zip1 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vzip1q_u64 (uint64x2_t a, uint64x2_t b)
{
  uint64x2_t result;
  __asm__ ("zip1 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vzip2_f32 (float32x2_t a, float32x2_t b)
{
  float32x2_t result;
  __asm__ ("zip2 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vzip2_p8 (poly8x8_t a, poly8x8_t b)
{
  poly8x8_t result;
  __asm__ ("zip2 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vzip2_p16 (poly16x4_t a, poly16x4_t b)
{
  poly16x4_t result;
  __asm__ ("zip2 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vzip2_s8 (int8x8_t a, int8x8_t b)
{
  int8x8_t result;
  __asm__ ("zip2 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vzip2_s16 (int16x4_t a, int16x4_t b)
{
  int16x4_t result;
  __asm__ ("zip2 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vzip2_s32 (int32x2_t a, int32x2_t b)
{
  int32x2_t result;
  __asm__ ("zip2 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vzip2_u8 (uint8x8_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("zip2 %0.8b,%1.8b,%2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vzip2_u16 (uint16x4_t a, uint16x4_t b)
{
  uint16x4_t result;
  __asm__ ("zip2 %0.4h,%1.4h,%2.4h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vzip2_u32 (uint32x2_t a, uint32x2_t b)
{
  uint32x2_t result;
  __asm__ ("zip2 %0.2s,%1.2s,%2.2s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vzip2q_f32 (float32x4_t a, float32x4_t b)
{
  float32x4_t result;
  __asm__ ("zip2 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vzip2q_f64 (float64x2_t a, float64x2_t b)
{
  float64x2_t result;
  __asm__ ("zip2 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vzip2q_p8 (poly8x16_t a, poly8x16_t b)
{
  poly8x16_t result;
  __asm__ ("zip2 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vzip2q_p16 (poly16x8_t a, poly16x8_t b)
{
  poly16x8_t result;
  __asm__ ("zip2 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vzip2q_s8 (int8x16_t a, int8x16_t b)
{
  int8x16_t result;
  __asm__ ("zip2 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vzip2q_s16 (int16x8_t a, int16x8_t b)
{
  int16x8_t result;
  __asm__ ("zip2 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vzip2q_s32 (int32x4_t a, int32x4_t b)
{
  int32x4_t result;
  __asm__ ("zip2 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vzip2q_s64 (int64x2_t a, int64x2_t b)
{
  int64x2_t result;
  __asm__ ("zip2 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vzip2q_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("zip2 %0.16b,%1.16b,%2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vzip2q_u16 (uint16x8_t a, uint16x8_t b)
{
  uint16x8_t result;
  __asm__ ("zip2 %0.8h,%1.8h,%2.8h"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vzip2q_u32 (uint32x4_t a, uint32x4_t b)
{
  uint32x4_t result;
  __asm__ ("zip2 %0.4s,%1.4s,%2.4s"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vzip2q_u64 (uint64x2_t a, uint64x2_t b)
{
  uint64x2_t result;
  __asm__ ("zip2 %0.2d,%1.2d,%2.2d"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

/* End of temporary inline asm implementations.  */

/* Start of temporary inline asm for vldn, vstn and friends.  */

/* Create struct element types for duplicating loads.

   Create 2 element structures of:

   +------+----+----+----+----+
   |      | 8  | 16 | 32 | 64 |
   +------+----+----+----+----+
   |int   | Y  | Y  | N  | N  |
   +------+----+----+----+----+
   |uint  | Y  | Y  | N  | N  |
   +------+----+----+----+----+
   |float | -  | -  | N  | N  |
   +------+----+----+----+----+
   |poly  | Y  | Y  | -  | -  |
   +------+----+----+----+----+

   Create 3 element structures of:

   +------+----+----+----+----+
   |      | 8  | 16 | 32 | 64 |
   +------+----+----+----+----+
   |int   | Y  | Y  | Y  | Y  |
   +------+----+----+----+----+
   |uint  | Y  | Y  | Y  | Y  |
   +------+----+----+----+----+
   |float | -  | -  | Y  | Y  |
   +------+----+----+----+----+
   |poly  | Y  | Y  | -  | -  |
   +------+----+----+----+----+

   Create 4 element structures of:

   +------+----+----+----+----+
   |      | 8  | 16 | 32 | 64 |
   +------+----+----+----+----+
   |int   | Y  | N  | N  | Y  |
   +------+----+----+----+----+
   |uint  | Y  | N  | N  | Y  |
   +------+----+----+----+----+
   |float | -  | -  | N  | Y  |
   +------+----+----+----+----+
   |poly  | Y  | N  | -  | -  |
   +------+----+----+----+----+

  This is required for casting memory reference.  */
#define __STRUCTN(t, sz, nelem)			\
  typedef struct t ## sz ## x ## nelem ## _t {	\
    t ## sz ## _t val[nelem];			\
  }  t ## sz ## x ## nelem ## _t;

/* 2-element structs.  */
__STRUCTN (int, 8, 2)
__STRUCTN (int, 16, 2)
__STRUCTN (uint, 8, 2)
__STRUCTN (uint, 16, 2)
__STRUCTN (poly, 8, 2)
__STRUCTN (poly, 16, 2)
/* 3-element structs.  */
__STRUCTN (int, 8, 3)
__STRUCTN (int, 16, 3)
__STRUCTN (int, 32, 3)
__STRUCTN (int, 64, 3)
__STRUCTN (uint, 8, 3)
__STRUCTN (uint, 16, 3)
__STRUCTN (uint, 32, 3)
__STRUCTN (uint, 64, 3)
__STRUCTN (float, 32, 3)
__STRUCTN (float, 64, 3)
__STRUCTN (poly, 8, 3)
__STRUCTN (poly, 16, 3)
/* 4-element structs.  */
__STRUCTN (int, 8, 4)
__STRUCTN (int, 64, 4)
__STRUCTN (uint, 8, 4)
__STRUCTN (uint, 64, 4)
__STRUCTN (poly, 8, 4)
__STRUCTN (float, 64, 4)
#undef __STRUCTN

#define __LD2R_FUNC(rettype, structtype, ptrtype,			\
		    regsuffix, funcsuffix, Q)				\
  __extension__ static __inline rettype					\
  __attribute__ ((__always_inline__)) 					\
  vld2 ## Q ## _dup_ ## funcsuffix (const ptrtype *ptr)			\
  {									\
    rettype result;							\
    __asm__ ("ld2r {v16." #regsuffix ", v17." #regsuffix "}, %1\n\t"	\
	     "st1 {v16." #regsuffix ", v17." #regsuffix "}, %0\n\t"	\
	     : "=Q"(result)						\
	     : "Q"(*(const structtype *)ptr)				\
	     : "memory", "v16", "v17");					\
    return result;							\
  }

__LD2R_FUNC (float32x2x2_t, float32x2_t, float32_t, 2s, f32,)
__LD2R_FUNC (float64x1x2_t, float64x2_t, float64_t, 1d, f64,)
__LD2R_FUNC (poly8x8x2_t, poly8x2_t, poly8_t, 8b, p8,)
__LD2R_FUNC (poly16x4x2_t, poly16x2_t, poly16_t, 4h, p16,)
__LD2R_FUNC (int8x8x2_t, int8x2_t, int8_t, 8b, s8,)
__LD2R_FUNC (int16x4x2_t, int16x2_t, int16_t, 4h, s16,)
__LD2R_FUNC (int32x2x2_t, int32x2_t, int32_t, 2s, s32,)
__LD2R_FUNC (int64x1x2_t, int64x2_t, int64_t, 1d, s64,)
__LD2R_FUNC (uint8x8x2_t, uint8x2_t, uint8_t, 8b, u8,)
__LD2R_FUNC (uint16x4x2_t, uint16x2_t, uint16_t, 4h, u16,)
__LD2R_FUNC (uint32x2x2_t, uint32x2_t, uint32_t, 2s, u32,)
__LD2R_FUNC (uint64x1x2_t, uint64x2_t, uint64_t, 1d, u64,)
__LD2R_FUNC (float32x4x2_t, float32x2_t, float32_t, 4s, f32, q)
__LD2R_FUNC (float64x2x2_t, float64x2_t, float64_t, 2d, f64, q)
__LD2R_FUNC (poly8x16x2_t, poly8x2_t, poly8_t, 16b, p8, q)
__LD2R_FUNC (poly16x8x2_t, poly16x2_t, poly16_t, 8h, p16, q)
__LD2R_FUNC (int8x16x2_t, int8x2_t, int8_t, 16b, s8, q)
__LD2R_FUNC (int16x8x2_t, int16x2_t, int16_t, 8h, s16, q)
__LD2R_FUNC (int32x4x2_t, int32x2_t, int32_t, 4s, s32, q)
__LD2R_FUNC (int64x2x2_t, int64x2_t, int64_t, 2d, s64, q)
__LD2R_FUNC (uint8x16x2_t, uint8x2_t, uint8_t, 16b, u8, q)
__LD2R_FUNC (uint16x8x2_t, uint16x2_t, uint16_t, 8h, u16, q)
__LD2R_FUNC (uint32x4x2_t, uint32x2_t, uint32_t, 4s, u32, q)
__LD2R_FUNC (uint64x2x2_t, uint64x2_t, uint64_t, 2d, u64, q)

#define __LD2_LANE_FUNC(rettype, ptrtype, regsuffix,			\
			lnsuffix, funcsuffix, Q)			\
  __extension__ static __inline rettype					\
  __attribute__ ((__always_inline__))					\
  vld2 ## Q ## _lane_ ## funcsuffix (const ptrtype *ptr,		\
				     rettype b, const int c)		\
  {									\
    rettype result;							\
    __asm__ ("ld1 {v16." #regsuffix ", v17." #regsuffix "}, %1\n\t"	\
	     "ld2 {v16." #lnsuffix ", v17." #lnsuffix "}[%3], %2\n\t"	\
	     "st1 {v16." #regsuffix ", v17." #regsuffix "}, %0\n\t"	\
	     : "=Q"(result)						\
	     : "Q"(b), "Q"(*(const rettype *)ptr), "i"(c)		\
	     : "memory", "v16", "v17");					\
    return result;							\
  }

__LD2_LANE_FUNC (int8x8x2_t, uint8_t, 8b, b, s8,)
__LD2_LANE_FUNC (float32x2x2_t, float32_t, 2s, s, f32,)
__LD2_LANE_FUNC (float64x1x2_t, float64_t, 1d, d, f64,)
__LD2_LANE_FUNC (poly8x8x2_t, poly8_t, 8b, b, p8,)
__LD2_LANE_FUNC (poly16x4x2_t, poly16_t, 4h, h, p16,)
__LD2_LANE_FUNC (int16x4x2_t, int16_t, 4h, h, s16,)
__LD2_LANE_FUNC (int32x2x2_t, int32_t, 2s, s, s32,)
__LD2_LANE_FUNC (int64x1x2_t, int64_t, 1d, d, s64,)
__LD2_LANE_FUNC (uint8x8x2_t, uint8_t, 8b, b, u8,)
__LD2_LANE_FUNC (uint16x4x2_t, uint16_t, 4h, h, u16,)
__LD2_LANE_FUNC (uint32x2x2_t, uint32_t, 2s, s, u32,)
__LD2_LANE_FUNC (uint64x1x2_t, uint64_t, 1d, d, u64,)
__LD2_LANE_FUNC (float32x4x2_t, float32_t, 4s, s, f32, q)
__LD2_LANE_FUNC (float64x2x2_t, float64_t, 2d, d, f64, q)
__LD2_LANE_FUNC (poly8x16x2_t, poly8_t, 16b, b, p8, q)
__LD2_LANE_FUNC (poly16x8x2_t, poly16_t, 8h, h, p16, q)
__LD2_LANE_FUNC (int8x16x2_t, int8_t, 16b, b, s8, q)
__LD2_LANE_FUNC (int16x8x2_t, int16_t, 8h, h, s16, q)
__LD2_LANE_FUNC (int32x4x2_t, int32_t, 4s, s, s32, q)
__LD2_LANE_FUNC (int64x2x2_t, int64_t, 2d, d, s64, q)
__LD2_LANE_FUNC (uint8x16x2_t, uint8_t, 16b, b, u8, q)
__LD2_LANE_FUNC (uint16x8x2_t, uint16_t, 8h, h, u16, q)
__LD2_LANE_FUNC (uint32x4x2_t, uint32_t, 4s, s, u32, q)
__LD2_LANE_FUNC (uint64x2x2_t, uint64_t, 2d, d, u64, q)

#define __LD3R_FUNC(rettype, structtype, ptrtype,			\
		    regsuffix, funcsuffix, Q)				\
  __extension__ static __inline rettype					\
  __attribute__ ((__always_inline__))					\
  vld3 ## Q ## _dup_ ## funcsuffix (const ptrtype *ptr)			\
  {									\
    rettype result;							\
    __asm__ ("ld3r {v16." #regsuffix " - v18." #regsuffix "}, %1\n\t"	\
	     "st1 {v16." #regsuffix " - v18." #regsuffix "}, %0\n\t"	\
	     : "=Q"(result)						\
	     : "Q"(*(const structtype *)ptr)				\
	     : "memory", "v16", "v17", "v18");				\
    return result;							\
  }

__LD3R_FUNC (float32x2x3_t, float32x3_t, float32_t, 2s, f32,)
__LD3R_FUNC (float64x1x3_t, float64x3_t, float64_t, 1d, f64,)
__LD3R_FUNC (poly8x8x3_t, poly8x3_t, poly8_t, 8b, p8,)
__LD3R_FUNC (poly16x4x3_t, poly16x3_t, poly16_t, 4h, p16,)
__LD3R_FUNC (int8x8x3_t, int8x3_t, int8_t, 8b, s8,)
__LD3R_FUNC (int16x4x3_t, int16x3_t, int16_t, 4h, s16,)
__LD3R_FUNC (int32x2x3_t, int32x3_t, int32_t, 2s, s32,)
__LD3R_FUNC (int64x1x3_t, int64x3_t, int64_t, 1d, s64,)
__LD3R_FUNC (uint8x8x3_t, uint8x3_t, uint8_t, 8b, u8,)
__LD3R_FUNC (uint16x4x3_t, uint16x3_t, uint16_t, 4h, u16,)
__LD3R_FUNC (uint32x2x3_t, uint32x3_t, uint32_t, 2s, u32,)
__LD3R_FUNC (uint64x1x3_t, uint64x3_t, uint64_t, 1d, u64,)
__LD3R_FUNC (float32x4x3_t, float32x3_t, float32_t, 4s, f32, q)
__LD3R_FUNC (float64x2x3_t, float64x3_t, float64_t, 2d, f64, q)
__LD3R_FUNC (poly8x16x3_t, poly8x3_t, poly8_t, 16b, p8, q)
__LD3R_FUNC (poly16x8x3_t, poly16x3_t, poly16_t, 8h, p16, q)
__LD3R_FUNC (int8x16x3_t, int8x3_t, int8_t, 16b, s8, q)
__LD3R_FUNC (int16x8x3_t, int16x3_t, int16_t, 8h, s16, q)
__LD3R_FUNC (int32x4x3_t, int32x3_t, int32_t, 4s, s32, q)
__LD3R_FUNC (int64x2x3_t, int64x3_t, int64_t, 2d, s64, q)
__LD3R_FUNC (uint8x16x3_t, uint8x3_t, uint8_t, 16b, u8, q)
__LD3R_FUNC (uint16x8x3_t, uint16x3_t, uint16_t, 8h, u16, q)
__LD3R_FUNC (uint32x4x3_t, uint32x3_t, uint32_t, 4s, u32, q)
__LD3R_FUNC (uint64x2x3_t, uint64x3_t, uint64_t, 2d, u64, q)

#define __LD3_LANE_FUNC(rettype, ptrtype, regsuffix,			\
			lnsuffix, funcsuffix, Q)			\
  __extension__ static __inline rettype					\
  __attribute__ ((__always_inline__))					\
  vld3 ## Q ## _lane_ ## funcsuffix (const ptrtype *ptr,		\
				     rettype b, const int c)		\
  {									\
    rettype result;							\
    __asm__ ("ld1 {v16." #regsuffix " - v18." #regsuffix "}, %1\n\t"	\
	     "ld3 {v16." #lnsuffix " - v18." #lnsuffix "}[%3], %2\n\t"	\
	     "st1 {v16." #regsuffix " - v18." #regsuffix "}, %0\n\t"	\
	     : "=Q"(result)						\
	     : "Q"(b), "Q"(*(const rettype *)ptr), "i"(c)		\
	     : "memory", "v16", "v17", "v18");				\
    return result;							\
  }

__LD3_LANE_FUNC (int8x8x3_t, uint8_t, 8b, b, s8,)
__LD3_LANE_FUNC (float32x2x3_t, float32_t, 2s, s, f32,)
__LD3_LANE_FUNC (float64x1x3_t, float64_t, 1d, d, f64,)
__LD3_LANE_FUNC (poly8x8x3_t, poly8_t, 8b, b, p8,)
__LD3_LANE_FUNC (poly16x4x3_t, poly16_t, 4h, h, p16,)
__LD3_LANE_FUNC (int16x4x3_t, int16_t, 4h, h, s16,)
__LD3_LANE_FUNC (int32x2x3_t, int32_t, 2s, s, s32,)
__LD3_LANE_FUNC (int64x1x3_t, int64_t, 1d, d, s64,)
__LD3_LANE_FUNC (uint8x8x3_t, uint8_t, 8b, b, u8,)
__LD3_LANE_FUNC (uint16x4x3_t, uint16_t, 4h, h, u16,)
__LD3_LANE_FUNC (uint32x2x3_t, uint32_t, 2s, s, u32,)
__LD3_LANE_FUNC (uint64x1x3_t, uint64_t, 1d, d, u64,)
__LD3_LANE_FUNC (float32x4x3_t, float32_t, 4s, s, f32, q)
__LD3_LANE_FUNC (float64x2x3_t, float64_t, 2d, d, f64, q)
__LD3_LANE_FUNC (poly8x16x3_t, poly8_t, 16b, b, p8, q)
__LD3_LANE_FUNC (poly16x8x3_t, poly16_t, 8h, h, p16, q)
__LD3_LANE_FUNC (int8x16x3_t, int8_t, 16b, b, s8, q)
__LD3_LANE_FUNC (int16x8x3_t, int16_t, 8h, h, s16, q)
__LD3_LANE_FUNC (int32x4x3_t, int32_t, 4s, s, s32, q)
__LD3_LANE_FUNC (int64x2x3_t, int64_t, 2d, d, s64, q)
__LD3_LANE_FUNC (uint8x16x3_t, uint8_t, 16b, b, u8, q)
__LD3_LANE_FUNC (uint16x8x3_t, uint16_t, 8h, h, u16, q)
__LD3_LANE_FUNC (uint32x4x3_t, uint32_t, 4s, s, u32, q)
__LD3_LANE_FUNC (uint64x2x3_t, uint64_t, 2d, d, u64, q)

#define __LD4R_FUNC(rettype, structtype, ptrtype,			\
		    regsuffix, funcsuffix, Q)				\
  __extension__ static __inline rettype					\
  __attribute__ ((__always_inline__))					\
  vld4 ## Q ## _dup_ ## funcsuffix (const ptrtype *ptr)			\
  {									\
    rettype result;							\
    __asm__ ("ld4r {v16." #regsuffix " - v19." #regsuffix "}, %1\n\t"	\
	     "st1 {v16." #regsuffix " - v19." #regsuffix "}, %0\n\t"	\
	     : "=Q"(result)						\
	     : "Q"(*(const structtype *)ptr)				\
	     : "memory", "v16", "v17", "v18", "v19");			\
    return result;							\
  }

__LD4R_FUNC (float32x2x4_t, float32x4_t, float32_t, 2s, f32,)
__LD4R_FUNC (float64x1x4_t, float64x4_t, float64_t, 1d, f64,)
__LD4R_FUNC (poly8x8x4_t, poly8x4_t, poly8_t, 8b, p8,)
__LD4R_FUNC (poly16x4x4_t, poly16x4_t, poly16_t, 4h, p16,)
__LD4R_FUNC (int8x8x4_t, int8x4_t, int8_t, 8b, s8,)
__LD4R_FUNC (int16x4x4_t, int16x4_t, int16_t, 4h, s16,)
__LD4R_FUNC (int32x2x4_t, int32x4_t, int32_t, 2s, s32,)
__LD4R_FUNC (int64x1x4_t, int64x4_t, int64_t, 1d, s64,)
__LD4R_FUNC (uint8x8x4_t, uint8x4_t, uint8_t, 8b, u8,)
__LD4R_FUNC (uint16x4x4_t, uint16x4_t, uint16_t, 4h, u16,)
__LD4R_FUNC (uint32x2x4_t, uint32x4_t, uint32_t, 2s, u32,)
__LD4R_FUNC (uint64x1x4_t, uint64x4_t, uint64_t, 1d, u64,)
__LD4R_FUNC (float32x4x4_t, float32x4_t, float32_t, 4s, f32, q)
__LD4R_FUNC (float64x2x4_t, float64x4_t, float64_t, 2d, f64, q)
__LD4R_FUNC (poly8x16x4_t, poly8x4_t, poly8_t, 16b, p8, q)
__LD4R_FUNC (poly16x8x4_t, poly16x4_t, poly16_t, 8h, p16, q)
__LD4R_FUNC (int8x16x4_t, int8x4_t, int8_t, 16b, s8, q)
__LD4R_FUNC (int16x8x4_t, int16x4_t, int16_t, 8h, s16, q)
__LD4R_FUNC (int32x4x4_t, int32x4_t, int32_t, 4s, s32, q)
__LD4R_FUNC (int64x2x4_t, int64x4_t, int64_t, 2d, s64, q)
__LD4R_FUNC (uint8x16x4_t, uint8x4_t, uint8_t, 16b, u8, q)
__LD4R_FUNC (uint16x8x4_t, uint16x4_t, uint16_t, 8h, u16, q)
__LD4R_FUNC (uint32x4x4_t, uint32x4_t, uint32_t, 4s, u32, q)
__LD4R_FUNC (uint64x2x4_t, uint64x4_t, uint64_t, 2d, u64, q)

#define __LD4_LANE_FUNC(rettype, ptrtype, regsuffix,			\
			lnsuffix, funcsuffix, Q)			\
  __extension__ static __inline rettype					\
  __attribute__ ((__always_inline__))					\
  vld4 ## Q ## _lane_ ## funcsuffix (const ptrtype *ptr,		\
				     rettype b, const int c)		\
  {									\
    rettype result;							\
    __asm__ ("ld1 {v16." #regsuffix " - v19." #regsuffix "}, %1\n\t"	\
	     "ld4 {v16." #lnsuffix " - v19." #lnsuffix "}[%3], %2\n\t"	\
	     "st1 {v16." #regsuffix " - v19." #regsuffix "}, %0\n\t"	\
	     : "=Q"(result)						\
	     : "Q"(b), "Q"(*(const rettype *)ptr), "i"(c)		\
	     : "memory", "v16", "v17", "v18", "v19");			\
    return result;							\
  }

__LD4_LANE_FUNC (int8x8x4_t, uint8_t, 8b, b, s8,)
__LD4_LANE_FUNC (float32x2x4_t, float32_t, 2s, s, f32,)
__LD4_LANE_FUNC (float64x1x4_t, float64_t, 1d, d, f64,)
__LD4_LANE_FUNC (poly8x8x4_t, poly8_t, 8b, b, p8,)
__LD4_LANE_FUNC (poly16x4x4_t, poly16_t, 4h, h, p16,)
__LD4_LANE_FUNC (int16x4x4_t, int16_t, 4h, h, s16,)
__LD4_LANE_FUNC (int32x2x4_t, int32_t, 2s, s, s32,)
__LD4_LANE_FUNC (int64x1x4_t, int64_t, 1d, d, s64,)
__LD4_LANE_FUNC (uint8x8x4_t, uint8_t, 8b, b, u8,)
__LD4_LANE_FUNC (uint16x4x4_t, uint16_t, 4h, h, u16,)
__LD4_LANE_FUNC (uint32x2x4_t, uint32_t, 2s, s, u32,)
__LD4_LANE_FUNC (uint64x1x4_t, uint64_t, 1d, d, u64,)
__LD4_LANE_FUNC (float32x4x4_t, float32_t, 4s, s, f32, q)
__LD4_LANE_FUNC (float64x2x4_t, float64_t, 2d, d, f64, q)
__LD4_LANE_FUNC (poly8x16x4_t, poly8_t, 16b, b, p8, q)
__LD4_LANE_FUNC (poly16x8x4_t, poly16_t, 8h, h, p16, q)
__LD4_LANE_FUNC (int8x16x4_t, int8_t, 16b, b, s8, q)
__LD4_LANE_FUNC (int16x8x4_t, int16_t, 8h, h, s16, q)
__LD4_LANE_FUNC (int32x4x4_t, int32_t, 4s, s, s32, q)
__LD4_LANE_FUNC (int64x2x4_t, int64_t, 2d, d, s64, q)
__LD4_LANE_FUNC (uint8x16x4_t, uint8_t, 16b, b, u8, q)
__LD4_LANE_FUNC (uint16x8x4_t, uint16_t, 8h, h, u16, q)
__LD4_LANE_FUNC (uint32x4x4_t, uint32_t, 4s, s, u32, q)
__LD4_LANE_FUNC (uint64x2x4_t, uint64_t, 2d, d, u64, q)

#define __ST2_LANE_FUNC(intype, ptrtype, regsuffix,			\
			lnsuffix, funcsuffix, Q)			\
  typedef struct { ptrtype __x[2]; } __ST2_LANE_STRUCTURE_##intype;	\
  __extension__ static __inline void					\
  __attribute__ ((__always_inline__))					\
  vst2 ## Q ## _lane_ ## funcsuffix (ptrtype *ptr,			\
				     intype b, const int c)		\
  {									\
    __ST2_LANE_STRUCTURE_##intype *__p =				\
				(__ST2_LANE_STRUCTURE_##intype *)ptr;	\
    __asm__ ("ld1 {v16." #regsuffix ", v17." #regsuffix "}, %1\n\t"	\
	     "st2 {v16." #lnsuffix ", v17." #lnsuffix "}[%2], %0\n\t"	\
	     : "=Q"(*__p)						\
	     : "Q"(b), "i"(c)						\
	     : "v16", "v17");						\
  }

__ST2_LANE_FUNC (int8x8x2_t, int8_t, 8b, b, s8,)
__ST2_LANE_FUNC (float32x2x2_t, float32_t, 2s, s, f32,)
__ST2_LANE_FUNC (float64x1x2_t, float64_t, 1d, d, f64,)
__ST2_LANE_FUNC (poly8x8x2_t, poly8_t, 8b, b, p8,)
__ST2_LANE_FUNC (poly16x4x2_t, poly16_t, 4h, h, p16,)
__ST2_LANE_FUNC (int16x4x2_t, int16_t, 4h, h, s16,)
__ST2_LANE_FUNC (int32x2x2_t, int32_t, 2s, s, s32,)
__ST2_LANE_FUNC (int64x1x2_t, int64_t, 1d, d, s64,)
__ST2_LANE_FUNC (uint8x8x2_t, uint8_t, 8b, b, u8,)
__ST2_LANE_FUNC (uint16x4x2_t, uint16_t, 4h, h, u16,)
__ST2_LANE_FUNC (uint32x2x2_t, uint32_t, 2s, s, u32,)
__ST2_LANE_FUNC (uint64x1x2_t, uint64_t, 1d, d, u64,)
__ST2_LANE_FUNC (float32x4x2_t, float32_t, 4s, s, f32, q)
__ST2_LANE_FUNC (float64x2x2_t, float64_t, 2d, d, f64, q)
__ST2_LANE_FUNC (poly8x16x2_t, poly8_t, 16b, b, p8, q)
__ST2_LANE_FUNC (poly16x8x2_t, poly16_t, 8h, h, p16, q)
__ST2_LANE_FUNC (int8x16x2_t, int8_t, 16b, b, s8, q)
__ST2_LANE_FUNC (int16x8x2_t, int16_t, 8h, h, s16, q)
__ST2_LANE_FUNC (int32x4x2_t, int32_t, 4s, s, s32, q)
__ST2_LANE_FUNC (int64x2x2_t, int64_t, 2d, d, s64, q)
__ST2_LANE_FUNC (uint8x16x2_t, uint8_t, 16b, b, u8, q)
__ST2_LANE_FUNC (uint16x8x2_t, uint16_t, 8h, h, u16, q)
__ST2_LANE_FUNC (uint32x4x2_t, uint32_t, 4s, s, u32, q)
__ST2_LANE_FUNC (uint64x2x2_t, uint64_t, 2d, d, u64, q)

#define __ST3_LANE_FUNC(intype, ptrtype, regsuffix,			\
			lnsuffix, funcsuffix, Q)			\
  typedef struct { ptrtype __x[3]; } __ST3_LANE_STRUCTURE_##intype;	\
  __extension__ static __inline void					\
  __attribute__ ((__always_inline__))					\
  vst3 ## Q ## _lane_ ## funcsuffix (ptrtype *ptr,			\
				     intype b, const int c)		\
  {									\
    __ST3_LANE_STRUCTURE_##intype *__p =				\
				(__ST3_LANE_STRUCTURE_##intype *)ptr;	\
    __asm__ ("ld1 {v16." #regsuffix " - v18." #regsuffix "}, %1\n\t"	\
	     "st3 {v16." #lnsuffix " - v18." #lnsuffix "}[%2], %0\n\t"	\
	     : "=Q"(*__p)						\
	     : "Q"(b), "i"(c)						\
	     : "v16", "v17", "v18");					\
  }

__ST3_LANE_FUNC (int8x8x3_t, int8_t, 8b, b, s8,)
__ST3_LANE_FUNC (float32x2x3_t, float32_t, 2s, s, f32,)
__ST3_LANE_FUNC (float64x1x3_t, float64_t, 1d, d, f64,)
__ST3_LANE_FUNC (poly8x8x3_t, poly8_t, 8b, b, p8,)
__ST3_LANE_FUNC (poly16x4x3_t, poly16_t, 4h, h, p16,)
__ST3_LANE_FUNC (int16x4x3_t, int16_t, 4h, h, s16,)
__ST3_LANE_FUNC (int32x2x3_t, int32_t, 2s, s, s32,)
__ST3_LANE_FUNC (int64x1x3_t, int64_t, 1d, d, s64,)
__ST3_LANE_FUNC (uint8x8x3_t, uint8_t, 8b, b, u8,)
__ST3_LANE_FUNC (uint16x4x3_t, uint16_t, 4h, h, u16,)
__ST3_LANE_FUNC (uint32x2x3_t, uint32_t, 2s, s, u32,)
__ST3_LANE_FUNC (uint64x1x3_t, uint64_t, 1d, d, u64,)
__ST3_LANE_FUNC (float32x4x3_t, float32_t, 4s, s, f32, q)
__ST3_LANE_FUNC (float64x2x3_t, float64_t, 2d, d, f64, q)
__ST3_LANE_FUNC (poly8x16x3_t, poly8_t, 16b, b, p8, q)
__ST3_LANE_FUNC (poly16x8x3_t, poly16_t, 8h, h, p16, q)
__ST3_LANE_FUNC (int8x16x3_t, int8_t, 16b, b, s8, q)
__ST3_LANE_FUNC (int16x8x3_t, int16_t, 8h, h, s16, q)
__ST3_LANE_FUNC (int32x4x3_t, int32_t, 4s, s, s32, q)
__ST3_LANE_FUNC (int64x2x3_t, int64_t, 2d, d, s64, q)
__ST3_LANE_FUNC (uint8x16x3_t, uint8_t, 16b, b, u8, q)
__ST3_LANE_FUNC (uint16x8x3_t, uint16_t, 8h, h, u16, q)
__ST3_LANE_FUNC (uint32x4x3_t, uint32_t, 4s, s, u32, q)
__ST3_LANE_FUNC (uint64x2x3_t, uint64_t, 2d, d, u64, q)

#define __ST4_LANE_FUNC(intype, ptrtype, regsuffix,			\
			lnsuffix, funcsuffix, Q)			\
  typedef struct { ptrtype __x[4]; } __ST4_LANE_STRUCTURE_##intype;	\
  __extension__ static __inline void					\
  __attribute__ ((__always_inline__))					\
  vst4 ## Q ## _lane_ ## funcsuffix (ptrtype *ptr,			\
				     intype b, const int c)		\
  {									\
    __ST4_LANE_STRUCTURE_##intype *__p =				\
				(__ST4_LANE_STRUCTURE_##intype *)ptr;	\
    __asm__ ("ld1 {v16." #regsuffix " - v19." #regsuffix "}, %1\n\t"	\
	     "st4 {v16." #lnsuffix " - v19." #lnsuffix "}[%2], %0\n\t"	\
	     : "=Q"(*__p)						\
	     : "Q"(b), "i"(c)						\
	     : "v16", "v17", "v18", "v19");				\
  }

__ST4_LANE_FUNC (int8x8x4_t, int8_t, 8b, b, s8,)
__ST4_LANE_FUNC (float32x2x4_t, float32_t, 2s, s, f32,)
__ST4_LANE_FUNC (float64x1x4_t, float64_t, 1d, d, f64,)
__ST4_LANE_FUNC (poly8x8x4_t, poly8_t, 8b, b, p8,)
__ST4_LANE_FUNC (poly16x4x4_t, poly16_t, 4h, h, p16,)
__ST4_LANE_FUNC (int16x4x4_t, int16_t, 4h, h, s16,)
__ST4_LANE_FUNC (int32x2x4_t, int32_t, 2s, s, s32,)
__ST4_LANE_FUNC (int64x1x4_t, int64_t, 1d, d, s64,)
__ST4_LANE_FUNC (uint8x8x4_t, uint8_t, 8b, b, u8,)
__ST4_LANE_FUNC (uint16x4x4_t, uint16_t, 4h, h, u16,)
__ST4_LANE_FUNC (uint32x2x4_t, uint32_t, 2s, s, u32,)
__ST4_LANE_FUNC (uint64x1x4_t, uint64_t, 1d, d, u64,)
__ST4_LANE_FUNC (float32x4x4_t, float32_t, 4s, s, f32, q)
__ST4_LANE_FUNC (float64x2x4_t, float64_t, 2d, d, f64, q)
__ST4_LANE_FUNC (poly8x16x4_t, poly8_t, 16b, b, p8, q)
__ST4_LANE_FUNC (poly16x8x4_t, poly16_t, 8h, h, p16, q)
__ST4_LANE_FUNC (int8x16x4_t, int8_t, 16b, b, s8, q)
__ST4_LANE_FUNC (int16x8x4_t, int16_t, 8h, h, s16, q)
__ST4_LANE_FUNC (int32x4x4_t, int32_t, 4s, s, s32, q)
__ST4_LANE_FUNC (int64x2x4_t, int64_t, 2d, d, s64, q)
__ST4_LANE_FUNC (uint8x16x4_t, uint8_t, 16b, b, u8, q)
__ST4_LANE_FUNC (uint16x8x4_t, uint16_t, 8h, h, u16, q)
__ST4_LANE_FUNC (uint32x4x4_t, uint32_t, 4s, s, u32, q)
__ST4_LANE_FUNC (uint64x2x4_t, uint64_t, 2d, d, u64, q)

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vaddlv_s32 (int32x2_t a)
{
  int64_t result;
  __asm__ ("saddlp %0.1d, %1.2s" : "=w"(result) : "w"(a) : );
  return result;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vaddlv_u32 (uint32x2_t a)
{
  uint64_t result;
  __asm__ ("uaddlp %0.1d, %1.2s" : "=w"(result) : "w"(a) : );
  return result;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vpaddd_s64 (int64x2_t __a)
{
  return __builtin_aarch64_addpdi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqdmulh_laneq_s16 (int16x4_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqdmulh_laneq_s32 (int32x2_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqv2si (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqdmulhq_laneq_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmulhq_laneq_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_laneqv4si (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmulh_laneq_s16 (int16x4_t __a, int16x8_t __b, const int __c)
{
  return  __builtin_aarch64_sqrdmulh_laneqv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmulh_laneq_s32 (int32x2_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_laneqv2si (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmulhq_laneq_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_laneqv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmulhq_laneq_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_laneqv4si (__a, __b, __c);
}

/* Table intrinsics.  */

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vqtbl1_p8 (poly8x16_t a, uint8x8_t b)
{
  poly8x8_t result;
  __asm__ ("tbl %0.8b, {%1.16b}, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqtbl1_s8 (int8x16_t a, uint8x8_t b)
{
  int8x8_t result;
  __asm__ ("tbl %0.8b, {%1.16b}, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqtbl1_u8 (uint8x16_t a, uint8x8_t b)
{
  uint8x8_t result;
  __asm__ ("tbl %0.8b, {%1.16b}, %2.8b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vqtbl1q_p8 (poly8x16_t a, uint8x16_t b)
{
  poly8x16_t result;
  __asm__ ("tbl %0.16b, {%1.16b}, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqtbl1q_s8 (int8x16_t a, uint8x16_t b)
{
  int8x16_t result;
  __asm__ ("tbl %0.16b, {%1.16b}, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqtbl1q_u8 (uint8x16_t a, uint8x16_t b)
{
  uint8x16_t result;
  __asm__ ("tbl %0.16b, {%1.16b}, %2.16b"
           : "=w"(result)
           : "w"(a), "w"(b)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqtbl2_s8 (int8x16x2_t tab, uint8x8_t idx)
{
  int8x8_t result;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbl %0.8b, {v16.16b, v17.16b}, %2.8b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqtbl2_u8 (uint8x16x2_t tab, uint8x8_t idx)
{
  uint8x8_t result;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbl %0.8b, {v16.16b, v17.16b}, %2.8b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vqtbl2_p8 (poly8x16x2_t tab, uint8x8_t idx)
{
  poly8x8_t result;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbl %0.8b, {v16.16b, v17.16b}, %2.8b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqtbl2q_s8 (int8x16x2_t tab, uint8x16_t idx)
{
  int8x16_t result;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbl %0.16b, {v16.16b, v17.16b}, %2.16b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqtbl2q_u8 (uint8x16x2_t tab, uint8x16_t idx)
{
  uint8x16_t result;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbl %0.16b, {v16.16b, v17.16b}, %2.16b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vqtbl2q_p8 (poly8x16x2_t tab, uint8x16_t idx)
{
  poly8x16_t result;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbl %0.16b, {v16.16b, v17.16b}, %2.16b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqtbl3_s8 (int8x16x3_t tab, uint8x8_t idx)
{
  int8x8_t result;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbl %0.8b, {v16.16b - v18.16b}, %2.8b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqtbl3_u8 (uint8x16x3_t tab, uint8x8_t idx)
{
  uint8x8_t result;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbl %0.8b, {v16.16b - v18.16b}, %2.8b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vqtbl3_p8 (poly8x16x3_t tab, uint8x8_t idx)
{
  poly8x8_t result;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbl %0.8b, {v16.16b - v18.16b}, %2.8b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqtbl3q_s8 (int8x16x3_t tab, uint8x16_t idx)
{
  int8x16_t result;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbl %0.16b, {v16.16b - v18.16b}, %2.16b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqtbl3q_u8 (uint8x16x3_t tab, uint8x16_t idx)
{
  uint8x16_t result;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbl %0.16b, {v16.16b - v18.16b}, %2.16b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vqtbl3q_p8 (poly8x16x3_t tab, uint8x16_t idx)
{
  poly8x16_t result;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbl %0.16b, {v16.16b - v18.16b}, %2.16b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqtbl4_s8 (int8x16x4_t tab, uint8x8_t idx)
{
  int8x8_t result;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbl %0.8b, {v16.16b - v19.16b}, %2.8b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqtbl4_u8 (uint8x16x4_t tab, uint8x8_t idx)
{
  uint8x8_t result;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbl %0.8b, {v16.16b - v19.16b}, %2.8b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vqtbl4_p8 (poly8x16x4_t tab, uint8x8_t idx)
{
  poly8x8_t result;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbl %0.8b, {v16.16b - v19.16b}, %2.8b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}


__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqtbl4q_s8 (int8x16x4_t tab, uint8x16_t idx)
{
  int8x16_t result;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbl %0.16b, {v16.16b - v19.16b}, %2.16b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqtbl4q_u8 (uint8x16x4_t tab, uint8x16_t idx)
{
  uint8x16_t result;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbl %0.16b, {v16.16b - v19.16b}, %2.16b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vqtbl4q_p8 (poly8x16x4_t tab, uint8x16_t idx)
{
  poly8x16_t result;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbl %0.16b, {v16.16b - v19.16b}, %2.16b\n\t"
	   :"=w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}


__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqtbx1_s8 (int8x8_t r, int8x16_t tab, uint8x8_t idx)
{
  int8x8_t result = r;
  __asm__ ("tbx %0.8b,{%1.16b},%2.8b"
           : "+w"(result)
           : "w"(tab), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqtbx1_u8 (uint8x8_t r, uint8x16_t tab, uint8x8_t idx)
{
  uint8x8_t result = r;
  __asm__ ("tbx %0.8b,{%1.16b},%2.8b"
           : "+w"(result)
           : "w"(tab), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vqtbx1_p8 (poly8x8_t r, poly8x16_t tab, uint8x8_t idx)
{
  poly8x8_t result = r;
  __asm__ ("tbx %0.8b,{%1.16b},%2.8b"
           : "+w"(result)
           : "w"(tab), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqtbx1q_s8 (int8x16_t r, int8x16_t tab, uint8x16_t idx)
{
  int8x16_t result = r;
  __asm__ ("tbx %0.16b,{%1.16b},%2.16b"
           : "+w"(result)
           : "w"(tab), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqtbx1q_u8 (uint8x16_t r, uint8x16_t tab, uint8x16_t idx)
{
  uint8x16_t result = r;
  __asm__ ("tbx %0.16b,{%1.16b},%2.16b"
           : "+w"(result)
           : "w"(tab), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vqtbx1q_p8 (poly8x16_t r, poly8x16_t tab, uint8x16_t idx)
{
  poly8x16_t result = r;
  __asm__ ("tbx %0.16b,{%1.16b},%2.16b"
           : "+w"(result)
           : "w"(tab), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqtbx2_s8 (int8x8_t r, int8x16x2_t tab, uint8x8_t idx)
{
  int8x8_t result = r;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbx %0.8b, {v16.16b, v17.16b}, %2.8b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqtbx2_u8 (uint8x8_t r, uint8x16x2_t tab, uint8x8_t idx)
{
  uint8x8_t result = r;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbx %0.8b, {v16.16b, v17.16b}, %2.8b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vqtbx2_p8 (poly8x8_t r, poly8x16x2_t tab, uint8x8_t idx)
{
  poly8x8_t result = r;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbx %0.8b, {v16.16b, v17.16b}, %2.8b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}


__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqtbx2q_s8 (int8x16_t r, int8x16x2_t tab, uint8x16_t idx)
{
  int8x16_t result = r;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbx %0.16b, {v16.16b, v17.16b}, %2.16b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqtbx2q_u8 (uint8x16_t r, uint8x16x2_t tab, uint8x16_t idx)
{
  uint8x16_t result = r;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbx %0.16b, {v16.16b, v17.16b}, %2.16b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vqtbx2q_p8 (poly8x16_t r, poly8x16x2_t tab, uint8x16_t idx)
{
  poly8x16_t result = r;
  __asm__ ("ld1 {v16.16b, v17.16b}, %1\n\t"
	   "tbx %0.16b, {v16.16b, v17.16b}, %2.16b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17");
  return result;
}


__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqtbx3_s8 (int8x8_t r, int8x16x3_t tab, uint8x8_t idx)
{
  int8x8_t result = r;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbx %0.8b, {v16.16b - v18.16b}, %2.8b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqtbx3_u8 (uint8x8_t r, uint8x16x3_t tab, uint8x8_t idx)
{
  uint8x8_t result = r;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbx %0.8b, {v16.16b - v18.16b}, %2.8b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vqtbx3_p8 (poly8x8_t r, poly8x16x3_t tab, uint8x8_t idx)
{
  poly8x8_t result = r;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbx %0.8b, {v16.16b - v18.16b}, %2.8b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}


__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqtbx3q_s8 (int8x16_t r, int8x16x3_t tab, uint8x16_t idx)
{
  int8x16_t result = r;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbx %0.16b, {v16.16b - v18.16b}, %2.16b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqtbx3q_u8 (uint8x16_t r, uint8x16x3_t tab, uint8x16_t idx)
{
  uint8x16_t result = r;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbx %0.16b, {v16.16b - v18.16b}, %2.16b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vqtbx3q_p8 (poly8x16_t r, poly8x16x3_t tab, uint8x16_t idx)
{
  poly8x16_t result = r;
  __asm__ ("ld1 {v16.16b - v18.16b}, %1\n\t"
	   "tbx %0.16b, {v16.16b - v18.16b}, %2.16b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18");
  return result;
}


__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqtbx4_s8 (int8x8_t r, int8x16x4_t tab, uint8x8_t idx)
{
  int8x8_t result = r;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbx %0.8b, {v16.16b - v19.16b}, %2.8b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqtbx4_u8 (uint8x8_t r, uint8x16x4_t tab, uint8x8_t idx)
{
  uint8x8_t result = r;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbx %0.8b, {v16.16b - v19.16b}, %2.8b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vqtbx4_p8 (poly8x8_t r, poly8x16x4_t tab, uint8x8_t idx)
{
  poly8x8_t result = r;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbx %0.8b, {v16.16b - v19.16b}, %2.8b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}


__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqtbx4q_s8 (int8x16_t r, int8x16x4_t tab, uint8x16_t idx)
{
  int8x16_t result = r;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbx %0.16b, {v16.16b - v19.16b}, %2.16b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqtbx4q_u8 (uint8x16_t r, uint8x16x4_t tab, uint8x16_t idx)
{
  uint8x16_t result = r;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbx %0.16b, {v16.16b - v19.16b}, %2.16b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vqtbx4q_p8 (poly8x16_t r, poly8x16x4_t tab, uint8x16_t idx)
{
  poly8x16_t result = r;
  __asm__ ("ld1 {v16.16b - v19.16b}, %1\n\t"
	   "tbx %0.16b, {v16.16b - v19.16b}, %2.16b\n\t"
	   :"+w"(result)
	   :"Q"(tab),"w"(idx)
	   :"memory", "v16", "v17", "v18", "v19");
  return result;
}

/* V7 legacy table intrinsics.  */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbl1_s8 (int8x8_t tab, int8x8_t idx)
{
  int8x8_t result;
  int8x16_t temp = vcombine_s8 (tab, vcreate_s8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("tbl %0.8b, {%1.16b}, %2.8b"
           : "=w"(result)
           : "w"(temp), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbl1_u8 (uint8x8_t tab, uint8x8_t idx)
{
  uint8x8_t result;
  uint8x16_t temp = vcombine_u8 (tab, vcreate_u8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("tbl %0.8b, {%1.16b}, %2.8b"
           : "=w"(result)
           : "w"(temp), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbl1_p8 (poly8x8_t tab, uint8x8_t idx)
{
  poly8x8_t result;
  poly8x16_t temp = vcombine_p8 (tab, vcreate_p8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("tbl %0.8b, {%1.16b}, %2.8b"
           : "=w"(result)
           : "w"(temp), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbl2_s8 (int8x8x2_t tab, int8x8_t idx)
{
  int8x8_t result;
  int8x16_t temp = vcombine_s8 (tab.val[0], tab.val[1]);
  __asm__ ("tbl %0.8b, {%1.16b}, %2.8b"
           : "=w"(result)
           : "w"(temp), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbl2_u8 (uint8x8x2_t tab, uint8x8_t idx)
{
  uint8x8_t result;
  uint8x16_t temp = vcombine_u8 (tab.val[0], tab.val[1]);
  __asm__ ("tbl %0.8b, {%1.16b}, %2.8b"
           : "=w"(result)
           : "w"(temp), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbl2_p8 (poly8x8x2_t tab, uint8x8_t idx)
{
  poly8x8_t result;
  poly8x16_t temp = vcombine_p8 (tab.val[0], tab.val[1]);
  __asm__ ("tbl %0.8b, {%1.16b}, %2.8b"
           : "=w"(result)
           : "w"(temp), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbl3_s8 (int8x8x3_t tab, int8x8_t idx)
{
  int8x8_t result;
  int8x16x2_t temp;
  temp.val[0] = vcombine_s8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_s8 (tab.val[2], vcreate_s8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("ld1 {v16.16b - v17.16b }, %1\n\t"
	   "tbl %0.8b, {v16.16b - v17.16b}, %2.8b\n\t"
           : "=w"(result)
           : "Q"(temp), "w"(idx)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbl3_u8 (uint8x8x3_t tab, uint8x8_t idx)
{
  uint8x8_t result;
  uint8x16x2_t temp;
  temp.val[0] = vcombine_u8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_u8 (tab.val[2], vcreate_u8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("ld1 {v16.16b - v17.16b }, %1\n\t"
	   "tbl %0.8b, {v16.16b - v17.16b}, %2.8b\n\t"
           : "=w"(result)
           : "Q"(temp), "w"(idx)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbl3_p8 (poly8x8x3_t tab, uint8x8_t idx)
{
  poly8x8_t result;
  poly8x16x2_t temp;
  temp.val[0] = vcombine_p8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_p8 (tab.val[2], vcreate_p8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("ld1 {v16.16b - v17.16b }, %1\n\t"
	   "tbl %0.8b, {v16.16b - v17.16b}, %2.8b\n\t"
           : "=w"(result)
           : "Q"(temp), "w"(idx)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbl4_s8 (int8x8x4_t tab, int8x8_t idx)
{
  int8x8_t result;
  int8x16x2_t temp;
  temp.val[0] = vcombine_s8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_s8 (tab.val[2], tab.val[3]);
  __asm__ ("ld1 {v16.16b - v17.16b }, %1\n\t"
	   "tbl %0.8b, {v16.16b - v17.16b}, %2.8b\n\t"
           : "=w"(result)
           : "Q"(temp), "w"(idx)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbl4_u8 (uint8x8x4_t tab, uint8x8_t idx)
{
  uint8x8_t result;
  uint8x16x2_t temp;
  temp.val[0] = vcombine_u8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_u8 (tab.val[2], tab.val[3]);
  __asm__ ("ld1 {v16.16b - v17.16b }, %1\n\t"
	   "tbl %0.8b, {v16.16b - v17.16b}, %2.8b\n\t"
           : "=w"(result)
           : "Q"(temp), "w"(idx)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbl4_p8 (poly8x8x4_t tab, uint8x8_t idx)
{
  poly8x8_t result;
  poly8x16x2_t temp;
  temp.val[0] = vcombine_p8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_p8 (tab.val[2], tab.val[3]);
  __asm__ ("ld1 {v16.16b - v17.16b }, %1\n\t"
	   "tbl %0.8b, {v16.16b - v17.16b}, %2.8b\n\t"
           : "=w"(result)
           : "Q"(temp), "w"(idx)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbx1_s8 (int8x8_t r, int8x8_t tab, int8x8_t idx)
{
  int8x8_t result;
  int8x8_t tmp1;
  int8x16_t temp = vcombine_s8 (tab, vcreate_s8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("movi %0.8b, 8\n\t"
	   "cmhs %0.8b, %3.8b, %0.8b\n\t"
	   "tbl %1.8b, {%2.16b}, %3.8b\n\t"
	   "bsl %0.8b, %4.8b, %1.8b\n\t"
           : "+w"(result), "=&w"(tmp1)
           : "w"(temp), "w"(idx), "w"(r)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbx1_u8 (uint8x8_t r, uint8x8_t tab, uint8x8_t idx)
{
  uint8x8_t result;
  uint8x8_t tmp1;
  uint8x16_t temp = vcombine_u8 (tab, vcreate_u8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("movi %0.8b, 8\n\t"
	   "cmhs %0.8b, %3.8b, %0.8b\n\t"
	   "tbl %1.8b, {%2.16b}, %3.8b\n\t"
	   "bsl %0.8b, %4.8b, %1.8b\n\t"
           : "+w"(result), "=&w"(tmp1)
           : "w"(temp), "w"(idx), "w"(r)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbx1_p8 (poly8x8_t r, poly8x8_t tab, uint8x8_t idx)
{
  poly8x8_t result;
  poly8x8_t tmp1;
  poly8x16_t temp = vcombine_p8 (tab, vcreate_p8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("movi %0.8b, 8\n\t"
	   "cmhs %0.8b, %3.8b, %0.8b\n\t"
	   "tbl %1.8b, {%2.16b}, %3.8b\n\t"
	   "bsl %0.8b, %4.8b, %1.8b\n\t"
           : "+w"(result), "=&w"(tmp1)
           : "w"(temp), "w"(idx), "w"(r)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbx2_s8 (int8x8_t r, int8x8x2_t tab, int8x8_t idx)
{
  int8x8_t result = r;
  int8x16_t temp = vcombine_s8 (tab.val[0], tab.val[1]);
  __asm__ ("tbx %0.8b, {%1.16b}, %2.8b"
           : "+w"(result)
           : "w"(temp), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbx2_u8 (uint8x8_t r, uint8x8x2_t tab, uint8x8_t idx)
{
  uint8x8_t result = r;
  uint8x16_t temp = vcombine_u8 (tab.val[0], tab.val[1]);
  __asm__ ("tbx %0.8b, {%1.16b}, %2.8b"
           : "+w"(result)
           : "w"(temp), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbx2_p8 (poly8x8_t r, poly8x8x2_t tab, uint8x8_t idx)
{
  poly8x8_t result = r;
  poly8x16_t temp = vcombine_p8 (tab.val[0], tab.val[1]);
  __asm__ ("tbx %0.8b, {%1.16b}, %2.8b"
           : "+w"(result)
           : "w"(temp), "w"(idx)
           : /* No clobbers */);
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbx3_s8 (int8x8_t r, int8x8x3_t tab, int8x8_t idx)
{
  int8x8_t result;
  int8x8_t tmp1;
  int8x16x2_t temp;
  temp.val[0] = vcombine_s8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_s8 (tab.val[2], vcreate_s8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("ld1 {v16.16b - v17.16b}, %2\n\t"
	   "movi %0.8b, 24\n\t"
	   "cmhs %0.8b, %3.8b, %0.8b\n\t"
	   "tbl %1.8b, {v16.16b - v17.16b}, %3.8b\n\t"
	   "bsl %0.8b, %4.8b, %1.8b\n\t"
           : "+w"(result), "=&w"(tmp1)
           : "Q"(temp), "w"(idx), "w"(r)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbx3_u8 (uint8x8_t r, uint8x8x3_t tab, uint8x8_t idx)
{
  uint8x8_t result;
  uint8x8_t tmp1;
  uint8x16x2_t temp;
  temp.val[0] = vcombine_u8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_u8 (tab.val[2], vcreate_u8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("ld1 {v16.16b - v17.16b}, %2\n\t"
	   "movi %0.8b, 24\n\t"
	   "cmhs %0.8b, %3.8b, %0.8b\n\t"
	   "tbl %1.8b, {v16.16b - v17.16b}, %3.8b\n\t"
	   "bsl %0.8b, %4.8b, %1.8b\n\t"
           : "+w"(result), "=&w"(tmp1)
           : "Q"(temp), "w"(idx), "w"(r)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbx3_p8 (poly8x8_t r, poly8x8x3_t tab, uint8x8_t idx)
{
  poly8x8_t result;
  poly8x8_t tmp1;
  poly8x16x2_t temp;
  temp.val[0] = vcombine_p8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_p8 (tab.val[2], vcreate_p8 (__AARCH64_UINT64_C (0x0)));
  __asm__ ("ld1 {v16.16b - v17.16b}, %2\n\t"
	   "movi %0.8b, 24\n\t"
	   "cmhs %0.8b, %3.8b, %0.8b\n\t"
	   "tbl %1.8b, {v16.16b - v17.16b}, %3.8b\n\t"
	   "bsl %0.8b, %4.8b, %1.8b\n\t"
           : "+w"(result), "=&w"(tmp1)
           : "Q"(temp), "w"(idx), "w"(r)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbx4_s8 (int8x8_t r, int8x8x4_t tab, int8x8_t idx)
{
  int8x8_t result = r;
  int8x16x2_t temp;
  temp.val[0] = vcombine_s8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_s8 (tab.val[2], tab.val[3]);
  __asm__ ("ld1 {v16.16b - v17.16b }, %1\n\t"
	   "tbx %0.8b, {v16.16b - v17.16b}, %2.8b\n\t"
           : "+w"(result)
           : "Q"(temp), "w"(idx)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbx4_u8 (uint8x8_t r, uint8x8x4_t tab, uint8x8_t idx)
{
  uint8x8_t result = r;
  uint8x16x2_t temp;
  temp.val[0] = vcombine_u8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_u8 (tab.val[2], tab.val[3]);
  __asm__ ("ld1 {v16.16b - v17.16b }, %1\n\t"
	   "tbx %0.8b, {v16.16b - v17.16b}, %2.8b\n\t"
           : "+w"(result)
           : "Q"(temp), "w"(idx)
           : "v16", "v17", "memory");
  return result;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbx4_p8 (poly8x8_t r, poly8x8x4_t tab, uint8x8_t idx)
{
  poly8x8_t result = r;
  poly8x16x2_t temp;
  temp.val[0] = vcombine_p8 (tab.val[0], tab.val[1]);
  temp.val[1] = vcombine_p8 (tab.val[2], tab.val[3]);
  __asm__ ("ld1 {v16.16b - v17.16b }, %1\n\t"
	   "tbx %0.8b, {v16.16b - v17.16b}, %2.8b\n\t"
           : "+w"(result)
           : "Q"(temp), "w"(idx)
           : "v16", "v17", "memory");
  return result;
}

/* End of temporary inline asm.  */

/* Start of optimal implementations in approved order.  */

/* vabs  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vabs_f32 (float32x2_t __a)
{
  return __builtin_aarch64_absv2sf (__a);
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vabs_f64 (float64x1_t __a)
{
  return __builtin_fabs (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vabs_s8 (int8x8_t __a)
{
  return __builtin_aarch64_absv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vabs_s16 (int16x4_t __a)
{
  return __builtin_aarch64_absv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vabs_s32 (int32x2_t __a)
{
  return __builtin_aarch64_absv2si (__a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vabs_s64 (int64x1_t __a)
{
  return __builtin_llabs (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vabsq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_absv4sf (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vabsq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_absv2df (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vabsq_s8 (int8x16_t __a)
{
  return __builtin_aarch64_absv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabsq_s16 (int16x8_t __a)
{
  return __builtin_aarch64_absv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabsq_s32 (int32x4_t __a)
{
  return __builtin_aarch64_absv4si (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vabsq_s64 (int64x2_t __a)
{
  return __builtin_aarch64_absv2di (__a);
}

/* vadd */

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vaddd_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a + __b;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vaddd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a + __b;
}

#if __AARCH64EB__
#define __LANE0(__t) ((__t) - 1)
#else
#define __LANE0(__t) 0
#endif

/* vaddv */

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vaddv_s8 (int8x8_t __a)
{
  return vget_lane_s8 (__builtin_aarch64_reduc_splus_v8qi (__a), __LANE0 (8));
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vaddv_s16 (int16x4_t __a)
{
  return vget_lane_s16 (__builtin_aarch64_reduc_splus_v4hi (__a), __LANE0 (4));
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vaddv_s32 (int32x2_t __a)
{
  return vget_lane_s32 (__builtin_aarch64_reduc_splus_v2si (__a), __LANE0 (2));
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vaddv_u8 (uint8x8_t __a)
{
  return vget_lane_u8 ((uint8x8_t)
		__builtin_aarch64_reduc_uplus_v8qi ((int8x8_t) __a),
		__LANE0 (8));
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vaddv_u16 (uint16x4_t __a)
{
  return vget_lane_u16 ((uint16x4_t)
		__builtin_aarch64_reduc_uplus_v4hi ((int16x4_t) __a),
		__LANE0 (4));
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vaddv_u32 (uint32x2_t __a)
{
  return vget_lane_u32 ((uint32x2_t)
		__builtin_aarch64_reduc_uplus_v2si ((int32x2_t) __a),
		__LANE0 (2));
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vaddvq_s8 (int8x16_t __a)
{
  return vgetq_lane_s8 (__builtin_aarch64_reduc_splus_v16qi (__a),
			__LANE0 (16));
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vaddvq_s16 (int16x8_t __a)
{
  return vgetq_lane_s16 (__builtin_aarch64_reduc_splus_v8hi (__a), __LANE0 (8));
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vaddvq_s32 (int32x4_t __a)
{
  return vgetq_lane_s32 (__builtin_aarch64_reduc_splus_v4si (__a), __LANE0 (4));
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vaddvq_s64 (int64x2_t __a)
{
  return vgetq_lane_s64 (__builtin_aarch64_reduc_splus_v2di (__a), __LANE0 (2));
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vaddvq_u8 (uint8x16_t __a)
{
  return vgetq_lane_u8 ((uint8x16_t)
		__builtin_aarch64_reduc_uplus_v16qi ((int8x16_t) __a),
		__LANE0 (16));
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vaddvq_u16 (uint16x8_t __a)
{
  return vgetq_lane_u16 ((uint16x8_t)
		__builtin_aarch64_reduc_uplus_v8hi ((int16x8_t) __a),
		__LANE0 (8));
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vaddvq_u32 (uint32x4_t __a)
{
  return vgetq_lane_u32 ((uint32x4_t)
		__builtin_aarch64_reduc_uplus_v4si ((int32x4_t) __a),
		__LANE0 (4));
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vaddvq_u64 (uint64x2_t __a)
{
  return vgetq_lane_u64 ((uint64x2_t)
		__builtin_aarch64_reduc_uplus_v2di ((int64x2_t) __a),
		__LANE0 (2));
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vaddv_f32 (float32x2_t __a)
{
  float32x2_t __t = __builtin_aarch64_reduc_splus_v2sf (__a);
  return vget_lane_f32 (__t, __LANE0 (2));
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vaddvq_f32 (float32x4_t __a)
{
  float32x4_t __t = __builtin_aarch64_reduc_splus_v4sf (__a);
  return vgetq_lane_f32 (__t, __LANE0 (4));
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vaddvq_f64 (float64x2_t __a)
{
  float64x2_t __t = __builtin_aarch64_reduc_splus_v2df (__a);
  return vgetq_lane_f64 (__t, __LANE0 (2));
}

/* vbsl  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vbsl_f32 (uint32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  return __builtin_aarch64_simd_bslv2sf_suss (__a, __b, __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vbsl_p8 (uint8x8_t __a, poly8x8_t __b, poly8x8_t __c)
{
  return __builtin_aarch64_simd_bslv8qi_pupp (__a, __b, __c);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vbsl_p16 (uint16x4_t __a, poly16x4_t __b, poly16x4_t __c)
{
  return __builtin_aarch64_simd_bslv4hi_pupp (__a, __b, __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vbsl_s8 (uint8x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return __builtin_aarch64_simd_bslv8qi_suss (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vbsl_s16 (uint16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_simd_bslv4hi_suss (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vbsl_s32 (uint32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_simd_bslv2si_suss (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vbsl_s64 (uint64x1_t __a, int64x1_t __b, int64x1_t __c)
{
  return __builtin_aarch64_simd_bsldi_suss (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vbsl_u8 (uint8x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return __builtin_aarch64_simd_bslv8qi_uuuu (__a, __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vbsl_u16 (uint16x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return __builtin_aarch64_simd_bslv4hi_uuuu (__a, __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vbsl_u32 (uint32x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return __builtin_aarch64_simd_bslv2si_uuuu (__a, __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vbsl_u64 (uint64x1_t __a, uint64x1_t __b, uint64x1_t __c)
{
  return __builtin_aarch64_simd_bsldi_uuuu (__a, __b, __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vbslq_f32 (uint32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return __builtin_aarch64_simd_bslv4sf_suss (__a, __b, __c);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vbslq_f64 (uint64x2_t __a, float64x2_t __b, float64x2_t __c)
{
  return __builtin_aarch64_simd_bslv2df_suss (__a, __b, __c);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vbslq_p8 (uint8x16_t __a, poly8x16_t __b, poly8x16_t __c)
{
  return __builtin_aarch64_simd_bslv16qi_pupp (__a, __b, __c);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vbslq_p16 (uint16x8_t __a, poly16x8_t __b, poly16x8_t __c)
{
  return __builtin_aarch64_simd_bslv8hi_pupp (__a, __b, __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vbslq_s8 (uint8x16_t __a, int8x16_t __b, int8x16_t __c)
{
  return __builtin_aarch64_simd_bslv16qi_suss (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vbslq_s16 (uint16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_simd_bslv8hi_suss (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vbslq_s32 (uint32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_simd_bslv4si_suss (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vbslq_s64 (uint64x2_t __a, int64x2_t __b, int64x2_t __c)
{
  return __builtin_aarch64_simd_bslv2di_suss (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vbslq_u8 (uint8x16_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return __builtin_aarch64_simd_bslv16qi_uuuu (__a, __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vbslq_u16 (uint16x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_aarch64_simd_bslv8hi_uuuu (__a, __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vbslq_u32 (uint32x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_aarch64_simd_bslv4si_uuuu (__a, __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vbslq_u64 (uint64x2_t __a, uint64x2_t __b, uint64x2_t __c)
{
  return __builtin_aarch64_simd_bslv2di_uuuu (__a, __b, __c);
}

/* vcage  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcages_f32 (float32_t __a, float32_t __b)
{
  return __builtin_fabsf (__a) >= __builtin_fabsf (__b) ? -1 : 0;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcage_f32 (float32x2_t __a, float32x2_t __b)
{
  return vabs_f32 (__a) >= vabs_f32 (__b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcageq_f32 (float32x4_t __a, float32x4_t __b)
{
  return vabsq_f32 (__a) >= vabsq_f32 (__b);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcaged_f64 (float64_t __a, float64_t __b)
{
  return __builtin_fabs (__a) >= __builtin_fabs (__b) ? -1 : 0;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcageq_f64 (float64x2_t __a, float64x2_t __b)
{
  return vabsq_f64 (__a) >= vabsq_f64 (__b);
}

/* vcagt  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcagts_f32 (float32_t __a, float32_t __b)
{
  return __builtin_fabsf (__a) > __builtin_fabsf (__b) ? -1 : 0;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcagt_f32 (float32x2_t __a, float32x2_t __b)
{
  return vabs_f32 (__a) > vabs_f32 (__b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcagtq_f32 (float32x4_t __a, float32x4_t __b)
{
  return vabsq_f32 (__a) > vabsq_f32 (__b);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcagtd_f64 (float64_t __a, float64_t __b)
{
  return __builtin_fabs (__a) > __builtin_fabs (__b) ? -1 : 0;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcagtq_f64 (float64x2_t __a, float64x2_t __b)
{
  return vabsq_f64 (__a) > vabsq_f64 (__b);
}

/* vcale  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcale_f32 (float32x2_t __a, float32x2_t __b)
{
  return vabs_f32 (__a) <= vabs_f32 (__b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcaleq_f32 (float32x4_t __a, float32x4_t __b)
{
  return vabsq_f32 (__a) <= vabsq_f32 (__b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcaleq_f64 (float64x2_t __a, float64x2_t __b)
{
  return vabsq_f64 (__a) <= vabsq_f64 (__b);
}

/* vcalt  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcalt_f32 (float32x2_t __a, float32x2_t __b)
{
  return vabs_f32 (__a) < vabs_f32 (__b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcaltq_f32 (float32x4_t __a, float32x4_t __b)
{
  return vabsq_f32 (__a) < vabsq_f32 (__b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcaltq_f64 (float64x2_t __a, float64x2_t __b)
{
  return vabsq_f64 (__a) < vabsq_f64 (__b);
}

/* vceq - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vceq_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmeqv2sf (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceq_f64 (float64x1_t __a, float64x1_t __b)
{
  return __a == __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vceq_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmeqv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vceq_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmeqv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vceq_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmeqv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vceq_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmeqv2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceq_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a == __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vceq_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmeqv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vceq_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmeqv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vceq_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmeqv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceq_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a == __b ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vceqq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmeqv4sf (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vceqq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmeqv2df (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vceqq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmeqv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vceqq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmeqv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vceqq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmeqv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vceqq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmeqv4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vceqq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmeqv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vceqq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmeqv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vceqq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmeqv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vceqq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmeqv4si ((int32x4_t) __a,
						  (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vceqq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmeqv2di ((int64x2_t) __a,
						  (int64x2_t) __b);
}

/* vceq - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vceqs_f32 (float32_t __a, float32_t __b)
{
  return __a == __b ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceqd_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a == __b ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceqd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a == __b ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vceqd_f64 (float64_t __a, float64_t __b)
{
  return __a == __b ? -1ll : 0ll;
}

/* vceqz - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vceqz_f32 (float32x2_t __a)
{
  float32x2_t __b = {0.0f, 0.0f};
  return (uint32x2_t) __builtin_aarch64_cmeqv2sf (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceqz_f64 (float64x1_t __a)
{
  return __a == 0.0 ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vceqz_p8 (poly8x8_t __a)
{
  poly8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmeqv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vceqz_s8 (int8x8_t __a)
{
  int8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmeqv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vceqz_s16 (int16x4_t __a)
{
  int16x4_t __b = {0, 0, 0, 0};
  return (uint16x4_t) __builtin_aarch64_cmeqv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vceqz_s32 (int32x2_t __a)
{
  int32x2_t __b = {0, 0};
  return (uint32x2_t) __builtin_aarch64_cmeqv2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceqz_s64 (int64x1_t __a)
{
  return __a == 0ll ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vceqz_u8 (uint8x8_t __a)
{
  uint8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmeqv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vceqz_u16 (uint16x4_t __a)
{
  uint16x4_t __b = {0, 0, 0, 0};
  return (uint16x4_t) __builtin_aarch64_cmeqv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vceqz_u32 (uint32x2_t __a)
{
  uint32x2_t __b = {0, 0};
  return (uint32x2_t) __builtin_aarch64_cmeqv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceqz_u64 (uint64x1_t __a)
{
  return __a == 0ll ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vceqzq_f32 (float32x4_t __a)
{
  float32x4_t __b = {0.0f, 0.0f, 0.0f, 0.0f};
  return (uint32x4_t) __builtin_aarch64_cmeqv4sf (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vceqzq_f64 (float64x2_t __a)
{
  float64x2_t __b = {0.0, 0.0};
  return (uint64x2_t) __builtin_aarch64_cmeqv2df (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vceqzq_p8 (poly8x16_t __a)
{
  poly8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmeqv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vceqzq_s8 (int8x16_t __a)
{
  int8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmeqv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vceqzq_s16 (int16x8_t __a)
{
  int16x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint16x8_t) __builtin_aarch64_cmeqv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vceqzq_s32 (int32x4_t __a)
{
  int32x4_t __b = {0, 0, 0, 0};
  return (uint32x4_t) __builtin_aarch64_cmeqv4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vceqzq_s64 (int64x2_t __a)
{
  int64x2_t __b = {0, 0};
  return (uint64x2_t) __builtin_aarch64_cmeqv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vceqzq_u8 (uint8x16_t __a)
{
  uint8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmeqv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vceqzq_u16 (uint16x8_t __a)
{
  uint16x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint16x8_t) __builtin_aarch64_cmeqv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vceqzq_u32 (uint32x4_t __a)
{
  uint32x4_t __b = {0, 0, 0, 0};
  return (uint32x4_t) __builtin_aarch64_cmeqv4si ((int32x4_t) __a,
						  (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vceqzq_u64 (uint64x2_t __a)
{
  uint64x2_t __b = {0, 0};
  return (uint64x2_t) __builtin_aarch64_cmeqv2di ((int64x2_t) __a,
						  (int64x2_t) __b);
}

/* vceqz - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vceqzs_f32 (float32_t __a)
{
  return __a == 0.0f ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceqzd_s64 (int64x1_t __a)
{
  return __a == 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceqzd_u64 (int64x1_t __a)
{
  return __a == 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vceqzd_f64 (float64_t __a)
{
  return __a == 0.0 ? -1ll : 0ll;
}

/* vcge - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcge_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgev2sf (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcge_f64 (float64x1_t __a, float64x1_t __b)
{
  return __a >= __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcge_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgev8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcge_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgev8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcge_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmgev4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcge_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgev2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcge_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a >= __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcge_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgeuv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcge_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmgeuv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcge_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgeuv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcge_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a >= __b ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgeq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgev4sf (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgeq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgev2df (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgeq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgev16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgeq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgev16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgeq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmgev8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgeq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgev4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgeq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgev2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgeq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgeuv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgeq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmgeuv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgeq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgeuv4si ((int32x4_t) __a,
						  (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgeq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgeuv2di ((int64x2_t) __a,
						  (int64x2_t) __b);
}

/* vcge - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcges_f32 (float32_t __a, float32_t __b)
{
  return __a >= __b ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcged_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a >= __b ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcged_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a >= __b ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcged_f64 (float64_t __a, float64_t __b)
{
  return __a >= __b ? -1ll : 0ll;
}

/* vcgez - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgez_f32 (float32x2_t __a)
{
  float32x2_t __b = {0.0f, 0.0f};
  return (uint32x2_t) __builtin_aarch64_cmgev2sf (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgez_f64 (float64x1_t __a)
{
  return __a >= 0.0 ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgez_p8 (poly8x8_t __a)
{
  poly8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmgev8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgez_s8 (int8x8_t __a)
{
  int8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmgev8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgez_s16 (int16x4_t __a)
{
  int16x4_t __b = {0, 0, 0, 0};
  return (uint16x4_t) __builtin_aarch64_cmgev4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgez_s32 (int32x2_t __a)
{
  int32x2_t __b = {0, 0};
  return (uint32x2_t) __builtin_aarch64_cmgev2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgez_s64 (int64x1_t __a)
{
  return __a >= 0ll ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgez_u8 (uint8x8_t __a)
{
  uint8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmgeuv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgez_u16 (uint16x4_t __a)
{
  uint16x4_t __b = {0, 0, 0, 0};
  return (uint16x4_t) __builtin_aarch64_cmgeuv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgez_u32 (uint32x2_t __a)
{
  uint32x2_t __b = {0, 0};
  return (uint32x2_t) __builtin_aarch64_cmgeuv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgez_u64 (uint64x1_t __a)
{
  return __a >= 0ll ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgezq_f32 (float32x4_t __a)
{
  float32x4_t __b = {0.0f, 0.0f, 0.0f, 0.0f};
  return (uint32x4_t) __builtin_aarch64_cmgev4sf (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgezq_f64 (float64x2_t __a)
{
  float64x2_t __b = {0.0, 0.0};
  return (uint64x2_t) __builtin_aarch64_cmgev2df (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgezq_p8 (poly8x16_t __a)
{
  poly8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmgev16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgezq_s8 (int8x16_t __a)
{
  int8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmgev16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgezq_s16 (int16x8_t __a)
{
  int16x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint16x8_t) __builtin_aarch64_cmgev8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgezq_s32 (int32x4_t __a)
{
  int32x4_t __b = {0, 0, 0, 0};
  return (uint32x4_t) __builtin_aarch64_cmgev4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgezq_s64 (int64x2_t __a)
{
  int64x2_t __b = {0, 0};
  return (uint64x2_t) __builtin_aarch64_cmgev2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgezq_u8 (uint8x16_t __a)
{
  uint8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmgeuv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgezq_u16 (uint16x8_t __a)
{
  uint16x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint16x8_t) __builtin_aarch64_cmgeuv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgezq_u32 (uint32x4_t __a)
{
  uint32x4_t __b = {0, 0, 0, 0};
  return (uint32x4_t) __builtin_aarch64_cmgeuv4si ((int32x4_t) __a,
						  (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgezq_u64 (uint64x2_t __a)
{
  uint64x2_t __b = {0, 0};
  return (uint64x2_t) __builtin_aarch64_cmgeuv2di ((int64x2_t) __a,
						  (int64x2_t) __b);
}

/* vcgez - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcgezs_f32 (float32_t __a)
{
  return __a >= 0.0f ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgezd_s64 (int64x1_t __a)
{
  return __a >= 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgezd_u64 (int64x1_t __a)
{
  return __a >= 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcgezd_f64 (float64_t __a)
{
  return __a >= 0.0 ? -1ll : 0ll;
}

/* vcgt - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgt_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgtv2sf (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgt_f64 (float64x1_t __a, float64x1_t __b)
{
  return __a > __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgt_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgtv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgt_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgtv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgt_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmgtv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgt_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgtv2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgt_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a > __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgt_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgtuv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgt_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmgtuv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgt_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgtuv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgt_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a > __b ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgtq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgtv4sf (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgtq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgtv2df (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgtq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgtv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgtq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgtv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgtq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmgtv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgtq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgtv4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgtq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgtv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgtq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgtuv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgtq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmgtuv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgtq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgtuv4si ((int32x4_t) __a,
						  (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgtq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgtuv2di ((int64x2_t) __a,
						  (int64x2_t) __b);
}

/* vcgt - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcgts_f32 (float32_t __a, float32_t __b)
{
  return __a > __b ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgtd_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a > __b ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgtd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a > __b ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcgtd_f64 (float64_t __a, float64_t __b)
{
  return __a > __b ? -1ll : 0ll;
}

/* vcgtz - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgtz_f32 (float32x2_t __a)
{
  float32x2_t __b = {0.0f, 0.0f};
  return (uint32x2_t) __builtin_aarch64_cmgtv2sf (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgtz_f64 (float64x1_t __a)
{
  return __a > 0.0 ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgtz_p8 (poly8x8_t __a)
{
  poly8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmgtv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgtz_s8 (int8x8_t __a)
{
  int8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmgtv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgtz_s16 (int16x4_t __a)
{
  int16x4_t __b = {0, 0, 0, 0};
  return (uint16x4_t) __builtin_aarch64_cmgtv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgtz_s32 (int32x2_t __a)
{
  int32x2_t __b = {0, 0};
  return (uint32x2_t) __builtin_aarch64_cmgtv2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgtz_s64 (int64x1_t __a)
{
  return __a > 0ll ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgtz_u8 (uint8x8_t __a)
{
  uint8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmgtuv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgtz_u16 (uint16x4_t __a)
{
  uint16x4_t __b = {0, 0, 0, 0};
  return (uint16x4_t) __builtin_aarch64_cmgtuv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgtz_u32 (uint32x2_t __a)
{
  uint32x2_t __b = {0, 0};
  return (uint32x2_t) __builtin_aarch64_cmgtuv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgtz_u64 (uint64x1_t __a)
{
  return __a > 0ll ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgtzq_f32 (float32x4_t __a)
{
  float32x4_t __b = {0.0f, 0.0f, 0.0f, 0.0f};
  return (uint32x4_t) __builtin_aarch64_cmgtv4sf (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgtzq_f64 (float64x2_t __a)
{
  float64x2_t __b = {0.0, 0.0};
  return (uint64x2_t) __builtin_aarch64_cmgtv2df (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgtzq_p8 (poly8x16_t __a)
{
  poly8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmgtv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgtzq_s8 (int8x16_t __a)
{
  int8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmgtv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgtzq_s16 (int16x8_t __a)
{
  int16x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint16x8_t) __builtin_aarch64_cmgtv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgtzq_s32 (int32x4_t __a)
{
  int32x4_t __b = {0, 0, 0, 0};
  return (uint32x4_t) __builtin_aarch64_cmgtv4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgtzq_s64 (int64x2_t __a)
{
  int64x2_t __b = {0, 0};
  return (uint64x2_t) __builtin_aarch64_cmgtv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgtzq_u8 (uint8x16_t __a)
{
  uint8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmgtuv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgtzq_u16 (uint16x8_t __a)
{
  uint16x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint16x8_t) __builtin_aarch64_cmgtuv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgtzq_u32 (uint32x4_t __a)
{
  uint32x4_t __b = {0, 0, 0, 0};
  return (uint32x4_t) __builtin_aarch64_cmgtuv4si ((int32x4_t) __a,
						  (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcgtzq_u64 (uint64x2_t __a)
{
  uint64x2_t __b = {0, 0};
  return (uint64x2_t) __builtin_aarch64_cmgtuv2di ((int64x2_t) __a,
						  (int64x2_t) __b);
}

/* vcgtz - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcgtzs_f32 (float32_t __a)
{
  return __a > 0.0f ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgtzd_s64 (int64x1_t __a)
{
  return __a > 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcgtzd_u64 (int64x1_t __a)
{
  return __a > 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcgtzd_f64 (float64_t __a)
{
  return __a > 0.0 ? -1ll : 0ll;
}

/* vcle - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcle_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgev2sf (__b, __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcle_f64 (float64x1_t __a, float64x1_t __b)
{
  return __a <= __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcle_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgev8qi ((int8x8_t) __b,
						 (int8x8_t) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcle_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgev8qi (__b, __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcle_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmgev4hi (__b, __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcle_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgev2si (__b, __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcle_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a <= __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcle_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgeuv8qi ((int8x8_t) __b,
						 (int8x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcle_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmgeuv4hi ((int16x4_t) __b,
						  (int16x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcle_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgeuv2si ((int32x2_t) __b,
						  (int32x2_t) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcle_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a <= __b ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcleq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgev4sf (__b, __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcleq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgev2df (__b, __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcleq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgev16qi ((int8x16_t) __b,
						   (int8x16_t) __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcleq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgev16qi (__b, __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcleq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmgev8hi (__b, __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcleq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgev4si (__b, __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcleq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgev2di (__b, __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcleq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgeuv16qi ((int8x16_t) __b,
						   (int8x16_t) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcleq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmgeuv8hi ((int16x8_t) __b,
						  (int16x8_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcleq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgeuv4si ((int32x4_t) __b,
						  (int32x4_t) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcleq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgeuv2di ((int64x2_t) __b,
						  (int64x2_t) __a);
}

/* vcle - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcles_f32 (float32_t __a, float32_t __b)
{
  return __a <= __b ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcled_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a <= __b ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcled_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a <= __b ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcled_f64 (float64_t __a, float64_t __b)
{
  return __a <= __b ? -1ll : 0ll;
}

/* vclez - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclez_f32 (float32x2_t __a)
{
  float32x2_t __b = {0.0f, 0.0f};
  return (uint32x2_t) __builtin_aarch64_cmlev2sf (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vclez_f64 (float64x1_t __a)
{
  return __a <= 0.0 ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vclez_p8 (poly8x8_t __a)
{
  poly8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmlev8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vclez_s8 (int8x8_t __a)
{
  int8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmlev8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vclez_s16 (int16x4_t __a)
{
  int16x4_t __b = {0, 0, 0, 0};
  return (uint16x4_t) __builtin_aarch64_cmlev4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclez_s32 (int32x2_t __a)
{
  int32x2_t __b = {0, 0};
  return (uint32x2_t) __builtin_aarch64_cmlev2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vclez_s64 (int64x1_t __a)
{
  return __a <= 0ll ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vclez_u64 (uint64x1_t __a)
{
  return __a <= 0ll ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vclezq_f32 (float32x4_t __a)
{
  float32x4_t __b = {0.0f, 0.0f, 0.0f, 0.0f};
  return (uint32x4_t) __builtin_aarch64_cmlev4sf (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vclezq_f64 (float64x2_t __a)
{
  float64x2_t __b = {0.0, 0.0};
  return (uint64x2_t) __builtin_aarch64_cmlev2df (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vclezq_p8 (poly8x16_t __a)
{
  poly8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmlev16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vclezq_s8 (int8x16_t __a)
{
  int8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmlev16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vclezq_s16 (int16x8_t __a)
{
  int16x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint16x8_t) __builtin_aarch64_cmlev8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vclezq_s32 (int32x4_t __a)
{
  int32x4_t __b = {0, 0, 0, 0};
  return (uint32x4_t) __builtin_aarch64_cmlev4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vclezq_s64 (int64x2_t __a)
{
  int64x2_t __b = {0, 0};
  return (uint64x2_t) __builtin_aarch64_cmlev2di (__a, __b);
}

/* vclez - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vclezs_f32 (float32_t __a)
{
  return __a <= 0.0f ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vclezd_s64 (int64x1_t __a)
{
  return __a <= 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vclezd_u64 (int64x1_t __a)
{
  return __a <= 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vclezd_f64 (float64_t __a)
{
  return __a <= 0.0 ? -1ll : 0ll;
}

/* vclt - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclt_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgtv2sf (__b, __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vclt_f64 (float64x1_t __a, float64x1_t __b)
{
  return __a < __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vclt_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgtv8qi ((int8x8_t) __b,
						 (int8x8_t) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vclt_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgtv8qi (__b, __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vclt_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmgtv4hi (__b, __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclt_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgtv2si (__b, __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vclt_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a < __b ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vclt_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmgtuv8qi ((int8x8_t) __b,
						 (int8x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vclt_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmgtuv4hi ((int16x4_t) __b,
						  (int16x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclt_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmgtuv2si ((int32x2_t) __b,
						  (int32x2_t) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vclt_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a < __b ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcltq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgtv4sf (__b, __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcltq_f64 (float64x2_t __a, float64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgtv2df (__b, __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcltq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgtv16qi ((int8x16_t) __b,
						   (int8x16_t) __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcltq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgtv16qi (__b, __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcltq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmgtv8hi (__b, __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcltq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgtv4si (__b, __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcltq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgtv2di (__b, __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcltq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmgtuv16qi ((int8x16_t) __b,
						   (int8x16_t) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcltq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmgtuv8hi ((int16x8_t) __b,
						  (int16x8_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcltq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmgtuv4si ((int32x4_t) __b,
						  (int32x4_t) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcltq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmgtuv2di ((int64x2_t) __b,
						  (int64x2_t) __a);
}

/* vclt - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vclts_f32 (float32_t __a, float32_t __b)
{
  return __a < __b ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcltd_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a < __b ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcltd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a < __b ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcltd_f64 (float64_t __a, float64_t __b)
{
  return __a < __b ? -1ll : 0ll;
}

/* vcltz - vector.  */

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcltz_f32 (float32x2_t __a)
{
  float32x2_t __b = {0.0f, 0.0f};
  return (uint32x2_t) __builtin_aarch64_cmltv2sf (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcltz_f64 (float64x1_t __a)
{
  return __a < 0.0 ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcltz_p8 (poly8x8_t __a)
{
  poly8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmltv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcltz_s8 (int8x8_t __a)
{
  int8x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x8_t) __builtin_aarch64_cmltv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcltz_s16 (int16x4_t __a)
{
  int16x4_t __b = {0, 0, 0, 0};
  return (uint16x4_t) __builtin_aarch64_cmltv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcltz_s32 (int32x2_t __a)
{
  int32x2_t __b = {0, 0};
  return (uint32x2_t) __builtin_aarch64_cmltv2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcltz_s64 (int64x1_t __a)
{
  return __a < 0ll ? -1ll : 0ll;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcltzq_f32 (float32x4_t __a)
{
  float32x4_t __b = {0.0f, 0.0f, 0.0f, 0.0f};
  return (uint32x4_t) __builtin_aarch64_cmltv4sf (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcltzq_f64 (float64x2_t __a)
{
  float64x2_t __b = {0.0, 0.0};
  return (uint64x2_t) __builtin_aarch64_cmltv2df (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcltzq_p8 (poly8x16_t __a)
{
  poly8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmltv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcltzq_s8 (int8x16_t __a)
{
  int8x16_t __b = {0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0};
  return (uint8x16_t) __builtin_aarch64_cmltv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcltzq_s16 (int16x8_t __a)
{
  int16x8_t __b = {0, 0, 0, 0, 0, 0, 0, 0};
  return (uint16x8_t) __builtin_aarch64_cmltv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcltzq_s32 (int32x4_t __a)
{
  int32x4_t __b = {0, 0, 0, 0};
  return (uint32x4_t) __builtin_aarch64_cmltv4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcltzq_s64 (int64x2_t __a)
{
  int64x2_t __b = {0, 0};
  return (uint64x2_t) __builtin_aarch64_cmltv2di (__a, __b);
}

/* vcltz - scalar.  */

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcltzs_f32 (float32_t __a)
{
  return __a < 0.0f ? -1 : 0;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcltzd_s64 (int64x1_t __a)
{
  return __a < 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcltzd_u64 (int64x1_t __a)
{
  return __a < 0 ? -1ll : 0ll;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcltzd_f64 (float64_t __a)
{
  return __a < 0.0 ? -1ll : 0ll;
}

/* vclz.  */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vclz_s8 (int8x8_t __a)
{
  return __builtin_aarch64_clzv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vclz_s16 (int16x4_t __a)
{
  return __builtin_aarch64_clzv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vclz_s32 (int32x2_t __a)
{
  return __builtin_aarch64_clzv2si (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vclz_u8 (uint8x8_t __a)
{
  return (uint8x8_t)__builtin_aarch64_clzv8qi ((int8x8_t)__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vclz_u16 (uint16x4_t __a)
{
  return (uint16x4_t)__builtin_aarch64_clzv4hi ((int16x4_t)__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclz_u32 (uint32x2_t __a)
{
  return (uint32x2_t)__builtin_aarch64_clzv2si ((int32x2_t)__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vclzq_s8 (int8x16_t __a)
{
  return __builtin_aarch64_clzv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vclzq_s16 (int16x8_t __a)
{
  return __builtin_aarch64_clzv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vclzq_s32 (int32x4_t __a)
{
  return __builtin_aarch64_clzv4si (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vclzq_u8 (uint8x16_t __a)
{
  return (uint8x16_t)__builtin_aarch64_clzv16qi ((int8x16_t)__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vclzq_u16 (uint16x8_t __a)
{
  return (uint16x8_t)__builtin_aarch64_clzv8hi ((int16x8_t)__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vclzq_u32 (uint32x4_t __a)
{
  return (uint32x4_t)__builtin_aarch64_clzv4si ((int32x4_t)__a);
}

/* vcvt (double -> float).  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcvt_f32_f64 (float64x2_t __a)
{
  return __builtin_aarch64_float_truncate_lo_v2sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcvt_high_f32_f64 (float32x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_float_truncate_hi_v4sf (__a, __b);
}

/* vcvt (float -> double).  */

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vcvt_f64_f32 (float32x2_t __a)
{

  return __builtin_aarch64_float_extend_lo_v2df (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vcvt_high_f64_f32 (float32x4_t __a)
{
  return __builtin_aarch64_vec_unpacks_hi_v4sf (__a);
}

/* vcvt  (<u>int -> float)  */

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vcvtd_f64_s64 (int64_t __a)
{
  return (float64_t) __a;
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vcvtd_f64_u64 (uint64_t __a)
{
  return (float64_t) __a;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vcvts_f32_s32 (int32_t __a)
{
  return (float32_t) __a;
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vcvts_f32_u32 (uint32_t __a)
{
  return (float32_t) __a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcvt_f32_s32 (int32x2_t __a)
{
  return __builtin_aarch64_floatv2siv2sf (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcvt_f32_u32 (uint32x2_t __a)
{
  return __builtin_aarch64_floatunsv2siv2sf ((int32x2_t) __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcvtq_f32_s32 (int32x4_t __a)
{
  return __builtin_aarch64_floatv4siv4sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcvtq_f32_u32 (uint32x4_t __a)
{
  return __builtin_aarch64_floatunsv4siv4sf ((int32x4_t) __a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vcvtq_f64_s64 (int64x2_t __a)
{
  return __builtin_aarch64_floatv2div2df (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vcvtq_f64_u64 (uint64x2_t __a)
{
  return __builtin_aarch64_floatunsv2div2df ((int64x2_t) __a);
}

/* vcvt (float -> <u>int)  */

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vcvtd_s64_f64 (float64_t __a)
{
  return (int64_t) __a;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcvtd_u64_f64 (float64_t __a)
{
  return (uint64_t) __a;
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvts_s32_f32 (float32_t __a)
{
  return (int32_t) __a;
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvts_u32_f32 (float32_t __a)
{
  return (uint32_t) __a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcvt_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lbtruncv2sfv2si (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcvt_u32_f32 (float32x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x2_t) __builtin_aarch64_lbtruncuv2sfv2si (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vcvtq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lbtruncv4sfv4si (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcvtq_u32_f32 (float32x4_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x4_t) __builtin_aarch64_lbtruncuv4sfv4si (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vcvtq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lbtruncv2dfv2di (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcvtq_u64_f64 (float64x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint64x2_t) __builtin_aarch64_lbtruncuv2dfv2di (__a);
}

/* vcvta  */

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vcvtad_s64_f64 (float64_t __a)
{
  return __builtin_aarch64_lrounddfdi (__a);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcvtad_u64_f64 (float64_t __a)
{
  return __builtin_aarch64_lroundudfdi (__a);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvtas_s32_f32 (float32_t __a)
{
  return __builtin_aarch64_lroundsfsi (__a);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvtas_u32_f32 (float32_t __a)
{
  return __builtin_aarch64_lroundusfsi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcvta_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lroundv2sfv2si (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcvta_u32_f32 (float32x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x2_t) __builtin_aarch64_lrounduv2sfv2si (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vcvtaq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lroundv4sfv4si (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcvtaq_u32_f32 (float32x4_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x4_t) __builtin_aarch64_lrounduv4sfv4si (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vcvtaq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lroundv2dfv2di (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcvtaq_u64_f64 (float64x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint64x2_t) __builtin_aarch64_lrounduv2dfv2di (__a);
}

/* vcvtm  */

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vcvtmd_s64_f64 (float64_t __a)
{
  return __builtin_lfloor (__a);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcvtmd_u64_f64 (float64_t __a)
{
  return __builtin_aarch64_lfloorudfdi (__a);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvtms_s32_f32 (float32_t __a)
{
  return __builtin_ifloorf (__a);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvtms_u32_f32 (float32_t __a)
{
  return __builtin_aarch64_lfloorusfsi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcvtm_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lfloorv2sfv2si (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcvtm_u32_f32 (float32x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x2_t) __builtin_aarch64_lflooruv2sfv2si (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vcvtmq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lfloorv4sfv4si (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcvtmq_u32_f32 (float32x4_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x4_t) __builtin_aarch64_lflooruv4sfv4si (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vcvtmq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lfloorv2dfv2di (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcvtmq_u64_f64 (float64x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint64x2_t) __builtin_aarch64_lflooruv2dfv2di (__a);
}

/* vcvtn  */

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vcvtnd_s64_f64 (float64_t __a)
{
  return __builtin_aarch64_lfrintndfdi (__a);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcvtnd_u64_f64 (float64_t __a)
{
  return __builtin_aarch64_lfrintnudfdi (__a);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvtns_s32_f32 (float32_t __a)
{
  return __builtin_aarch64_lfrintnsfsi (__a);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvtns_u32_f32 (float32_t __a)
{
  return __builtin_aarch64_lfrintnusfsi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcvtn_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lfrintnv2sfv2si (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcvtn_u32_f32 (float32x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x2_t) __builtin_aarch64_lfrintnuv2sfv2si (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vcvtnq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lfrintnv4sfv4si (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcvtnq_u32_f32 (float32x4_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x4_t) __builtin_aarch64_lfrintnuv4sfv4si (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vcvtnq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lfrintnv2dfv2di (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcvtnq_u64_f64 (float64x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint64x2_t) __builtin_aarch64_lfrintnuv2dfv2di (__a);
}

/* vcvtp  */

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vcvtpd_s64_f64 (float64_t __a)
{
  return __builtin_lceil (__a);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vcvtpd_u64_f64 (float64_t __a)
{
  return __builtin_aarch64_lceiludfdi (__a);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvtps_s32_f32 (float32_t __a)
{
  return __builtin_iceilf (__a);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvtps_u32_f32 (float32_t __a)
{
  return __builtin_aarch64_lceilusfsi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcvtp_s32_f32 (float32x2_t __a)
{
  return __builtin_aarch64_lceilv2sfv2si (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcvtp_u32_f32 (float32x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x2_t) __builtin_aarch64_lceiluv2sfv2si (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vcvtpq_s32_f32 (float32x4_t __a)
{
  return __builtin_aarch64_lceilv4sfv4si (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcvtpq_u32_f32 (float32x4_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint32x4_t) __builtin_aarch64_lceiluv4sfv4si (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vcvtpq_s64_f64 (float64x2_t __a)
{
  return __builtin_aarch64_lceilv2dfv2di (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcvtpq_u64_f64 (float64x2_t __a)
{
  /* TODO: This cast should go away when builtins have
     their correct types.  */
  return (uint64x2_t) __builtin_aarch64_lceiluv2dfv2di (__a);
}

/* vdup_n  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vdup_n_f32 (float32_t __a)
{
  return (float32x2_t) {__a, __a};
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vdup_n_f64 (float64_t __a)
{
  return __a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vdup_n_p8 (poly8_t __a)
{
  return (poly8x8_t) {__a, __a, __a, __a, __a, __a, __a, __a};
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vdup_n_p16 (poly16_t __a)
{
  return (poly16x4_t) {__a, __a, __a, __a};
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vdup_n_s8 (int8_t __a)
{
  return (int8x8_t) {__a, __a, __a, __a, __a, __a, __a, __a};
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vdup_n_s16 (int16_t __a)
{
  return (int16x4_t) {__a, __a, __a, __a};
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vdup_n_s32 (int32_t __a)
{
  return (int32x2_t) {__a, __a};
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vdup_n_s64 (int64_t __a)
{
  return __a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vdup_n_u8 (uint8_t __a)
{
  return (uint8x8_t) {__a, __a, __a, __a, __a, __a, __a, __a};
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vdup_n_u16 (uint16_t __a)
{
  return (uint16x4_t) {__a, __a, __a, __a};
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vdup_n_u32 (uint32_t __a)
{
  return (uint32x2_t) {__a, __a};
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vdup_n_u64 (uint64_t __a)
{
  return __a;
}

/* vdupq_n  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vdupq_n_f32 (float32_t __a)
{
  return (float32x4_t) {__a, __a, __a, __a};
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vdupq_n_f64 (float64_t __a)
{
  return (float64x2_t) {__a, __a};
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vdupq_n_p8 (uint32_t __a)
{
  return (poly8x16_t) {__a, __a, __a, __a, __a, __a, __a, __a,
		       __a, __a, __a, __a, __a, __a, __a, __a};
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vdupq_n_p16 (uint32_t __a)
{
  return (poly16x8_t) {__a, __a, __a, __a, __a, __a, __a, __a};
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vdupq_n_s8 (int32_t __a)
{
  return (int8x16_t) {__a, __a, __a, __a, __a, __a, __a, __a,
		      __a, __a, __a, __a, __a, __a, __a, __a};
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vdupq_n_s16 (int32_t __a)
{
  return (int16x8_t) {__a, __a, __a, __a, __a, __a, __a, __a};
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vdupq_n_s32 (int32_t __a)
{
  return (int32x4_t) {__a, __a, __a, __a};
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vdupq_n_s64 (int64_t __a)
{
  return (int64x2_t) {__a, __a};
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vdupq_n_u8 (uint32_t __a)
{
  return (uint8x16_t) {__a, __a, __a, __a, __a, __a, __a, __a,
		       __a, __a, __a, __a, __a, __a, __a, __a};
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vdupq_n_u16 (uint32_t __a)
{
  return (uint16x8_t) {__a, __a, __a, __a, __a, __a, __a, __a};
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vdupq_n_u32 (uint32_t __a)
{
  return (uint32x4_t) {__a, __a, __a, __a};
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vdupq_n_u64 (uint64_t __a)
{
  return (uint64x2_t) {__a, __a};
}

/* vdup_lane  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vdup_lane_f32 (float32x2_t __a, const int __b)
{
  return __aarch64_vdup_lane_f32 (__a, __b);
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vdup_lane_f64 (float64x1_t __a, const int __b)
{
  return __aarch64_vdup_lane_f64 (__a, __b);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vdup_lane_p8 (poly8x8_t __a, const int __b)
{
  return __aarch64_vdup_lane_p8 (__a, __b);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vdup_lane_p16 (poly16x4_t __a, const int __b)
{
  return __aarch64_vdup_lane_p16 (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vdup_lane_s8 (int8x8_t __a, const int __b)
{
  return __aarch64_vdup_lane_s8 (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vdup_lane_s16 (int16x4_t __a, const int __b)
{
  return __aarch64_vdup_lane_s16 (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vdup_lane_s32 (int32x2_t __a, const int __b)
{
  return __aarch64_vdup_lane_s32 (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vdup_lane_s64 (int64x1_t __a, const int __b)
{
  return __aarch64_vdup_lane_s64 (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vdup_lane_u8 (uint8x8_t __a, const int __b)
{
  return __aarch64_vdup_lane_u8 (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vdup_lane_u16 (uint16x4_t __a, const int __b)
{
  return __aarch64_vdup_lane_u16 (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vdup_lane_u32 (uint32x2_t __a, const int __b)
{
  return __aarch64_vdup_lane_u32 (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vdup_lane_u64 (uint64x1_t __a, const int __b)
{
  return __aarch64_vdup_lane_u64 (__a, __b);
}

/* vdup_laneq  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vdup_laneq_f32 (float32x4_t __a, const int __b)
{
  return __aarch64_vdup_laneq_f32 (__a, __b);
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vdup_laneq_f64 (float64x2_t __a, const int __b)
{
  return __aarch64_vdup_laneq_f64 (__a, __b);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vdup_laneq_p8 (poly8x16_t __a, const int __b)
{
  return __aarch64_vdup_laneq_p8 (__a, __b);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vdup_laneq_p16 (poly16x8_t __a, const int __b)
{
  return __aarch64_vdup_laneq_p16 (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vdup_laneq_s8 (int8x16_t __a, const int __b)
{
  return __aarch64_vdup_laneq_s8 (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vdup_laneq_s16 (int16x8_t __a, const int __b)
{
  return __aarch64_vdup_laneq_s16 (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vdup_laneq_s32 (int32x4_t __a, const int __b)
{
  return __aarch64_vdup_laneq_s32 (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vdup_laneq_s64 (int64x2_t __a, const int __b)
{
  return __aarch64_vdup_laneq_s64 (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vdup_laneq_u8 (uint8x16_t __a, const int __b)
{
  return __aarch64_vdup_laneq_u8 (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vdup_laneq_u16 (uint16x8_t __a, const int __b)
{
  return __aarch64_vdup_laneq_u16 (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vdup_laneq_u32 (uint32x4_t __a, const int __b)
{
  return __aarch64_vdup_laneq_u32 (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vdup_laneq_u64 (uint64x2_t __a, const int __b)
{
  return __aarch64_vdup_laneq_u64 (__a, __b);
}

/* vdupq_lane  */
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vdupq_lane_f32 (float32x2_t __a, const int __b)
{
  return __aarch64_vdupq_lane_f32 (__a, __b);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vdupq_lane_f64 (float64x1_t __a, const int __b)
{
  return __aarch64_vdupq_lane_f64 (__a, __b);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vdupq_lane_p8 (poly8x8_t __a, const int __b)
{
  return __aarch64_vdupq_lane_p8 (__a, __b);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vdupq_lane_p16 (poly16x4_t __a, const int __b)
{
  return __aarch64_vdupq_lane_p16 (__a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vdupq_lane_s8 (int8x8_t __a, const int __b)
{
  return __aarch64_vdupq_lane_s8 (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vdupq_lane_s16 (int16x4_t __a, const int __b)
{
  return __aarch64_vdupq_lane_s16 (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vdupq_lane_s32 (int32x2_t __a, const int __b)
{
  return __aarch64_vdupq_lane_s32 (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vdupq_lane_s64 (int64x1_t __a, const int __b)
{
  return __aarch64_vdupq_lane_s64 (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vdupq_lane_u8 (uint8x8_t __a, const int __b)
{
  return __aarch64_vdupq_lane_u8 (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vdupq_lane_u16 (uint16x4_t __a, const int __b)
{
  return __aarch64_vdupq_lane_u16 (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vdupq_lane_u32 (uint32x2_t __a, const int __b)
{
  return __aarch64_vdupq_lane_u32 (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vdupq_lane_u64 (uint64x1_t __a, const int __b)
{
  return __aarch64_vdupq_lane_u64 (__a, __b);
}

/* vdupq_laneq  */
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vdupq_laneq_f32 (float32x4_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_f32 (__a, __b);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vdupq_laneq_f64 (float64x2_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_f64 (__a, __b);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vdupq_laneq_p8 (poly8x16_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_p8 (__a, __b);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vdupq_laneq_p16 (poly16x8_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_p16 (__a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vdupq_laneq_s8 (int8x16_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_s8 (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vdupq_laneq_s16 (int16x8_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_s16 (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vdupq_laneq_s32 (int32x4_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_s32 (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vdupq_laneq_s64 (int64x2_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_s64 (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vdupq_laneq_u8 (uint8x16_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_u8 (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vdupq_laneq_u16 (uint16x8_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_u16 (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vdupq_laneq_u32 (uint32x4_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_u32 (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vdupq_laneq_u64 (uint64x2_t __a, const int __b)
{
  return __aarch64_vdupq_laneq_u64 (__a, __b);
}

/* vdupb_lane  */
__extension__ static __inline poly8_t __attribute__ ((__always_inline__))
vdupb_lane_p8 (poly8x8_t __a, const int __b)
{
  return __aarch64_vget_lane_p8 (__a, __b);
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vdupb_lane_s8 (int8x8_t __a, const int __b)
{
  return __aarch64_vget_lane_s8 (__a, __b);
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vdupb_lane_u8 (uint8x8_t __a, const int __b)
{
  return __aarch64_vget_lane_u8 (__a, __b);
}

/* vduph_lane  */
__extension__ static __inline poly16_t __attribute__ ((__always_inline__))
vduph_lane_p16 (poly16x4_t __a, const int __b)
{
  return __aarch64_vget_lane_p16 (__a, __b);
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vduph_lane_s16 (int16x4_t __a, const int __b)
{
  return __aarch64_vget_lane_s16 (__a, __b);
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vduph_lane_u16 (uint16x4_t __a, const int __b)
{
  return __aarch64_vget_lane_u16 (__a, __b);
}

/* vdups_lane  */
__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vdups_lane_f32 (float32x2_t __a, const int __b)
{
  return __aarch64_vget_lane_f32 (__a, __b);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vdups_lane_s32 (int32x2_t __a, const int __b)
{
  return __aarch64_vget_lane_s32 (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vdups_lane_u32 (uint32x2_t __a, const int __b)
{
  return __aarch64_vget_lane_u32 (__a, __b);
}

/* vdupd_lane  */
__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vdupd_lane_f64 (float64x1_t __a, const int __attribute__ ((unused)) __b)
{
  return __a;
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vdupd_lane_s64 (int64x1_t __a, const int __attribute__ ((unused)) __b)
{
  return __a;
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vdupd_lane_u64 (uint64x1_t __a, const int __attribute__ ((unused)) __b)
{
  return __a;
}

/* vdupb_laneq  */
__extension__ static __inline poly8_t __attribute__ ((__always_inline__))
vdupb_laneq_p8 (poly8x16_t __a, const int __b)
{
  return __aarch64_vgetq_lane_p8 (__a, __b);
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vdupb_laneq_s8 (int8x16_t __a, const int __attribute__ ((unused)) __b)
{
  return __aarch64_vgetq_lane_s8 (__a, __b);
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vdupb_laneq_u8 (uint8x16_t __a, const int __b)
{
  return __aarch64_vgetq_lane_u8 (__a, __b);
}

/* vduph_laneq  */
__extension__ static __inline poly16_t __attribute__ ((__always_inline__))
vduph_laneq_p16 (poly16x8_t __a, const int __b)
{
  return __aarch64_vgetq_lane_p16 (__a, __b);
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vduph_laneq_s16 (int16x8_t __a, const int __b)
{
  return __aarch64_vgetq_lane_s16 (__a, __b);
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vduph_laneq_u16 (uint16x8_t __a, const int __b)
{
  return __aarch64_vgetq_lane_u16 (__a, __b);
}

/* vdups_laneq  */
__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vdups_laneq_f32 (float32x4_t __a, const int __b)
{
  return __aarch64_vgetq_lane_f32 (__a, __b);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vdups_laneq_s32 (int32x4_t __a, const int __b)
{
  return __aarch64_vgetq_lane_s32 (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vdups_laneq_u32 (uint32x4_t __a, const int __b)
{
  return __aarch64_vgetq_lane_u32 (__a, __b);
}

/* vdupd_laneq  */
__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vdupd_laneq_f64 (float64x2_t __a, const int __b)
{
  return __aarch64_vgetq_lane_f64 (__a, __b);
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vdupd_laneq_s64 (int64x2_t __a, const int __b)
{
  return __aarch64_vgetq_lane_s64 (__a, __b);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vdupd_laneq_u64 (uint64x2_t __a, const int __b)
{
  return __aarch64_vgetq_lane_u64 (__a, __b);
}

/* vfma_lane  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vfma_lane_f32 (float32x2_t __a, float32x2_t __b,
	       float32x2_t __c, const int __lane)
{
  return __builtin_aarch64_fmav2sf (__b,
				    __aarch64_vdup_lane_f32 (__c, __lane),
				    __a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vfma_lane_f64 (float64_t __a, float64_t __b,
	       float64_t __c, const int __lane)
{
  return __builtin_fma (__b, __c, __a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vfmad_lane_f64 (float64_t __a, float64_t __b,
	        float64_t __c, const int __lane)
{
  return __builtin_fma (__b, __c, __a);
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vfmas_lane_f32 (float32_t __a, float32_t __b,
	        float32x2_t __c, const int __lane)
{
  return __builtin_fmaf (__b, __aarch64_vget_lane_f32 (__c, __lane), __a);
}

/* vfma_laneq  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vfma_laneq_f32 (float32x2_t __a, float32x2_t __b,
	        float32x4_t __c, const int __lane)
{
  return __builtin_aarch64_fmav2sf (__b,
				    __aarch64_vdup_laneq_f32 (__c, __lane),
				    __a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vfma_laneq_f64 (float64_t __a, float64_t __b,
	        float64x2_t __c, const int __lane)
{
  return __builtin_fma (__b, __aarch64_vgetq_lane_f64 (__c, __lane), __a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vfmad_laneq_f64 (float64_t __a, float64_t __b,
	         float64x2_t __c, const int __lane)
{
  return __builtin_fma (__b, __aarch64_vgetq_lane_f64 (__c, __lane), __a);
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vfmas_laneq_f32 (float32_t __a, float32_t __b,
		 float32x4_t __c, const int __lane)
{
  return __builtin_fmaf (__b, __aarch64_vgetq_lane_f32 (__c, __lane), __a);
}

/* vfmaq_lane  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vfmaq_lane_f32 (float32x4_t __a, float32x4_t __b,
	        float32x2_t __c, const int __lane)
{
  return __builtin_aarch64_fmav4sf (__b,
				    __aarch64_vdupq_lane_f32 (__c, __lane),
				    __a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vfmaq_lane_f64 (float64x2_t __a, float64x2_t __b,
	        float64_t __c, const int __lane)
{
  return __builtin_aarch64_fmav2df (__b, vdupq_n_f64 (__c), __a);
}

/* vfmaq_laneq  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vfmaq_laneq_f32 (float32x4_t __a, float32x4_t __b,
	         float32x4_t __c, const int __lane)
{
  return __builtin_aarch64_fmav4sf (__b,
				    __aarch64_vdupq_laneq_f32 (__c, __lane),
				    __a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vfmaq_laneq_f64 (float64x2_t __a, float64x2_t __b,
	         float64x2_t __c, const int __lane)
{
  return __builtin_aarch64_fmav2df (__b,
				    __aarch64_vdupq_laneq_f64 (__c, __lane),
				    __a);
}

/* vfms_lane  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vfms_lane_f32 (float32x2_t __a, float32x2_t __b,
	       float32x2_t __c, const int __lane)
{
  return __builtin_aarch64_fmav2sf (-__b,
				    __aarch64_vdup_lane_f32 (__c, __lane),
				    __a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vfms_lane_f64 (float64_t __a, float64_t __b,
	       float64_t __c, const int __lane)
{
  return __builtin_fma (-__b, __c, __a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vfmsd_lane_f64 (float64_t __a, float64_t __b,
	        float64_t __c, const int __lane)
{
  return __builtin_fma (-__b, __c, __a);
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vfmss_lane_f32 (float32_t __a, float32_t __b,
	        float32x2_t __c, const int __lane)
{
  return __builtin_fmaf (-__b, __aarch64_vget_lane_f32 (__c, __lane), __a);
}

/* vfms_laneq  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vfms_laneq_f32 (float32x2_t __a, float32x2_t __b,
	        float32x4_t __c, const int __lane)
{
  return __builtin_aarch64_fmav2sf (-__b,
				    __aarch64_vdup_laneq_f32 (__c, __lane),
				    __a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vfms_laneq_f64 (float64_t __a, float64_t __b,
	        float64x2_t __c, const int __lane)
{
  return __builtin_fma (-__b, __aarch64_vgetq_lane_f64 (__c, __lane), __a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vfmsd_laneq_f64 (float64_t __a, float64_t __b,
	         float64x2_t __c, const int __lane)
{
  return __builtin_fma (-__b, __aarch64_vgetq_lane_f64 (__c, __lane), __a);
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vfmss_laneq_f32 (float32_t __a, float32_t __b,
		 float32x4_t __c, const int __lane)
{
  return __builtin_fmaf (-__b, __aarch64_vgetq_lane_f32 (__c, __lane), __a);
}

/* vfmsq_lane  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vfmsq_lane_f32 (float32x4_t __a, float32x4_t __b,
	        float32x2_t __c, const int __lane)
{
  return __builtin_aarch64_fmav4sf (-__b,
				    __aarch64_vdupq_lane_f32 (__c, __lane),
				    __a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vfmsq_lane_f64 (float64x2_t __a, float64x2_t __b,
	        float64_t __c, const int __lane)
{
  return __builtin_aarch64_fmav2df (-__b, vdupq_n_f64 (__c), __a);
}

/* vfmsq_laneq  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vfmsq_laneq_f32 (float32x4_t __a, float32x4_t __b,
	         float32x4_t __c, const int __lane)
{
  return __builtin_aarch64_fmav4sf (-__b,
				    __aarch64_vdupq_laneq_f32 (__c, __lane),
				    __a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vfmsq_laneq_f64 (float64x2_t __a, float64x2_t __b,
	         float64x2_t __c, const int __lane)
{
  return __builtin_aarch64_fmav2df (-__b,
				    __aarch64_vdupq_laneq_f64 (__c, __lane),
				    __a);
}

/* vld1 */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vld1_f32 (const float32_t *a)
{
  return __builtin_aarch64_ld1v2sf ((const __builtin_aarch64_simd_sf *) a);
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vld1_f64 (const float64_t *a)
{
  return *a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vld1_p8 (const poly8_t *a)
{
  return (poly8x8_t)
    __builtin_aarch64_ld1v8qi ((const __builtin_aarch64_simd_qi *) a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vld1_p16 (const poly16_t *a)
{
  return (poly16x4_t)
    __builtin_aarch64_ld1v4hi ((const __builtin_aarch64_simd_hi *) a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vld1_s8 (const int8_t *a)
{
  return __builtin_aarch64_ld1v8qi ((const __builtin_aarch64_simd_qi *) a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vld1_s16 (const int16_t *a)
{
  return __builtin_aarch64_ld1v4hi ((const __builtin_aarch64_simd_hi *) a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vld1_s32 (const int32_t *a)
{
  return __builtin_aarch64_ld1v2si ((const __builtin_aarch64_simd_si *) a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vld1_s64 (const int64_t *a)
{
  return *a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vld1_u8 (const uint8_t *a)
{
  return (uint8x8_t)
    __builtin_aarch64_ld1v8qi ((const __builtin_aarch64_simd_qi *) a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vld1_u16 (const uint16_t *a)
{
  return (uint16x4_t)
    __builtin_aarch64_ld1v4hi ((const __builtin_aarch64_simd_hi *) a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vld1_u32 (const uint32_t *a)
{
  return (uint32x2_t)
    __builtin_aarch64_ld1v2si ((const __builtin_aarch64_simd_si *) a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vld1_u64 (const uint64_t *a)
{
  return *a;
}

/* vld1q */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vld1q_f32 (const float32_t *a)
{
  return __builtin_aarch64_ld1v4sf ((const __builtin_aarch64_simd_sf *) a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vld1q_f64 (const float64_t *a)
{
  return __builtin_aarch64_ld1v2df ((const __builtin_aarch64_simd_df *) a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vld1q_p8 (const poly8_t *a)
{
  return (poly8x16_t)
    __builtin_aarch64_ld1v16qi ((const __builtin_aarch64_simd_qi *) a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vld1q_p16 (const poly16_t *a)
{
  return (poly16x8_t)
    __builtin_aarch64_ld1v8hi ((const __builtin_aarch64_simd_hi *) a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vld1q_s8 (const int8_t *a)
{
  return __builtin_aarch64_ld1v16qi ((const __builtin_aarch64_simd_qi *) a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vld1q_s16 (const int16_t *a)
{
  return __builtin_aarch64_ld1v8hi ((const __builtin_aarch64_simd_hi *) a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vld1q_s32 (const int32_t *a)
{
  return __builtin_aarch64_ld1v4si ((const __builtin_aarch64_simd_si *) a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vld1q_s64 (const int64_t *a)
{
  return __builtin_aarch64_ld1v2di ((const __builtin_aarch64_simd_di *) a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vld1q_u8 (const uint8_t *a)
{
  return (uint8x16_t)
    __builtin_aarch64_ld1v16qi ((const __builtin_aarch64_simd_qi *) a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vld1q_u16 (const uint16_t *a)
{
  return (uint16x8_t)
    __builtin_aarch64_ld1v8hi ((const __builtin_aarch64_simd_hi *) a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vld1q_u32 (const uint32_t *a)
{
  return (uint32x4_t)
    __builtin_aarch64_ld1v4si ((const __builtin_aarch64_simd_si *) a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vld1q_u64 (const uint64_t *a)
{
  return (uint64x2_t)
    __builtin_aarch64_ld1v2di ((const __builtin_aarch64_simd_di *) a);
}

/* vldn */

__extension__ static __inline int64x1x2_t __attribute__ ((__always_inline__))
vld2_s64 (const int64_t * __a)
{
  int64x1x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (int64x1_t) __builtin_aarch64_get_dregoidi (__o, 0);
  ret.val[1] = (int64x1_t) __builtin_aarch64_get_dregoidi (__o, 1);
  return ret;
}

__extension__ static __inline uint64x1x2_t __attribute__ ((__always_inline__))
vld2_u64 (const uint64_t * __a)
{
  uint64x1x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (uint64x1_t) __builtin_aarch64_get_dregoidi (__o, 0);
  ret.val[1] = (uint64x1_t) __builtin_aarch64_get_dregoidi (__o, 1);
  return ret;
}

__extension__ static __inline float64x1x2_t __attribute__ ((__always_inline__))
vld2_f64 (const float64_t * __a)
{
  float64x1x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2df ((const __builtin_aarch64_simd_df *) __a);
  ret.val[0] = (float64x1_t) __builtin_aarch64_get_dregoidf (__o, 0);
  ret.val[1] = (float64x1_t) __builtin_aarch64_get_dregoidf (__o, 1);
  return ret;
}

__extension__ static __inline int8x8x2_t __attribute__ ((__always_inline__))
vld2_s8 (const int8_t * __a)
{
  int8x8x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v8qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (int8x8_t) __builtin_aarch64_get_dregoiv8qi (__o, 0);
  ret.val[1] = (int8x8_t) __builtin_aarch64_get_dregoiv8qi (__o, 1);
  return ret;
}

__extension__ static __inline poly8x8x2_t __attribute__ ((__always_inline__))
vld2_p8 (const poly8_t * __a)
{
  poly8x8x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v8qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (poly8x8_t) __builtin_aarch64_get_dregoiv8qi (__o, 0);
  ret.val[1] = (poly8x8_t) __builtin_aarch64_get_dregoiv8qi (__o, 1);
  return ret;
}

__extension__ static __inline int16x4x2_t __attribute__ ((__always_inline__))
vld2_s16 (const int16_t * __a)
{
  int16x4x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v4hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (int16x4_t) __builtin_aarch64_get_dregoiv4hi (__o, 0);
  ret.val[1] = (int16x4_t) __builtin_aarch64_get_dregoiv4hi (__o, 1);
  return ret;
}

__extension__ static __inline poly16x4x2_t __attribute__ ((__always_inline__))
vld2_p16 (const poly16_t * __a)
{
  poly16x4x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v4hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (poly16x4_t) __builtin_aarch64_get_dregoiv4hi (__o, 0);
  ret.val[1] = (poly16x4_t) __builtin_aarch64_get_dregoiv4hi (__o, 1);
  return ret;
}

__extension__ static __inline int32x2x2_t __attribute__ ((__always_inline__))
vld2_s32 (const int32_t * __a)
{
  int32x2x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v2si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (int32x2_t) __builtin_aarch64_get_dregoiv2si (__o, 0);
  ret.val[1] = (int32x2_t) __builtin_aarch64_get_dregoiv2si (__o, 1);
  return ret;
}

__extension__ static __inline uint8x8x2_t __attribute__ ((__always_inline__))
vld2_u8 (const uint8_t * __a)
{
  uint8x8x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v8qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (uint8x8_t) __builtin_aarch64_get_dregoiv8qi (__o, 0);
  ret.val[1] = (uint8x8_t) __builtin_aarch64_get_dregoiv8qi (__o, 1);
  return ret;
}

__extension__ static __inline uint16x4x2_t __attribute__ ((__always_inline__))
vld2_u16 (const uint16_t * __a)
{
  uint16x4x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v4hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (uint16x4_t) __builtin_aarch64_get_dregoiv4hi (__o, 0);
  ret.val[1] = (uint16x4_t) __builtin_aarch64_get_dregoiv4hi (__o, 1);
  return ret;
}

__extension__ static __inline uint32x2x2_t __attribute__ ((__always_inline__))
vld2_u32 (const uint32_t * __a)
{
  uint32x2x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v2si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (uint32x2_t) __builtin_aarch64_get_dregoiv2si (__o, 0);
  ret.val[1] = (uint32x2_t) __builtin_aarch64_get_dregoiv2si (__o, 1);
  return ret;
}

__extension__ static __inline float32x2x2_t __attribute__ ((__always_inline__))
vld2_f32 (const float32_t * __a)
{
  float32x2x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v2sf ((const __builtin_aarch64_simd_sf *) __a);
  ret.val[0] = (float32x2_t) __builtin_aarch64_get_dregoiv2sf (__o, 0);
  ret.val[1] = (float32x2_t) __builtin_aarch64_get_dregoiv2sf (__o, 1);
  return ret;
}

__extension__ static __inline int8x16x2_t __attribute__ ((__always_inline__))
vld2q_s8 (const int8_t * __a)
{
  int8x16x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v16qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (int8x16_t) __builtin_aarch64_get_qregoiv16qi (__o, 0);
  ret.val[1] = (int8x16_t) __builtin_aarch64_get_qregoiv16qi (__o, 1);
  return ret;
}

__extension__ static __inline poly8x16x2_t __attribute__ ((__always_inline__))
vld2q_p8 (const poly8_t * __a)
{
  poly8x16x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v16qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (poly8x16_t) __builtin_aarch64_get_qregoiv16qi (__o, 0);
  ret.val[1] = (poly8x16_t) __builtin_aarch64_get_qregoiv16qi (__o, 1);
  return ret;
}

__extension__ static __inline int16x8x2_t __attribute__ ((__always_inline__))
vld2q_s16 (const int16_t * __a)
{
  int16x8x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v8hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (int16x8_t) __builtin_aarch64_get_qregoiv8hi (__o, 0);
  ret.val[1] = (int16x8_t) __builtin_aarch64_get_qregoiv8hi (__o, 1);
  return ret;
}

__extension__ static __inline poly16x8x2_t __attribute__ ((__always_inline__))
vld2q_p16 (const poly16_t * __a)
{
  poly16x8x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v8hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (poly16x8_t) __builtin_aarch64_get_qregoiv8hi (__o, 0);
  ret.val[1] = (poly16x8_t) __builtin_aarch64_get_qregoiv8hi (__o, 1);
  return ret;
}

__extension__ static __inline int32x4x2_t __attribute__ ((__always_inline__))
vld2q_s32 (const int32_t * __a)
{
  int32x4x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v4si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (int32x4_t) __builtin_aarch64_get_qregoiv4si (__o, 0);
  ret.val[1] = (int32x4_t) __builtin_aarch64_get_qregoiv4si (__o, 1);
  return ret;
}

__extension__ static __inline int64x2x2_t __attribute__ ((__always_inline__))
vld2q_s64 (const int64_t * __a)
{
  int64x2x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v2di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (int64x2_t) __builtin_aarch64_get_qregoiv2di (__o, 0);
  ret.val[1] = (int64x2_t) __builtin_aarch64_get_qregoiv2di (__o, 1);
  return ret;
}

__extension__ static __inline uint8x16x2_t __attribute__ ((__always_inline__))
vld2q_u8 (const uint8_t * __a)
{
  uint8x16x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v16qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (uint8x16_t) __builtin_aarch64_get_qregoiv16qi (__o, 0);
  ret.val[1] = (uint8x16_t) __builtin_aarch64_get_qregoiv16qi (__o, 1);
  return ret;
}

__extension__ static __inline uint16x8x2_t __attribute__ ((__always_inline__))
vld2q_u16 (const uint16_t * __a)
{
  uint16x8x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v8hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (uint16x8_t) __builtin_aarch64_get_qregoiv8hi (__o, 0);
  ret.val[1] = (uint16x8_t) __builtin_aarch64_get_qregoiv8hi (__o, 1);
  return ret;
}

__extension__ static __inline uint32x4x2_t __attribute__ ((__always_inline__))
vld2q_u32 (const uint32_t * __a)
{
  uint32x4x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v4si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (uint32x4_t) __builtin_aarch64_get_qregoiv4si (__o, 0);
  ret.val[1] = (uint32x4_t) __builtin_aarch64_get_qregoiv4si (__o, 1);
  return ret;
}

__extension__ static __inline uint64x2x2_t __attribute__ ((__always_inline__))
vld2q_u64 (const uint64_t * __a)
{
  uint64x2x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v2di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (uint64x2_t) __builtin_aarch64_get_qregoiv2di (__o, 0);
  ret.val[1] = (uint64x2_t) __builtin_aarch64_get_qregoiv2di (__o, 1);
  return ret;
}

__extension__ static __inline float32x4x2_t __attribute__ ((__always_inline__))
vld2q_f32 (const float32_t * __a)
{
  float32x4x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v4sf ((const __builtin_aarch64_simd_sf *) __a);
  ret.val[0] = (float32x4_t) __builtin_aarch64_get_qregoiv4sf (__o, 0);
  ret.val[1] = (float32x4_t) __builtin_aarch64_get_qregoiv4sf (__o, 1);
  return ret;
}

__extension__ static __inline float64x2x2_t __attribute__ ((__always_inline__))
vld2q_f64 (const float64_t * __a)
{
  float64x2x2_t ret;
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_ld2v2df ((const __builtin_aarch64_simd_df *) __a);
  ret.val[0] = (float64x2_t) __builtin_aarch64_get_qregoiv2df (__o, 0);
  ret.val[1] = (float64x2_t) __builtin_aarch64_get_qregoiv2df (__o, 1);
  return ret;
}

__extension__ static __inline int64x1x3_t __attribute__ ((__always_inline__))
vld3_s64 (const int64_t * __a)
{
  int64x1x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (int64x1_t) __builtin_aarch64_get_dregcidi (__o, 0);
  ret.val[1] = (int64x1_t) __builtin_aarch64_get_dregcidi (__o, 1);
  ret.val[2] = (int64x1_t) __builtin_aarch64_get_dregcidi (__o, 2);
  return ret;
}

__extension__ static __inline uint64x1x3_t __attribute__ ((__always_inline__))
vld3_u64 (const uint64_t * __a)
{
  uint64x1x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (uint64x1_t) __builtin_aarch64_get_dregcidi (__o, 0);
  ret.val[1] = (uint64x1_t) __builtin_aarch64_get_dregcidi (__o, 1);
  ret.val[2] = (uint64x1_t) __builtin_aarch64_get_dregcidi (__o, 2);
  return ret;
}

__extension__ static __inline float64x1x3_t __attribute__ ((__always_inline__))
vld3_f64 (const float64_t * __a)
{
  float64x1x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3df ((const __builtin_aarch64_simd_df *) __a);
  ret.val[0] = (float64x1_t) __builtin_aarch64_get_dregcidf (__o, 0);
  ret.val[1] = (float64x1_t) __builtin_aarch64_get_dregcidf (__o, 1);
  ret.val[2] = (float64x1_t) __builtin_aarch64_get_dregcidf (__o, 2);
  return ret;
}

__extension__ static __inline int8x8x3_t __attribute__ ((__always_inline__))
vld3_s8 (const int8_t * __a)
{
  int8x8x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v8qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (int8x8_t) __builtin_aarch64_get_dregciv8qi (__o, 0);
  ret.val[1] = (int8x8_t) __builtin_aarch64_get_dregciv8qi (__o, 1);
  ret.val[2] = (int8x8_t) __builtin_aarch64_get_dregciv8qi (__o, 2);
  return ret;
}

__extension__ static __inline poly8x8x3_t __attribute__ ((__always_inline__))
vld3_p8 (const poly8_t * __a)
{
  poly8x8x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v8qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (poly8x8_t) __builtin_aarch64_get_dregciv8qi (__o, 0);
  ret.val[1] = (poly8x8_t) __builtin_aarch64_get_dregciv8qi (__o, 1);
  ret.val[2] = (poly8x8_t) __builtin_aarch64_get_dregciv8qi (__o, 2);
  return ret;
}

__extension__ static __inline int16x4x3_t __attribute__ ((__always_inline__))
vld3_s16 (const int16_t * __a)
{
  int16x4x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v4hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (int16x4_t) __builtin_aarch64_get_dregciv4hi (__o, 0);
  ret.val[1] = (int16x4_t) __builtin_aarch64_get_dregciv4hi (__o, 1);
  ret.val[2] = (int16x4_t) __builtin_aarch64_get_dregciv4hi (__o, 2);
  return ret;
}

__extension__ static __inline poly16x4x3_t __attribute__ ((__always_inline__))
vld3_p16 (const poly16_t * __a)
{
  poly16x4x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v4hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (poly16x4_t) __builtin_aarch64_get_dregciv4hi (__o, 0);
  ret.val[1] = (poly16x4_t) __builtin_aarch64_get_dregciv4hi (__o, 1);
  ret.val[2] = (poly16x4_t) __builtin_aarch64_get_dregciv4hi (__o, 2);
  return ret;
}

__extension__ static __inline int32x2x3_t __attribute__ ((__always_inline__))
vld3_s32 (const int32_t * __a)
{
  int32x2x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v2si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (int32x2_t) __builtin_aarch64_get_dregciv2si (__o, 0);
  ret.val[1] = (int32x2_t) __builtin_aarch64_get_dregciv2si (__o, 1);
  ret.val[2] = (int32x2_t) __builtin_aarch64_get_dregciv2si (__o, 2);
  return ret;
}

__extension__ static __inline uint8x8x3_t __attribute__ ((__always_inline__))
vld3_u8 (const uint8_t * __a)
{
  uint8x8x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v8qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (uint8x8_t) __builtin_aarch64_get_dregciv8qi (__o, 0);
  ret.val[1] = (uint8x8_t) __builtin_aarch64_get_dregciv8qi (__o, 1);
  ret.val[2] = (uint8x8_t) __builtin_aarch64_get_dregciv8qi (__o, 2);
  return ret;
}

__extension__ static __inline uint16x4x3_t __attribute__ ((__always_inline__))
vld3_u16 (const uint16_t * __a)
{
  uint16x4x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v4hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (uint16x4_t) __builtin_aarch64_get_dregciv4hi (__o, 0);
  ret.val[1] = (uint16x4_t) __builtin_aarch64_get_dregciv4hi (__o, 1);
  ret.val[2] = (uint16x4_t) __builtin_aarch64_get_dregciv4hi (__o, 2);
  return ret;
}

__extension__ static __inline uint32x2x3_t __attribute__ ((__always_inline__))
vld3_u32 (const uint32_t * __a)
{
  uint32x2x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v2si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (uint32x2_t) __builtin_aarch64_get_dregciv2si (__o, 0);
  ret.val[1] = (uint32x2_t) __builtin_aarch64_get_dregciv2si (__o, 1);
  ret.val[2] = (uint32x2_t) __builtin_aarch64_get_dregciv2si (__o, 2);
  return ret;
}

__extension__ static __inline float32x2x3_t __attribute__ ((__always_inline__))
vld3_f32 (const float32_t * __a)
{
  float32x2x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v2sf ((const __builtin_aarch64_simd_sf *) __a);
  ret.val[0] = (float32x2_t) __builtin_aarch64_get_dregciv2sf (__o, 0);
  ret.val[1] = (float32x2_t) __builtin_aarch64_get_dregciv2sf (__o, 1);
  ret.val[2] = (float32x2_t) __builtin_aarch64_get_dregciv2sf (__o, 2);
  return ret;
}

__extension__ static __inline int8x16x3_t __attribute__ ((__always_inline__))
vld3q_s8 (const int8_t * __a)
{
  int8x16x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v16qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (int8x16_t) __builtin_aarch64_get_qregciv16qi (__o, 0);
  ret.val[1] = (int8x16_t) __builtin_aarch64_get_qregciv16qi (__o, 1);
  ret.val[2] = (int8x16_t) __builtin_aarch64_get_qregciv16qi (__o, 2);
  return ret;
}

__extension__ static __inline poly8x16x3_t __attribute__ ((__always_inline__))
vld3q_p8 (const poly8_t * __a)
{
  poly8x16x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v16qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (poly8x16_t) __builtin_aarch64_get_qregciv16qi (__o, 0);
  ret.val[1] = (poly8x16_t) __builtin_aarch64_get_qregciv16qi (__o, 1);
  ret.val[2] = (poly8x16_t) __builtin_aarch64_get_qregciv16qi (__o, 2);
  return ret;
}

__extension__ static __inline int16x8x3_t __attribute__ ((__always_inline__))
vld3q_s16 (const int16_t * __a)
{
  int16x8x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v8hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (int16x8_t) __builtin_aarch64_get_qregciv8hi (__o, 0);
  ret.val[1] = (int16x8_t) __builtin_aarch64_get_qregciv8hi (__o, 1);
  ret.val[2] = (int16x8_t) __builtin_aarch64_get_qregciv8hi (__o, 2);
  return ret;
}

__extension__ static __inline poly16x8x3_t __attribute__ ((__always_inline__))
vld3q_p16 (const poly16_t * __a)
{
  poly16x8x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v8hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (poly16x8_t) __builtin_aarch64_get_qregciv8hi (__o, 0);
  ret.val[1] = (poly16x8_t) __builtin_aarch64_get_qregciv8hi (__o, 1);
  ret.val[2] = (poly16x8_t) __builtin_aarch64_get_qregciv8hi (__o, 2);
  return ret;
}

__extension__ static __inline int32x4x3_t __attribute__ ((__always_inline__))
vld3q_s32 (const int32_t * __a)
{
  int32x4x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v4si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (int32x4_t) __builtin_aarch64_get_qregciv4si (__o, 0);
  ret.val[1] = (int32x4_t) __builtin_aarch64_get_qregciv4si (__o, 1);
  ret.val[2] = (int32x4_t) __builtin_aarch64_get_qregciv4si (__o, 2);
  return ret;
}

__extension__ static __inline int64x2x3_t __attribute__ ((__always_inline__))
vld3q_s64 (const int64_t * __a)
{
  int64x2x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v2di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (int64x2_t) __builtin_aarch64_get_qregciv2di (__o, 0);
  ret.val[1] = (int64x2_t) __builtin_aarch64_get_qregciv2di (__o, 1);
  ret.val[2] = (int64x2_t) __builtin_aarch64_get_qregciv2di (__o, 2);
  return ret;
}

__extension__ static __inline uint8x16x3_t __attribute__ ((__always_inline__))
vld3q_u8 (const uint8_t * __a)
{
  uint8x16x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v16qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (uint8x16_t) __builtin_aarch64_get_qregciv16qi (__o, 0);
  ret.val[1] = (uint8x16_t) __builtin_aarch64_get_qregciv16qi (__o, 1);
  ret.val[2] = (uint8x16_t) __builtin_aarch64_get_qregciv16qi (__o, 2);
  return ret;
}

__extension__ static __inline uint16x8x3_t __attribute__ ((__always_inline__))
vld3q_u16 (const uint16_t * __a)
{
  uint16x8x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v8hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (uint16x8_t) __builtin_aarch64_get_qregciv8hi (__o, 0);
  ret.val[1] = (uint16x8_t) __builtin_aarch64_get_qregciv8hi (__o, 1);
  ret.val[2] = (uint16x8_t) __builtin_aarch64_get_qregciv8hi (__o, 2);
  return ret;
}

__extension__ static __inline uint32x4x3_t __attribute__ ((__always_inline__))
vld3q_u32 (const uint32_t * __a)
{
  uint32x4x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v4si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (uint32x4_t) __builtin_aarch64_get_qregciv4si (__o, 0);
  ret.val[1] = (uint32x4_t) __builtin_aarch64_get_qregciv4si (__o, 1);
  ret.val[2] = (uint32x4_t) __builtin_aarch64_get_qregciv4si (__o, 2);
  return ret;
}

__extension__ static __inline uint64x2x3_t __attribute__ ((__always_inline__))
vld3q_u64 (const uint64_t * __a)
{
  uint64x2x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v2di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (uint64x2_t) __builtin_aarch64_get_qregciv2di (__o, 0);
  ret.val[1] = (uint64x2_t) __builtin_aarch64_get_qregciv2di (__o, 1);
  ret.val[2] = (uint64x2_t) __builtin_aarch64_get_qregciv2di (__o, 2);
  return ret;
}

__extension__ static __inline float32x4x3_t __attribute__ ((__always_inline__))
vld3q_f32 (const float32_t * __a)
{
  float32x4x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v4sf ((const __builtin_aarch64_simd_sf *) __a);
  ret.val[0] = (float32x4_t) __builtin_aarch64_get_qregciv4sf (__o, 0);
  ret.val[1] = (float32x4_t) __builtin_aarch64_get_qregciv4sf (__o, 1);
  ret.val[2] = (float32x4_t) __builtin_aarch64_get_qregciv4sf (__o, 2);
  return ret;
}

__extension__ static __inline float64x2x3_t __attribute__ ((__always_inline__))
vld3q_f64 (const float64_t * __a)
{
  float64x2x3_t ret;
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_ld3v2df ((const __builtin_aarch64_simd_df *) __a);
  ret.val[0] = (float64x2_t) __builtin_aarch64_get_qregciv2df (__o, 0);
  ret.val[1] = (float64x2_t) __builtin_aarch64_get_qregciv2df (__o, 1);
  ret.val[2] = (float64x2_t) __builtin_aarch64_get_qregciv2df (__o, 2);
  return ret;
}

__extension__ static __inline int64x1x4_t __attribute__ ((__always_inline__))
vld4_s64 (const int64_t * __a)
{
  int64x1x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (int64x1_t) __builtin_aarch64_get_dregxidi (__o, 0);
  ret.val[1] = (int64x1_t) __builtin_aarch64_get_dregxidi (__o, 1);
  ret.val[2] = (int64x1_t) __builtin_aarch64_get_dregxidi (__o, 2);
  ret.val[3] = (int64x1_t) __builtin_aarch64_get_dregxidi (__o, 3);
  return ret;
}

__extension__ static __inline uint64x1x4_t __attribute__ ((__always_inline__))
vld4_u64 (const uint64_t * __a)
{
  uint64x1x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (uint64x1_t) __builtin_aarch64_get_dregxidi (__o, 0);
  ret.val[1] = (uint64x1_t) __builtin_aarch64_get_dregxidi (__o, 1);
  ret.val[2] = (uint64x1_t) __builtin_aarch64_get_dregxidi (__o, 2);
  ret.val[3] = (uint64x1_t) __builtin_aarch64_get_dregxidi (__o, 3);
  return ret;
}

__extension__ static __inline float64x1x4_t __attribute__ ((__always_inline__))
vld4_f64 (const float64_t * __a)
{
  float64x1x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4df ((const __builtin_aarch64_simd_df *) __a);
  ret.val[0] = (float64x1_t) __builtin_aarch64_get_dregxidf (__o, 0);
  ret.val[1] = (float64x1_t) __builtin_aarch64_get_dregxidf (__o, 1);
  ret.val[2] = (float64x1_t) __builtin_aarch64_get_dregxidf (__o, 2);
  ret.val[3] = (float64x1_t) __builtin_aarch64_get_dregxidf (__o, 3);
  return ret;
}

__extension__ static __inline int8x8x4_t __attribute__ ((__always_inline__))
vld4_s8 (const int8_t * __a)
{
  int8x8x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v8qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (int8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 0);
  ret.val[1] = (int8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 1);
  ret.val[2] = (int8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 2);
  ret.val[3] = (int8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 3);
  return ret;
}

__extension__ static __inline poly8x8x4_t __attribute__ ((__always_inline__))
vld4_p8 (const poly8_t * __a)
{
  poly8x8x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v8qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (poly8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 0);
  ret.val[1] = (poly8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 1);
  ret.val[2] = (poly8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 2);
  ret.val[3] = (poly8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 3);
  return ret;
}

__extension__ static __inline int16x4x4_t __attribute__ ((__always_inline__))
vld4_s16 (const int16_t * __a)
{
  int16x4x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v4hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (int16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 0);
  ret.val[1] = (int16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 1);
  ret.val[2] = (int16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 2);
  ret.val[3] = (int16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 3);
  return ret;
}

__extension__ static __inline poly16x4x4_t __attribute__ ((__always_inline__))
vld4_p16 (const poly16_t * __a)
{
  poly16x4x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v4hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (poly16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 0);
  ret.val[1] = (poly16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 1);
  ret.val[2] = (poly16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 2);
  ret.val[3] = (poly16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 3);
  return ret;
}

__extension__ static __inline int32x2x4_t __attribute__ ((__always_inline__))
vld4_s32 (const int32_t * __a)
{
  int32x2x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v2si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (int32x2_t) __builtin_aarch64_get_dregxiv2si (__o, 0);
  ret.val[1] = (int32x2_t) __builtin_aarch64_get_dregxiv2si (__o, 1);
  ret.val[2] = (int32x2_t) __builtin_aarch64_get_dregxiv2si (__o, 2);
  ret.val[3] = (int32x2_t) __builtin_aarch64_get_dregxiv2si (__o, 3);
  return ret;
}

__extension__ static __inline uint8x8x4_t __attribute__ ((__always_inline__))
vld4_u8 (const uint8_t * __a)
{
  uint8x8x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v8qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (uint8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 0);
  ret.val[1] = (uint8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 1);
  ret.val[2] = (uint8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 2);
  ret.val[3] = (uint8x8_t) __builtin_aarch64_get_dregxiv8qi (__o, 3);
  return ret;
}

__extension__ static __inline uint16x4x4_t __attribute__ ((__always_inline__))
vld4_u16 (const uint16_t * __a)
{
  uint16x4x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v4hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (uint16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 0);
  ret.val[1] = (uint16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 1);
  ret.val[2] = (uint16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 2);
  ret.val[3] = (uint16x4_t) __builtin_aarch64_get_dregxiv4hi (__o, 3);
  return ret;
}

__extension__ static __inline uint32x2x4_t __attribute__ ((__always_inline__))
vld4_u32 (const uint32_t * __a)
{
  uint32x2x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v2si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (uint32x2_t) __builtin_aarch64_get_dregxiv2si (__o, 0);
  ret.val[1] = (uint32x2_t) __builtin_aarch64_get_dregxiv2si (__o, 1);
  ret.val[2] = (uint32x2_t) __builtin_aarch64_get_dregxiv2si (__o, 2);
  ret.val[3] = (uint32x2_t) __builtin_aarch64_get_dregxiv2si (__o, 3);
  return ret;
}

__extension__ static __inline float32x2x4_t __attribute__ ((__always_inline__))
vld4_f32 (const float32_t * __a)
{
  float32x2x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v2sf ((const __builtin_aarch64_simd_sf *) __a);
  ret.val[0] = (float32x2_t) __builtin_aarch64_get_dregxiv2sf (__o, 0);
  ret.val[1] = (float32x2_t) __builtin_aarch64_get_dregxiv2sf (__o, 1);
  ret.val[2] = (float32x2_t) __builtin_aarch64_get_dregxiv2sf (__o, 2);
  ret.val[3] = (float32x2_t) __builtin_aarch64_get_dregxiv2sf (__o, 3);
  return ret;
}

__extension__ static __inline int8x16x4_t __attribute__ ((__always_inline__))
vld4q_s8 (const int8_t * __a)
{
  int8x16x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v16qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (int8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 0);
  ret.val[1] = (int8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 1);
  ret.val[2] = (int8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 2);
  ret.val[3] = (int8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 3);
  return ret;
}

__extension__ static __inline poly8x16x4_t __attribute__ ((__always_inline__))
vld4q_p8 (const poly8_t * __a)
{
  poly8x16x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v16qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (poly8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 0);
  ret.val[1] = (poly8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 1);
  ret.val[2] = (poly8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 2);
  ret.val[3] = (poly8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 3);
  return ret;
}

__extension__ static __inline int16x8x4_t __attribute__ ((__always_inline__))
vld4q_s16 (const int16_t * __a)
{
  int16x8x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v8hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (int16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 0);
  ret.val[1] = (int16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 1);
  ret.val[2] = (int16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 2);
  ret.val[3] = (int16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 3);
  return ret;
}

__extension__ static __inline poly16x8x4_t __attribute__ ((__always_inline__))
vld4q_p16 (const poly16_t * __a)
{
  poly16x8x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v8hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (poly16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 0);
  ret.val[1] = (poly16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 1);
  ret.val[2] = (poly16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 2);
  ret.val[3] = (poly16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 3);
  return ret;
}

__extension__ static __inline int32x4x4_t __attribute__ ((__always_inline__))
vld4q_s32 (const int32_t * __a)
{
  int32x4x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v4si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (int32x4_t) __builtin_aarch64_get_qregxiv4si (__o, 0);
  ret.val[1] = (int32x4_t) __builtin_aarch64_get_qregxiv4si (__o, 1);
  ret.val[2] = (int32x4_t) __builtin_aarch64_get_qregxiv4si (__o, 2);
  ret.val[3] = (int32x4_t) __builtin_aarch64_get_qregxiv4si (__o, 3);
  return ret;
}

__extension__ static __inline int64x2x4_t __attribute__ ((__always_inline__))
vld4q_s64 (const int64_t * __a)
{
  int64x2x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v2di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (int64x2_t) __builtin_aarch64_get_qregxiv2di (__o, 0);
  ret.val[1] = (int64x2_t) __builtin_aarch64_get_qregxiv2di (__o, 1);
  ret.val[2] = (int64x2_t) __builtin_aarch64_get_qregxiv2di (__o, 2);
  ret.val[3] = (int64x2_t) __builtin_aarch64_get_qregxiv2di (__o, 3);
  return ret;
}

__extension__ static __inline uint8x16x4_t __attribute__ ((__always_inline__))
vld4q_u8 (const uint8_t * __a)
{
  uint8x16x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v16qi ((const __builtin_aarch64_simd_qi *) __a);
  ret.val[0] = (uint8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 0);
  ret.val[1] = (uint8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 1);
  ret.val[2] = (uint8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 2);
  ret.val[3] = (uint8x16_t) __builtin_aarch64_get_qregxiv16qi (__o, 3);
  return ret;
}

__extension__ static __inline uint16x8x4_t __attribute__ ((__always_inline__))
vld4q_u16 (const uint16_t * __a)
{
  uint16x8x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v8hi ((const __builtin_aarch64_simd_hi *) __a);
  ret.val[0] = (uint16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 0);
  ret.val[1] = (uint16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 1);
  ret.val[2] = (uint16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 2);
  ret.val[3] = (uint16x8_t) __builtin_aarch64_get_qregxiv8hi (__o, 3);
  return ret;
}

__extension__ static __inline uint32x4x4_t __attribute__ ((__always_inline__))
vld4q_u32 (const uint32_t * __a)
{
  uint32x4x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v4si ((const __builtin_aarch64_simd_si *) __a);
  ret.val[0] = (uint32x4_t) __builtin_aarch64_get_qregxiv4si (__o, 0);
  ret.val[1] = (uint32x4_t) __builtin_aarch64_get_qregxiv4si (__o, 1);
  ret.val[2] = (uint32x4_t) __builtin_aarch64_get_qregxiv4si (__o, 2);
  ret.val[3] = (uint32x4_t) __builtin_aarch64_get_qregxiv4si (__o, 3);
  return ret;
}

__extension__ static __inline uint64x2x4_t __attribute__ ((__always_inline__))
vld4q_u64 (const uint64_t * __a)
{
  uint64x2x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v2di ((const __builtin_aarch64_simd_di *) __a);
  ret.val[0] = (uint64x2_t) __builtin_aarch64_get_qregxiv2di (__o, 0);
  ret.val[1] = (uint64x2_t) __builtin_aarch64_get_qregxiv2di (__o, 1);
  ret.val[2] = (uint64x2_t) __builtin_aarch64_get_qregxiv2di (__o, 2);
  ret.val[3] = (uint64x2_t) __builtin_aarch64_get_qregxiv2di (__o, 3);
  return ret;
}

__extension__ static __inline float32x4x4_t __attribute__ ((__always_inline__))
vld4q_f32 (const float32_t * __a)
{
  float32x4x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v4sf ((const __builtin_aarch64_simd_sf *) __a);
  ret.val[0] = (float32x4_t) __builtin_aarch64_get_qregxiv4sf (__o, 0);
  ret.val[1] = (float32x4_t) __builtin_aarch64_get_qregxiv4sf (__o, 1);
  ret.val[2] = (float32x4_t) __builtin_aarch64_get_qregxiv4sf (__o, 2);
  ret.val[3] = (float32x4_t) __builtin_aarch64_get_qregxiv4sf (__o, 3);
  return ret;
}

__extension__ static __inline float64x2x4_t __attribute__ ((__always_inline__))
vld4q_f64 (const float64_t * __a)
{
  float64x2x4_t ret;
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_ld4v2df ((const __builtin_aarch64_simd_df *) __a);
  ret.val[0] = (float64x2_t) __builtin_aarch64_get_qregxiv2df (__o, 0);
  ret.val[1] = (float64x2_t) __builtin_aarch64_get_qregxiv2df (__o, 1);
  ret.val[2] = (float64x2_t) __builtin_aarch64_get_qregxiv2df (__o, 2);
  ret.val[3] = (float64x2_t) __builtin_aarch64_get_qregxiv2df (__o, 3);
  return ret;
}

/* vmax */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmax_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_smax_nanv2sf (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmax_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_smaxv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmax_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_smaxv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmax_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_smaxv2si (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmax_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_umaxv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmax_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_umaxv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmax_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_umaxv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmaxq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_smax_nanv4sf (__a, __b);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmaxq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_smax_nanv2df (__a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmaxq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_smaxv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmaxq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_smaxv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmaxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_smaxv4si (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmaxq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_umaxv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmaxq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_umaxv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmaxq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_umaxv4si ((int32x4_t) __a,
						  (int32x4_t) __b);
}

/* vmaxnm  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmaxnm_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_smaxv2sf (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmaxnmq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_smaxv4sf (__a, __b);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmaxnmq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_smaxv2df (__a, __b);
}

/* vmaxv  */

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vmaxv_f32 (float32x2_t __a)
{
  return vget_lane_f32 (__builtin_aarch64_reduc_smax_nan_v2sf (__a),
			__LANE0 (2));
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vmaxv_s8 (int8x8_t __a)
{
  return vget_lane_s8 (__builtin_aarch64_reduc_smax_v8qi (__a), __LANE0 (8));
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vmaxv_s16 (int16x4_t __a)
{
  return vget_lane_s16 (__builtin_aarch64_reduc_smax_v4hi (__a), __LANE0 (4));
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vmaxv_s32 (int32x2_t __a)
{
  return vget_lane_s32 (__builtin_aarch64_reduc_smax_v2si (__a), __LANE0 (2));
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vmaxv_u8 (uint8x8_t __a)
{
  return vget_lane_u8 ((uint8x8_t)
		__builtin_aarch64_reduc_umax_v8qi ((int8x8_t) __a),
		__LANE0 (8));
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vmaxv_u16 (uint16x4_t __a)
{
  return vget_lane_u16 ((uint16x4_t)
		__builtin_aarch64_reduc_umax_v4hi ((int16x4_t) __a),
		__LANE0 (4));
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vmaxv_u32 (uint32x2_t __a)
{
  return vget_lane_u32 ((uint32x2_t)
		__builtin_aarch64_reduc_umax_v2si ((int32x2_t) __a),
		__LANE0 (2));
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vmaxvq_f32 (float32x4_t __a)
{
  return vgetq_lane_f32 (__builtin_aarch64_reduc_smax_nan_v4sf (__a),
			 __LANE0 (4));
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vmaxvq_f64 (float64x2_t __a)
{
  return vgetq_lane_f64 (__builtin_aarch64_reduc_smax_nan_v2df (__a),
			 __LANE0 (2));
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vmaxvq_s8 (int8x16_t __a)
{
  return vgetq_lane_s8 (__builtin_aarch64_reduc_smax_v16qi (__a), __LANE0 (16));
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vmaxvq_s16 (int16x8_t __a)
{
  return vgetq_lane_s16 (__builtin_aarch64_reduc_smax_v8hi (__a), __LANE0 (8));
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vmaxvq_s32 (int32x4_t __a)
{
  return vgetq_lane_s32 (__builtin_aarch64_reduc_smax_v4si (__a), __LANE0 (4));
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vmaxvq_u8 (uint8x16_t __a)
{
  return vgetq_lane_u8 ((uint8x16_t)
		__builtin_aarch64_reduc_umax_v16qi ((int8x16_t) __a),
		__LANE0 (16));
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vmaxvq_u16 (uint16x8_t __a)
{
  return vgetq_lane_u16 ((uint16x8_t)
		__builtin_aarch64_reduc_umax_v8hi ((int16x8_t) __a),
		__LANE0 (8));
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vmaxvq_u32 (uint32x4_t __a)
{
  return vgetq_lane_u32 ((uint32x4_t)
		__builtin_aarch64_reduc_umax_v4si ((int32x4_t) __a),
		__LANE0 (4));
}

/* vmaxnmv  */

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vmaxnmv_f32 (float32x2_t __a)
{
  return vget_lane_f32 (__builtin_aarch64_reduc_smax_v2sf (__a),
			__LANE0 (2));
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vmaxnmvq_f32 (float32x4_t __a)
{
  return vgetq_lane_f32 (__builtin_aarch64_reduc_smax_v4sf (__a), __LANE0 (4));
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vmaxnmvq_f64 (float64x2_t __a)
{
  return vgetq_lane_f64 (__builtin_aarch64_reduc_smax_v2df (__a), __LANE0 (2));
}

/* vmin  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmin_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_smin_nanv2sf (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmin_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_sminv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmin_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_sminv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmin_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_sminv2si (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmin_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_uminv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmin_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_uminv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmin_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_uminv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vminq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_smin_nanv4sf (__a, __b);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vminq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_smin_nanv2df (__a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vminq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_sminv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vminq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sminv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vminq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sminv4si (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vminq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_uminv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vminq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uminv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vminq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uminv4si ((int32x4_t) __a,
						  (int32x4_t) __b);
}

/* vminnm  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vminnm_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_sminv2sf (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vminnmq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_sminv4sf (__a, __b);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vminnmq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_sminv2df (__a, __b);
}

/* vminv  */

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vminv_f32 (float32x2_t __a)
{
  return vget_lane_f32 (__builtin_aarch64_reduc_smin_nan_v2sf (__a),
			__LANE0 (2));
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vminv_s8 (int8x8_t __a)
{
  return vget_lane_s8 (__builtin_aarch64_reduc_smin_v8qi (__a),
		       __LANE0 (8));
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vminv_s16 (int16x4_t __a)
{
  return vget_lane_s16 (__builtin_aarch64_reduc_smin_v4hi (__a), __LANE0 (4));
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vminv_s32 (int32x2_t __a)
{
  return vget_lane_s32 (__builtin_aarch64_reduc_smin_v2si (__a), __LANE0 (2));
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vminv_u8 (uint8x8_t __a)
{
  return vget_lane_u8 ((uint8x8_t)
		__builtin_aarch64_reduc_umin_v8qi ((int8x8_t) __a),
		__LANE0 (8));
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vminv_u16 (uint16x4_t __a)
{
  return vget_lane_u16 ((uint16x4_t)
		__builtin_aarch64_reduc_umin_v4hi ((int16x4_t) __a),
		__LANE0 (4));
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vminv_u32 (uint32x2_t __a)
{
  return vget_lane_u32 ((uint32x2_t)
		__builtin_aarch64_reduc_umin_v2si ((int32x2_t) __a),
		__LANE0 (2));
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vminvq_f32 (float32x4_t __a)
{
  return vgetq_lane_f32 (__builtin_aarch64_reduc_smin_nan_v4sf (__a),
			 __LANE0 (4));
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vminvq_f64 (float64x2_t __a)
{
  return vgetq_lane_f64 (__builtin_aarch64_reduc_smin_nan_v2df (__a),
			 __LANE0 (2));
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vminvq_s8 (int8x16_t __a)
{
  return vgetq_lane_s8 (__builtin_aarch64_reduc_smin_v16qi (__a), __LANE0 (16));
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vminvq_s16 (int16x8_t __a)
{
  return vgetq_lane_s16 (__builtin_aarch64_reduc_smin_v8hi (__a), __LANE0 (8));
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vminvq_s32 (int32x4_t __a)
{
  return vgetq_lane_s32 (__builtin_aarch64_reduc_smin_v4si (__a), __LANE0 (4));
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vminvq_u8 (uint8x16_t __a)
{
  return vgetq_lane_u8 ((uint8x16_t)
		__builtin_aarch64_reduc_umin_v16qi ((int8x16_t) __a),
		__LANE0 (16));
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vminvq_u16 (uint16x8_t __a)
{
  return vgetq_lane_u16 ((uint16x8_t)
		__builtin_aarch64_reduc_umin_v8hi ((int16x8_t) __a),
		__LANE0 (8));
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vminvq_u32 (uint32x4_t __a)
{
  return vgetq_lane_u32 ((uint32x4_t)
		__builtin_aarch64_reduc_umin_v4si ((int32x4_t) __a),
		__LANE0 (4));
}

/* vminnmv  */

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vminnmv_f32 (float32x2_t __a)
{
  return vget_lane_f32 (__builtin_aarch64_reduc_smin_v2sf (__a), __LANE0 (2));
}

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vminnmvq_f32 (float32x4_t __a)
{
  return vgetq_lane_f32 (__builtin_aarch64_reduc_smin_v4sf (__a), __LANE0 (4));
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vminnmvq_f64 (float64x2_t __a)
{
  return vgetq_lane_f64 (__builtin_aarch64_reduc_smin_v2df (__a), __LANE0 (2));
}

/* vmla */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmla_f32 (float32x2_t a, float32x2_t b, float32x2_t c)
{
  return a + b * c;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlaq_f32 (float32x4_t a, float32x4_t b, float32x4_t c)
{
  return a + b * c;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmlaq_f64 (float64x2_t a, float64x2_t b, float64x2_t c)
{
  return a + b * c;
}

/* vmla_lane  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmla_lane_f32 (float32x2_t __a, float32x2_t __b,
	       float32x2_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_f32 (__c, __lane)));
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmla_lane_s16 (int16x4_t __a, int16x4_t __b,
		int16x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_s16 (__c, __lane)));
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmla_lane_s32 (int32x2_t __a, int32x2_t __b,
		int32x2_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_s32 (__c, __lane)));
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmla_lane_u16 (uint16x4_t __a, uint16x4_t __b,
		uint16x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_u16 (__c, __lane)));
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmla_lane_u32 (uint32x2_t __a, uint32x2_t __b,
	       uint32x2_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_u32 (__c, __lane)));
}

/* vmla_laneq  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmla_laneq_f32 (float32x2_t __a, float32x2_t __b,
	        float32x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_f32 (__c, __lane)));
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmla_laneq_s16 (int16x4_t __a, int16x4_t __b,
		int16x8_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_s16 (__c, __lane)));
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmla_laneq_s32 (int32x2_t __a, int32x2_t __b,
		int32x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_s32 (__c, __lane)));
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmla_laneq_u16 (uint16x4_t __a, uint16x4_t __b,
		uint16x8_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_u16 (__c, __lane)));
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmla_laneq_u32 (uint32x2_t __a, uint32x2_t __b,
		uint32x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_u32 (__c, __lane)));
}

/* vmlaq_lane  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlaq_lane_f32 (float32x4_t __a, float32x4_t __b,
		float32x2_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_f32 (__c, __lane)));
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlaq_lane_s16 (int16x8_t __a, int16x8_t __b,
		int16x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_s16 (__c, __lane)));
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlaq_lane_s32 (int32x4_t __a, int32x4_t __b,
		int32x2_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_s32 (__c, __lane)));
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlaq_lane_u16 (uint16x8_t __a, uint16x8_t __b,
		uint16x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_u16 (__c, __lane)));
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlaq_lane_u32 (uint32x4_t __a, uint32x4_t __b,
		uint32x2_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vget_lane_u32 (__c, __lane)));
}

  /* vmlaq_laneq  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlaq_laneq_f32 (float32x4_t __a, float32x4_t __b,
		 float32x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_f32 (__c, __lane)));
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlaq_laneq_s16 (int16x8_t __a, int16x8_t __b,
		int16x8_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_s16 (__c, __lane)));
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlaq_laneq_s32 (int32x4_t __a, int32x4_t __b,
		int32x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_s32 (__c, __lane)));
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlaq_laneq_u16 (uint16x8_t __a, uint16x8_t __b,
		uint16x8_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_u16 (__c, __lane)));
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlaq_laneq_u32 (uint32x4_t __a, uint32x4_t __b,
		uint32x4_t __c, const int __lane)
{
  return (__a + (__b * __aarch64_vgetq_lane_u32 (__c, __lane)));
}

/* vmls  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmls_f32 (float32x2_t a, float32x2_t b, float32x2_t c)
{
  return a - b * c;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlsq_f32 (float32x4_t a, float32x4_t b, float32x4_t c)
{
  return a - b * c;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmlsq_f64 (float64x2_t a, float64x2_t b, float64x2_t c)
{
  return a - b * c;
}

/* vmls_lane  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmls_lane_f32 (float32x2_t __a, float32x2_t __b,
	       float32x2_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_f32 (__c, __lane)));
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmls_lane_s16 (int16x4_t __a, int16x4_t __b,
		int16x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_s16 (__c, __lane)));
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmls_lane_s32 (int32x2_t __a, int32x2_t __b,
		int32x2_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_s32 (__c, __lane)));
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmls_lane_u16 (uint16x4_t __a, uint16x4_t __b,
		uint16x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_u16 (__c, __lane)));
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmls_lane_u32 (uint32x2_t __a, uint32x2_t __b,
	       uint32x2_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_u32 (__c, __lane)));
}

/* vmls_laneq  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmls_laneq_f32 (float32x2_t __a, float32x2_t __b,
	       float32x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_f32 (__c, __lane)));
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmls_laneq_s16 (int16x4_t __a, int16x4_t __b,
		int16x8_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_s16 (__c, __lane)));
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmls_laneq_s32 (int32x2_t __a, int32x2_t __b,
		int32x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_s32 (__c, __lane)));
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmls_laneq_u16 (uint16x4_t __a, uint16x4_t __b,
		uint16x8_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_u16 (__c, __lane)));
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmls_laneq_u32 (uint32x2_t __a, uint32x2_t __b,
		uint32x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_u32 (__c, __lane)));
}

/* vmlsq_lane  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlsq_lane_f32 (float32x4_t __a, float32x4_t __b,
		float32x2_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_f32 (__c, __lane)));
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsq_lane_s16 (int16x8_t __a, int16x8_t __b,
		int16x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_s16 (__c, __lane)));
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsq_lane_s32 (int32x4_t __a, int32x4_t __b,
		int32x2_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_s32 (__c, __lane)));
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsq_lane_u16 (uint16x8_t __a, uint16x8_t __b,
		uint16x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_u16 (__c, __lane)));
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsq_lane_u32 (uint32x4_t __a, uint32x4_t __b,
		uint32x2_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vget_lane_u32 (__c, __lane)));
}

  /* vmlsq_laneq  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlsq_laneq_f32 (float32x4_t __a, float32x4_t __b,
		float32x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_f32 (__c, __lane)));
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsq_laneq_s16 (int16x8_t __a, int16x8_t __b,
		int16x8_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_s16 (__c, __lane)));
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsq_laneq_s32 (int32x4_t __a, int32x4_t __b,
		int32x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_s32 (__c, __lane)));
}
__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsq_laneq_u16 (uint16x8_t __a, uint16x8_t __b,
		uint16x8_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_u16 (__c, __lane)));
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsq_laneq_u32 (uint32x4_t __a, uint32x4_t __b,
		uint32x4_t __c, const int __lane)
{
  return (__a - (__b * __aarch64_vgetq_lane_u32 (__c, __lane)));
}

/* vmov_n_  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmov_n_f32 (float32_t __a)
{
  return vdup_n_f32 (__a);
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vmov_n_f64 (float64_t __a)
{
  return __a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vmov_n_p8 (poly8_t __a)
{
  return vdup_n_p8 (__a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vmov_n_p16 (poly16_t __a)
{
  return vdup_n_p16 (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmov_n_s8 (int8_t __a)
{
  return vdup_n_s8 (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmov_n_s16 (int16_t __a)
{
  return vdup_n_s16 (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmov_n_s32 (int32_t __a)
{
  return vdup_n_s32 (__a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vmov_n_s64 (int64_t __a)
{
  return __a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmov_n_u8 (uint8_t __a)
{
  return vdup_n_u8 (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmov_n_u16 (uint16_t __a)
{
    return vdup_n_u16 (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmov_n_u32 (uint32_t __a)
{
   return vdup_n_u32 (__a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vmov_n_u64 (uint64_t __a)
{
   return __a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmovq_n_f32 (float32_t __a)
{
  return vdupq_n_f32 (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmovq_n_f64 (float64_t __a)
{
  return vdupq_n_f64 (__a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vmovq_n_p8 (poly8_t __a)
{
  return vdupq_n_p8 (__a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vmovq_n_p16 (poly16_t __a)
{
  return vdupq_n_p16 (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmovq_n_s8 (int8_t __a)
{
  return vdupq_n_s8 (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmovq_n_s16 (int16_t __a)
{
  return vdupq_n_s16 (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmovq_n_s32 (int32_t __a)
{
  return vdupq_n_s32 (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmovq_n_s64 (int64_t __a)
{
  return vdupq_n_s64 (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmovq_n_u8 (uint8_t __a)
{
  return vdupq_n_u8 (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmovq_n_u16 (uint16_t __a)
{
  return vdupq_n_u16 (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmovq_n_u32 (uint32_t __a)
{
  return vdupq_n_u32 (__a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmovq_n_u64 (uint64_t __a)
{
  return vdupq_n_u64 (__a);
}

/* vmul_lane  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmul_lane_f32 (float32x2_t __a, float32x2_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_f32 (__b, __lane);
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vmul_lane_f64 (float64x1_t __a, float64x1_t __b, const int __lane)
{
  return __a * __b;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmul_lane_s16 (int16x4_t __a, int16x4_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_s16 (__b, __lane);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmul_lane_s32 (int32x2_t __a, int32x2_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_s32 (__b, __lane);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmul_lane_u16 (uint16x4_t __a, uint16x4_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_u16 (__b, __lane);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmul_lane_u32 (uint32x2_t __a, uint32x2_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_u32 (__b, __lane);
}

/* vmul_laneq  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmul_laneq_f32 (float32x2_t __a, float32x4_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_f32 (__b, __lane);
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vmul_laneq_f64 (float64x1_t __a, float64x2_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_f64 (__b, __lane);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmul_laneq_s16 (int16x4_t __a, int16x8_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_s16 (__b, __lane);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmul_laneq_s32 (int32x2_t __a, int32x4_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_s32 (__b, __lane);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmul_laneq_u16 (uint16x4_t __a, uint16x8_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_u16 (__b, __lane);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmul_laneq_u32 (uint32x2_t __a, uint32x4_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_u32 (__b, __lane);
}

/* vmulq_lane  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmulq_lane_f32 (float32x4_t __a, float32x2_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_f32 (__b, __lane);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmulq_lane_f64 (float64x2_t __a, float64x1_t __b, const int __lane)
{
  return __a * __b;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmulq_lane_s16 (int16x8_t __a, int16x4_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_s16 (__b, __lane);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmulq_lane_s32 (int32x4_t __a, int32x2_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_s32 (__b, __lane);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmulq_lane_u16 (uint16x8_t __a, uint16x4_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_u16 (__b, __lane);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmulq_lane_u32 (uint32x4_t __a, uint32x2_t __b, const int __lane)
{
  return __a * __aarch64_vget_lane_u32 (__b, __lane);
}

/* vmulq_laneq  */

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmulq_laneq_f32 (float32x4_t __a, float32x4_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_f32 (__b, __lane);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vmulq_laneq_f64 (float64x2_t __a, float64x2_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_f64 (__b, __lane);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmulq_laneq_s16 (int16x8_t __a, int16x8_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_s16 (__b, __lane);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmulq_laneq_s32 (int32x4_t __a, int32x4_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_s32 (__b, __lane);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmulq_laneq_u16 (uint16x8_t __a, uint16x8_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_u16 (__b, __lane);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmulq_laneq_u32 (uint32x4_t __a, uint32x4_t __b, const int __lane)
{
  return __a * __aarch64_vgetq_lane_u32 (__b, __lane);
}

/* vneg  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vneg_f32 (float32x2_t __a)
{
  return -__a;
}

__extension__ static __inline float64x1_t __attribute__ ((__always_inline__))
vneg_f64 (float64x1_t __a)
{
  return -__a;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vneg_s8 (int8x8_t __a)
{
  return -__a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vneg_s16 (int16x4_t __a)
{
  return -__a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vneg_s32 (int32x2_t __a)
{
  return -__a;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vneg_s64 (int64x1_t __a)
{
  return -__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vnegq_f32 (float32x4_t __a)
{
  return -__a;
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vnegq_f64 (float64x2_t __a)
{
  return -__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vnegq_s8 (int8x16_t __a)
{
  return -__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vnegq_s16 (int16x8_t __a)
{
  return -__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vnegq_s32 (int32x4_t __a)
{
  return -__a;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vnegq_s64 (int64x2_t __a)
{
  return -__a;
}

/* vqabs */

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqabsq_s64 (int64x2_t __a)
{
  return (int64x2_t) __builtin_aarch64_sqabsv2di (__a);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqabsb_s8 (int8x1_t __a)
{
  return (int8x1_t) __builtin_aarch64_sqabsqi (__a);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqabsh_s16 (int16x1_t __a)
{
  return (int16x1_t) __builtin_aarch64_sqabshi (__a);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqabss_s32 (int32x1_t __a)
{
  return (int32x1_t) __builtin_aarch64_sqabssi (__a);
}

/* vqadd */

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqaddb_s8 (int8x1_t __a, int8x1_t __b)
{
  return (int8x1_t) __builtin_aarch64_sqaddqi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqaddh_s16 (int16x1_t __a, int16x1_t __b)
{
  return (int16x1_t) __builtin_aarch64_sqaddhi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqadds_s32 (int32x1_t __a, int32x1_t __b)
{
  return (int32x1_t) __builtin_aarch64_sqaddsi (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqaddd_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_sqadddi (__a, __b);
}

__extension__ static __inline uint8x1_t __attribute__ ((__always_inline__))
vqaddb_u8 (uint8x1_t __a, uint8x1_t __b)
{
  return (uint8x1_t) __builtin_aarch64_uqaddqi (__a, __b);
}

__extension__ static __inline uint16x1_t __attribute__ ((__always_inline__))
vqaddh_u16 (uint16x1_t __a, uint16x1_t __b)
{
  return (uint16x1_t) __builtin_aarch64_uqaddhi (__a, __b);
}

__extension__ static __inline uint32x1_t __attribute__ ((__always_inline__))
vqadds_u32 (uint32x1_t __a, uint32x1_t __b)
{
  return (uint32x1_t) __builtin_aarch64_uqaddsi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqaddd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_uqadddi (__a, __b);
}

/* vqdmlal */

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_sqdmlalv4hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_high_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_sqdmlal2v8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_high_lane_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c,
		       int const __d)
{
  return __builtin_aarch64_sqdmlal2_lanev8hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_high_laneq_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c,
			int const __d)
{
  return __builtin_aarch64_sqdmlal2_laneqv8hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_high_n_s16 (int32x4_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlal2_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_lane_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c, int const __d)
{
  int16x8_t __tmp = vcombine_s16 (__c, vcreate_s16 (__AARCH64_INT64_C (0)));
  return __builtin_aarch64_sqdmlal_lanev4hi (__a, __b, __tmp, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_laneq_s16 (int32x4_t __a, int16x4_t __b, int16x8_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlal_laneqv4hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlal_nv4hi (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_sqdmlalv2si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_high_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_sqdmlal2v4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_high_lane_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c,
		       int const __d)
{
  return __builtin_aarch64_sqdmlal2_lanev4si (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_high_laneq_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c,
			int const __d)
{
  return __builtin_aarch64_sqdmlal2_laneqv4si (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_high_n_s32 (int64x2_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlal2_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c, int const __d)
{
  int32x4_t __tmp = vcombine_s32 (__c, vcreate_s32 (__AARCH64_INT64_C (0)));
  return __builtin_aarch64_sqdmlal_lanev2si (__a, __b, __tmp, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_laneq_s32 (int64x2_t __a, int32x2_t __b, int32x4_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlal_laneqv2si (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlal_nv2si (__a, __b, __c);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqdmlalh_s16 (int32x1_t __a, int16x1_t __b, int16x1_t __c)
{
  return __builtin_aarch64_sqdmlalhi (__a, __b, __c);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqdmlalh_lane_s16 (int32x1_t __a, int16x1_t __b, int16x8_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlal_lanehi (__a, __b, __c, __d);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqdmlals_s32 (int64x1_t __a, int32x1_t __b, int32x1_t __c)
{
  return __builtin_aarch64_sqdmlalsi (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqdmlals_lane_s32 (int64x1_t __a, int32x1_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlal_lanesi (__a, __b, __c, __d);
}

/* vqdmlsl */

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return __builtin_aarch64_sqdmlslv4hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_high_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_aarch64_sqdmlsl2v8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_high_lane_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c,
		       int const __d)
{
  return __builtin_aarch64_sqdmlsl2_lanev8hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_high_laneq_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c,
			int const __d)
{
  return __builtin_aarch64_sqdmlsl2_laneqv8hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_high_n_s16 (int32x4_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlsl2_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_lane_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c, int const __d)
{
  int16x8_t __tmp = vcombine_s16 (__c, vcreate_s16 (__AARCH64_INT64_C (0)));
  return __builtin_aarch64_sqdmlsl_lanev4hi (__a, __b, __tmp, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_laneq_s16 (int32x4_t __a, int16x4_t __b, int16x8_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlsl_laneqv4hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return __builtin_aarch64_sqdmlsl_nv4hi (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return __builtin_aarch64_sqdmlslv2si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_high_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_aarch64_sqdmlsl2v4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_high_lane_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c,
		       int const __d)
{
  return __builtin_aarch64_sqdmlsl2_lanev4si (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_high_laneq_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c,
			int const __d)
{
  return __builtin_aarch64_sqdmlsl2_laneqv4si (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_high_n_s32 (int64x2_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlsl2_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c, int const __d)
{
  int32x4_t __tmp = vcombine_s32 (__c, vcreate_s32 (__AARCH64_INT64_C (0)));
  return __builtin_aarch64_sqdmlsl_lanev2si (__a, __b, __tmp, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_laneq_s32 (int64x2_t __a, int32x2_t __b, int32x4_t __c, int const __d)
{
  return __builtin_aarch64_sqdmlsl_laneqv2si (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return __builtin_aarch64_sqdmlsl_nv2si (__a, __b, __c);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqdmlslh_s16 (int32x1_t __a, int16x1_t __b, int16x1_t __c)
{
  return __builtin_aarch64_sqdmlslhi (__a, __b, __c);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqdmlslh_lane_s16 (int32x1_t __a, int16x1_t __b, int16x8_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlsl_lanehi (__a, __b, __c, __d);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqdmlsls_s32 (int64x1_t __a, int32x1_t __b, int32x1_t __c)
{
  return __builtin_aarch64_sqdmlslsi (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqdmlsls_lane_s32 (int64x1_t __a, int32x1_t __b, int32x4_t __c, const int __d)
{
  return __builtin_aarch64_sqdmlsl_lanesi (__a, __b, __c, __d);
}

/* vqdmulh */

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqdmulh_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanev4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqdmulh_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanev2si (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqdmulhq_lane_s16 (int16x8_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanev8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmulhq_lane_s32 (int32x4_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanev4si (__a, __b, __c);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqdmulhh_s16 (int16x1_t __a, int16x1_t __b)
{
  return (int16x1_t) __builtin_aarch64_sqdmulhhi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqdmulhh_lane_s16 (int16x1_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanehi (__a, __b, __c);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqdmulhs_s32 (int32x1_t __a, int32x1_t __b)
{
  return (int32x1_t) __builtin_aarch64_sqdmulhsi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqdmulhs_lane_s32 (int32x1_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmulh_lanesi (__a, __b, __c);
}

/* vqdmull */

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_sqdmullv4hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_high_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sqdmull2v8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_high_lane_s16 (int16x8_t __a, int16x8_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull2_lanev8hi (__a, __b,__c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_high_laneq_s16 (int16x8_t __a, int16x8_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull2_laneqv8hi (__a, __b,__c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_high_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_aarch64_sqdmull2_nv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_lane_s16 (int16x4_t __a, int16x4_t __b, int const __c)
{
  int16x8_t __tmp = vcombine_s16 (__b, vcreate_s16 (__AARCH64_INT64_C (0)));
  return __builtin_aarch64_sqdmull_lanev4hi (__a, __tmp, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_laneq_s16 (int16x4_t __a, int16x8_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull_laneqv4hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_n_s16 (int16x4_t __a, int16_t __b)
{
  return __builtin_aarch64_sqdmull_nv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_sqdmullv2si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_high_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sqdmull2v4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_high_lane_s32 (int32x4_t __a, int32x4_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull2_lanev4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_high_laneq_s32 (int32x4_t __a, int32x4_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull2_laneqv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_high_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_aarch64_sqdmull2_nv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_lane_s32 (int32x2_t __a, int32x2_t __b, int const __c)
{
  int32x4_t __tmp = vcombine_s32 (__b, vcreate_s32 (__AARCH64_INT64_C (0)));
  return __builtin_aarch64_sqdmull_lanev2si (__a, __tmp, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_laneq_s32 (int32x2_t __a, int32x4_t __b, int const __c)
{
  return __builtin_aarch64_sqdmull_laneqv2si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_n_s32 (int32x2_t __a, int32_t __b)
{
  return __builtin_aarch64_sqdmull_nv2si (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqdmullh_s16 (int16x1_t __a, int16x1_t __b)
{
  return (int32x1_t) __builtin_aarch64_sqdmullhi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqdmullh_lane_s16 (int16x1_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqdmull_lanehi (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqdmulls_s32 (int32x1_t __a, int32x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_sqdmullsi (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqdmulls_lane_s32 (int32x1_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqdmull_lanesi (__a, __b, __c);
}

/* vqmovn */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqmovn_s16 (int16x8_t __a)
{
  return (int8x8_t) __builtin_aarch64_sqmovnv8hi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqmovn_s32 (int32x4_t __a)
{
  return (int16x4_t) __builtin_aarch64_sqmovnv4si (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqmovn_s64 (int64x2_t __a)
{
  return (int32x2_t) __builtin_aarch64_sqmovnv2di (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqmovn_u16 (uint16x8_t __a)
{
  return (uint8x8_t) __builtin_aarch64_uqmovnv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqmovn_u32 (uint32x4_t __a)
{
  return (uint16x4_t) __builtin_aarch64_uqmovnv4si ((int32x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqmovn_u64 (uint64x2_t __a)
{
  return (uint32x2_t) __builtin_aarch64_uqmovnv2di ((int64x2_t) __a);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqmovnh_s16 (int16x1_t __a)
{
  return (int8x1_t) __builtin_aarch64_sqmovnhi (__a);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqmovns_s32 (int32x1_t __a)
{
  return (int16x1_t) __builtin_aarch64_sqmovnsi (__a);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqmovnd_s64 (int64x1_t __a)
{
  return (int32x1_t) __builtin_aarch64_sqmovndi (__a);
}

__extension__ static __inline uint8x1_t __attribute__ ((__always_inline__))
vqmovnh_u16 (uint16x1_t __a)
{
  return (uint8x1_t) __builtin_aarch64_uqmovnhi (__a);
}

__extension__ static __inline uint16x1_t __attribute__ ((__always_inline__))
vqmovns_u32 (uint32x1_t __a)
{
  return (uint16x1_t) __builtin_aarch64_uqmovnsi (__a);
}

__extension__ static __inline uint32x1_t __attribute__ ((__always_inline__))
vqmovnd_u64 (uint64x1_t __a)
{
  return (uint32x1_t) __builtin_aarch64_uqmovndi (__a);
}

/* vqmovun */

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqmovun_s16 (int16x8_t __a)
{
  return (uint8x8_t) __builtin_aarch64_sqmovunv8hi (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqmovun_s32 (int32x4_t __a)
{
  return (uint16x4_t) __builtin_aarch64_sqmovunv4si (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqmovun_s64 (int64x2_t __a)
{
  return (uint32x2_t) __builtin_aarch64_sqmovunv2di (__a);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqmovunh_s16 (int16x1_t __a)
{
  return (int8x1_t) __builtin_aarch64_sqmovunhi (__a);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqmovuns_s32 (int32x1_t __a)
{
  return (int16x1_t) __builtin_aarch64_sqmovunsi (__a);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqmovund_s64 (int64x1_t __a)
{
  return (int32x1_t) __builtin_aarch64_sqmovundi (__a);
}

/* vqneg */

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqnegq_s64 (int64x2_t __a)
{
  return (int64x2_t) __builtin_aarch64_sqnegv2di (__a);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqnegb_s8 (int8x1_t __a)
{
  return (int8x1_t) __builtin_aarch64_sqnegqi (__a);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqnegh_s16 (int16x1_t __a)
{
  return (int16x1_t) __builtin_aarch64_sqneghi (__a);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqnegs_s32 (int32x1_t __a)
{
  return (int32x1_t) __builtin_aarch64_sqnegsi (__a);
}

/* vqrdmulh */

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmulh_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return  __builtin_aarch64_sqrdmulh_lanev4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmulh_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanev2si (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmulhq_lane_s16 (int16x8_t __a, int16x4_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanev8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmulhq_lane_s32 (int32x4_t __a, int32x2_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanev4si (__a, __b, __c);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqrdmulhh_s16 (int16x1_t __a, int16x1_t __b)
{
  return (int16x1_t) __builtin_aarch64_sqrdmulhhi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqrdmulhh_lane_s16 (int16x1_t __a, int16x8_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanehi (__a, __b, __c);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqrdmulhs_s32 (int32x1_t __a, int32x1_t __b)
{
  return (int32x1_t) __builtin_aarch64_sqrdmulhsi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqrdmulhs_lane_s32 (int32x1_t __a, int32x4_t __b, const int __c)
{
  return __builtin_aarch64_sqrdmulh_lanesi (__a, __b, __c);
}

/* vqrshl */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqrshl_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_sqrshlv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrshl_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_sqrshlv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrshl_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_sqrshlv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqrshl_s64 (int64x1_t __a, int64x1_t __b)
{
  return __builtin_aarch64_sqrshldi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqrshl_u8 (uint8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_uqrshlv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqrshl_u16 (uint16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_uqrshlv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqrshl_u32 (uint32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_uqrshlv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqrshl_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_uqrshldi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqrshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_sqrshlv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sqrshlv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sqrshlv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqrshlq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_sqrshlv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqrshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_uqrshlv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqrshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uqrshlv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqrshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uqrshlv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqrshlq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_uqrshlv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqrshlb_s8 (int8x1_t __a, int8x1_t __b)
{
  return __builtin_aarch64_sqrshlqi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqrshlh_s16 (int16x1_t __a, int16x1_t __b)
{
  return __builtin_aarch64_sqrshlhi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqrshls_s32 (int32x1_t __a, int32x1_t __b)
{
  return __builtin_aarch64_sqrshlsi (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqrshld_s64 (int64x1_t __a, int64x1_t __b)
{
  return __builtin_aarch64_sqrshldi (__a, __b);
}

__extension__ static __inline uint8x1_t __attribute__ ((__always_inline__))
vqrshlb_u8 (uint8x1_t __a, uint8x1_t __b)
{
  return (uint8x1_t) __builtin_aarch64_uqrshlqi (__a, __b);
}

__extension__ static __inline uint16x1_t __attribute__ ((__always_inline__))
vqrshlh_u16 (uint16x1_t __a, uint16x1_t __b)
{
  return (uint16x1_t) __builtin_aarch64_uqrshlhi (__a, __b);
}

__extension__ static __inline uint32x1_t __attribute__ ((__always_inline__))
vqrshls_u32 (uint32x1_t __a, uint32x1_t __b)
{
  return (uint32x1_t) __builtin_aarch64_uqrshlsi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqrshld_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_uqrshldi (__a, __b);
}

/* vqrshrn */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqrshrn_n_s16 (int16x8_t __a, const int __b)
{
  return (int8x8_t) __builtin_aarch64_sqrshrn_nv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrshrn_n_s32 (int32x4_t __a, const int __b)
{
  return (int16x4_t) __builtin_aarch64_sqrshrn_nv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrshrn_n_s64 (int64x2_t __a, const int __b)
{
  return (int32x2_t) __builtin_aarch64_sqrshrn_nv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqrshrn_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint8x8_t) __builtin_aarch64_uqrshrn_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqrshrn_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint16x4_t) __builtin_aarch64_uqrshrn_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqrshrn_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint32x2_t) __builtin_aarch64_uqrshrn_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqrshrnh_n_s16 (int16x1_t __a, const int __b)
{
  return (int8x1_t) __builtin_aarch64_sqrshrn_nhi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqrshrns_n_s32 (int32x1_t __a, const int __b)
{
  return (int16x1_t) __builtin_aarch64_sqrshrn_nsi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqrshrnd_n_s64 (int64x1_t __a, const int __b)
{
  return (int32x1_t) __builtin_aarch64_sqrshrn_ndi (__a, __b);
}

__extension__ static __inline uint8x1_t __attribute__ ((__always_inline__))
vqrshrnh_n_u16 (uint16x1_t __a, const int __b)
{
  return (uint8x1_t) __builtin_aarch64_uqrshrn_nhi (__a, __b);
}

__extension__ static __inline uint16x1_t __attribute__ ((__always_inline__))
vqrshrns_n_u32 (uint32x1_t __a, const int __b)
{
  return (uint16x1_t) __builtin_aarch64_uqrshrn_nsi (__a, __b);
}

__extension__ static __inline uint32x1_t __attribute__ ((__always_inline__))
vqrshrnd_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint32x1_t) __builtin_aarch64_uqrshrn_ndi (__a, __b);
}

/* vqrshrun */

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqrshrun_n_s16 (int16x8_t __a, const int __b)
{
  return (uint8x8_t) __builtin_aarch64_sqrshrun_nv8hi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqrshrun_n_s32 (int32x4_t __a, const int __b)
{
  return (uint16x4_t) __builtin_aarch64_sqrshrun_nv4si (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqrshrun_n_s64 (int64x2_t __a, const int __b)
{
  return (uint32x2_t) __builtin_aarch64_sqrshrun_nv2di (__a, __b);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqrshrunh_n_s16 (int16x1_t __a, const int __b)
{
  return (int8x1_t) __builtin_aarch64_sqrshrun_nhi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqrshruns_n_s32 (int32x1_t __a, const int __b)
{
  return (int16x1_t) __builtin_aarch64_sqrshrun_nsi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqrshrund_n_s64 (int64x1_t __a, const int __b)
{
  return (int32x1_t) __builtin_aarch64_sqrshrun_ndi (__a, __b);
}

/* vqshl */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqshl_s8 (int8x8_t __a, int8x8_t __b)
{
  return __builtin_aarch64_sqshlv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqshl_s16 (int16x4_t __a, int16x4_t __b)
{
  return __builtin_aarch64_sqshlv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqshl_s32 (int32x2_t __a, int32x2_t __b)
{
  return __builtin_aarch64_sqshlv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqshl_s64 (int64x1_t __a, int64x1_t __b)
{
  return __builtin_aarch64_sqshldi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshl_u8 (uint8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_uqshlv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshl_u16 (uint16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_uqshlv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshl_u32 (uint32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_uqshlv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqshl_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_uqshldi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_aarch64_sqshlv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_aarch64_sqshlv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_aarch64_sqshlv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqshlq_s64 (int64x2_t __a, int64x2_t __b)
{
  return __builtin_aarch64_sqshlv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_uqshlv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_uqshlv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_uqshlv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqshlq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_uqshlv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqshlb_s8 (int8x1_t __a, int8x1_t __b)
{
  return __builtin_aarch64_sqshlqi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqshlh_s16 (int16x1_t __a, int16x1_t __b)
{
  return __builtin_aarch64_sqshlhi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqshls_s32 (int32x1_t __a, int32x1_t __b)
{
  return __builtin_aarch64_sqshlsi (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqshld_s64 (int64x1_t __a, int64x1_t __b)
{
  return __builtin_aarch64_sqshldi (__a, __b);
}

__extension__ static __inline uint8x1_t __attribute__ ((__always_inline__))
vqshlb_u8 (uint8x1_t __a, uint8x1_t __b)
{
  return (uint8x1_t) __builtin_aarch64_uqshlqi (__a, __b);
}

__extension__ static __inline uint16x1_t __attribute__ ((__always_inline__))
vqshlh_u16 (uint16x1_t __a, uint16x1_t __b)
{
  return (uint16x1_t) __builtin_aarch64_uqshlhi (__a, __b);
}

__extension__ static __inline uint32x1_t __attribute__ ((__always_inline__))
vqshls_u32 (uint32x1_t __a, uint32x1_t __b)
{
  return (uint32x1_t) __builtin_aarch64_uqshlsi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqshld_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_uqshldi (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqshl_n_s8 (int8x8_t __a, const int __b)
{
  return (int8x8_t) __builtin_aarch64_sqshl_nv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqshl_n_s16 (int16x4_t __a, const int __b)
{
  return (int16x4_t) __builtin_aarch64_sqshl_nv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqshl_n_s32 (int32x2_t __a, const int __b)
{
  return (int32x2_t) __builtin_aarch64_sqshl_nv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqshl_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t) __builtin_aarch64_sqshl_ndi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshl_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x8_t) __builtin_aarch64_uqshl_nv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshl_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x4_t) __builtin_aarch64_uqshl_nv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshl_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x2_t) __builtin_aarch64_uqshl_nv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqshl_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t) __builtin_aarch64_uqshl_ndi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqshlq_n_s8 (int8x16_t __a, const int __b)
{
  return (int8x16_t) __builtin_aarch64_sqshl_nv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqshlq_n_s16 (int16x8_t __a, const int __b)
{
  return (int16x8_t) __builtin_aarch64_sqshl_nv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqshlq_n_s32 (int32x4_t __a, const int __b)
{
  return (int32x4_t) __builtin_aarch64_sqshl_nv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqshlq_n_s64 (int64x2_t __a, const int __b)
{
  return (int64x2_t) __builtin_aarch64_sqshl_nv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqshlq_n_u8 (uint8x16_t __a, const int __b)
{
  return (uint8x16_t) __builtin_aarch64_uqshl_nv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqshlq_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint16x8_t) __builtin_aarch64_uqshl_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqshlq_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint32x4_t) __builtin_aarch64_uqshl_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqshlq_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint64x2_t) __builtin_aarch64_uqshl_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqshlb_n_s8 (int8x1_t __a, const int __b)
{
  return (int8x1_t) __builtin_aarch64_sqshl_nqi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqshlh_n_s16 (int16x1_t __a, const int __b)
{
  return (int16x1_t) __builtin_aarch64_sqshl_nhi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqshls_n_s32 (int32x1_t __a, const int __b)
{
  return (int32x1_t) __builtin_aarch64_sqshl_nsi (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqshld_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t) __builtin_aarch64_sqshl_ndi (__a, __b);
}

__extension__ static __inline uint8x1_t __attribute__ ((__always_inline__))
vqshlb_n_u8 (uint8x1_t __a, const int __b)
{
  return (uint8x1_t) __builtin_aarch64_uqshl_nqi (__a, __b);
}

__extension__ static __inline uint16x1_t __attribute__ ((__always_inline__))
vqshlh_n_u16 (uint16x1_t __a, const int __b)
{
  return (uint16x1_t) __builtin_aarch64_uqshl_nhi (__a, __b);
}

__extension__ static __inline uint32x1_t __attribute__ ((__always_inline__))
vqshls_n_u32 (uint32x1_t __a, const int __b)
{
  return (uint32x1_t) __builtin_aarch64_uqshl_nsi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqshld_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t) __builtin_aarch64_uqshl_ndi (__a, __b);
}

/* vqshlu */

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshlu_n_s8 (int8x8_t __a, const int __b)
{
  return (uint8x8_t) __builtin_aarch64_sqshlu_nv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshlu_n_s16 (int16x4_t __a, const int __b)
{
  return (uint16x4_t) __builtin_aarch64_sqshlu_nv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshlu_n_s32 (int32x2_t __a, const int __b)
{
  return (uint32x2_t) __builtin_aarch64_sqshlu_nv2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqshlu_n_s64 (int64x1_t __a, const int __b)
{
  return (uint64x1_t) __builtin_aarch64_sqshlu_ndi (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqshluq_n_s8 (int8x16_t __a, const int __b)
{
  return (uint8x16_t) __builtin_aarch64_sqshlu_nv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqshluq_n_s16 (int16x8_t __a, const int __b)
{
  return (uint16x8_t) __builtin_aarch64_sqshlu_nv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqshluq_n_s32 (int32x4_t __a, const int __b)
{
  return (uint32x4_t) __builtin_aarch64_sqshlu_nv4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqshluq_n_s64 (int64x2_t __a, const int __b)
{
  return (uint64x2_t) __builtin_aarch64_sqshlu_nv2di (__a, __b);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqshlub_n_s8 (int8x1_t __a, const int __b)
{
  return (int8x1_t) __builtin_aarch64_sqshlu_nqi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqshluh_n_s16 (int16x1_t __a, const int __b)
{
  return (int16x1_t) __builtin_aarch64_sqshlu_nhi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqshlus_n_s32 (int32x1_t __a, const int __b)
{
  return (int32x1_t) __builtin_aarch64_sqshlu_nsi (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqshlud_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t) __builtin_aarch64_sqshlu_ndi (__a, __b);
}

/* vqshrn */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqshrn_n_s16 (int16x8_t __a, const int __b)
{
  return (int8x8_t) __builtin_aarch64_sqshrn_nv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqshrn_n_s32 (int32x4_t __a, const int __b)
{
  return (int16x4_t) __builtin_aarch64_sqshrn_nv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqshrn_n_s64 (int64x2_t __a, const int __b)
{
  return (int32x2_t) __builtin_aarch64_sqshrn_nv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshrn_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint8x8_t) __builtin_aarch64_uqshrn_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshrn_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint16x4_t) __builtin_aarch64_uqshrn_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshrn_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint32x2_t) __builtin_aarch64_uqshrn_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqshrnh_n_s16 (int16x1_t __a, const int __b)
{
  return (int8x1_t) __builtin_aarch64_sqshrn_nhi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqshrns_n_s32 (int32x1_t __a, const int __b)
{
  return (int16x1_t) __builtin_aarch64_sqshrn_nsi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqshrnd_n_s64 (int64x1_t __a, const int __b)
{
  return (int32x1_t) __builtin_aarch64_sqshrn_ndi (__a, __b);
}

__extension__ static __inline uint8x1_t __attribute__ ((__always_inline__))
vqshrnh_n_u16 (uint16x1_t __a, const int __b)
{
  return (uint8x1_t) __builtin_aarch64_uqshrn_nhi (__a, __b);
}

__extension__ static __inline uint16x1_t __attribute__ ((__always_inline__))
vqshrns_n_u32 (uint32x1_t __a, const int __b)
{
  return (uint16x1_t) __builtin_aarch64_uqshrn_nsi (__a, __b);
}

__extension__ static __inline uint32x1_t __attribute__ ((__always_inline__))
vqshrnd_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint32x1_t) __builtin_aarch64_uqshrn_ndi (__a, __b);
}

/* vqshrun */

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshrun_n_s16 (int16x8_t __a, const int __b)
{
  return (uint8x8_t) __builtin_aarch64_sqshrun_nv8hi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshrun_n_s32 (int32x4_t __a, const int __b)
{
  return (uint16x4_t) __builtin_aarch64_sqshrun_nv4si (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshrun_n_s64 (int64x2_t __a, const int __b)
{
  return (uint32x2_t) __builtin_aarch64_sqshrun_nv2di (__a, __b);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqshrunh_n_s16 (int16x1_t __a, const int __b)
{
  return (int8x1_t) __builtin_aarch64_sqshrun_nhi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqshruns_n_s32 (int32x1_t __a, const int __b)
{
  return (int16x1_t) __builtin_aarch64_sqshrun_nsi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqshrund_n_s64 (int64x1_t __a, const int __b)
{
  return (int32x1_t) __builtin_aarch64_sqshrun_ndi (__a, __b);
}

/* vqsub */

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vqsubb_s8 (int8x1_t __a, int8x1_t __b)
{
  return (int8x1_t) __builtin_aarch64_sqsubqi (__a, __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vqsubh_s16 (int16x1_t __a, int16x1_t __b)
{
  return (int16x1_t) __builtin_aarch64_sqsubhi (__a, __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vqsubs_s32 (int32x1_t __a, int32x1_t __b)
{
  return (int32x1_t) __builtin_aarch64_sqsubsi (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqsubd_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_sqsubdi (__a, __b);
}

__extension__ static __inline uint8x1_t __attribute__ ((__always_inline__))
vqsubb_u8 (uint8x1_t __a, uint8x1_t __b)
{
  return (uint8x1_t) __builtin_aarch64_uqsubqi (__a, __b);
}

__extension__ static __inline uint16x1_t __attribute__ ((__always_inline__))
vqsubh_u16 (uint16x1_t __a, uint16x1_t __b)
{
  return (uint16x1_t) __builtin_aarch64_uqsubhi (__a, __b);
}

__extension__ static __inline uint32x1_t __attribute__ ((__always_inline__))
vqsubs_u32 (uint32x1_t __a, uint32x1_t __b)
{
  return (uint32x1_t) __builtin_aarch64_uqsubsi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqsubd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_uqsubdi (__a, __b);
}

/* vrecpe  */

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vrecpes_f32 (float32_t __a)
{
  return __builtin_aarch64_frecpesf (__a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vrecped_f64 (float64_t __a)
{
  return __builtin_aarch64_frecpedf (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrecpe_f32 (float32x2_t __a)
{
  return __builtin_aarch64_frecpev2sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrecpeq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_frecpev4sf (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrecpeq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_frecpev2df (__a);
}

/* vrecps  */

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vrecpss_f32 (float32_t __a, float32_t __b)
{
  return __builtin_aarch64_frecpssf (__a, __b);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vrecpsd_f64 (float64_t __a, float64_t __b)
{
  return __builtin_aarch64_frecpsdf (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrecps_f32 (float32x2_t __a, float32x2_t __b)
{
  return __builtin_aarch64_frecpsv2sf (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrecpsq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_aarch64_frecpsv4sf (__a, __b);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrecpsq_f64 (float64x2_t __a, float64x2_t __b)
{
  return __builtin_aarch64_frecpsv2df (__a, __b);
}

/* vrecpx  */

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vrecpxs_f32 (float32_t __a)
{
  return __builtin_aarch64_frecpxsf (__a);
}

__extension__ static __inline float64_t __attribute__ ((__always_inline__))
vrecpxd_f64 (float64_t __a)
{
  return __builtin_aarch64_frecpxdf (__a);
}

/* vrnd  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrnd_f32 (float32x2_t __a)
{
  return __builtin_aarch64_btruncv2sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_btruncv4sf (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrndq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_btruncv2df (__a);
}

/* vrnda  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrnda_f32 (float32x2_t __a)
{
  return __builtin_aarch64_roundv2sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndaq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_roundv4sf (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrndaq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_roundv2df (__a);
}

/* vrndi  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrndi_f32 (float32x2_t __a)
{
  return __builtin_aarch64_nearbyintv2sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndiq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_nearbyintv4sf (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrndiq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_nearbyintv2df (__a);
}

/* vrndm  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrndm_f32 (float32x2_t __a)
{
  return __builtin_aarch64_floorv2sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndmq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_floorv4sf (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrndmq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_floorv2df (__a);
}

/* vrndn  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrndn_f32 (float32x2_t __a)
{
  return __builtin_aarch64_frintnv2sf (__a);
}
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndnq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_frintnv4sf (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrndnq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_frintnv2df (__a);
}

/* vrndp  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrndp_f32 (float32x2_t __a)
{
  return __builtin_aarch64_ceilv2sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndpq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_ceilv4sf (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrndpq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_ceilv2df (__a);
}

/* vrndx  */

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrndx_f32 (float32x2_t __a)
{
  return __builtin_aarch64_rintv2sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndxq_f32 (float32x4_t __a)
{
  return __builtin_aarch64_rintv4sf (__a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vrndxq_f64 (float64x2_t __a)
{
  return __builtin_aarch64_rintv2df (__a);
}

/* vrshl */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrshl_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t) __builtin_aarch64_srshlv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrshl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_srshlv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrshl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_srshlv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vrshl_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_srshldi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrshl_u8 (uint8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_urshlv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrshl_u16 (uint16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_urshlv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrshl_u32 (uint32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_urshlv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vrshl_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_urshldi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t) __builtin_aarch64_srshlv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_srshlv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_srshlv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vrshlq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t) __builtin_aarch64_srshlv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_urshlv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_urshlv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_urshlv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vrshlq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_urshlv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vrshld_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_srshldi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vrshld_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_urshldi (__a, __b);
}

/* vrshr */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrshr_n_s8 (int8x8_t __a, const int __b)
{
  return (int8x8_t) __builtin_aarch64_srshr_nv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrshr_n_s16 (int16x4_t __a, const int __b)
{
  return (int16x4_t) __builtin_aarch64_srshr_nv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrshr_n_s32 (int32x2_t __a, const int __b)
{
  return (int32x2_t) __builtin_aarch64_srshr_nv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vrshr_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t) __builtin_aarch64_srshr_ndi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrshr_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x8_t) __builtin_aarch64_urshr_nv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrshr_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x4_t) __builtin_aarch64_urshr_nv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrshr_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x2_t) __builtin_aarch64_urshr_nv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vrshr_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t) __builtin_aarch64_urshr_ndi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrshrq_n_s8 (int8x16_t __a, const int __b)
{
  return (int8x16_t) __builtin_aarch64_srshr_nv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrshrq_n_s16 (int16x8_t __a, const int __b)
{
  return (int16x8_t) __builtin_aarch64_srshr_nv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrshrq_n_s32 (int32x4_t __a, const int __b)
{
  return (int32x4_t) __builtin_aarch64_srshr_nv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vrshrq_n_s64 (int64x2_t __a, const int __b)
{
  return (int64x2_t) __builtin_aarch64_srshr_nv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrshrq_n_u8 (uint8x16_t __a, const int __b)
{
  return (uint8x16_t) __builtin_aarch64_urshr_nv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrshrq_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint16x8_t) __builtin_aarch64_urshr_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrshrq_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint32x4_t) __builtin_aarch64_urshr_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vrshrq_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint64x2_t) __builtin_aarch64_urshr_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vrshrd_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t) __builtin_aarch64_srshr_ndi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vrshrd_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t) __builtin_aarch64_urshr_ndi (__a, __b);
}

/* vrsra */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrsra_n_s8 (int8x8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t) __builtin_aarch64_srsra_nv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrsra_n_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t) __builtin_aarch64_srsra_nv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrsra_n_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t) __builtin_aarch64_srsra_nv2si (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vrsra_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t) __builtin_aarch64_srsra_ndi (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrsra_n_u8 (uint8x8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t) __builtin_aarch64_ursra_nv8qi ((int8x8_t) __a,
						    (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrsra_n_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t) __builtin_aarch64_ursra_nv4hi ((int16x4_t) __a,
						     (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrsra_n_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t) __builtin_aarch64_ursra_nv2si ((int32x2_t) __a,
						     (int32x2_t) __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vrsra_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t) __builtin_aarch64_ursra_ndi ((int64x1_t) __a,
						   (int64x1_t) __b, __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrsraq_n_s8 (int8x16_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t) __builtin_aarch64_srsra_nv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrsraq_n_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t) __builtin_aarch64_srsra_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrsraq_n_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t) __builtin_aarch64_srsra_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vrsraq_n_s64 (int64x2_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t) __builtin_aarch64_srsra_nv2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrsraq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t) __builtin_aarch64_ursra_nv16qi ((int8x16_t) __a,
						      (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrsraq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t) __builtin_aarch64_ursra_nv8hi ((int16x8_t) __a,
						     (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrsraq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t) __builtin_aarch64_ursra_nv4si ((int32x4_t) __a,
						     (int32x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vrsraq_n_u64 (uint64x2_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t) __builtin_aarch64_ursra_nv2di ((int64x2_t) __a,
						     (int64x2_t) __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vrsrad_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t) __builtin_aarch64_srsra_ndi (__a, __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vrsrad_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t) __builtin_aarch64_ursra_ndi (__a, __b, __c);
}

/* vshl */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vshl_n_s8 (int8x8_t __a, const int __b)
{
  return (int8x8_t) __builtin_aarch64_ashlv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vshl_n_s16 (int16x4_t __a, const int __b)
{
  return (int16x4_t) __builtin_aarch64_ashlv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vshl_n_s32 (int32x2_t __a, const int __b)
{
  return (int32x2_t) __builtin_aarch64_ashlv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vshl_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t) __builtin_aarch64_ashldi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vshl_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x8_t) __builtin_aarch64_ashlv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vshl_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x4_t) __builtin_aarch64_ashlv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vshl_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x2_t) __builtin_aarch64_ashlv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vshl_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t) __builtin_aarch64_ashldi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vshlq_n_s8 (int8x16_t __a, const int __b)
{
  return (int8x16_t) __builtin_aarch64_ashlv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vshlq_n_s16 (int16x8_t __a, const int __b)
{
  return (int16x8_t) __builtin_aarch64_ashlv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vshlq_n_s32 (int32x4_t __a, const int __b)
{
  return (int32x4_t) __builtin_aarch64_ashlv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vshlq_n_s64 (int64x2_t __a, const int __b)
{
  return (int64x2_t) __builtin_aarch64_ashlv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vshlq_n_u8 (uint8x16_t __a, const int __b)
{
  return (uint8x16_t) __builtin_aarch64_ashlv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vshlq_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint16x8_t) __builtin_aarch64_ashlv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vshlq_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint32x4_t) __builtin_aarch64_ashlv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vshlq_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint64x2_t) __builtin_aarch64_ashlv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vshld_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t) __builtin_aarch64_ashldi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vshld_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t) __builtin_aarch64_ashldi (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vshl_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t) __builtin_aarch64_sshlv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vshl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_sshlv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vshl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_sshlv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vshl_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_sshldi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vshl_u8 (uint8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_ushlv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vshl_u16 (uint16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_ushlv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vshl_u32 (uint32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_ushlv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vshl_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_ushldi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t) __builtin_aarch64_sshlv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_sshlv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_sshlv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vshlq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t) __builtin_aarch64_sshlv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_ushlv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_ushlv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_ushlv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vshlq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_ushlv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vshld_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_sshldi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vshld_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_ushldi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vshll_high_n_s8 (int8x16_t __a, const int __b)
{
  return __builtin_aarch64_sshll2_nv16qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vshll_high_n_s16 (int16x8_t __a, const int __b)
{
  return __builtin_aarch64_sshll2_nv8hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vshll_high_n_s32 (int32x4_t __a, const int __b)
{
  return __builtin_aarch64_sshll2_nv4si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vshll_high_n_u8 (uint8x16_t __a, const int __b)
{
  return (uint16x8_t) __builtin_aarch64_ushll2_nv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vshll_high_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint32x4_t) __builtin_aarch64_ushll2_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vshll_high_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint64x2_t) __builtin_aarch64_ushll2_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vshll_n_s8 (int8x8_t __a, const int __b)
{
  return __builtin_aarch64_sshll_nv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vshll_n_s16 (int16x4_t __a, const int __b)
{
  return __builtin_aarch64_sshll_nv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vshll_n_s32 (int32x2_t __a, const int __b)
{
  return __builtin_aarch64_sshll_nv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vshll_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint16x8_t) __builtin_aarch64_ushll_nv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vshll_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint32x4_t) __builtin_aarch64_ushll_nv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vshll_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint64x2_t) __builtin_aarch64_ushll_nv2si ((int32x2_t) __a, __b);
}

/* vshr */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vshr_n_s8 (int8x8_t __a, const int __b)
{
  return (int8x8_t) __builtin_aarch64_ashrv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vshr_n_s16 (int16x4_t __a, const int __b)
{
  return (int16x4_t) __builtin_aarch64_ashrv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vshr_n_s32 (int32x2_t __a, const int __b)
{
  return (int32x2_t) __builtin_aarch64_ashrv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vshr_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t) __builtin_aarch64_ashrdi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vshr_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x8_t) __builtin_aarch64_lshrv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vshr_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x4_t) __builtin_aarch64_lshrv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vshr_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x2_t) __builtin_aarch64_lshrv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vshr_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t) __builtin_aarch64_lshrdi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vshrq_n_s8 (int8x16_t __a, const int __b)
{
  return (int8x16_t) __builtin_aarch64_ashrv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vshrq_n_s16 (int16x8_t __a, const int __b)
{
  return (int16x8_t) __builtin_aarch64_ashrv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vshrq_n_s32 (int32x4_t __a, const int __b)
{
  return (int32x4_t) __builtin_aarch64_ashrv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vshrq_n_s64 (int64x2_t __a, const int __b)
{
  return (int64x2_t) __builtin_aarch64_ashrv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vshrq_n_u8 (uint8x16_t __a, const int __b)
{
  return (uint8x16_t) __builtin_aarch64_lshrv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vshrq_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint16x8_t) __builtin_aarch64_lshrv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vshrq_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint32x4_t) __builtin_aarch64_lshrv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vshrq_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint64x2_t) __builtin_aarch64_lshrv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vshrd_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t) __builtin_aarch64_ashrdi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vshrd_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t) __builtin_aarch64_lshrdi (__a, __b);
}

/* vsli */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vsli_n_s8 (int8x8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t) __builtin_aarch64_ssli_nv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vsli_n_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t) __builtin_aarch64_ssli_nv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vsli_n_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t) __builtin_aarch64_ssli_nv2si (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsli_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t) __builtin_aarch64_ssli_ndi (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsli_n_u8 (uint8x8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t) __builtin_aarch64_usli_nv8qi ((int8x8_t) __a,
						   (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsli_n_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t) __builtin_aarch64_usli_nv4hi ((int16x4_t) __a,
						    (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsli_n_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t) __builtin_aarch64_usli_nv2si ((int32x2_t) __a,
						    (int32x2_t) __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsli_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t) __builtin_aarch64_usli_ndi ((int64x1_t) __a,
						  (int64x1_t) __b, __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vsliq_n_s8 (int8x16_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t) __builtin_aarch64_ssli_nv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsliq_n_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t) __builtin_aarch64_ssli_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsliq_n_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t) __builtin_aarch64_ssli_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsliq_n_s64 (int64x2_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t) __builtin_aarch64_ssli_nv2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsliq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t) __builtin_aarch64_usli_nv16qi ((int8x16_t) __a,
						     (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsliq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t) __builtin_aarch64_usli_nv8hi ((int16x8_t) __a,
						    (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsliq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t) __builtin_aarch64_usli_nv4si ((int32x4_t) __a,
						    (int32x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsliq_n_u64 (uint64x2_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t) __builtin_aarch64_usli_nv2di ((int64x2_t) __a,
						    (int64x2_t) __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vslid_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t) __builtin_aarch64_ssli_ndi (__a, __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vslid_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t) __builtin_aarch64_usli_ndi (__a, __b, __c);
}

/* vsqadd */

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsqadd_u8 (uint8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_usqaddv8qi ((int8x8_t) __a,
						   (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsqadd_u16 (uint16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_usqaddv4hi ((int16x4_t) __a,
						    (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsqadd_u32 (uint32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_usqaddv2si ((int32x2_t) __a,
						    (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsqadd_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_usqadddi ((int64x1_t) __a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsqaddq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_usqaddv16qi ((int8x16_t) __a,
						     (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsqaddq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_usqaddv8hi ((int16x8_t) __a,
						    (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsqaddq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_usqaddv4si ((int32x4_t) __a,
						    (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsqaddq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_usqaddv2di ((int64x2_t) __a,
						    (int64x2_t) __b);
}

__extension__ static __inline uint8x1_t __attribute__ ((__always_inline__))
vsqaddb_u8 (uint8x1_t __a, int8x1_t __b)
{
  return (uint8x1_t) __builtin_aarch64_usqaddqi ((int8x1_t) __a, __b);
}

__extension__ static __inline uint16x1_t __attribute__ ((__always_inline__))
vsqaddh_u16 (uint16x1_t __a, int16x1_t __b)
{
  return (uint16x1_t) __builtin_aarch64_usqaddhi ((int16x1_t) __a, __b);
}

__extension__ static __inline uint32x1_t __attribute__ ((__always_inline__))
vsqadds_u32 (uint32x1_t __a, int32x1_t __b)
{
  return (uint32x1_t) __builtin_aarch64_usqaddsi ((int32x1_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsqaddd_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t) __builtin_aarch64_usqadddi ((int64x1_t) __a, __b);
}

/* vsqrt */
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vsqrt_f32 (float32x2_t a)
{
  return __builtin_aarch64_sqrtv2sf (a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vsqrtq_f32 (float32x4_t a)
{
  return __builtin_aarch64_sqrtv4sf (a);
}

__extension__ static __inline float64x2_t __attribute__ ((__always_inline__))
vsqrtq_f64 (float64x2_t a)
{
  return __builtin_aarch64_sqrtv2df (a);
}

/* vsra */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vsra_n_s8 (int8x8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t) __builtin_aarch64_ssra_nv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vsra_n_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t) __builtin_aarch64_ssra_nv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vsra_n_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t) __builtin_aarch64_ssra_nv2si (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsra_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t) __builtin_aarch64_ssra_ndi (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsra_n_u8 (uint8x8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t) __builtin_aarch64_usra_nv8qi ((int8x8_t) __a,
						   (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsra_n_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t) __builtin_aarch64_usra_nv4hi ((int16x4_t) __a,
						    (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsra_n_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t) __builtin_aarch64_usra_nv2si ((int32x2_t) __a,
						    (int32x2_t) __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsra_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t) __builtin_aarch64_usra_ndi ((int64x1_t) __a,
						  (int64x1_t) __b, __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vsraq_n_s8 (int8x16_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t) __builtin_aarch64_ssra_nv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsraq_n_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t) __builtin_aarch64_ssra_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsraq_n_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t) __builtin_aarch64_ssra_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsraq_n_s64 (int64x2_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t) __builtin_aarch64_ssra_nv2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsraq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t) __builtin_aarch64_usra_nv16qi ((int8x16_t) __a,
						     (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsraq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t) __builtin_aarch64_usra_nv8hi ((int16x8_t) __a,
						    (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsraq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t) __builtin_aarch64_usra_nv4si ((int32x4_t) __a,
						    (int32x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsraq_n_u64 (uint64x2_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t) __builtin_aarch64_usra_nv2di ((int64x2_t) __a,
						    (int64x2_t) __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsrad_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t) __builtin_aarch64_ssra_ndi (__a, __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsrad_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t) __builtin_aarch64_usra_ndi (__a, __b, __c);
}

/* vsri */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vsri_n_s8 (int8x8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t) __builtin_aarch64_ssri_nv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vsri_n_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t) __builtin_aarch64_ssri_nv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vsri_n_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t) __builtin_aarch64_ssri_nv2si (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsri_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t) __builtin_aarch64_ssri_ndi (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsri_n_u8 (uint8x8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t) __builtin_aarch64_usri_nv8qi ((int8x8_t) __a,
						   (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsri_n_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t) __builtin_aarch64_usri_nv4hi ((int16x4_t) __a,
						    (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsri_n_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t) __builtin_aarch64_usri_nv2si ((int32x2_t) __a,
						    (int32x2_t) __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsri_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t) __builtin_aarch64_usri_ndi ((int64x1_t) __a,
						  (int64x1_t) __b, __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vsriq_n_s8 (int8x16_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t) __builtin_aarch64_ssri_nv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsriq_n_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t) __builtin_aarch64_ssri_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsriq_n_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t) __builtin_aarch64_ssri_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsriq_n_s64 (int64x2_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t) __builtin_aarch64_ssri_nv2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsriq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t) __builtin_aarch64_usri_nv16qi ((int8x16_t) __a,
						     (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsriq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t) __builtin_aarch64_usri_nv8hi ((int16x8_t) __a,
						    (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsriq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t) __builtin_aarch64_usri_nv4si ((int32x4_t) __a,
						    (int32x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsriq_n_u64 (uint64x2_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t) __builtin_aarch64_usri_nv2di ((int64x2_t) __a,
						    (int64x2_t) __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsrid_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t) __builtin_aarch64_ssri_ndi (__a, __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsrid_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t) __builtin_aarch64_usri_ndi (__a, __b, __c);
}

/* vst1 */

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_f32 (float32_t *a, float32x2_t b)
{
  __builtin_aarch64_st1v2sf ((__builtin_aarch64_simd_sf *) a, b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_f64 (float64_t *a, float64x1_t b)
{
  *a = b;
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_p8 (poly8_t *a, poly8x8_t b)
{
  __builtin_aarch64_st1v8qi ((__builtin_aarch64_simd_qi *) a,
			     (int8x8_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_p16 (poly16_t *a, poly16x4_t b)
{
  __builtin_aarch64_st1v4hi ((__builtin_aarch64_simd_hi *) a,
			     (int16x4_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_s8 (int8_t *a, int8x8_t b)
{
  __builtin_aarch64_st1v8qi ((__builtin_aarch64_simd_qi *) a, b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_s16 (int16_t *a, int16x4_t b)
{
  __builtin_aarch64_st1v4hi ((__builtin_aarch64_simd_hi *) a, b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_s32 (int32_t *a, int32x2_t b)
{
  __builtin_aarch64_st1v2si ((__builtin_aarch64_simd_si *) a, b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_s64 (int64_t *a, int64x1_t b)
{
  *a = b;
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_u8 (uint8_t *a, uint8x8_t b)
{
  __builtin_aarch64_st1v8qi ((__builtin_aarch64_simd_qi *) a,
			     (int8x8_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_u16 (uint16_t *a, uint16x4_t b)
{
  __builtin_aarch64_st1v4hi ((__builtin_aarch64_simd_hi *) a,
			     (int16x4_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_u32 (uint32_t *a, uint32x2_t b)
{
  __builtin_aarch64_st1v2si ((__builtin_aarch64_simd_si *) a,
			     (int32x2_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_u64 (uint64_t *a, uint64x1_t b)
{
  *a = b;
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_f32 (float32_t *a, float32x4_t b)
{
  __builtin_aarch64_st1v4sf ((__builtin_aarch64_simd_sf *) a, b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_f64 (float64_t *a, float64x2_t b)
{
  __builtin_aarch64_st1v2df ((__builtin_aarch64_simd_df *) a, b);
}

/* vst1q */

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_p8 (poly8_t *a, poly8x16_t b)
{
  __builtin_aarch64_st1v16qi ((__builtin_aarch64_simd_qi *) a,
			      (int8x16_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_p16 (poly16_t *a, poly16x8_t b)
{
  __builtin_aarch64_st1v8hi ((__builtin_aarch64_simd_hi *) a,
			     (int16x8_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_s8 (int8_t *a, int8x16_t b)
{
  __builtin_aarch64_st1v16qi ((__builtin_aarch64_simd_qi *) a, b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_s16 (int16_t *a, int16x8_t b)
{
  __builtin_aarch64_st1v8hi ((__builtin_aarch64_simd_hi *) a, b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_s32 (int32_t *a, int32x4_t b)
{
  __builtin_aarch64_st1v4si ((__builtin_aarch64_simd_si *) a, b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_s64 (int64_t *a, int64x2_t b)
{
  __builtin_aarch64_st1v2di ((__builtin_aarch64_simd_di *) a, b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_u8 (uint8_t *a, uint8x16_t b)
{
  __builtin_aarch64_st1v16qi ((__builtin_aarch64_simd_qi *) a,
			      (int8x16_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_u16 (uint16_t *a, uint16x8_t b)
{
  __builtin_aarch64_st1v8hi ((__builtin_aarch64_simd_hi *) a,
			     (int16x8_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_u32 (uint32_t *a, uint32x4_t b)
{
  __builtin_aarch64_st1v4si ((__builtin_aarch64_simd_si *) a,
			     (int32x4_t) b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_u64 (uint64_t *a, uint64x2_t b)
{
  __builtin_aarch64_st1v2di ((__builtin_aarch64_simd_di *) a,
			     (int64x2_t) b);
}

/* vstn */

__extension__ static __inline void
vst2_s64 (int64_t * __a, int64x1x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  int64x2x2_t temp;
  temp.val[0] = vcombine_s64 (val.val[0], vcreate_s64 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s64 (val.val[1], vcreate_s64 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv2di (__o, (int64x2_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv2di (__o, (int64x2_t) temp.val[1], 1);
  __builtin_aarch64_st2di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void
vst2_u64 (uint64_t * __a, uint64x1x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  uint64x2x2_t temp;
  temp.val[0] = vcombine_u64 (val.val[0], vcreate_u64 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u64 (val.val[1], vcreate_u64 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv2di (__o, (int64x2_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv2di (__o, (int64x2_t) temp.val[1], 1);
  __builtin_aarch64_st2di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void
vst2_f64 (float64_t * __a, float64x1x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  float64x2x2_t temp;
  temp.val[0] = vcombine_f64 (val.val[0], vcreate_f64 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_f64 (val.val[1], vcreate_f64 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv2df (__o, (float64x2_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv2df (__o, (float64x2_t) temp.val[1], 1);
  __builtin_aarch64_st2df ((__builtin_aarch64_simd_df *) __a, __o);
}

__extension__ static __inline void
vst2_s8 (int8_t * __a, int8x8x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  int8x16x2_t temp;
  temp.val[0] = vcombine_s8 (val.val[0], vcreate_s8 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s8 (val.val[1], vcreate_s8 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) temp.val[1], 1);
  __builtin_aarch64_st2v8qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_p8 (poly8_t * __a, poly8x8x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  poly8x16x2_t temp;
  temp.val[0] = vcombine_p8 (val.val[0], vcreate_p8 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_p8 (val.val[1], vcreate_p8 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) temp.val[1], 1);
  __builtin_aarch64_st2v8qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_s16 (int16_t * __a, int16x4x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  int16x8x2_t temp;
  temp.val[0] = vcombine_s16 (val.val[0], vcreate_s16 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s16 (val.val[1], vcreate_s16 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) temp.val[1], 1);
  __builtin_aarch64_st2v4hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_p16 (poly16_t * __a, poly16x4x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  poly16x8x2_t temp;
  temp.val[0] = vcombine_p16 (val.val[0], vcreate_p16 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_p16 (val.val[1], vcreate_p16 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) temp.val[1], 1);
  __builtin_aarch64_st2v4hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_s32 (int32_t * __a, int32x2x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  int32x4x2_t temp;
  temp.val[0] = vcombine_s32 (val.val[0], vcreate_s32 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s32 (val.val[1], vcreate_s32 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv4si (__o, (int32x4_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv4si (__o, (int32x4_t) temp.val[1], 1);
  __builtin_aarch64_st2v2si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_u8 (uint8_t * __a, uint8x8x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  uint8x16x2_t temp;
  temp.val[0] = vcombine_u8 (val.val[0], vcreate_u8 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u8 (val.val[1], vcreate_u8 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) temp.val[1], 1);
  __builtin_aarch64_st2v8qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_u16 (uint16_t * __a, uint16x4x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  uint16x8x2_t temp;
  temp.val[0] = vcombine_u16 (val.val[0], vcreate_u16 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u16 (val.val[1], vcreate_u16 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) temp.val[1], 1);
  __builtin_aarch64_st2v4hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_u32 (uint32_t * __a, uint32x2x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  uint32x4x2_t temp;
  temp.val[0] = vcombine_u32 (val.val[0], vcreate_u32 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u32 (val.val[1], vcreate_u32 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv4si (__o, (int32x4_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv4si (__o, (int32x4_t) temp.val[1], 1);
  __builtin_aarch64_st2v2si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_f32 (float32_t * __a, float32x2x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  float32x4x2_t temp;
  temp.val[0] = vcombine_f32 (val.val[0], vcreate_f32 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_f32 (val.val[1], vcreate_f32 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregoiv4sf (__o, (float32x4_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv4sf (__o, (float32x4_t) temp.val[1], 1);
  __builtin_aarch64_st2v2sf ((__builtin_aarch64_simd_sf *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_s8 (int8_t * __a, int8x16x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) val.val[1], 1);
  __builtin_aarch64_st2v16qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_p8 (poly8_t * __a, poly8x16x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) val.val[1], 1);
  __builtin_aarch64_st2v16qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_s16 (int16_t * __a, int16x8x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) val.val[1], 1);
  __builtin_aarch64_st2v8hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_p16 (poly16_t * __a, poly16x8x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) val.val[1], 1);
  __builtin_aarch64_st2v8hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_s32 (int32_t * __a, int32x4x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv4si (__o, (int32x4_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv4si (__o, (int32x4_t) val.val[1], 1);
  __builtin_aarch64_st2v4si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_s64 (int64_t * __a, int64x2x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv2di (__o, (int64x2_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv2di (__o, (int64x2_t) val.val[1], 1);
  __builtin_aarch64_st2v2di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_u8 (uint8_t * __a, uint8x16x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv16qi (__o, (int8x16_t) val.val[1], 1);
  __builtin_aarch64_st2v16qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_u16 (uint16_t * __a, uint16x8x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv8hi (__o, (int16x8_t) val.val[1], 1);
  __builtin_aarch64_st2v8hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_u32 (uint32_t * __a, uint32x4x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv4si (__o, (int32x4_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv4si (__o, (int32x4_t) val.val[1], 1);
  __builtin_aarch64_st2v4si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_u64 (uint64_t * __a, uint64x2x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv2di (__o, (int64x2_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv2di (__o, (int64x2_t) val.val[1], 1);
  __builtin_aarch64_st2v2di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_f32 (float32_t * __a, float32x4x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv4sf (__o, (float32x4_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv4sf (__o, (float32x4_t) val.val[1], 1);
  __builtin_aarch64_st2v4sf ((__builtin_aarch64_simd_sf *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_f64 (float64_t * __a, float64x2x2_t val)
{
  __builtin_aarch64_simd_oi __o;
  __o = __builtin_aarch64_set_qregoiv2df (__o, (float64x2_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregoiv2df (__o, (float64x2_t) val.val[1], 1);
  __builtin_aarch64_st2v2df ((__builtin_aarch64_simd_df *) __a, __o);
}

__extension__ static __inline void
vst3_s64 (int64_t * __a, int64x1x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  int64x2x3_t temp;
  temp.val[0] = vcombine_s64 (val.val[0], vcreate_s64 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s64 (val.val[1], vcreate_s64 (__AARCH64_INT64_C (0)));
  temp.val[2] = vcombine_s64 (val.val[2], vcreate_s64 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) temp.val[2], 2);
  __builtin_aarch64_st3di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void
vst3_u64 (uint64_t * __a, uint64x1x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  uint64x2x3_t temp;
  temp.val[0] = vcombine_u64 (val.val[0], vcreate_u64 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u64 (val.val[1], vcreate_u64 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_u64 (val.val[2], vcreate_u64 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) temp.val[2], 2);
  __builtin_aarch64_st3di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void
vst3_f64 (float64_t * __a, float64x1x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  float64x2x3_t temp;
  temp.val[0] = vcombine_f64 (val.val[0], vcreate_f64 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_f64 (val.val[1], vcreate_f64 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_f64 (val.val[2], vcreate_f64 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregciv2df (__o, (float64x2_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv2df (__o, (float64x2_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv2df (__o, (float64x2_t) temp.val[2], 2);
  __builtin_aarch64_st3df ((__builtin_aarch64_simd_df *) __a, __o);
}

__extension__ static __inline void
vst3_s8 (int8_t * __a, int8x8x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  int8x16x3_t temp;
  temp.val[0] = vcombine_s8 (val.val[0], vcreate_s8 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s8 (val.val[1], vcreate_s8 (__AARCH64_INT64_C (0)));
  temp.val[2] = vcombine_s8 (val.val[2], vcreate_s8 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) temp.val[2], 2);
  __builtin_aarch64_st3v8qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_p8 (poly8_t * __a, poly8x8x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  poly8x16x3_t temp;
  temp.val[0] = vcombine_p8 (val.val[0], vcreate_p8 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_p8 (val.val[1], vcreate_p8 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_p8 (val.val[2], vcreate_p8 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) temp.val[2], 2);
  __builtin_aarch64_st3v8qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_s16 (int16_t * __a, int16x4x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  int16x8x3_t temp;
  temp.val[0] = vcombine_s16 (val.val[0], vcreate_s16 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s16 (val.val[1], vcreate_s16 (__AARCH64_INT64_C (0)));
  temp.val[2] = vcombine_s16 (val.val[2], vcreate_s16 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) temp.val[2], 2);
  __builtin_aarch64_st3v4hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_p16 (poly16_t * __a, poly16x4x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  poly16x8x3_t temp;
  temp.val[0] = vcombine_p16 (val.val[0], vcreate_p16 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_p16 (val.val[1], vcreate_p16 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_p16 (val.val[2], vcreate_p16 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) temp.val[2], 2);
  __builtin_aarch64_st3v4hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_s32 (int32_t * __a, int32x2x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  int32x4x3_t temp;
  temp.val[0] = vcombine_s32 (val.val[0], vcreate_s32 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s32 (val.val[1], vcreate_s32 (__AARCH64_INT64_C (0)));
  temp.val[2] = vcombine_s32 (val.val[2], vcreate_s32 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) temp.val[2], 2);
  __builtin_aarch64_st3v2si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_u8 (uint8_t * __a, uint8x8x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  uint8x16x3_t temp;
  temp.val[0] = vcombine_u8 (val.val[0], vcreate_u8 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u8 (val.val[1], vcreate_u8 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_u8 (val.val[2], vcreate_u8 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) temp.val[2], 2);
  __builtin_aarch64_st3v8qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_u16 (uint16_t * __a, uint16x4x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  uint16x8x3_t temp;
  temp.val[0] = vcombine_u16 (val.val[0], vcreate_u16 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u16 (val.val[1], vcreate_u16 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_u16 (val.val[2], vcreate_u16 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) temp.val[2], 2);
  __builtin_aarch64_st3v4hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_u32 (uint32_t * __a, uint32x2x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  uint32x4x3_t temp;
  temp.val[0] = vcombine_u32 (val.val[0], vcreate_u32 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u32 (val.val[1], vcreate_u32 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_u32 (val.val[2], vcreate_u32 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) temp.val[2], 2);
  __builtin_aarch64_st3v2si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_f32 (float32_t * __a, float32x2x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  float32x4x3_t temp;
  temp.val[0] = vcombine_f32 (val.val[0], vcreate_f32 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_f32 (val.val[1], vcreate_f32 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_f32 (val.val[2], vcreate_f32 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregciv4sf (__o, (float32x4_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregciv4sf (__o, (float32x4_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregciv4sf (__o, (float32x4_t) temp.val[2], 2);
  __builtin_aarch64_st3v2sf ((__builtin_aarch64_simd_sf *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_s8 (int8_t * __a, int8x16x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) val.val[2], 2);
  __builtin_aarch64_st3v16qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_p8 (poly8_t * __a, poly8x16x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) val.val[2], 2);
  __builtin_aarch64_st3v16qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_s16 (int16_t * __a, int16x8x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) val.val[2], 2);
  __builtin_aarch64_st3v8hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_p16 (poly16_t * __a, poly16x8x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) val.val[2], 2);
  __builtin_aarch64_st3v8hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_s32 (int32_t * __a, int32x4x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) val.val[2], 2);
  __builtin_aarch64_st3v4si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_s64 (int64_t * __a, int64x2x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) val.val[2], 2);
  __builtin_aarch64_st3v2di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_u8 (uint8_t * __a, uint8x16x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv16qi (__o, (int8x16_t) val.val[2], 2);
  __builtin_aarch64_st3v16qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_u16 (uint16_t * __a, uint16x8x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv8hi (__o, (int16x8_t) val.val[2], 2);
  __builtin_aarch64_st3v8hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_u32 (uint32_t * __a, uint32x4x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv4si (__o, (int32x4_t) val.val[2], 2);
  __builtin_aarch64_st3v4si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_u64 (uint64_t * __a, uint64x2x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv2di (__o, (int64x2_t) val.val[2], 2);
  __builtin_aarch64_st3v2di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_f32 (float32_t * __a, float32x4x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv4sf (__o, (float32x4_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv4sf (__o, (float32x4_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv4sf (__o, (float32x4_t) val.val[2], 2);
  __builtin_aarch64_st3v4sf ((__builtin_aarch64_simd_sf *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_f64 (float64_t * __a, float64x2x3_t val)
{
  __builtin_aarch64_simd_ci __o;
  __o = __builtin_aarch64_set_qregciv2df (__o, (float64x2_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregciv2df (__o, (float64x2_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregciv2df (__o, (float64x2_t) val.val[2], 2);
  __builtin_aarch64_st3v2df ((__builtin_aarch64_simd_df *) __a, __o);
}

__extension__ static __inline void
vst4_s64 (int64_t * __a, int64x1x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  int64x2x4_t temp;
  temp.val[0] = vcombine_s64 (val.val[0], vcreate_s64 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s64 (val.val[1], vcreate_s64 (__AARCH64_INT64_C (0)));
  temp.val[2] = vcombine_s64 (val.val[2], vcreate_s64 (__AARCH64_INT64_C (0)));
  temp.val[3] = vcombine_s64 (val.val[3], vcreate_s64 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) temp.val[3], 3);
  __builtin_aarch64_st4di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void
vst4_u64 (uint64_t * __a, uint64x1x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  uint64x2x4_t temp;
  temp.val[0] = vcombine_u64 (val.val[0], vcreate_u64 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u64 (val.val[1], vcreate_u64 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_u64 (val.val[2], vcreate_u64 (__AARCH64_UINT64_C (0)));
  temp.val[3] = vcombine_u64 (val.val[3], vcreate_u64 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) temp.val[3], 3);
  __builtin_aarch64_st4di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void
vst4_f64 (float64_t * __a, float64x1x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  float64x2x4_t temp;
  temp.val[0] = vcombine_f64 (val.val[0], vcreate_f64 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_f64 (val.val[1], vcreate_f64 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_f64 (val.val[2], vcreate_f64 (__AARCH64_UINT64_C (0)));
  temp.val[3] = vcombine_f64 (val.val[3], vcreate_f64 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv2df (__o, (float64x2_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv2df (__o, (float64x2_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv2df (__o, (float64x2_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv2df (__o, (float64x2_t) temp.val[3], 3);
  __builtin_aarch64_st4df ((__builtin_aarch64_simd_df *) __a, __o);
}

__extension__ static __inline void
vst4_s8 (int8_t * __a, int8x8x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  int8x16x4_t temp;
  temp.val[0] = vcombine_s8 (val.val[0], vcreate_s8 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s8 (val.val[1], vcreate_s8 (__AARCH64_INT64_C (0)));
  temp.val[2] = vcombine_s8 (val.val[2], vcreate_s8 (__AARCH64_INT64_C (0)));
  temp.val[3] = vcombine_s8 (val.val[3], vcreate_s8 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[3], 3);
  __builtin_aarch64_st4v8qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_p8 (poly8_t * __a, poly8x8x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  poly8x16x4_t temp;
  temp.val[0] = vcombine_p8 (val.val[0], vcreate_p8 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_p8 (val.val[1], vcreate_p8 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_p8 (val.val[2], vcreate_p8 (__AARCH64_UINT64_C (0)));
  temp.val[3] = vcombine_p8 (val.val[3], vcreate_p8 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[3], 3);
  __builtin_aarch64_st4v8qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_s16 (int16_t * __a, int16x4x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  int16x8x4_t temp;
  temp.val[0] = vcombine_s16 (val.val[0], vcreate_s16 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s16 (val.val[1], vcreate_s16 (__AARCH64_INT64_C (0)));
  temp.val[2] = vcombine_s16 (val.val[2], vcreate_s16 (__AARCH64_INT64_C (0)));
  temp.val[3] = vcombine_s16 (val.val[3], vcreate_s16 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[3], 3);
  __builtin_aarch64_st4v4hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_p16 (poly16_t * __a, poly16x4x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  poly16x8x4_t temp;
  temp.val[0] = vcombine_p16 (val.val[0], vcreate_p16 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_p16 (val.val[1], vcreate_p16 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_p16 (val.val[2], vcreate_p16 (__AARCH64_UINT64_C (0)));
  temp.val[3] = vcombine_p16 (val.val[3], vcreate_p16 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[3], 3);
  __builtin_aarch64_st4v4hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_s32 (int32_t * __a, int32x2x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  int32x4x4_t temp;
  temp.val[0] = vcombine_s32 (val.val[0], vcreate_s32 (__AARCH64_INT64_C (0)));
  temp.val[1] = vcombine_s32 (val.val[1], vcreate_s32 (__AARCH64_INT64_C (0)));
  temp.val[2] = vcombine_s32 (val.val[2], vcreate_s32 (__AARCH64_INT64_C (0)));
  temp.val[3] = vcombine_s32 (val.val[3], vcreate_s32 (__AARCH64_INT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) temp.val[3], 3);
  __builtin_aarch64_st4v2si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_u8 (uint8_t * __a, uint8x8x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  uint8x16x4_t temp;
  temp.val[0] = vcombine_u8 (val.val[0], vcreate_u8 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u8 (val.val[1], vcreate_u8 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_u8 (val.val[2], vcreate_u8 (__AARCH64_UINT64_C (0)));
  temp.val[3] = vcombine_u8 (val.val[3], vcreate_u8 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) temp.val[3], 3);
  __builtin_aarch64_st4v8qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_u16 (uint16_t * __a, uint16x4x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  uint16x8x4_t temp;
  temp.val[0] = vcombine_u16 (val.val[0], vcreate_u16 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u16 (val.val[1], vcreate_u16 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_u16 (val.val[2], vcreate_u16 (__AARCH64_UINT64_C (0)));
  temp.val[3] = vcombine_u16 (val.val[3], vcreate_u16 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) temp.val[3], 3);
  __builtin_aarch64_st4v4hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_u32 (uint32_t * __a, uint32x2x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  uint32x4x4_t temp;
  temp.val[0] = vcombine_u32 (val.val[0], vcreate_u32 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_u32 (val.val[1], vcreate_u32 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_u32 (val.val[2], vcreate_u32 (__AARCH64_UINT64_C (0)));
  temp.val[3] = vcombine_u32 (val.val[3], vcreate_u32 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) temp.val[3], 3);
  __builtin_aarch64_st4v2si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_f32 (float32_t * __a, float32x2x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  float32x4x4_t temp;
  temp.val[0] = vcombine_f32 (val.val[0], vcreate_f32 (__AARCH64_UINT64_C (0)));
  temp.val[1] = vcombine_f32 (val.val[1], vcreate_f32 (__AARCH64_UINT64_C (0)));
  temp.val[2] = vcombine_f32 (val.val[2], vcreate_f32 (__AARCH64_UINT64_C (0)));
  temp.val[3] = vcombine_f32 (val.val[3], vcreate_f32 (__AARCH64_UINT64_C (0)));
  __o = __builtin_aarch64_set_qregxiv4sf (__o, (float32x4_t) temp.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv4sf (__o, (float32x4_t) temp.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv4sf (__o, (float32x4_t) temp.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv4sf (__o, (float32x4_t) temp.val[3], 3);
  __builtin_aarch64_st4v2sf ((__builtin_aarch64_simd_sf *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_s8 (int8_t * __a, int8x16x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[3], 3);
  __builtin_aarch64_st4v16qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_p8 (poly8_t * __a, poly8x16x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[3], 3);
  __builtin_aarch64_st4v16qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_s16 (int16_t * __a, int16x8x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[3], 3);
  __builtin_aarch64_st4v8hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_p16 (poly16_t * __a, poly16x8x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[3], 3);
  __builtin_aarch64_st4v8hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_s32 (int32_t * __a, int32x4x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) val.val[3], 3);
  __builtin_aarch64_st4v4si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_s64 (int64_t * __a, int64x2x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) val.val[3], 3);
  __builtin_aarch64_st4v2di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_u8 (uint8_t * __a, uint8x16x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv16qi (__o, (int8x16_t) val.val[3], 3);
  __builtin_aarch64_st4v16qi ((__builtin_aarch64_simd_qi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_u16 (uint16_t * __a, uint16x8x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv8hi (__o, (int16x8_t) val.val[3], 3);
  __builtin_aarch64_st4v8hi ((__builtin_aarch64_simd_hi *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_u32 (uint32_t * __a, uint32x4x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv4si (__o, (int32x4_t) val.val[3], 3);
  __builtin_aarch64_st4v4si ((__builtin_aarch64_simd_si *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_u64 (uint64_t * __a, uint64x2x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv2di (__o, (int64x2_t) val.val[3], 3);
  __builtin_aarch64_st4v2di ((__builtin_aarch64_simd_di *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_f32 (float32_t * __a, float32x4x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv4sf (__o, (float32x4_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv4sf (__o, (float32x4_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv4sf (__o, (float32x4_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv4sf (__o, (float32x4_t) val.val[3], 3);
  __builtin_aarch64_st4v4sf ((__builtin_aarch64_simd_sf *) __a, __o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_f64 (float64_t * __a, float64x2x4_t val)
{
  __builtin_aarch64_simd_xi __o;
  __o = __builtin_aarch64_set_qregxiv2df (__o, (float64x2_t) val.val[0], 0);
  __o = __builtin_aarch64_set_qregxiv2df (__o, (float64x2_t) val.val[1], 1);
  __o = __builtin_aarch64_set_qregxiv2df (__o, (float64x2_t) val.val[2], 2);
  __o = __builtin_aarch64_set_qregxiv2df (__o, (float64x2_t) val.val[3], 3);
  __builtin_aarch64_st4v2df ((__builtin_aarch64_simd_df *) __a, __o);
}

/* vsub */

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsubd_s64 (int64x1_t __a, int64x1_t __b)
{
  return __a - __b;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsubd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return __a - __b;
}

/* vtrn */

__extension__ static __inline float32x2x2_t __attribute__ ((__always_inline__))
vtrn_f32 (float32x2_t a, float32x2_t b)
{
  return (float32x2x2_t) {vtrn1_f32 (a, b), vtrn2_f32 (a, b)};
}

__extension__ static __inline poly8x8x2_t __attribute__ ((__always_inline__))
vtrn_p8 (poly8x8_t a, poly8x8_t b)
{
  return (poly8x8x2_t) {vtrn1_p8 (a, b), vtrn2_p8 (a, b)};
}

__extension__ static __inline poly16x4x2_t __attribute__ ((__always_inline__))
vtrn_p16 (poly16x4_t a, poly16x4_t b)
{
  return (poly16x4x2_t) {vtrn1_p16 (a, b), vtrn2_p16 (a, b)};
}

__extension__ static __inline int8x8x2_t __attribute__ ((__always_inline__))
vtrn_s8 (int8x8_t a, int8x8_t b)
{
  return (int8x8x2_t) {vtrn1_s8 (a, b), vtrn2_s8 (a, b)};
}

__extension__ static __inline int16x4x2_t __attribute__ ((__always_inline__))
vtrn_s16 (int16x4_t a, int16x4_t b)
{
  return (int16x4x2_t) {vtrn1_s16 (a, b), vtrn2_s16 (a, b)};
}

__extension__ static __inline int32x2x2_t __attribute__ ((__always_inline__))
vtrn_s32 (int32x2_t a, int32x2_t b)
{
  return (int32x2x2_t) {vtrn1_s32 (a, b), vtrn2_s32 (a, b)};
}

__extension__ static __inline uint8x8x2_t __attribute__ ((__always_inline__))
vtrn_u8 (uint8x8_t a, uint8x8_t b)
{
  return (uint8x8x2_t) {vtrn1_u8 (a, b), vtrn2_u8 (a, b)};
}

__extension__ static __inline uint16x4x2_t __attribute__ ((__always_inline__))
vtrn_u16 (uint16x4_t a, uint16x4_t b)
{
  return (uint16x4x2_t) {vtrn1_u16 (a, b), vtrn2_u16 (a, b)};
}

__extension__ static __inline uint32x2x2_t __attribute__ ((__always_inline__))
vtrn_u32 (uint32x2_t a, uint32x2_t b)
{
  return (uint32x2x2_t) {vtrn1_u32 (a, b), vtrn2_u32 (a, b)};
}

__extension__ static __inline float32x4x2_t __attribute__ ((__always_inline__))
vtrnq_f32 (float32x4_t a, float32x4_t b)
{
  return (float32x4x2_t) {vtrn1q_f32 (a, b), vtrn2q_f32 (a, b)};
}

__extension__ static __inline poly8x16x2_t __attribute__ ((__always_inline__))
vtrnq_p8 (poly8x16_t a, poly8x16_t b)
{
  return (poly8x16x2_t) {vtrn1q_p8 (a, b), vtrn2q_p8 (a, b)};
}

__extension__ static __inline poly16x8x2_t __attribute__ ((__always_inline__))
vtrnq_p16 (poly16x8_t a, poly16x8_t b)
{
  return (poly16x8x2_t) {vtrn1q_p16 (a, b), vtrn2q_p16 (a, b)};
}

__extension__ static __inline int8x16x2_t __attribute__ ((__always_inline__))
vtrnq_s8 (int8x16_t a, int8x16_t b)
{
  return (int8x16x2_t) {vtrn1q_s8 (a, b), vtrn2q_s8 (a, b)};
}

__extension__ static __inline int16x8x2_t __attribute__ ((__always_inline__))
vtrnq_s16 (int16x8_t a, int16x8_t b)
{
  return (int16x8x2_t) {vtrn1q_s16 (a, b), vtrn2q_s16 (a, b)};
}

__extension__ static __inline int32x4x2_t __attribute__ ((__always_inline__))
vtrnq_s32 (int32x4_t a, int32x4_t b)
{
  return (int32x4x2_t) {vtrn1q_s32 (a, b), vtrn2q_s32 (a, b)};
}

__extension__ static __inline uint8x16x2_t __attribute__ ((__always_inline__))
vtrnq_u8 (uint8x16_t a, uint8x16_t b)
{
  return (uint8x16x2_t) {vtrn1q_u8 (a, b), vtrn2q_u8 (a, b)};
}

__extension__ static __inline uint16x8x2_t __attribute__ ((__always_inline__))
vtrnq_u16 (uint16x8_t a, uint16x8_t b)
{
  return (uint16x8x2_t) {vtrn1q_u16 (a, b), vtrn2q_u16 (a, b)};
}

__extension__ static __inline uint32x4x2_t __attribute__ ((__always_inline__))
vtrnq_u32 (uint32x4_t a, uint32x4_t b)
{
  return (uint32x4x2_t) {vtrn1q_u32 (a, b), vtrn2q_u32 (a, b)};
}

/* vtst */

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtst_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmtstv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vtst_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmtstv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vtst_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmtstv2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vtst_s64 (int64x1_t __a, int64x1_t __b)
{
  return (__a & __b) ? -1ll : 0ll;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtst_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t) __builtin_aarch64_cmtstv8qi ((int8x8_t) __a,
						 (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vtst_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t) __builtin_aarch64_cmtstv4hi ((int16x4_t) __a,
						  (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vtst_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t) __builtin_aarch64_cmtstv2si ((int32x2_t) __a,
						  (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vtst_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (__a & __b) ? -1ll : 0ll;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vtstq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmtstv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vtstq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmtstv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vtstq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmtstv4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vtstq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmtstv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vtstq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t) __builtin_aarch64_cmtstv16qi ((int8x16_t) __a,
						   (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vtstq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t) __builtin_aarch64_cmtstv8hi ((int16x8_t) __a,
						  (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vtstq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t) __builtin_aarch64_cmtstv4si ((int32x4_t) __a,
						  (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vtstq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t) __builtin_aarch64_cmtstv2di ((int64x2_t) __a,
						  (int64x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vtstd_s64 (int64x1_t __a, int64x1_t __b)
{
  return (__a & __b) ? -1ll : 0ll;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vtstd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (__a & __b) ? -1ll : 0ll;
}

/* vuqadd */

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vuqadd_s8 (int8x8_t __a, uint8x8_t __b)
{
  return (int8x8_t) __builtin_aarch64_suqaddv8qi (__a, (int8x8_t) __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vuqadd_s16 (int16x4_t __a, uint16x4_t __b)
{
  return (int16x4_t) __builtin_aarch64_suqaddv4hi (__a, (int16x4_t) __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vuqadd_s32 (int32x2_t __a, uint32x2_t __b)
{
  return (int32x2_t) __builtin_aarch64_suqaddv2si (__a, (int32x2_t) __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vuqadd_s64 (int64x1_t __a, uint64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_suqadddi (__a, (int64x1_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vuqaddq_s8 (int8x16_t __a, uint8x16_t __b)
{
  return (int8x16_t) __builtin_aarch64_suqaddv16qi (__a, (int8x16_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vuqaddq_s16 (int16x8_t __a, uint16x8_t __b)
{
  return (int16x8_t) __builtin_aarch64_suqaddv8hi (__a, (int16x8_t) __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vuqaddq_s32 (int32x4_t __a, uint32x4_t __b)
{
  return (int32x4_t) __builtin_aarch64_suqaddv4si (__a, (int32x4_t) __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vuqaddq_s64 (int64x2_t __a, uint64x2_t __b)
{
  return (int64x2_t) __builtin_aarch64_suqaddv2di (__a, (int64x2_t) __b);
}

__extension__ static __inline int8x1_t __attribute__ ((__always_inline__))
vuqaddb_s8 (int8x1_t __a, uint8x1_t __b)
{
  return (int8x1_t) __builtin_aarch64_suqaddqi (__a, (int8x1_t) __b);
}

__extension__ static __inline int16x1_t __attribute__ ((__always_inline__))
vuqaddh_s16 (int16x1_t __a, uint16x1_t __b)
{
  return (int16x1_t) __builtin_aarch64_suqaddhi (__a, (int16x1_t) __b);
}

__extension__ static __inline int32x1_t __attribute__ ((__always_inline__))
vuqadds_s32 (int32x1_t __a, uint32x1_t __b)
{
  return (int32x1_t) __builtin_aarch64_suqaddsi (__a, (int32x1_t) __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vuqaddd_s64 (int64x1_t __a, uint64x1_t __b)
{
  return (int64x1_t) __builtin_aarch64_suqadddi (__a, (int64x1_t) __b);
}

#define __DEFINTERLEAVE(op, rettype, intype, funcsuffix, Q) 		\
  __extension__ static __inline rettype					\
  __attribute__ ((__always_inline__))					\
  v ## op ## Q ## _ ## funcsuffix (intype a, intype b)			\
  {									\
    return (rettype) {v ## op ## 1 ## Q ## _ ## funcsuffix (a, b),	\
		      v ## op ## 2 ## Q ## _ ## funcsuffix (a, b)};	\
  }

#define __INTERLEAVE_LIST(op)					\
  __DEFINTERLEAVE (op, float32x2x2_t, float32x2_t, f32,)	\
  __DEFINTERLEAVE (op, poly8x8x2_t, poly8x8_t, p8,)		\
  __DEFINTERLEAVE (op, poly16x4x2_t, poly16x4_t, p16,)		\
  __DEFINTERLEAVE (op, int8x8x2_t, int8x8_t, s8,)		\
  __DEFINTERLEAVE (op, int16x4x2_t, int16x4_t, s16,)		\
  __DEFINTERLEAVE (op, int32x2x2_t, int32x2_t, s32,)		\
  __DEFINTERLEAVE (op, uint8x8x2_t, uint8x8_t, u8,)		\
  __DEFINTERLEAVE (op, uint16x4x2_t, uint16x4_t, u16,)		\
  __DEFINTERLEAVE (op, uint32x2x2_t, uint32x2_t, u32,)		\
  __DEFINTERLEAVE (op, float32x4x2_t, float32x4_t, f32, q)	\
  __DEFINTERLEAVE (op, poly8x16x2_t, poly8x16_t, p8, q)		\
  __DEFINTERLEAVE (op, poly16x8x2_t, poly16x8_t, p16, q)	\
  __DEFINTERLEAVE (op, int8x16x2_t, int8x16_t, s8, q)		\
  __DEFINTERLEAVE (op, int16x8x2_t, int16x8_t, s16, q)		\
  __DEFINTERLEAVE (op, int32x4x2_t, int32x4_t, s32, q)		\
  __DEFINTERLEAVE (op, uint8x16x2_t, uint8x16_t, u8, q)		\
  __DEFINTERLEAVE (op, uint16x8x2_t, uint16x8_t, u16, q)	\
  __DEFINTERLEAVE (op, uint32x4x2_t, uint32x4_t, u32, q)

/* vuzp */

__INTERLEAVE_LIST (uzp)

/* vzip */

__INTERLEAVE_LIST (zip)

#undef __INTERLEAVE_LIST
#undef __DEFINTERLEAVE

/* End of optimal implementations in approved order.  */

#undef __LANE0

#undef __aarch64_vget_lane_any
#undef __aarch64_vget_lane_f32
#undef __aarch64_vget_lane_f64
#undef __aarch64_vget_lane_p8
#undef __aarch64_vget_lane_p16
#undef __aarch64_vget_lane_s8
#undef __aarch64_vget_lane_s16
#undef __aarch64_vget_lane_s32
#undef __aarch64_vget_lane_s64
#undef __aarch64_vget_lane_u8
#undef __aarch64_vget_lane_u16
#undef __aarch64_vget_lane_u32
#undef __aarch64_vget_lane_u64

#undef __aarch64_vgetq_lane_f32
#undef __aarch64_vgetq_lane_f64
#undef __aarch64_vgetq_lane_p8
#undef __aarch64_vgetq_lane_p16
#undef __aarch64_vgetq_lane_s8
#undef __aarch64_vgetq_lane_s16
#undef __aarch64_vgetq_lane_s32
#undef __aarch64_vgetq_lane_s64
#undef __aarch64_vgetq_lane_u8
#undef __aarch64_vgetq_lane_u16
#undef __aarch64_vgetq_lane_u32
#undef __aarch64_vgetq_lane_u64

#undef __aarch64_vdup_lane_any
#undef __aarch64_vdup_lane_f32
#undef __aarch64_vdup_lane_f64
#undef __aarch64_vdup_lane_p8
#undef __aarch64_vdup_lane_p16
#undef __aarch64_vdup_lane_s8
#undef __aarch64_vdup_lane_s16
#undef __aarch64_vdup_lane_s32
#undef __aarch64_vdup_lane_s64
#undef __aarch64_vdup_lane_u8
#undef __aarch64_vdup_lane_u16
#undef __aarch64_vdup_lane_u32
#undef __aarch64_vdup_lane_u64
#undef __aarch64_vdup_laneq_f32
#undef __aarch64_vdup_laneq_f64
#undef __aarch64_vdup_laneq_p8
#undef __aarch64_vdup_laneq_p16
#undef __aarch64_vdup_laneq_s8
#undef __aarch64_vdup_laneq_s16
#undef __aarch64_vdup_laneq_s32
#undef __aarch64_vdup_laneq_s64
#undef __aarch64_vdup_laneq_u8
#undef __aarch64_vdup_laneq_u16
#undef __aarch64_vdup_laneq_u32
#undef __aarch64_vdup_laneq_u64
#undef __aarch64_vdupq_lane_f32
#undef __aarch64_vdupq_lane_f64
#undef __aarch64_vdupq_lane_p8
#undef __aarch64_vdupq_lane_p16
#undef __aarch64_vdupq_lane_s8
#undef __aarch64_vdupq_lane_s16
#undef __aarch64_vdupq_lane_s32
#undef __aarch64_vdupq_lane_s64
#undef __aarch64_vdupq_lane_u8
#undef __aarch64_vdupq_lane_u16
#undef __aarch64_vdupq_lane_u32
#undef __aarch64_vdupq_lane_u64
#undef __aarch64_vdupq_laneq_f32
#undef __aarch64_vdupq_laneq_f64
#undef __aarch64_vdupq_laneq_p8
#undef __aarch64_vdupq_laneq_p16
#undef __aarch64_vdupq_laneq_s8
#undef __aarch64_vdupq_laneq_s16
#undef __aarch64_vdupq_laneq_s32
#undef __aarch64_vdupq_laneq_s64
#undef __aarch64_vdupq_laneq_u8
#undef __aarch64_vdupq_laneq_u16
#undef __aarch64_vdupq_laneq_u32
#undef __aarch64_vdupq_laneq_u64

#endif
