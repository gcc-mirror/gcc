/* Arm MVE intrinsics include file.

   Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef _GCC_ARM_MVE_TYPES_H
#define _GCC_ARM_MVE_TYPES_H

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */
typedef __fp16 float16_t;
typedef float float32_t;
typedef __simd128_float16_t float16x8_t;
typedef __simd128_float32_t float32x4_t;

typedef struct { float16x8_t val[2]; } float16x8x2_t;
typedef struct { float16x8_t val[4]; } float16x8x4_t;
typedef struct { float32x4_t val[2]; } float32x4x2_t;
typedef struct { float32x4_t val[4]; } float32x4x4_t;
#endif

typedef uint16_t mve_pred16_t;
typedef __simd128_uint8_t uint8x16_t;
typedef __simd128_uint16_t uint16x8_t;
typedef __simd128_uint32_t uint32x4_t;
typedef __simd128_uint64_t uint64x2_t;
typedef __simd128_int8_t int8x16_t;
typedef __simd128_int16_t int16x8_t;
typedef __simd128_int32_t int32x4_t;
typedef __simd128_int64_t int64x2_t;

typedef struct { int16x8_t val[2]; } int16x8x2_t;
typedef struct { int16x8_t val[4]; } int16x8x4_t;
typedef struct { int32x4_t val[2]; } int32x4x2_t;
typedef struct { int32x4_t val[4]; } int32x4x4_t;
typedef struct { int8x16_t val[2]; } int8x16x2_t;
typedef struct { int8x16_t val[4]; } int8x16x4_t;
typedef struct { uint16x8_t val[2]; } uint16x8x2_t;
typedef struct { uint16x8_t val[4]; } uint16x8x4_t;
typedef struct { uint32x4_t val[2]; } uint32x4x2_t;
typedef struct { uint32x4_t val[4]; } uint32x4x4_t;
typedef struct { uint8x16_t val[2]; } uint8x16x2_t;
typedef struct { uint8x16_t val[4]; } uint8x16x4_t;

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16_s32 (int32x4_t __a)
{
  return (int16x8_t)  __a;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16_s64 (int64x2_t __a)
{
  return (int16x8_t)  __a;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16_s8 (int8x16_t __a)
{
  return (int16x8_t)  __a;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16_u16 (uint16x8_t __a)
{
  return (int16x8_t)  __a;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16_u32 (uint32x4_t __a)
{
  return (int16x8_t)  __a;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16_u64 (uint64x2_t __a)
{
  return (int16x8_t)  __a;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16_u8 (uint8x16_t __a)
{
  return (int16x8_t)  __a;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32_s16 (int16x8_t __a)
{
  return (int32x4_t)  __a;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32_s64 (int64x2_t __a)
{
  return (int32x4_t)  __a;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32_s8 (int8x16_t __a)
{
  return (int32x4_t)  __a;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32_u16 (uint16x8_t __a)
{
  return (int32x4_t)  __a;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32_u32 (uint32x4_t __a)
{
  return (int32x4_t)  __a;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32_u64 (uint64x2_t __a)
{
  return (int32x4_t)  __a;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32_u8 (uint8x16_t __a)
{
  return (int32x4_t)  __a;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64_s16 (int16x8_t __a)
{
  return (int64x2_t)  __a;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64_s32 (int32x4_t __a)
{
  return (int64x2_t)  __a;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64_s8 (int8x16_t __a)
{
  return (int64x2_t)  __a;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64_u16 (uint16x8_t __a)
{
  return (int64x2_t)  __a;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64_u32 (uint32x4_t __a)
{
  return (int64x2_t)  __a;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64_u64 (uint64x2_t __a)
{
  return (int64x2_t)  __a;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64_u8 (uint8x16_t __a)
{
  return (int64x2_t)  __a;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8_s16 (int16x8_t __a)
{
  return (int8x16_t)  __a;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8_s32 (int32x4_t __a)
{
  return (int8x16_t)  __a;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8_s64 (int64x2_t __a)
{
  return (int8x16_t)  __a;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8_u16 (uint16x8_t __a)
{
  return (int8x16_t)  __a;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8_u32 (uint32x4_t __a)
{
  return (int8x16_t)  __a;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8_u64 (uint64x2_t __a)
{
  return (int8x16_t)  __a;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8_u8 (uint8x16_t __a)
{
  return (int8x16_t)  __a;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16_s16 (int16x8_t __a)
{
  return (uint16x8_t)  __a;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16_s32 (int32x4_t __a)
{
  return (uint16x8_t)  __a;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16_s64 (int64x2_t __a)
{
  return (uint16x8_t)  __a;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16_s8 (int8x16_t __a)
{
  return (uint16x8_t)  __a;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16_u32 (uint32x4_t __a)
{
  return (uint16x8_t)  __a;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16_u64 (uint64x2_t __a)
{
  return (uint16x8_t)  __a;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16_u8 (uint8x16_t __a)
{
  return (uint16x8_t)  __a;
}


__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32_s16 (int16x8_t __a)
{
  return (uint32x4_t)  __a;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32_s32 (int32x4_t __a)
{
  return (uint32x4_t)  __a;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32_s64 (int64x2_t __a)
{
  return (uint32x4_t)  __a;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32_s8 (int8x16_t __a)
{
  return (uint32x4_t)  __a;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32_u16 (uint16x8_t __a)
{
  return (uint32x4_t)  __a;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32_u64 (uint64x2_t __a)
{
  return (uint32x4_t)  __a;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32_u8 (uint8x16_t __a)
{
  return (uint32x4_t)  __a;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64_s16 (int16x8_t __a)
{
  return (uint64x2_t)  __a;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64_s32 (int32x4_t __a)
{
  return (uint64x2_t)  __a;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64_s64 (int64x2_t __a)
{
  return (uint64x2_t)  __a;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64_s8 (int8x16_t __a)
{
  return (uint64x2_t)  __a;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64_u16 (uint16x8_t __a)
{
  return (uint64x2_t)  __a;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64_u32 (uint32x4_t __a)
{
  return (uint64x2_t)  __a;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64_u8 (uint8x16_t __a)
{
  return (uint64x2_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8_s16 (int16x8_t __a)
{
  return (uint8x16_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8_s32 (int32x4_t __a)
{
  return (uint8x16_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8_s64 (int64x2_t __a)
{
  return (uint8x16_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8_s8 (int8x16_t __a)
{
  return (uint8x16_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8_u16 (uint16x8_t __a)
{
  return (uint8x16_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8_u32 (uint32x4_t __a)
{
  return (uint8x16_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8_u64 (uint64x2_t __a)
{
  return (uint8x16_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_u8 (void)
{
  uint8x16_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_u16 (void)
{
  uint16x8_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_u32 (void)
{
  uint32x4_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_u64 (void)
{
  uint64x2_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_s8 (void)
{
  int8x16_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_s16 (void)
{
  int16x8_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_s32 (void)
{
  int32x4_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_s64 (void)
{
  int64x2_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32_f16 (float16x8_t __a)
{
  return (int32x4_t)  __a;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32_f32 (float32x4_t __a)
{
  return (int32x4_t)  __a;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16_f16 (float16x8_t __a)
{
  return (int16x8_t)  __a;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16_f32 (float32x4_t __a)
{
  return (int16x8_t)  __a;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64_f16 (float16x8_t __a)
{
  return (int64x2_t)  __a;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64_f32 (float32x4_t __a)
{
  return (int64x2_t)  __a;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8_f16 (float16x8_t __a)
{
  return (int8x16_t)  __a;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8_f32 (float32x4_t __a)
{
  return (int8x16_t)  __a;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16_f16 (float16x8_t __a)
{
  return (uint16x8_t)  __a;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16_f32 (float32x4_t __a)
{
  return (uint16x8_t)  __a;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32_f16 (float16x8_t __a)
{
  return (uint32x4_t)  __a;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32_f32 (float32x4_t __a)
{
  return (uint32x4_t)  __a;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64_f16 (float16x8_t __a)
{
  return (uint64x2_t)  __a;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64_f32 (float32x4_t __a)
{
  return (uint64x2_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8_f16 (float16x8_t __a)
{
  return (uint8x16_t)  __a;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8_f32 (float32x4_t __a)
{
  return (uint8x16_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16_f32 (float32x4_t __a)
{
  return (float16x8_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16_s16 (int16x8_t __a)
{
  return (float16x8_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16_s32 (int32x4_t __a)
{
  return (float16x8_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16_s64 (int64x2_t __a)
{
  return (float16x8_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16_s8 (int8x16_t __a)
{
  return (float16x8_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16_u16 (uint16x8_t __a)
{
  return (float16x8_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16_u32 (uint32x4_t __a)
{
  return (float16x8_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16_u64 (uint64x2_t __a)
{
  return (float16x8_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16_u8 (uint8x16_t __a)
{
  return (float16x8_t)  __a;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32_f16 (float16x8_t __a)
{
  return (float32x4_t)  __a;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32_s16 (int16x8_t __a)
{
  return (float32x4_t)  __a;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32_s32 (int32x4_t __a)
{
  return (float32x4_t)  __a;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32_s64 (int64x2_t __a)
{
  return (float32x4_t)  __a;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32_s8 (int8x16_t __a)
{
  return (float32x4_t)  __a;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32_u16 (uint16x8_t __a)
{
  return (float32x4_t)  __a;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32_u32 (uint32x4_t __a)
{
  return (float32x4_t)  __a;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32_u64 (uint64x2_t __a)
{
  return (float32x4_t)  __a;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32_u8 (uint8x16_t __a)
{
  return (float32x4_t)  __a;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_f16 (void)
{
  float16x8_t __uninit;
  __asm__ ("": "=w" (__uninit));
  return __uninit;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_f32 (void)
{
  float32x4_t __uninit;
  __asm__ ("": "=w" (__uninit));
  return __uninit;
}

#endif

#ifdef __cplusplus

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16 (int32x4_t __a)
{
 return __arm_vreinterpretq_s16_s32 (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16 (int64x2_t __a)
{
 return __arm_vreinterpretq_s16_s64 (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16 (int8x16_t __a)
{
 return __arm_vreinterpretq_s16_s8 (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16 (uint16x8_t __a)
{
 return __arm_vreinterpretq_s16_u16 (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16 (uint32x4_t __a)
{
 return __arm_vreinterpretq_s16_u32 (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16 (uint64x2_t __a)
{
 return __arm_vreinterpretq_s16_u64 (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16 (uint8x16_t __a)
{
 return __arm_vreinterpretq_s16_u8 (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32 (int16x8_t __a)
{
 return __arm_vreinterpretq_s32_s16 (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32 (int64x2_t __a)
{
 return __arm_vreinterpretq_s32_s64 (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32 (int8x16_t __a)
{
 return __arm_vreinterpretq_s32_s8 (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32 (uint16x8_t __a)
{
 return __arm_vreinterpretq_s32_u16 (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32 (uint32x4_t __a)
{
 return __arm_vreinterpretq_s32_u32 (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32 (uint64x2_t __a)
{
 return __arm_vreinterpretq_s32_u64 (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32 (uint8x16_t __a)
{
 return __arm_vreinterpretq_s32_u8 (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64 (int16x8_t __a)
{
 return __arm_vreinterpretq_s64_s16 (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64 (int32x4_t __a)
{
 return __arm_vreinterpretq_s64_s32 (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64 (int8x16_t __a)
{
 return __arm_vreinterpretq_s64_s8 (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64 (uint16x8_t __a)
{
 return __arm_vreinterpretq_s64_u16 (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64 (uint32x4_t __a)
{
 return __arm_vreinterpretq_s64_u32 (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64 (uint64x2_t __a)
{
 return __arm_vreinterpretq_s64_u64 (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64 (uint8x16_t __a)
{
 return __arm_vreinterpretq_s64_u8 (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8 (int16x8_t __a)
{
 return __arm_vreinterpretq_s8_s16 (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8 (int32x4_t __a)
{
 return __arm_vreinterpretq_s8_s32 (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8 (int64x2_t __a)
{
 return __arm_vreinterpretq_s8_s64 (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8 (uint16x8_t __a)
{
 return __arm_vreinterpretq_s8_u16 (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8 (uint32x4_t __a)
{
 return __arm_vreinterpretq_s8_u32 (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8 (uint64x2_t __a)
{
 return __arm_vreinterpretq_s8_u64 (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8 (uint8x16_t __a)
{
 return __arm_vreinterpretq_s8_u8 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16 (int16x8_t __a)
{
 return __arm_vreinterpretq_u16_s16 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16 (int32x4_t __a)
{
 return __arm_vreinterpretq_u16_s32 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16 (int64x2_t __a)
{
 return __arm_vreinterpretq_u16_s64 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16 (int8x16_t __a)
{
 return __arm_vreinterpretq_u16_s8 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16 (uint32x4_t __a)
{
 return __arm_vreinterpretq_u16_u32 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16 (uint64x2_t __a)
{
 return __arm_vreinterpretq_u16_u64 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16 (uint8x16_t __a)
{
 return __arm_vreinterpretq_u16_u8 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32 (int16x8_t __a)
{
 return __arm_vreinterpretq_u32_s16 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32 (int32x4_t __a)
{
 return __arm_vreinterpretq_u32_s32 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32 (int64x2_t __a)
{
 return __arm_vreinterpretq_u32_s64 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32 (int8x16_t __a)
{
 return __arm_vreinterpretq_u32_s8 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32 (uint16x8_t __a)
{
 return __arm_vreinterpretq_u32_u16 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32 (uint64x2_t __a)
{
 return __arm_vreinterpretq_u32_u64 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32 (uint8x16_t __a)
{
 return __arm_vreinterpretq_u32_u8 (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64 (int16x8_t __a)
{
 return __arm_vreinterpretq_u64_s16 (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64 (int32x4_t __a)
{
 return __arm_vreinterpretq_u64_s32 (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64 (int64x2_t __a)
{
 return __arm_vreinterpretq_u64_s64 (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64 (int8x16_t __a)
{
 return __arm_vreinterpretq_u64_s8 (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64 (uint16x8_t __a)
{
 return __arm_vreinterpretq_u64_u16 (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64 (uint32x4_t __a)
{
 return __arm_vreinterpretq_u64_u32 (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64 (uint8x16_t __a)
{
 return __arm_vreinterpretq_u64_u8 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8 (int16x8_t __a)
{
 return __arm_vreinterpretq_u8_s16 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8 (int32x4_t __a)
{
 return __arm_vreinterpretq_u8_s32 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8 (int64x2_t __a)
{
 return __arm_vreinterpretq_u8_s64 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8 (int8x16_t __a)
{
 return __arm_vreinterpretq_u8_s8 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8 (uint16x8_t __a)
{
 return __arm_vreinterpretq_u8_u16 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8 (uint32x4_t __a)
{
 return __arm_vreinterpretq_u8_u32 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8 (uint64x2_t __a)
{
 return __arm_vreinterpretq_u8_u64 (__a);
}

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
__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32 (float16x8_t __a)
{
 return __arm_vreinterpretq_s32_f16 (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s32 (float32x4_t __a)
{
 return __arm_vreinterpretq_s32_f32 (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16 (float16x8_t __a)
{
 return __arm_vreinterpretq_s16_f16 (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s16 (float32x4_t __a)
{
 return __arm_vreinterpretq_s16_f32 (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64 (float16x8_t __a)
{
 return __arm_vreinterpretq_s64_f16 (__a);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s64 (float32x4_t __a)
{
 return __arm_vreinterpretq_s64_f32 (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8 (float16x8_t __a)
{
 return __arm_vreinterpretq_s8_f16 (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_s8 (float32x4_t __a)
{
 return __arm_vreinterpretq_s8_f32 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16 (float16x8_t __a)
{
 return __arm_vreinterpretq_u16_f16 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u16 (float32x4_t __a)
{
 return __arm_vreinterpretq_u16_f32 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32 (float16x8_t __a)
{
 return __arm_vreinterpretq_u32_f16 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u32 (float32x4_t __a)
{
 return __arm_vreinterpretq_u32_f32 (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64 (float16x8_t __a)
{
 return __arm_vreinterpretq_u64_f16 (__a);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u64 (float32x4_t __a)
{
 return __arm_vreinterpretq_u64_f32 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8 (float16x8_t __a)
{
 return __arm_vreinterpretq_u8_f16 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_u8 (float32x4_t __a)
{
 return __arm_vreinterpretq_u8_f32 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16 (float32x4_t __a)
{
 return __arm_vreinterpretq_f16_f32 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16 (int16x8_t __a)
{
 return __arm_vreinterpretq_f16_s16 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16 (int32x4_t __a)
{
 return __arm_vreinterpretq_f16_s32 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16 (int64x2_t __a)
{
 return __arm_vreinterpretq_f16_s64 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16 (int8x16_t __a)
{
 return __arm_vreinterpretq_f16_s8 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16 (uint16x8_t __a)
{
 return __arm_vreinterpretq_f16_u16 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16 (uint32x4_t __a)
{
 return __arm_vreinterpretq_f16_u32 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16 (uint64x2_t __a)
{
 return __arm_vreinterpretq_f16_u64 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f16 (uint8x16_t __a)
{
 return __arm_vreinterpretq_f16_u8 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32 (float16x8_t __a)
{
 return __arm_vreinterpretq_f32_f16 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32 (int16x8_t __a)
{
 return __arm_vreinterpretq_f32_s16 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32 (int32x4_t __a)
{
 return __arm_vreinterpretq_f32_s32 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32 (int64x2_t __a)
{
 return __arm_vreinterpretq_f32_s64 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32 (int8x16_t __a)
{
 return __arm_vreinterpretq_f32_s8 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32 (uint16x8_t __a)
{
 return __arm_vreinterpretq_f32_u16 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32 (uint32x4_t __a)
{
 return __arm_vreinterpretq_f32_u32 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32 (uint64x2_t __a)
{
 return __arm_vreinterpretq_f32_u64 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vreinterpretq_f32 (uint8x16_t __a)
{
 return __arm_vreinterpretq_f32_u8 (__a);
}

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
#endif /* __cplusplus */

#endif /* _GCC_ARM_MVE_H.  */
