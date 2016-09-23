/* ARM NEON intrinsics include file.

   Copyright (C) 2006-2016 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

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

#ifndef _GCC_ARM_NEON_H
#define _GCC_ARM_NEON_H 1

#ifndef __ARM_FP
#error "NEON intrinsics not available with the soft-float ABI.  Please use -mfloat-abi=softp or -mfloat-abi=hard"
#else

#pragma GCC push_options
#pragma GCC target ("fpu=neon")

#ifdef __cplusplus
extern "C" {
#endif

#include <arm_fp16.h>
#include <stdint.h>

typedef __simd64_int8_t int8x8_t;
typedef __simd64_int16_t int16x4_t;
typedef __simd64_int32_t int32x2_t;
typedef __builtin_neon_di int64x1_t;
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
typedef __fp16 float16_t;
typedef __simd64_float16_t float16x4_t;
#endif
typedef __simd64_float32_t float32x2_t;
typedef __simd64_poly8_t poly8x8_t;
typedef __simd64_poly16_t poly16x4_t;
#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
typedef __builtin_neon_poly64 poly64x1_t;
#pragma GCC pop_options
typedef __simd64_uint8_t uint8x8_t;
typedef __simd64_uint16_t uint16x4_t;
typedef __simd64_uint32_t uint32x2_t;
typedef __builtin_neon_udi uint64x1_t;

typedef __simd128_int8_t int8x16_t;
typedef __simd128_int16_t int16x8_t;
typedef __simd128_int32_t int32x4_t;
typedef __simd128_int64_t int64x2_t;
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
typedef __simd128_float16_t float16x8_t;
#endif
typedef __simd128_float32_t float32x4_t;
typedef __simd128_poly8_t poly8x16_t;
typedef __simd128_poly16_t poly16x8_t;
#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
typedef __builtin_neon_poly64 poly64x2_t __attribute__ ((__vector_size__ (16)));
#pragma GCC pop_options

typedef __simd128_uint8_t uint8x16_t;
typedef __simd128_uint16_t uint16x8_t;
typedef __simd128_uint32_t uint32x4_t;
typedef __simd128_uint64_t uint64x2_t;

typedef float float32_t;

/* The Poly types are user visible and live in their own world,
   keep them that way.  */
typedef __builtin_neon_poly8 poly8_t;
typedef __builtin_neon_poly16 poly16_t;
#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
typedef __builtin_neon_poly64 poly64_t;
typedef __builtin_neon_poly128 poly128_t;
#pragma GCC pop_options

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

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
typedef struct float16x4x2_t
{
  float16x4_t val[2];
} float16x4x2_t;
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
typedef struct float16x8x2_t
{
  float16x8_t val[2];
} float16x8x2_t;
#endif

typedef struct float32x2x2_t
{
  float32x2_t val[2];
} float32x2x2_t;

typedef struct float32x4x2_t
{
  float32x4_t val[2];
} float32x4x2_t;

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

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
typedef struct poly64x1x2_t
{
  poly64x1_t val[2];
} poly64x1x2_t;


typedef struct poly64x2x2_t
{
  poly64x2_t val[2];
} poly64x2x2_t;
#pragma GCC pop_options


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

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
typedef struct float16x4x3_t
{
  float16x4_t val[3];
} float16x4x3_t;
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
typedef struct float16x8x3_t
{
  float16x8_t val[3];
} float16x8x3_t;
#endif

typedef struct float32x2x3_t
{
  float32x2_t val[3];
} float32x2x3_t;

typedef struct float32x4x3_t
{
  float32x4_t val[3];
} float32x4x3_t;

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

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
typedef struct poly64x1x3_t
{
  poly64x1_t val[3];
} poly64x1x3_t;


typedef struct poly64x2x3_t
{
  poly64x2_t val[3];
} poly64x2x3_t;
#pragma GCC pop_options


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

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
typedef struct float16x4x4_t
{
  float16x4_t val[4];
} float16x4x4_t;
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
typedef struct float16x8x4_t
{
  float16x8_t val[4];
} float16x8x4_t;
#endif

typedef struct float32x2x4_t
{
  float32x2_t val[4];
} float32x2x4_t;

typedef struct float32x4x4_t
{
  float32x4_t val[4];
} float32x4x4_t;

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

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
typedef struct poly64x1x4_t
{
  poly64x1_t val[4];
} poly64x1x4_t;


typedef struct poly64x2x4_t
{
  poly64x2_t val[4];
} poly64x2x4_t;
#pragma GCC pop_options

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
#ifdef __FAST_MATH__
  return __a + __b;
#else
  return (float32x2_t) __builtin_neon_vaddv2sf (__a, __b);
#endif
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
#ifdef __FAST_MATH__
  return __a + __b;
#else
  return (float32x4_t) __builtin_neon_vaddv4sf (__a, __b);
#endif
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
  return (int16x8_t)__builtin_neon_vaddlsv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vaddl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int32x4_t)__builtin_neon_vaddlsv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vaddl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int64x2_t)__builtin_neon_vaddlsv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vaddl_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vaddluv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vaddl_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vaddluv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vaddl_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vaddluv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vaddw_s8 (int16x8_t __a, int8x8_t __b)
{
  return (int16x8_t)__builtin_neon_vaddwsv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vaddw_s16 (int32x4_t __a, int16x4_t __b)
{
  return (int32x4_t)__builtin_neon_vaddwsv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vaddw_s32 (int64x2_t __a, int32x2_t __b)
{
  return (int64x2_t)__builtin_neon_vaddwsv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vaddw_u8 (uint16x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vaddwuv8qi ((int16x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vaddw_u16 (uint32x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vaddwuv4hi ((int32x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vaddw_u32 (uint64x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vaddwuv2si ((int64x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vhadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vhaddsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vhadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vhaddsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vhadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vhaddsv2si (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vhadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vhadduv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vhadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vhadduv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vhadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vhadduv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vhaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vhaddsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vhaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vhaddsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vhaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vhaddsv4si (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vhaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vhadduv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vhaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vhadduv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vhaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vhadduv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrhadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vrhaddsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrhadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vrhaddsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrhadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vrhaddsv2si (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrhadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vrhadduv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrhadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vrhadduv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrhadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vrhadduv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrhaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vrhaddsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrhaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vrhaddsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrhaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vrhaddsv4si (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrhaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vrhadduv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrhaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vrhadduv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrhaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vrhadduv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vqaddsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vqaddsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vqaddsv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqadd_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t)__builtin_neon_vqaddsdi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vqadduv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vqadduv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vqadduv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqadd_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)__builtin_neon_vqaddudi ((int64x1_t) __a, (int64x1_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vqaddsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vqaddsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vqaddsv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqaddq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t)__builtin_neon_vqaddsv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vqadduv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vqadduv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vqadduv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqaddq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vqadduv2di ((int64x2_t) __a, (int64x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vaddhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int8x8_t)__builtin_neon_vaddhnv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vaddhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int16x4_t)__builtin_neon_vaddhnv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vaddhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int32x2_t)__builtin_neon_vaddhnv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vaddhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vaddhnv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vaddhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vaddhnv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vaddhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vaddhnv2di ((int64x2_t) __a, (int64x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vraddhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int8x8_t)__builtin_neon_vraddhnv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vraddhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int16x4_t)__builtin_neon_vraddhnv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vraddhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int32x2_t)__builtin_neon_vraddhnv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vraddhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vraddhnv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vraddhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vraddhnv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vraddhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vraddhnv2di ((int64x2_t) __a, (int64x2_t) __b);
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
#ifdef __FAST_MATH__
  return __a * __b;
#else
  return (float32x2_t) __builtin_neon_vmulfv2sf (__a, __b);
#endif

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
#ifdef __FAST_MATH__
  return __a * __b;
#else
  return (float32x4_t) __builtin_neon_vmulfv4sf (__a, __b);
#endif
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

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vmul_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (poly8x8_t)__builtin_neon_vmulpv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vmulq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (poly8x16_t)__builtin_neon_vmulpv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqdmulh_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vqdmulhv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqdmulh_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vqdmulhv2si (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqdmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vqdmulhv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vqdmulhv4si (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmulh_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vqrdmulhv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmulh_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vqrdmulhv2si (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vqrdmulhv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vqrdmulhv4si (__a, __b);
}

#ifdef __ARM_FEATURE_QRDMX
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmlah_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int16x4_t)__builtin_neon_vqrdmlahv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmlah_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int32x2_t)__builtin_neon_vqrdmlahv2si (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmlahq_s16 (int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int16x8_t)__builtin_neon_vqrdmlahv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmlahq_s32 (int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int32x4_t)__builtin_neon_vqrdmlahv4si (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmlsh_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int16x4_t)__builtin_neon_vqrdmlshv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmlsh_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int32x2_t)__builtin_neon_vqrdmlshv2si (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmlshq_s16 (int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int16x8_t)__builtin_neon_vqrdmlshv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmlshq_s32 (int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int32x4_t)__builtin_neon_vqrdmlshv4si (__a, __b, __c);
}
#endif

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmull_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int16x8_t)__builtin_neon_vmullsv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmull_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int32x4_t)__builtin_neon_vmullsv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmull_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int64x2_t)__builtin_neon_vmullsv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmull_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vmulluv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmull_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vmulluv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmull_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vmulluv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vmull_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (poly16x8_t)__builtin_neon_vmullpv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int32x4_t)__builtin_neon_vqdmullv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int64x2_t)__builtin_neon_vqdmullv2si (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmla_s8 (int8x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int8x8_t)__builtin_neon_vmlav8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmla_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int16x4_t)__builtin_neon_vmlav4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmla_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int32x2_t)__builtin_neon_vmlav2si (__a, __b, __c);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmla_f32 (float32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  return (float32x2_t)__builtin_neon_vmlav2sf (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmla_u8 (uint8x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint8x8_t)__builtin_neon_vmlav8qi ((int8x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmla_u16 (uint16x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return (uint16x4_t)__builtin_neon_vmlav4hi ((int16x4_t) __a, (int16x4_t) __b, (int16x4_t) __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmla_u32 (uint32x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return (uint32x2_t)__builtin_neon_vmlav2si ((int32x2_t) __a, (int32x2_t) __b, (int32x2_t) __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmlaq_s8 (int8x16_t __a, int8x16_t __b, int8x16_t __c)
{
  return (int8x16_t)__builtin_neon_vmlav16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlaq_s16 (int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int16x8_t)__builtin_neon_vmlav8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlaq_s32 (int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int32x4_t)__builtin_neon_vmlav4si (__a, __b, __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlaq_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return (float32x4_t)__builtin_neon_vmlav4sf (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmlaq_u8 (uint8x16_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return (uint8x16_t)__builtin_neon_vmlav16qi ((int8x16_t) __a, (int8x16_t) __b, (int8x16_t) __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlaq_u16 (uint16x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return (uint16x8_t)__builtin_neon_vmlav8hi ((int16x8_t) __a, (int16x8_t) __b, (int16x8_t) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlaq_u32 (uint32x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return (uint32x4_t)__builtin_neon_vmlav4si ((int32x4_t) __a, (int32x4_t) __b, (int32x4_t) __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlal_s8 (int16x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int16x8_t)__builtin_neon_vmlalsv8qi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlal_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int32x4_t)__builtin_neon_vmlalsv4hi (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlal_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int64x2_t)__builtin_neon_vmlalsv2si (__a, __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlal_u8 (uint16x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint16x8_t)__builtin_neon_vmlaluv8qi ((int16x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlal_u16 (uint32x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return (uint32x4_t)__builtin_neon_vmlaluv4hi ((int32x4_t) __a, (int16x4_t) __b, (int16x4_t) __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlal_u32 (uint64x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return (uint64x2_t)__builtin_neon_vmlaluv2si ((int64x2_t) __a, (int32x2_t) __b, (int32x2_t) __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int32x4_t)__builtin_neon_vqdmlalv4hi (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int64x2_t)__builtin_neon_vqdmlalv2si (__a, __b, __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmls_s8 (int8x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int8x8_t)__builtin_neon_vmlsv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmls_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int16x4_t)__builtin_neon_vmlsv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmls_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int32x2_t)__builtin_neon_vmlsv2si (__a, __b, __c);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmls_f32 (float32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  return (float32x2_t)__builtin_neon_vmlsv2sf (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmls_u8 (uint8x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint8x8_t)__builtin_neon_vmlsv8qi ((int8x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmls_u16 (uint16x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return (uint16x4_t)__builtin_neon_vmlsv4hi ((int16x4_t) __a, (int16x4_t) __b, (int16x4_t) __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmls_u32 (uint32x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return (uint32x2_t)__builtin_neon_vmlsv2si ((int32x2_t) __a, (int32x2_t) __b, (int32x2_t) __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmlsq_s8 (int8x16_t __a, int8x16_t __b, int8x16_t __c)
{
  return (int8x16_t)__builtin_neon_vmlsv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsq_s16 (int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int16x8_t)__builtin_neon_vmlsv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsq_s32 (int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int32x4_t)__builtin_neon_vmlsv4si (__a, __b, __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlsq_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return (float32x4_t)__builtin_neon_vmlsv4sf (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmlsq_u8 (uint8x16_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return (uint8x16_t)__builtin_neon_vmlsv16qi ((int8x16_t) __a, (int8x16_t) __b, (int8x16_t) __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsq_u16 (uint16x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return (uint16x8_t)__builtin_neon_vmlsv8hi ((int16x8_t) __a, (int16x8_t) __b, (int16x8_t) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsq_u32 (uint32x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return (uint32x4_t)__builtin_neon_vmlsv4si ((int32x4_t) __a, (int32x4_t) __b, (int32x4_t) __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsl_s8 (int16x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int16x8_t)__builtin_neon_vmlslsv8qi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsl_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int32x4_t)__builtin_neon_vmlslsv4hi (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlsl_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int64x2_t)__builtin_neon_vmlslsv2si (__a, __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsl_u8 (uint16x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint16x8_t)__builtin_neon_vmlsluv8qi ((int16x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsl_u16 (uint32x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return (uint32x4_t)__builtin_neon_vmlsluv4hi ((int32x4_t) __a, (int16x4_t) __b, (int16x4_t) __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlsl_u32 (uint64x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return (uint64x2_t)__builtin_neon_vmlsluv2si ((int64x2_t) __a, (int32x2_t) __b, (int32x2_t) __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int32x4_t)__builtin_neon_vqdmlslv4hi (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int64x2_t)__builtin_neon_vqdmlslv2si (__a, __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=neon-vfpv4")
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vfma_f32 (float32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  return (float32x2_t)__builtin_neon_vfmav2sf (__a, __b, __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vfmaq_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return (float32x4_t)__builtin_neon_vfmav4sf (__a, __b, __c);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vfms_f32 (float32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  return (float32x2_t)__builtin_neon_vfmsv2sf (__a, __b, __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vfmsq_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return (float32x4_t)__builtin_neon_vfmsv4sf (__a, __b, __c);
}
#pragma GCC pop_options

#if __ARM_ARCH >= 8
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrndn_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vrintnv2sf (__a);
}

#endif
#if __ARM_ARCH >= 8
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndnq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vrintnv4sf (__a);
}

#endif
#if __ARM_ARCH >= 8
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrnda_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vrintav2sf (__a);
}

#endif
#if __ARM_ARCH >= 8
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndaq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vrintav4sf (__a);
}

#endif
#if __ARM_ARCH >= 8
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrndp_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vrintpv2sf (__a);
}

#endif
#if __ARM_ARCH >= 8
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndpq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vrintpv4sf (__a);
}

#endif
#if __ARM_ARCH >= 8
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrndm_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vrintmv2sf (__a);
}

#endif
#if __ARM_ARCH >= 8
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndmq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vrintmv4sf (__a);
}

#endif

#if __ARM_ARCH >= 8
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrndx_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vrintxv2sf (__a);
}

#endif

#if __ARM_ARCH >= 8
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndxq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vrintxv4sf (__a);
}

#endif

#if __ARM_ARCH >= 8
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrnd_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vrintzv2sf (__a);
}

#endif
#if __ARM_ARCH >= 8
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrndq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vrintzv4sf (__a);
}

#endif

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
#ifdef __FAST_MATH__
  return __a - __b;
#else
  return (float32x2_t) __builtin_neon_vsubv2sf (__a, __b);
#endif
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
#ifdef __FAST_MATH__
  return __a - __b;
#else
  return (float32x4_t) __builtin_neon_vsubv4sf (__a, __b);
#endif
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
  return (int16x8_t)__builtin_neon_vsublsv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsubl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int32x4_t)__builtin_neon_vsublsv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsubl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int64x2_t)__builtin_neon_vsublsv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsubl_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vsubluv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsubl_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vsubluv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsubl_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vsubluv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsubw_s8 (int16x8_t __a, int8x8_t __b)
{
  return (int16x8_t)__builtin_neon_vsubwsv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsubw_s16 (int32x4_t __a, int16x4_t __b)
{
  return (int32x4_t)__builtin_neon_vsubwsv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsubw_s32 (int64x2_t __a, int32x2_t __b)
{
  return (int64x2_t)__builtin_neon_vsubwsv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsubw_u8 (uint16x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vsubwuv8qi ((int16x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsubw_u16 (uint32x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vsubwuv4hi ((int32x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsubw_u32 (uint64x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vsubwuv2si ((int64x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vhsub_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vhsubsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vhsub_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vhsubsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vhsub_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vhsubsv2si (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vhsub_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vhsubuv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vhsub_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vhsubuv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vhsub_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vhsubuv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vhsubq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vhsubsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vhsubq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vhsubsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vhsubq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vhsubsv4si (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vhsubq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vhsubuv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vhsubq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vhsubuv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vhsubq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vhsubuv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqsub_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vqsubsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqsub_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vqsubsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqsub_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vqsubsv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqsub_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t)__builtin_neon_vqsubsdi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqsub_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vqsubuv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqsub_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vqsubuv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqsub_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vqsubuv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqsub_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x1_t)__builtin_neon_vqsubudi ((int64x1_t) __a, (int64x1_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqsubq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vqsubsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqsubq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vqsubsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqsubq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vqsubsv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqsubq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t)__builtin_neon_vqsubsv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqsubq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vqsubuv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqsubq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vqsubuv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqsubq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vqsubuv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqsubq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vqsubuv2di ((int64x2_t) __a, (int64x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vsubhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int8x8_t)__builtin_neon_vsubhnv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vsubhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int16x4_t)__builtin_neon_vsubhnv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vsubhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int32x2_t)__builtin_neon_vsubhnv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsubhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vsubhnv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsubhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vsubhnv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsubhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vsubhnv2di ((int64x2_t) __a, (int64x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrsubhn_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int8x8_t)__builtin_neon_vrsubhnv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrsubhn_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int16x4_t)__builtin_neon_vrsubhnv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrsubhn_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int32x2_t)__builtin_neon_vrsubhnv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrsubhn_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vrsubhnv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrsubhn_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vrsubhnv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrsubhn_u64 (uint64x2_t __a, uint64x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vrsubhnv2di ((int64x2_t) __a, (int64x2_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vceq_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vceqv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vceq_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vceqv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vceq_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vceqv2si (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vceq_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vceqv2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vceq_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vceqv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vceq_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vceqv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vceq_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vceqv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vceq_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vceqv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vceqq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vceqv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vceqq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vceqv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vceqq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vceqv4si (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vceqq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vceqv4sf (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vceqq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vceqv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vceqq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vceqv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vceqq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vceqv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vceqq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vceqv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcge_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vcgev8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcge_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgev4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcge_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgev2si (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcge_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgev2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcge_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vcgeuv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcge_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgeuv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcge_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgeuv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgeq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vcgev16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgeq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgev8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgeq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgev4si (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgeq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgev4sf (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgeq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vcgeuv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgeq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgeuv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgeq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgeuv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcle_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vcgev8qi (__b, __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcle_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgev4hi (__b, __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcle_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgev2si (__b, __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcle_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgev2sf (__b, __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcle_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vcgeuv8qi ((int8x8_t) __b, (int8x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcle_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgeuv4hi ((int16x4_t) __b, (int16x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcle_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgeuv2si ((int32x2_t) __b, (int32x2_t) __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcleq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vcgev16qi (__b, __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcleq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgev8hi (__b, __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcleq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgev4si (__b, __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcleq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgev4sf (__b, __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcleq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vcgeuv16qi ((int8x16_t) __b, (int8x16_t) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcleq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgeuv8hi ((int16x8_t) __b, (int16x8_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcleq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgeuv4si ((int32x4_t) __b, (int32x4_t) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgt_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vcgtv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgt_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgtv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgt_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgtv2si (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgt_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgtv2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcgt_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vcgtuv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgt_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgtuv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcgt_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgtuv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgtq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vcgtv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgtq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgtv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgtq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgtv4si (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgtq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgtv4sf (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcgtq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vcgtuv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgtq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgtuv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcgtq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgtuv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vclt_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vcgtv8qi (__b, __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vclt_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgtv4hi (__b, __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclt_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgtv2si (__b, __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclt_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgtv2sf (__b, __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vclt_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vcgtuv8qi ((int8x8_t) __b, (int8x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vclt_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgtuv4hi ((int16x4_t) __b, (int16x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclt_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcgtuv2si ((int32x2_t) __b, (int32x2_t) __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcltq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vcgtv16qi (__b, __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcltq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgtv8hi (__b, __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcltq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgtv4si (__b, __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcltq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgtv4sf (__b, __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcltq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vcgtuv16qi ((int8x16_t) __b, (int8x16_t) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcltq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgtuv8hi ((int16x8_t) __b, (int16x8_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcltq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcgtuv4si ((int32x4_t) __b, (int32x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcage_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcagev2sf (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcageq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcagev4sf (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcale_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcagev2sf (__b, __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcaleq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcagev4sf (__b, __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcagt_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcagtv2sf (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcagtq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcagtv4sf (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcalt_f32 (float32x2_t __a, float32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vcagtv2sf (__b, __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcaltq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vcagtv4sf (__b, __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtst_s8 (int8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vtstv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vtst_s16 (int16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vtstv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vtst_s32 (int32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vtstv2si (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtst_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vtstv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vtst_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vtstv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vtst_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vtstv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtst_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vtstv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vtst_p16 (poly16x4_t __a, poly16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vtstv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vtstq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vtstv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vtstq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vtstv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vtstq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vtstv4si (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vtstq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vtstv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vtstq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vtstv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vtstq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vtstv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vtstq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vtstv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vtstq_p16 (poly16x8_t __a, poly16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vtstv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vabd_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vabdsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vabd_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vabdsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vabd_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vabdsv2si (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vabd_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x2_t)__builtin_neon_vabdfv2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vabd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vabduv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vabd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vabduv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vabd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vabduv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vabdq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vabdsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabdq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vabdsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabdq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vabdsv4si (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vabdq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (float32x4_t)__builtin_neon_vabdfv4sf (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vabdq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vabduv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabdq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vabduv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabdq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vabduv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabdl_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int16x8_t)__builtin_neon_vabdlsv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabdl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int32x4_t)__builtin_neon_vabdlsv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vabdl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int64x2_t)__builtin_neon_vabdlsv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabdl_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vabdluv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabdl_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vabdluv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vabdl_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vabdluv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vaba_s8 (int8x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int8x8_t)__builtin_neon_vabasv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vaba_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int16x4_t)__builtin_neon_vabasv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vaba_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int32x2_t)__builtin_neon_vabasv2si (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vaba_u8 (uint8x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint8x8_t)__builtin_neon_vabauv8qi ((int8x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vaba_u16 (uint16x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return (uint16x4_t)__builtin_neon_vabauv4hi ((int16x4_t) __a, (int16x4_t) __b, (int16x4_t) __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vaba_u32 (uint32x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return (uint32x2_t)__builtin_neon_vabauv2si ((int32x2_t) __a, (int32x2_t) __b, (int32x2_t) __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vabaq_s8 (int8x16_t __a, int8x16_t __b, int8x16_t __c)
{
  return (int8x16_t)__builtin_neon_vabasv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabaq_s16 (int16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int16x8_t)__builtin_neon_vabasv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabaq_s32 (int32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int32x4_t)__builtin_neon_vabasv4si (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vabaq_u8 (uint8x16_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return (uint8x16_t)__builtin_neon_vabauv16qi ((int8x16_t) __a, (int8x16_t) __b, (int8x16_t) __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabaq_u16 (uint16x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return (uint16x8_t)__builtin_neon_vabauv8hi ((int16x8_t) __a, (int16x8_t) __b, (int16x8_t) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabaq_u32 (uint32x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return (uint32x4_t)__builtin_neon_vabauv4si ((int32x4_t) __a, (int32x4_t) __b, (int32x4_t) __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabal_s8 (int16x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int16x8_t)__builtin_neon_vabalsv8qi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabal_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int32x4_t)__builtin_neon_vabalsv4hi (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vabal_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int64x2_t)__builtin_neon_vabalsv2si (__a, __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vabal_u8 (uint16x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint16x8_t)__builtin_neon_vabaluv8qi ((int16x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vabal_u16 (uint32x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return (uint32x4_t)__builtin_neon_vabaluv4hi ((int32x4_t) __a, (int16x4_t) __b, (int16x4_t) __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vabal_u32 (uint64x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return (uint64x2_t)__builtin_neon_vabaluv2si ((int64x2_t) __a, (int32x2_t) __b, (int32x2_t) __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmax_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vmaxsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmax_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vmaxsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmax_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vmaxsv2si (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmax_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x2_t)__builtin_neon_vmaxfv2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmax_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vmaxuv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmax_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vmaxuv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmax_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vmaxuv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmaxq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vmaxsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmaxq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vmaxsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmaxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vmaxsv4si (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmaxq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (float32x4_t)__builtin_neon_vmaxfv4sf (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmaxq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vmaxuv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmaxq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vmaxuv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmaxq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vmaxuv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmin_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vminsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmin_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vminsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmin_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vminsv2si (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmin_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x2_t)__builtin_neon_vminfv2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmin_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vminuv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmin_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vminuv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmin_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vminuv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vminq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vminsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vminq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vminsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vminq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vminsv4si (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vminq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (float32x4_t)__builtin_neon_vminfv4sf (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vminq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vminuv16qi ((int8x16_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vminq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vminuv8hi ((int16x8_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vminq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vminuv4si ((int32x4_t) __a, (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vpadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vpaddv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vpaddv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vpaddv2si (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vpadd_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x2_t)__builtin_neon_vpaddv2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vpadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vpaddv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vpaddv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vpaddv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpaddl_s8 (int8x8_t __a)
{
  return (int16x4_t)__builtin_neon_vpaddlsv8qi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpaddl_s16 (int16x4_t __a)
{
  return (int32x2_t)__builtin_neon_vpaddlsv4hi (__a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vpaddl_s32 (int32x2_t __a)
{
  return (int64x1_t)__builtin_neon_vpaddlsv2si (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpaddl_u8 (uint8x8_t __a)
{
  return (uint16x4_t)__builtin_neon_vpaddluv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpaddl_u16 (uint16x4_t __a)
{
  return (uint32x2_t)__builtin_neon_vpaddluv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vpaddl_u32 (uint32x2_t __a)
{
  return (uint64x1_t)__builtin_neon_vpaddluv2si ((int32x2_t) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vpaddlq_s8 (int8x16_t __a)
{
  return (int16x8_t)__builtin_neon_vpaddlsv16qi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vpaddlq_s16 (int16x8_t __a)
{
  return (int32x4_t)__builtin_neon_vpaddlsv8hi (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vpaddlq_s32 (int32x4_t __a)
{
  return (int64x2_t)__builtin_neon_vpaddlsv4si (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vpaddlq_u8 (uint8x16_t __a)
{
  return (uint16x8_t)__builtin_neon_vpaddluv16qi ((int8x16_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vpaddlq_u16 (uint16x8_t __a)
{
  return (uint32x4_t)__builtin_neon_vpaddluv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vpaddlq_u32 (uint32x4_t __a)
{
  return (uint64x2_t)__builtin_neon_vpaddluv4si ((int32x4_t) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpadal_s8 (int16x4_t __a, int8x8_t __b)
{
  return (int16x4_t)__builtin_neon_vpadalsv8qi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpadal_s16 (int32x2_t __a, int16x4_t __b)
{
  return (int32x2_t)__builtin_neon_vpadalsv4hi (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vpadal_s32 (int64x1_t __a, int32x2_t __b)
{
  return (int64x1_t)__builtin_neon_vpadalsv2si (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpadal_u8 (uint16x4_t __a, uint8x8_t __b)
{
  return (uint16x4_t)__builtin_neon_vpadaluv8qi ((int16x4_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpadal_u16 (uint32x2_t __a, uint16x4_t __b)
{
  return (uint32x2_t)__builtin_neon_vpadaluv4hi ((int32x2_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vpadal_u32 (uint64x1_t __a, uint32x2_t __b)
{
  return (uint64x1_t)__builtin_neon_vpadaluv2si ((int64x1_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vpadalq_s8 (int16x8_t __a, int8x16_t __b)
{
  return (int16x8_t)__builtin_neon_vpadalsv16qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vpadalq_s16 (int32x4_t __a, int16x8_t __b)
{
  return (int32x4_t)__builtin_neon_vpadalsv8hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vpadalq_s32 (int64x2_t __a, int32x4_t __b)
{
  return (int64x2_t)__builtin_neon_vpadalsv4si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vpadalq_u8 (uint16x8_t __a, uint8x16_t __b)
{
  return (uint16x8_t)__builtin_neon_vpadaluv16qi ((int16x8_t) __a, (int8x16_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vpadalq_u16 (uint32x4_t __a, uint16x8_t __b)
{
  return (uint32x4_t)__builtin_neon_vpadaluv8hi ((int32x4_t) __a, (int16x8_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vpadalq_u32 (uint64x2_t __a, uint32x4_t __b)
{
  return (uint64x2_t)__builtin_neon_vpadaluv4si ((int64x2_t) __a, (int32x4_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vpmax_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vpmaxsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpmax_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vpmaxsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpmax_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vpmaxsv2si (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vpmax_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x2_t)__builtin_neon_vpmaxfv2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vpmax_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vpmaxuv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpmax_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vpmaxuv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpmax_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vpmaxuv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vpmin_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vpminsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vpmin_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vpminsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vpmin_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vpminsv2si (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vpmin_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x2_t)__builtin_neon_vpminfv2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vpmin_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vpminuv8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vpmin_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vpminuv4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vpmin_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vpminuv2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrecps_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x2_t)__builtin_neon_vrecpsv2sf (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrecpsq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (float32x4_t)__builtin_neon_vrecpsv4sf (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrsqrts_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x2_t)__builtin_neon_vrsqrtsv2sf (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrsqrtsq_f32 (float32x4_t __a, float32x4_t __b)
{
  return (float32x4_t)__builtin_neon_vrsqrtsv4sf (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vshl_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vshlsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vshl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vshlsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vshl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vshlsv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vshl_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t)__builtin_neon_vshlsdi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vshl_u8 (uint8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vshluv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vshl_u16 (uint16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vshluv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vshl_u32 (uint32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vshluv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vshl_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)__builtin_neon_vshludi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vshlsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vshlsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vshlsv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vshlq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t)__builtin_neon_vshlsv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vshluv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vshluv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vshluv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vshlq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vshluv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrshl_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vrshlsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrshl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vrshlsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrshl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vrshlsv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vrshl_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t)__builtin_neon_vrshlsdi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrshl_u8 (uint8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vrshluv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrshl_u16 (uint16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vrshluv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrshl_u32 (uint32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vrshluv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vrshl_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)__builtin_neon_vrshludi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vrshlsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vrshlsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vrshlsv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vrshlq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t)__builtin_neon_vrshlsv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vrshluv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vrshluv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vrshluv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vrshlq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vrshluv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqshl_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vqshlsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqshl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vqshlsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqshl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vqshlsv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqshl_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t)__builtin_neon_vqshlsdi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshl_u8 (uint8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vqshluv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshl_u16 (uint16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vqshluv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshl_u32 (uint32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vqshluv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqshl_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)__builtin_neon_vqshludi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vqshlsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vqshlsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vqshlsv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqshlq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t)__builtin_neon_vqshlsv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vqshluv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vqshluv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vqshluv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqshlq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vqshluv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqrshl_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vqrshlsv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrshl_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x4_t)__builtin_neon_vqrshlsv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrshl_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x2_t)__builtin_neon_vqrshlsv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqrshl_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x1_t)__builtin_neon_vqrshlsdi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqrshl_u8 (uint8x8_t __a, int8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vqrshluv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqrshl_u16 (uint16x4_t __a, int16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vqrshluv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqrshl_u32 (uint32x2_t __a, int32x2_t __b)
{
  return (uint32x2_t)__builtin_neon_vqrshluv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqrshl_u64 (uint64x1_t __a, int64x1_t __b)
{
  return (uint64x1_t)__builtin_neon_vqrshludi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqrshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return (int8x16_t)__builtin_neon_vqrshlsv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return (int16x8_t)__builtin_neon_vqrshlsv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return (int32x4_t)__builtin_neon_vqrshlsv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqrshlq_s64 (int64x2_t __a, int64x2_t __b)
{
  return (int64x2_t)__builtin_neon_vqrshlsv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqrshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return (uint8x16_t)__builtin_neon_vqrshluv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqrshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vqrshluv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqrshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return (uint32x4_t)__builtin_neon_vqrshluv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqrshlq_u64 (uint64x2_t __a, int64x2_t __b)
{
  return (uint64x2_t)__builtin_neon_vqrshluv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vshr_n_s8 (int8x8_t __a, const int __b)
{
  return (int8x8_t)__builtin_neon_vshrs_nv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vshr_n_s16 (int16x4_t __a, const int __b)
{
  return (int16x4_t)__builtin_neon_vshrs_nv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vshr_n_s32 (int32x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vshrs_nv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vshr_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t)__builtin_neon_vshrs_ndi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vshr_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vshru_nv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vshr_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vshru_nv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vshr_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vshru_nv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vshr_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t)__builtin_neon_vshru_ndi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vshrq_n_s8 (int8x16_t __a, const int __b)
{
  return (int8x16_t)__builtin_neon_vshrs_nv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vshrq_n_s16 (int16x8_t __a, const int __b)
{
  return (int16x8_t)__builtin_neon_vshrs_nv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vshrq_n_s32 (int32x4_t __a, const int __b)
{
  return (int32x4_t)__builtin_neon_vshrs_nv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vshrq_n_s64 (int64x2_t __a, const int __b)
{
  return (int64x2_t)__builtin_neon_vshrs_nv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vshrq_n_u8 (uint8x16_t __a, const int __b)
{
  return (uint8x16_t)__builtin_neon_vshru_nv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vshrq_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint16x8_t)__builtin_neon_vshru_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vshrq_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint32x4_t)__builtin_neon_vshru_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vshrq_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint64x2_t)__builtin_neon_vshru_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrshr_n_s8 (int8x8_t __a, const int __b)
{
  return (int8x8_t)__builtin_neon_vrshrs_nv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrshr_n_s16 (int16x4_t __a, const int __b)
{
  return (int16x4_t)__builtin_neon_vrshrs_nv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrshr_n_s32 (int32x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vrshrs_nv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vrshr_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t)__builtin_neon_vrshrs_ndi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrshr_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vrshru_nv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrshr_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vrshru_nv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrshr_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vrshru_nv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vrshr_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t)__builtin_neon_vrshru_ndi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrshrq_n_s8 (int8x16_t __a, const int __b)
{
  return (int8x16_t)__builtin_neon_vrshrs_nv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrshrq_n_s16 (int16x8_t __a, const int __b)
{
  return (int16x8_t)__builtin_neon_vrshrs_nv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrshrq_n_s32 (int32x4_t __a, const int __b)
{
  return (int32x4_t)__builtin_neon_vrshrs_nv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vrshrq_n_s64 (int64x2_t __a, const int __b)
{
  return (int64x2_t)__builtin_neon_vrshrs_nv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrshrq_n_u8 (uint8x16_t __a, const int __b)
{
  return (uint8x16_t)__builtin_neon_vrshru_nv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrshrq_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint16x8_t)__builtin_neon_vrshru_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrshrq_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint32x4_t)__builtin_neon_vrshru_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vrshrq_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint64x2_t)__builtin_neon_vrshru_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vshrn_n_s16 (int16x8_t __a, const int __b)
{
  return (int8x8_t)__builtin_neon_vshrn_nv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vshrn_n_s32 (int32x4_t __a, const int __b)
{
  return (int16x4_t)__builtin_neon_vshrn_nv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vshrn_n_s64 (int64x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vshrn_nv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vshrn_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vshrn_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vshrn_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vshrn_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vshrn_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vshrn_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrshrn_n_s16 (int16x8_t __a, const int __b)
{
  return (int8x8_t)__builtin_neon_vrshrn_nv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrshrn_n_s32 (int32x4_t __a, const int __b)
{
  return (int16x4_t)__builtin_neon_vrshrn_nv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrshrn_n_s64 (int64x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vrshrn_nv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrshrn_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vrshrn_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrshrn_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vrshrn_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrshrn_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vrshrn_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqshrn_n_s16 (int16x8_t __a, const int __b)
{
  return (int8x8_t)__builtin_neon_vqshrns_nv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqshrn_n_s32 (int32x4_t __a, const int __b)
{
  return (int16x4_t)__builtin_neon_vqshrns_nv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqshrn_n_s64 (int64x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vqshrns_nv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshrn_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vqshrnu_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshrn_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vqshrnu_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshrn_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vqshrnu_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqrshrn_n_s16 (int16x8_t __a, const int __b)
{
  return (int8x8_t)__builtin_neon_vqrshrns_nv8hi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrshrn_n_s32 (int32x4_t __a, const int __b)
{
  return (int16x4_t)__builtin_neon_vqrshrns_nv4si (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrshrn_n_s64 (int64x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vqrshrns_nv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqrshrn_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vqrshrnu_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqrshrn_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vqrshrnu_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqrshrn_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vqrshrnu_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshrun_n_s16 (int16x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vqshrun_nv8hi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshrun_n_s32 (int32x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vqshrun_nv4si (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshrun_n_s64 (int64x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vqshrun_nv2di (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqrshrun_n_s16 (int16x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vqrshrun_nv8hi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqrshrun_n_s32 (int32x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vqrshrun_nv4si (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqrshrun_n_s64 (int64x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vqrshrun_nv2di (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vshl_n_s8 (int8x8_t __a, const int __b)
{
  return (int8x8_t)__builtin_neon_vshl_nv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vshl_n_s16 (int16x4_t __a, const int __b)
{
  return (int16x4_t)__builtin_neon_vshl_nv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vshl_n_s32 (int32x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vshl_nv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vshl_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t)__builtin_neon_vshl_ndi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vshl_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vshl_nv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vshl_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vshl_nv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vshl_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vshl_nv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vshl_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t)__builtin_neon_vshl_ndi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vshlq_n_s8 (int8x16_t __a, const int __b)
{
  return (int8x16_t)__builtin_neon_vshl_nv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vshlq_n_s16 (int16x8_t __a, const int __b)
{
  return (int16x8_t)__builtin_neon_vshl_nv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vshlq_n_s32 (int32x4_t __a, const int __b)
{
  return (int32x4_t)__builtin_neon_vshl_nv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vshlq_n_s64 (int64x2_t __a, const int __b)
{
  return (int64x2_t)__builtin_neon_vshl_nv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vshlq_n_u8 (uint8x16_t __a, const int __b)
{
  return (uint8x16_t)__builtin_neon_vshl_nv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vshlq_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint16x8_t)__builtin_neon_vshl_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vshlq_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint32x4_t)__builtin_neon_vshl_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vshlq_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint64x2_t)__builtin_neon_vshl_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqshl_n_s8 (int8x8_t __a, const int __b)
{
  return (int8x8_t)__builtin_neon_vqshl_s_nv8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqshl_n_s16 (int16x4_t __a, const int __b)
{
  return (int16x4_t)__builtin_neon_vqshl_s_nv4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqshl_n_s32 (int32x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vqshl_s_nv2si (__a, __b);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vqshl_n_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t)__builtin_neon_vqshl_s_ndi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshl_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vqshl_u_nv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshl_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vqshl_u_nv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshl_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vqshl_u_nv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqshl_n_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t)__builtin_neon_vqshl_u_ndi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqshlq_n_s8 (int8x16_t __a, const int __b)
{
  return (int8x16_t)__builtin_neon_vqshl_s_nv16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqshlq_n_s16 (int16x8_t __a, const int __b)
{
  return (int16x8_t)__builtin_neon_vqshl_s_nv8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqshlq_n_s32 (int32x4_t __a, const int __b)
{
  return (int32x4_t)__builtin_neon_vqshl_s_nv4si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqshlq_n_s64 (int64x2_t __a, const int __b)
{
  return (int64x2_t)__builtin_neon_vqshl_s_nv2di (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqshlq_n_u8 (uint8x16_t __a, const int __b)
{
  return (uint8x16_t)__builtin_neon_vqshl_u_nv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqshlq_n_u16 (uint16x8_t __a, const int __b)
{
  return (uint16x8_t)__builtin_neon_vqshl_u_nv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqshlq_n_u32 (uint32x4_t __a, const int __b)
{
  return (uint32x4_t)__builtin_neon_vqshl_u_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqshlq_n_u64 (uint64x2_t __a, const int __b)
{
  return (uint64x2_t)__builtin_neon_vqshl_u_nv2di ((int64x2_t) __a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqshlu_n_s8 (int8x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vqshlu_nv8qi (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqshlu_n_s16 (int16x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vqshlu_nv4hi (__a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqshlu_n_s32 (int32x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vqshlu_nv2si (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vqshlu_n_s64 (int64x1_t __a, const int __b)
{
  return (uint64x1_t)__builtin_neon_vqshlu_ndi (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vqshluq_n_s8 (int8x16_t __a, const int __b)
{
  return (uint8x16_t)__builtin_neon_vqshlu_nv16qi (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vqshluq_n_s16 (int16x8_t __a, const int __b)
{
  return (uint16x8_t)__builtin_neon_vqshlu_nv8hi (__a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vqshluq_n_s32 (int32x4_t __a, const int __b)
{
  return (uint32x4_t)__builtin_neon_vqshlu_nv4si (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vqshluq_n_s64 (int64x2_t __a, const int __b)
{
  return (uint64x2_t)__builtin_neon_vqshlu_nv2di (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vshll_n_s8 (int8x8_t __a, const int __b)
{
  return (int16x8_t)__builtin_neon_vshlls_nv8qi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vshll_n_s16 (int16x4_t __a, const int __b)
{
  return (int32x4_t)__builtin_neon_vshlls_nv4hi (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vshll_n_s32 (int32x2_t __a, const int __b)
{
  return (int64x2_t)__builtin_neon_vshlls_nv2si (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vshll_n_u8 (uint8x8_t __a, const int __b)
{
  return (uint16x8_t)__builtin_neon_vshllu_nv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vshll_n_u16 (uint16x4_t __a, const int __b)
{
  return (uint32x4_t)__builtin_neon_vshllu_nv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vshll_n_u32 (uint32x2_t __a, const int __b)
{
  return (uint64x2_t)__builtin_neon_vshllu_nv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vsra_n_s8 (int8x8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t)__builtin_neon_vsras_nv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vsra_n_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vsras_nv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vsra_n_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vsras_nv2si (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsra_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t)__builtin_neon_vsras_ndi (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsra_n_u8 (uint8x8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t)__builtin_neon_vsrau_nv8qi ((int8x8_t) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsra_n_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t)__builtin_neon_vsrau_nv4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsra_n_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t)__builtin_neon_vsrau_nv2si ((int32x2_t) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsra_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t)__builtin_neon_vsrau_ndi ((int64x1_t) __a, (int64x1_t) __b, __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vsraq_n_s8 (int8x16_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t)__builtin_neon_vsras_nv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsraq_n_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vsras_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsraq_n_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vsras_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsraq_n_s64 (int64x2_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t)__builtin_neon_vsras_nv2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsraq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t)__builtin_neon_vsrau_nv16qi ((int8x16_t) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsraq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t)__builtin_neon_vsrau_nv8hi ((int16x8_t) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsraq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t)__builtin_neon_vsrau_nv4si ((int32x4_t) __a, (int32x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsraq_n_u64 (uint64x2_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t)__builtin_neon_vsrau_nv2di ((int64x2_t) __a, (int64x2_t) __b, __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrsra_n_s8 (int8x8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t)__builtin_neon_vrsras_nv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrsra_n_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vrsras_nv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrsra_n_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vrsras_nv2si (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vrsra_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t)__builtin_neon_vrsras_ndi (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrsra_n_u8 (uint8x8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t)__builtin_neon_vrsrau_nv8qi ((int8x8_t) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrsra_n_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t)__builtin_neon_vrsrau_nv4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrsra_n_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t)__builtin_neon_vrsrau_nv2si ((int32x2_t) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vrsra_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t)__builtin_neon_vrsrau_ndi ((int64x1_t) __a, (int64x1_t) __b, __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrsraq_n_s8 (int8x16_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t)__builtin_neon_vrsras_nv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrsraq_n_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vrsras_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrsraq_n_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vrsras_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vrsraq_n_s64 (int64x2_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t)__builtin_neon_vrsras_nv2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrsraq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t)__builtin_neon_vrsrau_nv16qi ((int8x16_t) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrsraq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t)__builtin_neon_vrsrau_nv8hi ((int16x8_t) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrsraq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t)__builtin_neon_vrsrau_nv4si ((int32x4_t) __a, (int32x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vrsraq_n_u64 (uint64x2_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t)__builtin_neon_vrsrau_nv2di ((int64x2_t) __a, (int64x2_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vsri_n_p64 (poly64x1_t __a, poly64x1_t __b, const int __c)
{
  return (poly64x1_t)__builtin_neon_vsri_ndi (__a, __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vsri_n_s8 (int8x8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t)__builtin_neon_vsri_nv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vsri_n_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vsri_nv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vsri_n_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vsri_nv2si (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsri_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t)__builtin_neon_vsri_ndi (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsri_n_u8 (uint8x8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t)__builtin_neon_vsri_nv8qi ((int8x8_t) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsri_n_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t)__builtin_neon_vsri_nv4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsri_n_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t)__builtin_neon_vsri_nv2si ((int32x2_t) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsri_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t)__builtin_neon_vsri_ndi ((int64x1_t) __a, (int64x1_t) __b, __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vsri_n_p8 (poly8x8_t __a, poly8x8_t __b, const int __c)
{
  return (poly8x8_t)__builtin_neon_vsri_nv8qi ((int8x8_t) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vsri_n_p16 (poly16x4_t __a, poly16x4_t __b, const int __c)
{
  return (poly16x4_t)__builtin_neon_vsri_nv4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vsriq_n_p64 (poly64x2_t __a, poly64x2_t __b, const int __c)
{
  return (poly64x2_t)__builtin_neon_vsri_nv2di ((int64x2_t) __a, (int64x2_t) __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vsriq_n_s8 (int8x16_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t)__builtin_neon_vsri_nv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsriq_n_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vsri_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsriq_n_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vsri_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsriq_n_s64 (int64x2_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t)__builtin_neon_vsri_nv2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsriq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t)__builtin_neon_vsri_nv16qi ((int8x16_t) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsriq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t)__builtin_neon_vsri_nv8hi ((int16x8_t) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsriq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t)__builtin_neon_vsri_nv4si ((int32x4_t) __a, (int32x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsriq_n_u64 (uint64x2_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t)__builtin_neon_vsri_nv2di ((int64x2_t) __a, (int64x2_t) __b, __c);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vsriq_n_p8 (poly8x16_t __a, poly8x16_t __b, const int __c)
{
  return (poly8x16_t)__builtin_neon_vsri_nv16qi ((int8x16_t) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vsriq_n_p16 (poly16x8_t __a, poly16x8_t __b, const int __c)
{
  return (poly16x8_t)__builtin_neon_vsri_nv8hi ((int16x8_t) __a, (int16x8_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vsli_n_p64 (poly64x1_t __a, poly64x1_t __b, const int __c)
{
  return (poly64x1_t)__builtin_neon_vsli_ndi (__a, __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vsli_n_s8 (int8x8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t)__builtin_neon_vsli_nv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vsli_n_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vsli_nv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vsli_n_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vsli_nv2si (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vsli_n_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t)__builtin_neon_vsli_ndi (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vsli_n_u8 (uint8x8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t)__builtin_neon_vsli_nv8qi ((int8x8_t) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vsli_n_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t)__builtin_neon_vsli_nv4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vsli_n_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t)__builtin_neon_vsli_nv2si ((int32x2_t) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vsli_n_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t)__builtin_neon_vsli_ndi ((int64x1_t) __a, (int64x1_t) __b, __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vsli_n_p8 (poly8x8_t __a, poly8x8_t __b, const int __c)
{
  return (poly8x8_t)__builtin_neon_vsli_nv8qi ((int8x8_t) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vsli_n_p16 (poly16x4_t __a, poly16x4_t __b, const int __c)
{
  return (poly16x4_t)__builtin_neon_vsli_nv4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vsliq_n_p64 (poly64x2_t __a, poly64x2_t __b, const int __c)
{
  return (poly64x2_t)__builtin_neon_vsli_nv2di ((int64x2_t) __a, (int64x2_t) __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vsliq_n_s8 (int8x16_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t)__builtin_neon_vsli_nv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsliq_n_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vsli_nv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsliq_n_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vsli_nv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsliq_n_s64 (int64x2_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t)__builtin_neon_vsli_nv2di (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsliq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t)__builtin_neon_vsli_nv16qi ((int8x16_t) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsliq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t)__builtin_neon_vsli_nv8hi ((int16x8_t) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsliq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t)__builtin_neon_vsli_nv4si ((int32x4_t) __a, (int32x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsliq_n_u64 (uint64x2_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t)__builtin_neon_vsli_nv2di ((int64x2_t) __a, (int64x2_t) __b, __c);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vsliq_n_p8 (poly8x16_t __a, poly8x16_t __b, const int __c)
{
  return (poly8x16_t)__builtin_neon_vsli_nv16qi ((int8x16_t) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vsliq_n_p16 (poly16x8_t __a, poly16x8_t __b, const int __c)
{
  return (poly16x8_t)__builtin_neon_vsli_nv8hi ((int16x8_t) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vabs_s8 (int8x8_t __a)
{
  return (int8x8_t)__builtin_neon_vabsv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vabs_s16 (int16x4_t __a)
{
  return (int16x4_t)__builtin_neon_vabsv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vabs_s32 (int32x2_t __a)
{
  return (int32x2_t)__builtin_neon_vabsv2si (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vabs_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vabsv2sf (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vabsq_s8 (int8x16_t __a)
{
  return (int8x16_t)__builtin_neon_vabsv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vabsq_s16 (int16x8_t __a)
{
  return (int16x8_t)__builtin_neon_vabsv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vabsq_s32 (int32x4_t __a)
{
  return (int32x4_t)__builtin_neon_vabsv4si (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vabsq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vabsv4sf (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqabs_s8 (int8x8_t __a)
{
  return (int8x8_t)__builtin_neon_vqabsv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqabs_s16 (int16x4_t __a)
{
  return (int16x4_t)__builtin_neon_vqabsv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqabs_s32 (int32x2_t __a)
{
  return (int32x2_t)__builtin_neon_vqabsv2si (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqabsq_s8 (int8x16_t __a)
{
  return (int8x16_t)__builtin_neon_vqabsv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqabsq_s16 (int16x8_t __a)
{
  return (int16x8_t)__builtin_neon_vqabsv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqabsq_s32 (int32x4_t __a)
{
  return (int32x4_t)__builtin_neon_vqabsv4si (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vneg_s8 (int8x8_t __a)
{
  return (int8x8_t)__builtin_neon_vnegv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vneg_s16 (int16x4_t __a)
{
  return (int16x4_t)__builtin_neon_vnegv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vneg_s32 (int32x2_t __a)
{
  return (int32x2_t)__builtin_neon_vnegv2si (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vneg_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vnegv2sf (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vnegq_s8 (int8x16_t __a)
{
  return (int8x16_t)__builtin_neon_vnegv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vnegq_s16 (int16x8_t __a)
{
  return (int16x8_t)__builtin_neon_vnegv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vnegq_s32 (int32x4_t __a)
{
  return (int32x4_t)__builtin_neon_vnegv4si (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vnegq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vnegv4sf (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqneg_s8 (int8x8_t __a)
{
  return (int8x8_t)__builtin_neon_vqnegv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqneg_s16 (int16x4_t __a)
{
  return (int16x4_t)__builtin_neon_vqnegv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqneg_s32 (int32x2_t __a)
{
  return (int32x2_t)__builtin_neon_vqnegv2si (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vqnegq_s8 (int8x16_t __a)
{
  return (int8x16_t)__builtin_neon_vqnegv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqnegq_s16 (int16x8_t __a)
{
  return (int16x8_t)__builtin_neon_vqnegv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqnegq_s32 (int32x4_t __a)
{
  return (int32x4_t)__builtin_neon_vqnegv4si (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmvn_s8 (int8x8_t __a)
{
  return (int8x8_t)__builtin_neon_vmvnv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmvn_s16 (int16x4_t __a)
{
  return (int16x4_t)__builtin_neon_vmvnv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmvn_s32 (int32x2_t __a)
{
  return (int32x2_t)__builtin_neon_vmvnv2si (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmvn_u8 (uint8x8_t __a)
{
  return (uint8x8_t)__builtin_neon_vmvnv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmvn_u16 (uint16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vmvnv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmvn_u32 (uint32x2_t __a)
{
  return (uint32x2_t)__builtin_neon_vmvnv2si ((int32x2_t) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vmvn_p8 (poly8x8_t __a)
{
  return (poly8x8_t)__builtin_neon_vmvnv8qi ((int8x8_t) __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmvnq_s8 (int8x16_t __a)
{
  return (int8x16_t)__builtin_neon_vmvnv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmvnq_s16 (int16x8_t __a)
{
  return (int16x8_t)__builtin_neon_vmvnv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmvnq_s32 (int32x4_t __a)
{
  return (int32x4_t)__builtin_neon_vmvnv4si (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmvnq_u8 (uint8x16_t __a)
{
  return (uint8x16_t)__builtin_neon_vmvnv16qi ((int8x16_t) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmvnq_u16 (uint16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vmvnv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmvnq_u32 (uint32x4_t __a)
{
  return (uint32x4_t)__builtin_neon_vmvnv4si ((int32x4_t) __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vmvnq_p8 (poly8x16_t __a)
{
  return (poly8x16_t)__builtin_neon_vmvnv16qi ((int8x16_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vcls_s8 (int8x8_t __a)
{
  return (int8x8_t)__builtin_neon_vclsv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcls_s16 (int16x4_t __a)
{
  return (int16x4_t)__builtin_neon_vclsv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcls_s32 (int32x2_t __a)
{
  return (int32x2_t)__builtin_neon_vclsv2si (__a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vclsq_s8 (int8x16_t __a)
{
  return (int8x16_t)__builtin_neon_vclsv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vclsq_s16 (int16x8_t __a)
{
  return (int16x8_t)__builtin_neon_vclsv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vclsq_s32 (int32x4_t __a)
{
  return (int32x4_t)__builtin_neon_vclsv4si (__a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vclz_s8 (int8x8_t __a)
{
  return (int8x8_t)__builtin_neon_vclzv8qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vclz_s16 (int16x4_t __a)
{
  return (int16x4_t)__builtin_neon_vclzv4hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vclz_s32 (int32x2_t __a)
{
  return (int32x2_t)__builtin_neon_vclzv2si (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vclz_u8 (uint8x8_t __a)
{
  return (uint8x8_t)__builtin_neon_vclzv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vclz_u16 (uint16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vclzv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vclz_u32 (uint32x2_t __a)
{
  return (uint32x2_t)__builtin_neon_vclzv2si ((int32x2_t) __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vclzq_s8 (int8x16_t __a)
{
  return (int8x16_t)__builtin_neon_vclzv16qi (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vclzq_s16 (int16x8_t __a)
{
  return (int16x8_t)__builtin_neon_vclzv8hi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vclzq_s32 (int32x4_t __a)
{
  return (int32x4_t)__builtin_neon_vclzv4si (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vclzq_u8 (uint8x16_t __a)
{
  return (uint8x16_t)__builtin_neon_vclzv16qi ((int8x16_t) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vclzq_u16 (uint16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vclzv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vclzq_u32 (uint32x4_t __a)
{
  return (uint32x4_t)__builtin_neon_vclzv4si ((int32x4_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vcnt_s8 (int8x8_t __a)
{
  return (int8x8_t)__builtin_neon_vcntv8qi (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcnt_u8 (uint8x8_t __a)
{
  return (uint8x8_t)__builtin_neon_vcntv8qi ((int8x8_t) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vcnt_p8 (poly8x8_t __a)
{
  return (poly8x8_t)__builtin_neon_vcntv8qi ((int8x8_t) __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vcntq_s8 (int8x16_t __a)
{
  return (int8x16_t)__builtin_neon_vcntv16qi (__a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcntq_u8 (uint8x16_t __a)
{
  return (uint8x16_t)__builtin_neon_vcntv16qi ((int8x16_t) __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vcntq_p8 (poly8x16_t __a)
{
  return (poly8x16_t)__builtin_neon_vcntv16qi ((int8x16_t) __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrecpe_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vrecpev2sf (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrecpe_u32 (uint32x2_t __a)
{
  return (uint32x2_t)__builtin_neon_vrecpev2si ((int32x2_t) __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrecpeq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vrecpev4sf (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrecpeq_u32 (uint32x4_t __a)
{
  return (uint32x4_t)__builtin_neon_vrecpev4si ((int32x4_t) __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrsqrte_f32 (float32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vrsqrtev2sf (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrsqrte_u32 (uint32x2_t __a)
{
  return (uint32x2_t)__builtin_neon_vrsqrtev2si ((int32x2_t) __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrsqrteq_f32 (float32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vrsqrtev4sf (__a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrsqrteq_u32 (uint32x4_t __a)
{
  return (uint32x4_t)__builtin_neon_vrsqrtev4si ((int32x4_t) __a);
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vget_lane_s8 (int8x8_t __a, const int __b)
{
  return (int8_t)__builtin_neon_vget_lanev8qi (__a, __b);
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vget_lane_s16 (int16x4_t __a, const int __b)
{
  return (int16_t)__builtin_neon_vget_lanev4hi (__a, __b);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vget_lane_s32 (int32x2_t __a, const int __b)
{
  return (int32_t)__builtin_neon_vget_lanev2si (__a, __b);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
/* Functions cannot accept or return __FP16 types.  Even if the function
   were marked always-inline so there were no call sites, the declaration
   would nonetheless raise an error.  Hence, we must use a macro instead.  */

  /* For big-endian, GCC's vector indices are reversed within each 64
     bits compared to the architectural lane indices used by Neon
     intrinsics.  */
#ifdef __ARM_BIG_ENDIAN
#define __ARM_NUM_LANES(__v) (sizeof (__v) / sizeof (__v[0]))
#define __arm_lane(__vec, __idx) (__idx ^ (__ARM_NUM_LANES(__vec) - 1))
#define __arm_laneq(__vec, __idx) (__idx ^ (__ARM_NUM_LANES(__vec)/2 - 1))
#else
#define __arm_lane(__vec, __idx) __idx
#define __arm_laneq(__vec, __idx) __idx
#endif

#define vget_lane_f16(__v, __idx)			\
  __extension__						\
  ({							\
    float16x4_t __vec = (__v);				\
    __builtin_arm_lane_check (4, __idx);		\
    float16_t __res = __vec[__arm_lane(__vec, __idx)];	\
    __res;						\
  })
#endif

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vget_lane_f32 (float32x2_t __a, const int __b)
{
  return (float32_t)__builtin_neon_vget_lanev2sf (__a, __b);
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vget_lane_u8 (uint8x8_t __a, const int __b)
{
  return (uint8_t)__builtin_neon_vget_laneuv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vget_lane_u16 (uint16x4_t __a, const int __b)
{
  return (uint16_t)__builtin_neon_vget_laneuv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vget_lane_u32 (uint32x2_t __a, const int __b)
{
  return (uint32_t)__builtin_neon_vget_laneuv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline poly8_t __attribute__ ((__always_inline__))
vget_lane_p8 (poly8x8_t __a, const int __b)
{
  return (poly8_t)__builtin_neon_vget_laneuv8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline poly16_t __attribute__ ((__always_inline__))
vget_lane_p16 (poly16x4_t __a, const int __b)
{
  return (poly16_t)__builtin_neon_vget_laneuv4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vget_lane_s64 (int64x1_t __a, const int __b)
{
  return (int64_t)__builtin_neon_vget_lanedi (__a, __b);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vget_lane_u64 (uint64x1_t __a, const int __b)
{
  return (uint64_t)__builtin_neon_vget_lanedi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8_t __attribute__ ((__always_inline__))
vgetq_lane_s8 (int8x16_t __a, const int __b)
{
  return (int8_t)__builtin_neon_vget_lanev16qi (__a, __b);
}

__extension__ static __inline int16_t __attribute__ ((__always_inline__))
vgetq_lane_s16 (int16x8_t __a, const int __b)
{
  return (int16_t)__builtin_neon_vget_lanev8hi (__a, __b);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vgetq_lane_s32 (int32x4_t __a, const int __b)
{
  return (int32_t)__builtin_neon_vget_lanev4si (__a, __b);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define vgetq_lane_f16(__v, __idx)			\
  __extension__						\
  ({							\
    float16x8_t __vec = (__v);				\
    __builtin_arm_lane_check (8, __idx);		\
    float16_t __res = __vec[__arm_laneq(__vec, __idx)];	\
    __res;						\
  })
#endif

__extension__ static __inline float32_t __attribute__ ((__always_inline__))
vgetq_lane_f32 (float32x4_t __a, const int __b)
{
  return (float32_t)__builtin_neon_vget_lanev4sf (__a, __b);
}

__extension__ static __inline uint8_t __attribute__ ((__always_inline__))
vgetq_lane_u8 (uint8x16_t __a, const int __b)
{
  return (uint8_t)__builtin_neon_vget_laneuv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline uint16_t __attribute__ ((__always_inline__))
vgetq_lane_u16 (uint16x8_t __a, const int __b)
{
  return (uint16_t)__builtin_neon_vget_laneuv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vgetq_lane_u32 (uint32x4_t __a, const int __b)
{
  return (uint32_t)__builtin_neon_vget_laneuv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline poly8_t __attribute__ ((__always_inline__))
vgetq_lane_p8 (poly8x16_t __a, const int __b)
{
  return (poly8_t)__builtin_neon_vget_laneuv16qi ((int8x16_t) __a, __b);
}

__extension__ static __inline poly16_t __attribute__ ((__always_inline__))
vgetq_lane_p16 (poly16x8_t __a, const int __b)
{
  return (poly16_t)__builtin_neon_vget_laneuv8hi ((int16x8_t) __a, __b);
}

__extension__ static __inline int64_t __attribute__ ((__always_inline__))
vgetq_lane_s64 (int64x2_t __a, const int __b)
{
  return (int64_t)__builtin_neon_vget_lanev2di (__a, __b);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
vgetq_lane_u64 (uint64x2_t __a, const int __b)
{
  return (uint64_t)__builtin_neon_vget_lanev2di ((int64x2_t) __a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vset_lane_s8 (int8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t)__builtin_neon_vset_lanev8qi ((__builtin_neon_qi) __a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vset_lane_s16 (int16_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vset_lanev4hi ((__builtin_neon_hi) __a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vset_lane_s32 (int32_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vset_lanev2si ((__builtin_neon_si) __a, __b, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define vset_lane_f16(__e, __v, __idx)		\
  __extension__					\
  ({						\
    float16_t __elem = (__e);			\
    float16x4_t __vec = (__v);			\
    __builtin_arm_lane_check (4, __idx);	\
    __vec[__arm_lane (__vec, __idx)] = __elem;	\
    __vec;					\
  })
#endif

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vset_lane_f32 (float32_t __a, float32x2_t __b, const int __c)
{
  return (float32x2_t)__builtin_neon_vset_lanev2sf ((__builtin_neon_sf) __a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vset_lane_u8 (uint8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t)__builtin_neon_vset_lanev8qi ((__builtin_neon_qi) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vset_lane_u16 (uint16_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t)__builtin_neon_vset_lanev4hi ((__builtin_neon_hi) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vset_lane_u32 (uint32_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t)__builtin_neon_vset_lanev2si ((__builtin_neon_si) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vset_lane_p8 (poly8_t __a, poly8x8_t __b, const int __c)
{
  return (poly8x8_t)__builtin_neon_vset_lanev8qi ((__builtin_neon_qi) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vset_lane_p16 (poly16_t __a, poly16x4_t __b, const int __c)
{
  return (poly16x4_t)__builtin_neon_vset_lanev4hi ((__builtin_neon_hi) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vset_lane_s64 (int64_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t)__builtin_neon_vset_lanedi ((__builtin_neon_di) __a, __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vset_lane_u64 (uint64_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t)__builtin_neon_vset_lanedi ((__builtin_neon_di) __a, (int64x1_t) __b, __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vsetq_lane_s8 (int8_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t)__builtin_neon_vset_lanev16qi ((__builtin_neon_qi) __a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vsetq_lane_s16 (int16_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vset_lanev8hi ((__builtin_neon_hi) __a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vsetq_lane_s32 (int32_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vset_lanev4si ((__builtin_neon_si) __a, __b, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define vsetq_lane_f16(__e, __v, __idx)		\
  __extension__					\
  ({						\
    float16_t __elem = (__e);			\
    float16x8_t __vec = (__v);			\
    __builtin_arm_lane_check (8, __idx);	\
    __vec[__arm_laneq (__vec, __idx)] = __elem;	\
    __vec;					\
  })
#endif

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vsetq_lane_f32 (float32_t __a, float32x4_t __b, const int __c)
{
  return (float32x4_t)__builtin_neon_vset_lanev4sf ((__builtin_neon_sf) __a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vsetq_lane_u8 (uint8_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t)__builtin_neon_vset_lanev16qi ((__builtin_neon_qi) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vsetq_lane_u16 (uint16_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t)__builtin_neon_vset_lanev8hi ((__builtin_neon_hi) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsetq_lane_u32 (uint32_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t)__builtin_neon_vset_lanev4si ((__builtin_neon_si) __a, (int32x4_t) __b, __c);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vsetq_lane_p8 (poly8_t __a, poly8x16_t __b, const int __c)
{
  return (poly8x16_t)__builtin_neon_vset_lanev16qi ((__builtin_neon_qi) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vsetq_lane_p16 (poly16_t __a, poly16x8_t __b, const int __c)
{
  return (poly16x8_t)__builtin_neon_vset_lanev8hi ((__builtin_neon_hi) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vsetq_lane_s64 (int64_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t)__builtin_neon_vset_lanev2di ((__builtin_neon_di) __a, __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vsetq_lane_u64 (uint64_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t)__builtin_neon_vset_lanev2di ((__builtin_neon_di) __a, (int64x2_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vcreate_p64 (uint64_t __a)
{
  return (poly64x1_t)__builtin_neon_vcreatedi ((__builtin_neon_di) __a);
}

#pragma GCC pop_options
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vcreate_s8 (uint64_t __a)
{
  return (int8x8_t)__builtin_neon_vcreatev8qi ((__builtin_neon_di) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcreate_s16 (uint64_t __a)
{
  return (int16x4_t)__builtin_neon_vcreatev4hi ((__builtin_neon_di) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcreate_s32 (uint64_t __a)
{
  return (int32x2_t)__builtin_neon_vcreatev2si ((__builtin_neon_di) __a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vcreate_s64 (uint64_t __a)
{
  return (int64x1_t)__builtin_neon_vcreatedi ((__builtin_neon_di) __a);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vcreate_f16 (uint64_t __a)
{
  return (float16x4_t) __a;
}
#endif

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcreate_f32 (uint64_t __a)
{
  return (float32x2_t)__builtin_neon_vcreatev2sf ((__builtin_neon_di) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vcreate_u8 (uint64_t __a)
{
  return (uint8x8_t)__builtin_neon_vcreatev8qi ((__builtin_neon_di) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcreate_u16 (uint64_t __a)
{
  return (uint16x4_t)__builtin_neon_vcreatev4hi ((__builtin_neon_di) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcreate_u32 (uint64_t __a)
{
  return (uint32x2_t)__builtin_neon_vcreatev2si ((__builtin_neon_di) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vcreate_u64 (uint64_t __a)
{
  return (uint64x1_t)__builtin_neon_vcreatedi ((__builtin_neon_di) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vcreate_p8 (uint64_t __a)
{
  return (poly8x8_t)__builtin_neon_vcreatev8qi ((__builtin_neon_di) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vcreate_p16 (uint64_t __a)
{
  return (poly16x4_t)__builtin_neon_vcreatev4hi ((__builtin_neon_di) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vdup_n_s8 (int8_t __a)
{
  return (int8x8_t)__builtin_neon_vdup_nv8qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vdup_n_s16 (int16_t __a)
{
  return (int16x4_t)__builtin_neon_vdup_nv4hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vdup_n_s32 (int32_t __a)
{
  return (int32x2_t)__builtin_neon_vdup_nv2si ((__builtin_neon_si) __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vdup_n_f32 (float32_t __a)
{
  return (float32x2_t)__builtin_neon_vdup_nv2sf ((__builtin_neon_sf) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vdup_n_u8 (uint8_t __a)
{
  return (uint8x8_t)__builtin_neon_vdup_nv8qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vdup_n_u16 (uint16_t __a)
{
  return (uint16x4_t)__builtin_neon_vdup_nv4hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vdup_n_u32 (uint32_t __a)
{
  return (uint32x2_t)__builtin_neon_vdup_nv2si ((__builtin_neon_si) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vdup_n_p8 (poly8_t __a)
{
  return (poly8x8_t)__builtin_neon_vdup_nv8qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vdup_n_p16 (poly16_t __a)
{
  return (poly16x4_t)__builtin_neon_vdup_nv4hi ((__builtin_neon_hi) __a);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vdup_n_p64 (poly64_t __a)
{
  return (poly64x1_t)__builtin_neon_vdup_ndi ((__builtin_neon_di) __a);
}

#pragma GCC pop_options
__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vdup_n_s64 (int64_t __a)
{
  return (int64x1_t)__builtin_neon_vdup_ndi ((__builtin_neon_di) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vdup_n_u64 (uint64_t __a)
{
  return (uint64x1_t)__builtin_neon_vdup_ndi ((__builtin_neon_di) __a);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vdupq_n_p64 (poly64_t __a)
{
  return (poly64x2_t)__builtin_neon_vdup_nv2di ((__builtin_neon_di) __a);
}

#pragma GCC pop_options
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vdupq_n_s8 (int8_t __a)
{
  return (int8x16_t)__builtin_neon_vdup_nv16qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vdupq_n_s16 (int16_t __a)
{
  return (int16x8_t)__builtin_neon_vdup_nv8hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vdupq_n_s32 (int32_t __a)
{
  return (int32x4_t)__builtin_neon_vdup_nv4si ((__builtin_neon_si) __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vdupq_n_f32 (float32_t __a)
{
  return (float32x4_t)__builtin_neon_vdup_nv4sf ((__builtin_neon_sf) __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vdupq_n_u8 (uint8_t __a)
{
  return (uint8x16_t)__builtin_neon_vdup_nv16qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vdupq_n_u16 (uint16_t __a)
{
  return (uint16x8_t)__builtin_neon_vdup_nv8hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vdupq_n_u32 (uint32_t __a)
{
  return (uint32x4_t)__builtin_neon_vdup_nv4si ((__builtin_neon_si) __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vdupq_n_p8 (poly8_t __a)
{
  return (poly8x16_t)__builtin_neon_vdup_nv16qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vdupq_n_p16 (poly16_t __a)
{
  return (poly16x8_t)__builtin_neon_vdup_nv8hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vdupq_n_s64 (int64_t __a)
{
  return (int64x2_t)__builtin_neon_vdup_nv2di ((__builtin_neon_di) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vdupq_n_u64 (uint64_t __a)
{
  return (uint64x2_t)__builtin_neon_vdup_nv2di ((__builtin_neon_di) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmov_n_s8 (int8_t __a)
{
  return (int8x8_t)__builtin_neon_vdup_nv8qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmov_n_s16 (int16_t __a)
{
  return (int16x4_t)__builtin_neon_vdup_nv4hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmov_n_s32 (int32_t __a)
{
  return (int32x2_t)__builtin_neon_vdup_nv2si ((__builtin_neon_si) __a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmov_n_f32 (float32_t __a)
{
  return (float32x2_t)__builtin_neon_vdup_nv2sf ((__builtin_neon_sf) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmov_n_u8 (uint8_t __a)
{
  return (uint8x8_t)__builtin_neon_vdup_nv8qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmov_n_u16 (uint16_t __a)
{
  return (uint16x4_t)__builtin_neon_vdup_nv4hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmov_n_u32 (uint32_t __a)
{
  return (uint32x2_t)__builtin_neon_vdup_nv2si ((__builtin_neon_si) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vmov_n_p8 (poly8_t __a)
{
  return (poly8x8_t)__builtin_neon_vdup_nv8qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vmov_n_p16 (poly16_t __a)
{
  return (poly16x4_t)__builtin_neon_vdup_nv4hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vmov_n_s64 (int64_t __a)
{
  return (int64x1_t)__builtin_neon_vdup_ndi ((__builtin_neon_di) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vmov_n_u64 (uint64_t __a)
{
  return (uint64x1_t)__builtin_neon_vdup_ndi ((__builtin_neon_di) __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vmovq_n_s8 (int8_t __a)
{
  return (int8x16_t)__builtin_neon_vdup_nv16qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmovq_n_s16 (int16_t __a)
{
  return (int16x8_t)__builtin_neon_vdup_nv8hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmovq_n_s32 (int32_t __a)
{
  return (int32x4_t)__builtin_neon_vdup_nv4si ((__builtin_neon_si) __a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmovq_n_f32 (float32_t __a)
{
  return (float32x4_t)__builtin_neon_vdup_nv4sf ((__builtin_neon_sf) __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vmovq_n_u8 (uint8_t __a)
{
  return (uint8x16_t)__builtin_neon_vdup_nv16qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmovq_n_u16 (uint16_t __a)
{
  return (uint16x8_t)__builtin_neon_vdup_nv8hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmovq_n_u32 (uint32_t __a)
{
  return (uint32x4_t)__builtin_neon_vdup_nv4si ((__builtin_neon_si) __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vmovq_n_p8 (poly8_t __a)
{
  return (poly8x16_t)__builtin_neon_vdup_nv16qi ((__builtin_neon_qi) __a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vmovq_n_p16 (poly16_t __a)
{
  return (poly16x8_t)__builtin_neon_vdup_nv8hi ((__builtin_neon_hi) __a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmovq_n_s64 (int64_t __a)
{
  return (int64x2_t)__builtin_neon_vdup_nv2di ((__builtin_neon_di) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmovq_n_u64 (uint64_t __a)
{
  return (uint64x2_t)__builtin_neon_vdup_nv2di ((__builtin_neon_di) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vdup_lane_s8 (int8x8_t __a, const int __b)
{
  return (int8x8_t)__builtin_neon_vdup_lanev8qi (__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vdup_lane_s16 (int16x4_t __a, const int __b)
{
  return (int16x4_t)__builtin_neon_vdup_lanev4hi (__a, __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vdup_lane_s32 (int32x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vdup_lanev2si (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vdup_lane_f32 (float32x2_t __a, const int __b)
{
  return (float32x2_t)__builtin_neon_vdup_lanev2sf (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vdup_lane_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x8_t)__builtin_neon_vdup_lanev8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vdup_lane_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vdup_lanev4hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vdup_lane_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vdup_lanev2si ((int32x2_t) __a, __b);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vdup_lane_p8 (poly8x8_t __a, const int __b)
{
  return (poly8x8_t)__builtin_neon_vdup_lanev8qi ((int8x8_t) __a, __b);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vdup_lane_p16 (poly16x4_t __a, const int __b)
{
  return (poly16x4_t)__builtin_neon_vdup_lanev4hi ((int16x4_t) __a, __b);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vdup_lane_p64 (poly64x1_t __a, const int __b)
{
  return (poly64x1_t)__builtin_neon_vdup_lanedi (__a, __b);
}

#pragma GCC pop_options
__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vdup_lane_s64 (int64x1_t __a, const int __b)
{
  return (int64x1_t)__builtin_neon_vdup_lanedi (__a, __b);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vdup_lane_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x1_t)__builtin_neon_vdup_lanedi ((int64x1_t) __a, __b);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vdupq_lane_s8 (int8x8_t __a, const int __b)
{
  return (int8x16_t)__builtin_neon_vdup_lanev16qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vdupq_lane_s16 (int16x4_t __a, const int __b)
{
  return (int16x8_t)__builtin_neon_vdup_lanev8hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vdupq_lane_s32 (int32x2_t __a, const int __b)
{
  return (int32x4_t)__builtin_neon_vdup_lanev4si (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vdupq_lane_f32 (float32x2_t __a, const int __b)
{
  return (float32x4_t)__builtin_neon_vdup_lanev4sf (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vdupq_lane_u8 (uint8x8_t __a, const int __b)
{
  return (uint8x16_t)__builtin_neon_vdup_lanev16qi ((int8x8_t) __a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vdupq_lane_u16 (uint16x4_t __a, const int __b)
{
  return (uint16x8_t)__builtin_neon_vdup_lanev8hi ((int16x4_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vdupq_lane_u32 (uint32x2_t __a, const int __b)
{
  return (uint32x4_t)__builtin_neon_vdup_lanev4si ((int32x2_t) __a, __b);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vdupq_lane_p8 (poly8x8_t __a, const int __b)
{
  return (poly8x16_t)__builtin_neon_vdup_lanev16qi ((int8x8_t) __a, __b);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vdupq_lane_p16 (poly16x4_t __a, const int __b)
{
  return (poly16x8_t)__builtin_neon_vdup_lanev8hi ((int16x4_t) __a, __b);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vdupq_lane_p64 (poly64x1_t __a, const int __b)
{
  return (poly64x2_t)__builtin_neon_vdup_lanev2di (__a, __b);
}

#pragma GCC pop_options
__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vdupq_lane_s64 (int64x1_t __a, const int __b)
{
  return (int64x2_t)__builtin_neon_vdup_lanev2di (__a, __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vdupq_lane_u64 (uint64x1_t __a, const int __b)
{
  return (uint64x2_t)__builtin_neon_vdup_lanev2di ((int64x1_t) __a, __b);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vcombine_p64 (poly64x1_t __a, poly64x1_t __b)
{
  return (poly64x2_t)__builtin_neon_vcombinedi (__a, __b);
}

#pragma GCC pop_options
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vcombine_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x16_t)__builtin_neon_vcombinev8qi (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vcombine_s16 (int16x4_t __a, int16x4_t __b)
{
  return (int16x8_t)__builtin_neon_vcombinev4hi (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vcombine_s32 (int32x2_t __a, int32x2_t __b)
{
  return (int32x4_t)__builtin_neon_vcombinev2si (__a, __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vcombine_s64 (int64x1_t __a, int64x1_t __b)
{
  return (int64x2_t)__builtin_neon_vcombinedi (__a, __b);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vcombine_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vcombinev4hf (__a, __b);
}
#endif

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcombine_f32 (float32x2_t __a, float32x2_t __b)
{
  return (float32x4_t)__builtin_neon_vcombinev2sf (__a, __b);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vcombine_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x16_t)__builtin_neon_vcombinev8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcombine_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return (uint16x8_t)__builtin_neon_vcombinev4hi ((int16x4_t) __a, (int16x4_t) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcombine_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return (uint32x4_t)__builtin_neon_vcombinev2si ((int32x2_t) __a, (int32x2_t) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vcombine_u64 (uint64x1_t __a, uint64x1_t __b)
{
  return (uint64x2_t)__builtin_neon_vcombinedi ((int64x1_t) __a, (int64x1_t) __b);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vcombine_p8 (poly8x8_t __a, poly8x8_t __b)
{
  return (poly8x16_t)__builtin_neon_vcombinev8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vcombine_p16 (poly16x4_t __a, poly16x4_t __b)
{
  return (poly16x8_t)__builtin_neon_vcombinev4hi ((int16x4_t) __a, (int16x4_t) __b);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vget_high_p64 (poly64x2_t __a)
{
  return (poly64x1_t)__builtin_neon_vget_highv2di ((int64x2_t) __a);
}

#pragma GCC pop_options
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vget_high_s8 (int8x16_t __a)
{
  return (int8x8_t)__builtin_neon_vget_highv16qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vget_high_s16 (int16x8_t __a)
{
  return (int16x4_t)__builtin_neon_vget_highv8hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vget_high_s32 (int32x4_t __a)
{
  return (int32x2_t)__builtin_neon_vget_highv4si (__a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vget_high_s64 (int64x2_t __a)
{
  return (int64x1_t)__builtin_neon_vget_highv2di (__a);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vget_high_f16 (float16x8_t __a)
{
  return __builtin_neon_vget_highv8hf (__a);
}
#endif

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vget_high_f32 (float32x4_t __a)
{
  return (float32x2_t)__builtin_neon_vget_highv4sf (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vget_high_u8 (uint8x16_t __a)
{
  return (uint8x8_t)__builtin_neon_vget_highv16qi ((int8x16_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vget_high_u16 (uint16x8_t __a)
{
  return (uint16x4_t)__builtin_neon_vget_highv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vget_high_u32 (uint32x4_t __a)
{
  return (uint32x2_t)__builtin_neon_vget_highv4si ((int32x4_t) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vget_high_u64 (uint64x2_t __a)
{
  return (uint64x1_t)__builtin_neon_vget_highv2di ((int64x2_t) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vget_high_p8 (poly8x16_t __a)
{
  return (poly8x8_t)__builtin_neon_vget_highv16qi ((int8x16_t) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vget_high_p16 (poly16x8_t __a)
{
  return (poly16x4_t)__builtin_neon_vget_highv8hi ((int16x8_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vget_low_s8 (int8x16_t __a)
{
  return (int8x8_t)__builtin_neon_vget_lowv16qi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vget_low_s16 (int16x8_t __a)
{
  return (int16x4_t)__builtin_neon_vget_lowv8hi (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vget_low_s32 (int32x4_t __a)
{
  return (int32x2_t)__builtin_neon_vget_lowv4si (__a);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vget_low_f16 (float16x8_t __a)
{
  return __builtin_neon_vget_lowv8hf (__a);
}
#endif

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vget_low_f32 (float32x4_t __a)
{
  return (float32x2_t)__builtin_neon_vget_lowv4sf (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vget_low_u8 (uint8x16_t __a)
{
  return (uint8x8_t)__builtin_neon_vget_lowv16qi ((int8x16_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vget_low_u16 (uint16x8_t __a)
{
  return (uint16x4_t)__builtin_neon_vget_lowv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vget_low_u32 (uint32x4_t __a)
{
  return (uint32x2_t)__builtin_neon_vget_lowv4si ((int32x4_t) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vget_low_p8 (poly8x16_t __a)
{
  return (poly8x8_t)__builtin_neon_vget_lowv16qi ((int8x16_t) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vget_low_p16 (poly16x8_t __a)
{
  return (poly16x4_t)__builtin_neon_vget_lowv8hi ((int16x8_t) __a);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vget_low_p64 (poly64x2_t __a)
{
  return (poly64x1_t)__builtin_neon_vget_lowv2di ((int64x2_t) __a);
}

#pragma GCC pop_options
__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vget_low_s64 (int64x2_t __a)
{
  return (int64x1_t)__builtin_neon_vget_lowv2di (__a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vget_low_u64 (uint64x2_t __a)
{
  return (uint64x1_t)__builtin_neon_vget_lowv2di ((int64x2_t) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcvt_s32_f32 (float32x2_t __a)
{
  return (int32x2_t)__builtin_neon_vcvtsv2sf (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcvt_f32_s32 (int32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vcvtsv2si (__a);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcvt_f32_u32 (uint32x2_t __a)
{
  return (float32x2_t)__builtin_neon_vcvtuv2si ((int32x2_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcvt_u32_f32 (float32x2_t __a)
{
  return (uint32x2_t)__builtin_neon_vcvtuv2sf (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vcvtq_s32_f32 (float32x4_t __a)
{
  return (int32x4_t)__builtin_neon_vcvtsv4sf (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcvtq_f32_s32 (int32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vcvtsv4si (__a);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcvtq_f32_u32 (uint32x4_t __a)
{
  return (float32x4_t)__builtin_neon_vcvtuv4si ((int32x4_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcvtq_u32_f32 (float32x4_t __a)
{
  return (uint32x4_t)__builtin_neon_vcvtuv4sf (__a);
}

#pragma GCC push_options
#pragma GCC target ("fpu=neon-fp16")
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vcvt_f16_f32 (float32x4_t __a)
{
  return (float16x4_t)__builtin_neon_vcvtv4hfv4sf (__a);
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcvt_f32_f16 (float16x4_t __a)
{
  return (float32x4_t)__builtin_neon_vcvtv4sfv4hf (__a);
}
#endif
#pragma GCC pop_options

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vcvt_n_s32_f32 (float32x2_t __a, const int __b)
{
  return (int32x2_t)__builtin_neon_vcvts_nv2sf (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcvt_n_f32_s32 (int32x2_t __a, const int __b)
{
  return (float32x2_t)__builtin_neon_vcvts_nv2si (__a, __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vcvt_n_f32_u32 (uint32x2_t __a, const int __b)
{
  return (float32x2_t)__builtin_neon_vcvtu_nv2si ((int32x2_t) __a, __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vcvt_n_u32_f32 (float32x2_t __a, const int __b)
{
  return (uint32x2_t)__builtin_neon_vcvtu_nv2sf (__a, __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vcvtq_n_s32_f32 (float32x4_t __a, const int __b)
{
  return (int32x4_t)__builtin_neon_vcvts_nv4sf (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcvtq_n_f32_s32 (int32x4_t __a, const int __b)
{
  return (float32x4_t)__builtin_neon_vcvts_nv4si (__a, __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vcvtq_n_f32_u32 (uint32x4_t __a, const int __b)
{
  return (float32x4_t)__builtin_neon_vcvtu_nv4si ((int32x4_t) __a, __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vcvtq_n_u32_f32 (float32x4_t __a, const int __b)
{
  return (uint32x4_t)__builtin_neon_vcvtu_nv4sf (__a, __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vmovn_s16 (int16x8_t __a)
{
  return (int8x8_t)__builtin_neon_vmovnv8hi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmovn_s32 (int32x4_t __a)
{
  return (int16x4_t)__builtin_neon_vmovnv4si (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmovn_s64 (int64x2_t __a)
{
  return (int32x2_t)__builtin_neon_vmovnv2di (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vmovn_u16 (uint16x8_t __a)
{
  return (uint8x8_t)__builtin_neon_vmovnv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmovn_u32 (uint32x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vmovnv4si ((int32x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmovn_u64 (uint64x2_t __a)
{
  return (uint32x2_t)__builtin_neon_vmovnv2di ((int64x2_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vqmovn_s16 (int16x8_t __a)
{
  return (int8x8_t)__builtin_neon_vqmovnsv8hi (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqmovn_s32 (int32x4_t __a)
{
  return (int16x4_t)__builtin_neon_vqmovnsv4si (__a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqmovn_s64 (int64x2_t __a)
{
  return (int32x2_t)__builtin_neon_vqmovnsv2di (__a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqmovn_u16 (uint16x8_t __a)
{
  return (uint8x8_t)__builtin_neon_vqmovnuv8hi ((int16x8_t) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqmovn_u32 (uint32x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vqmovnuv4si ((int32x4_t) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqmovn_u64 (uint64x2_t __a)
{
  return (uint32x2_t)__builtin_neon_vqmovnuv2di ((int64x2_t) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vqmovun_s16 (int16x8_t __a)
{
  return (uint8x8_t)__builtin_neon_vqmovunv8hi (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vqmovun_s32 (int32x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vqmovunv4si (__a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vqmovun_s64 (int64x2_t __a)
{
  return (uint32x2_t)__builtin_neon_vqmovunv2di (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmovl_s8 (int8x8_t __a)
{
  return (int16x8_t)__builtin_neon_vmovlsv8qi (__a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmovl_s16 (int16x4_t __a)
{
  return (int32x4_t)__builtin_neon_vmovlsv4hi (__a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmovl_s32 (int32x2_t __a)
{
  return (int64x2_t)__builtin_neon_vmovlsv2si (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmovl_u8 (uint8x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vmovluv8qi ((int8x8_t) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmovl_u16 (uint16x4_t __a)
{
  return (uint32x4_t)__builtin_neon_vmovluv4hi ((int16x4_t) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmovl_u32 (uint32x2_t __a)
{
  return (uint64x2_t)__builtin_neon_vmovluv2si ((int32x2_t) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbl1_s8 (int8x8_t __a, int8x8_t __b)
{
  return (int8x8_t)__builtin_neon_vtbl1v8qi (__a, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbl1_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return (uint8x8_t)__builtin_neon_vtbl1v8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbl1_p8 (poly8x8_t __a, uint8x8_t __b)
{
  return (poly8x8_t)__builtin_neon_vtbl1v8qi ((int8x8_t) __a, (int8x8_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbl2_s8 (int8x8x2_t __a, int8x8_t __b)
{
  union { int8x8x2_t __i; __builtin_neon_ti __o; } __au = { __a };
  return (int8x8_t)__builtin_neon_vtbl2v8qi (__au.__o, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbl2_u8 (uint8x8x2_t __a, uint8x8_t __b)
{
  union { uint8x8x2_t __i; __builtin_neon_ti __o; } __au = { __a };
  return (uint8x8_t)__builtin_neon_vtbl2v8qi (__au.__o, (int8x8_t) __b);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbl2_p8 (poly8x8x2_t __a, uint8x8_t __b)
{
  union { poly8x8x2_t __i; __builtin_neon_ti __o; } __au = { __a };
  return (poly8x8_t)__builtin_neon_vtbl2v8qi (__au.__o, (int8x8_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbl3_s8 (int8x8x3_t __a, int8x8_t __b)
{
  union { int8x8x3_t __i; __builtin_neon_ei __o; } __au = { __a };
  return (int8x8_t)__builtin_neon_vtbl3v8qi (__au.__o, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbl3_u8 (uint8x8x3_t __a, uint8x8_t __b)
{
  union { uint8x8x3_t __i; __builtin_neon_ei __o; } __au = { __a };
  return (uint8x8_t)__builtin_neon_vtbl3v8qi (__au.__o, (int8x8_t) __b);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbl3_p8 (poly8x8x3_t __a, uint8x8_t __b)
{
  union { poly8x8x3_t __i; __builtin_neon_ei __o; } __au = { __a };
  return (poly8x8_t)__builtin_neon_vtbl3v8qi (__au.__o, (int8x8_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbl4_s8 (int8x8x4_t __a, int8x8_t __b)
{
  union { int8x8x4_t __i; __builtin_neon_oi __o; } __au = { __a };
  return (int8x8_t)__builtin_neon_vtbl4v8qi (__au.__o, __b);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbl4_u8 (uint8x8x4_t __a, uint8x8_t __b)
{
  union { uint8x8x4_t __i; __builtin_neon_oi __o; } __au = { __a };
  return (uint8x8_t)__builtin_neon_vtbl4v8qi (__au.__o, (int8x8_t) __b);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbl4_p8 (poly8x8x4_t __a, uint8x8_t __b)
{
  union { poly8x8x4_t __i; __builtin_neon_oi __o; } __au = { __a };
  return (poly8x8_t)__builtin_neon_vtbl4v8qi (__au.__o, (int8x8_t) __b);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbx1_s8 (int8x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int8x8_t)__builtin_neon_vtbx1v8qi (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbx1_u8 (uint8x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint8x8_t)__builtin_neon_vtbx1v8qi ((int8x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbx1_p8 (poly8x8_t __a, poly8x8_t __b, uint8x8_t __c)
{
  return (poly8x8_t)__builtin_neon_vtbx1v8qi ((int8x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbx2_s8 (int8x8_t __a, int8x8x2_t __b, int8x8_t __c)
{
  union { int8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  return (int8x8_t)__builtin_neon_vtbx2v8qi (__a, __bu.__o, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbx2_u8 (uint8x8_t __a, uint8x8x2_t __b, uint8x8_t __c)
{
  union { uint8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  return (uint8x8_t)__builtin_neon_vtbx2v8qi ((int8x8_t) __a, __bu.__o, (int8x8_t) __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbx2_p8 (poly8x8_t __a, poly8x8x2_t __b, uint8x8_t __c)
{
  union { poly8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  return (poly8x8_t)__builtin_neon_vtbx2v8qi ((int8x8_t) __a, __bu.__o, (int8x8_t) __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbx3_s8 (int8x8_t __a, int8x8x3_t __b, int8x8_t __c)
{
  union { int8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  return (int8x8_t)__builtin_neon_vtbx3v8qi (__a, __bu.__o, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbx3_u8 (uint8x8_t __a, uint8x8x3_t __b, uint8x8_t __c)
{
  union { uint8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  return (uint8x8_t)__builtin_neon_vtbx3v8qi ((int8x8_t) __a, __bu.__o, (int8x8_t) __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbx3_p8 (poly8x8_t __a, poly8x8x3_t __b, uint8x8_t __c)
{
  union { poly8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  return (poly8x8_t)__builtin_neon_vtbx3v8qi ((int8x8_t) __a, __bu.__o, (int8x8_t) __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vtbx4_s8 (int8x8_t __a, int8x8x4_t __b, int8x8_t __c)
{
  union { int8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  return (int8x8_t)__builtin_neon_vtbx4v8qi (__a, __bu.__o, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vtbx4_u8 (uint8x8_t __a, uint8x8x4_t __b, uint8x8_t __c)
{
  union { uint8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  return (uint8x8_t)__builtin_neon_vtbx4v8qi ((int8x8_t) __a, __bu.__o, (int8x8_t) __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vtbx4_p8 (poly8x8_t __a, poly8x8x4_t __b, uint8x8_t __c)
{
  union { poly8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  return (poly8x8_t)__builtin_neon_vtbx4v8qi ((int8x8_t) __a, __bu.__o, (int8x8_t) __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmul_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vmul_lanev4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmul_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vmul_lanev2si (__a, __b, __c);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmul_lane_f32 (float32x2_t __a, float32x2_t __b, const int __c)
{
  return (float32x2_t)__builtin_neon_vmul_lanev2sf (__a, __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmul_lane_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t)__builtin_neon_vmul_lanev4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmul_lane_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t)__builtin_neon_vmul_lanev2si ((int32x2_t) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmulq_lane_s16 (int16x8_t __a, int16x4_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vmul_lanev8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmulq_lane_s32 (int32x4_t __a, int32x2_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vmul_lanev4si (__a, __b, __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmulq_lane_f32 (float32x4_t __a, float32x2_t __b, const int __c)
{
  return (float32x4_t)__builtin_neon_vmul_lanev4sf (__a, __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmulq_lane_u16 (uint16x8_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x8_t)__builtin_neon_vmul_lanev8hi ((int16x8_t) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmulq_lane_u32 (uint32x4_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x4_t)__builtin_neon_vmul_lanev4si ((int32x4_t) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmla_lane_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return (int16x4_t)__builtin_neon_vmla_lanev4hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmla_lane_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return (int32x2_t)__builtin_neon_vmla_lanev2si (__a, __b, __c, __d);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmla_lane_f32 (float32x2_t __a, float32x2_t __b, float32x2_t __c, const int __d)
{
  return (float32x2_t)__builtin_neon_vmla_lanev2sf (__a, __b, __c, __d);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmla_lane_u16 (uint16x4_t __a, uint16x4_t __b, uint16x4_t __c, const int __d)
{
  return (uint16x4_t)__builtin_neon_vmla_lanev4hi ((int16x4_t) __a, (int16x4_t) __b, (int16x4_t) __c, __d);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmla_lane_u32 (uint32x2_t __a, uint32x2_t __b, uint32x2_t __c, const int __d)
{
  return (uint32x2_t)__builtin_neon_vmla_lanev2si ((int32x2_t) __a, (int32x2_t) __b, (int32x2_t) __c, __d);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlaq_lane_s16 (int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __d)
{
  return (int16x8_t)__builtin_neon_vmla_lanev8hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlaq_lane_s32 (int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __d)
{
  return (int32x4_t)__builtin_neon_vmla_lanev4si (__a, __b, __c, __d);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlaq_lane_f32 (float32x4_t __a, float32x4_t __b, float32x2_t __c, const int __d)
{
  return (float32x4_t)__builtin_neon_vmla_lanev4sf (__a, __b, __c, __d);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlaq_lane_u16 (uint16x8_t __a, uint16x8_t __b, uint16x4_t __c, const int __d)
{
  return (uint16x8_t)__builtin_neon_vmla_lanev8hi ((int16x8_t) __a, (int16x8_t) __b, (int16x4_t) __c, __d);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlaq_lane_u32 (uint32x4_t __a, uint32x4_t __b, uint32x2_t __c, const int __d)
{
  return (uint32x4_t)__builtin_neon_vmla_lanev4si ((int32x4_t) __a, (int32x4_t) __b, (int32x2_t) __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlal_lane_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return (int32x4_t)__builtin_neon_vmlals_lanev4hi (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlal_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return (int64x2_t)__builtin_neon_vmlals_lanev2si (__a, __b, __c, __d);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlal_lane_u16 (uint32x4_t __a, uint16x4_t __b, uint16x4_t __c, const int __d)
{
  return (uint32x4_t)__builtin_neon_vmlalu_lanev4hi ((int32x4_t) __a, (int16x4_t) __b, (int16x4_t) __c, __d);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlal_lane_u32 (uint64x2_t __a, uint32x2_t __b, uint32x2_t __c, const int __d)
{
  return (uint64x2_t)__builtin_neon_vmlalu_lanev2si ((int64x2_t) __a, (int32x2_t) __b, (int32x2_t) __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_lane_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return (int32x4_t)__builtin_neon_vqdmlal_lanev4hi (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return (int64x2_t)__builtin_neon_vqdmlal_lanev2si (__a, __b, __c, __d);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmls_lane_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return (int16x4_t)__builtin_neon_vmls_lanev4hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmls_lane_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return (int32x2_t)__builtin_neon_vmls_lanev2si (__a, __b, __c, __d);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmls_lane_f32 (float32x2_t __a, float32x2_t __b, float32x2_t __c, const int __d)
{
  return (float32x2_t)__builtin_neon_vmls_lanev2sf (__a, __b, __c, __d);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmls_lane_u16 (uint16x4_t __a, uint16x4_t __b, uint16x4_t __c, const int __d)
{
  return (uint16x4_t)__builtin_neon_vmls_lanev4hi ((int16x4_t) __a, (int16x4_t) __b, (int16x4_t) __c, __d);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmls_lane_u32 (uint32x2_t __a, uint32x2_t __b, uint32x2_t __c, const int __d)
{
  return (uint32x2_t)__builtin_neon_vmls_lanev2si ((int32x2_t) __a, (int32x2_t) __b, (int32x2_t) __c, __d);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsq_lane_s16 (int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __d)
{
  return (int16x8_t)__builtin_neon_vmls_lanev8hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsq_lane_s32 (int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __d)
{
  return (int32x4_t)__builtin_neon_vmls_lanev4si (__a, __b, __c, __d);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlsq_lane_f32 (float32x4_t __a, float32x4_t __b, float32x2_t __c, const int __d)
{
  return (float32x4_t)__builtin_neon_vmls_lanev4sf (__a, __b, __c, __d);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsq_lane_u16 (uint16x8_t __a, uint16x8_t __b, uint16x4_t __c, const int __d)
{
  return (uint16x8_t)__builtin_neon_vmls_lanev8hi ((int16x8_t) __a, (int16x8_t) __b, (int16x4_t) __c, __d);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsq_lane_u32 (uint32x4_t __a, uint32x4_t __b, uint32x2_t __c, const int __d)
{
  return (uint32x4_t)__builtin_neon_vmls_lanev4si ((int32x4_t) __a, (int32x4_t) __b, (int32x2_t) __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsl_lane_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return (int32x4_t)__builtin_neon_vmlsls_lanev4hi (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlsl_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return (int64x2_t)__builtin_neon_vmlsls_lanev2si (__a, __b, __c, __d);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsl_lane_u16 (uint32x4_t __a, uint16x4_t __b, uint16x4_t __c, const int __d)
{
  return (uint32x4_t)__builtin_neon_vmlslu_lanev4hi ((int32x4_t) __a, (int16x4_t) __b, (int16x4_t) __c, __d);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlsl_lane_u32 (uint64x2_t __a, uint32x2_t __b, uint32x2_t __c, const int __d)
{
  return (uint64x2_t)__builtin_neon_vmlslu_lanev2si ((int64x2_t) __a, (int32x2_t) __b, (int32x2_t) __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_lane_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return (int32x4_t)__builtin_neon_vqdmlsl_lanev4hi (__a, __b, __c, __d);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return (int64x2_t)__builtin_neon_vqdmlsl_lanev2si (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmull_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vmulls_lanev4hi (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmull_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int64x2_t)__builtin_neon_vmulls_lanev2si (__a, __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmull_lane_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint32x4_t)__builtin_neon_vmullu_lanev4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmull_lane_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint64x2_t)__builtin_neon_vmullu_lanev2si ((int32x2_t) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vqdmull_lanev4hi (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int64x2_t)__builtin_neon_vqdmull_lanev2si (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqdmulhq_lane_s16 (int16x8_t __a, int16x4_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vqdmulh_lanev8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmulhq_lane_s32 (int32x4_t __a, int32x2_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vqdmulh_lanev4si (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqdmulh_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vqdmulh_lanev4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqdmulh_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vqdmulh_lanev2si (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmulhq_lane_s16 (int16x8_t __a, int16x4_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vqrdmulh_lanev8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmulhq_lane_s32 (int32x4_t __a, int32x2_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vqrdmulh_lanev4si (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmulh_lane_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vqrdmulh_lanev4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmulh_lane_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vqrdmulh_lanev2si (__a, __b, __c);
}

#ifdef __ARM_FEATURE_QRDMX
__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmlahq_lane_s16 (int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __d)
{
  return (int16x8_t)__builtin_neon_vqrdmlah_lanev8hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmlahq_lane_s32 (int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __d)
{
  return (int32x4_t)__builtin_neon_vqrdmlah_lanev4si (__a, __b, __c, __d);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmlah_lane_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return (int16x4_t)__builtin_neon_vqrdmlah_lanev4hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmlah_lane_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return (int32x2_t)__builtin_neon_vqrdmlah_lanev2si (__a, __b, __c, __d);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmlshq_lane_s16 (int16x8_t __a, int16x8_t __b, int16x4_t __c, const int __d)
{
  return (int16x8_t)__builtin_neon_vqrdmlsh_lanev8hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmlshq_lane_s32 (int32x4_t __a, int32x4_t __b, int32x2_t __c, const int __d)
{
  return (int32x4_t)__builtin_neon_vqrdmlsh_lanev4si (__a, __b, __c, __d);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmlsh_lane_s16 (int16x4_t __a, int16x4_t __b, int16x4_t __c, const int __d)
{
  return (int16x4_t)__builtin_neon_vqrdmlsh_lanev4hi (__a, __b, __c, __d);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmlsh_lane_s32 (int32x2_t __a, int32x2_t __b, int32x2_t __c, const int __d)
{
  return (int32x2_t)__builtin_neon_vqrdmlsh_lanev2si (__a, __b, __c, __d);
}
#endif

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmul_n_s16 (int16x4_t __a, int16_t __b)
{
  return (int16x4_t)__builtin_neon_vmul_nv4hi (__a, (__builtin_neon_hi) __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmul_n_s32 (int32x2_t __a, int32_t __b)
{
  return (int32x2_t)__builtin_neon_vmul_nv2si (__a, (__builtin_neon_si) __b);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmul_n_f32 (float32x2_t __a, float32_t __b)
{
  return (float32x2_t)__builtin_neon_vmul_nv2sf (__a, (__builtin_neon_sf) __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmul_n_u16 (uint16x4_t __a, uint16_t __b)
{
  return (uint16x4_t)__builtin_neon_vmul_nv4hi ((int16x4_t) __a, (__builtin_neon_hi) __b);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmul_n_u32 (uint32x2_t __a, uint32_t __b)
{
  return (uint32x2_t)__builtin_neon_vmul_nv2si ((int32x2_t) __a, (__builtin_neon_si) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmulq_n_s16 (int16x8_t __a, int16_t __b)
{
  return (int16x8_t)__builtin_neon_vmul_nv8hi (__a, (__builtin_neon_hi) __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmulq_n_s32 (int32x4_t __a, int32_t __b)
{
  return (int32x4_t)__builtin_neon_vmul_nv4si (__a, (__builtin_neon_si) __b);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmulq_n_f32 (float32x4_t __a, float32_t __b)
{
  return (float32x4_t)__builtin_neon_vmul_nv4sf (__a, (__builtin_neon_sf) __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmulq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return (uint16x8_t)__builtin_neon_vmul_nv8hi ((int16x8_t) __a, (__builtin_neon_hi) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmulq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return (uint32x4_t)__builtin_neon_vmul_nv4si ((int32x4_t) __a, (__builtin_neon_si) __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmull_n_s16 (int16x4_t __a, int16_t __b)
{
  return (int32x4_t)__builtin_neon_vmulls_nv4hi (__a, (__builtin_neon_hi) __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmull_n_s32 (int32x2_t __a, int32_t __b)
{
  return (int64x2_t)__builtin_neon_vmulls_nv2si (__a, (__builtin_neon_si) __b);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmull_n_u16 (uint16x4_t __a, uint16_t __b)
{
  return (uint32x4_t)__builtin_neon_vmullu_nv4hi ((int16x4_t) __a, (__builtin_neon_hi) __b);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmull_n_u32 (uint32x2_t __a, uint32_t __b)
{
  return (uint64x2_t)__builtin_neon_vmullu_nv2si ((int32x2_t) __a, (__builtin_neon_si) __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmull_n_s16 (int16x4_t __a, int16_t __b)
{
  return (int32x4_t)__builtin_neon_vqdmull_nv4hi (__a, (__builtin_neon_hi) __b);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmull_n_s32 (int32x2_t __a, int32_t __b)
{
  return (int64x2_t)__builtin_neon_vqdmull_nv2si (__a, (__builtin_neon_si) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqdmulhq_n_s16 (int16x8_t __a, int16_t __b)
{
  return (int16x8_t)__builtin_neon_vqdmulh_nv8hi (__a, (__builtin_neon_hi) __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmulhq_n_s32 (int32x4_t __a, int32_t __b)
{
  return (int32x4_t)__builtin_neon_vqdmulh_nv4si (__a, (__builtin_neon_si) __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqdmulh_n_s16 (int16x4_t __a, int16_t __b)
{
  return (int16x4_t)__builtin_neon_vqdmulh_nv4hi (__a, (__builtin_neon_hi) __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqdmulh_n_s32 (int32x2_t __a, int32_t __b)
{
  return (int32x2_t)__builtin_neon_vqdmulh_nv2si (__a, (__builtin_neon_si) __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vqrdmulhq_n_s16 (int16x8_t __a, int16_t __b)
{
  return (int16x8_t)__builtin_neon_vqrdmulh_nv8hi (__a, (__builtin_neon_hi) __b);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqrdmulhq_n_s32 (int32x4_t __a, int32_t __b)
{
  return (int32x4_t)__builtin_neon_vqrdmulh_nv4si (__a, (__builtin_neon_si) __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vqrdmulh_n_s16 (int16x4_t __a, int16_t __b)
{
  return (int16x4_t)__builtin_neon_vqrdmulh_nv4hi (__a, (__builtin_neon_hi) __b);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vqrdmulh_n_s32 (int32x2_t __a, int32_t __b)
{
  return (int32x2_t)__builtin_neon_vqrdmulh_nv2si (__a, (__builtin_neon_si) __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmla_n_s16 (int16x4_t __a, int16x4_t __b, int16_t __c)
{
  return (int16x4_t)__builtin_neon_vmla_nv4hi (__a, __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmla_n_s32 (int32x2_t __a, int32x2_t __b, int32_t __c)
{
  return (int32x2_t)__builtin_neon_vmla_nv2si (__a, __b, (__builtin_neon_si) __c);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmla_n_f32 (float32x2_t __a, float32x2_t __b, float32_t __c)
{
  return (float32x2_t)__builtin_neon_vmla_nv2sf (__a, __b, (__builtin_neon_sf) __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmla_n_u16 (uint16x4_t __a, uint16x4_t __b, uint16_t __c)
{
  return (uint16x4_t)__builtin_neon_vmla_nv4hi ((int16x4_t) __a, (int16x4_t) __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmla_n_u32 (uint32x2_t __a, uint32x2_t __b, uint32_t __c)
{
  return (uint32x2_t)__builtin_neon_vmla_nv2si ((int32x2_t) __a, (int32x2_t) __b, (__builtin_neon_si) __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlaq_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c)
{
  return (int16x8_t)__builtin_neon_vmla_nv8hi (__a, __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlaq_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c)
{
  return (int32x4_t)__builtin_neon_vmla_nv4si (__a, __b, (__builtin_neon_si) __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlaq_n_f32 (float32x4_t __a, float32x4_t __b, float32_t __c)
{
  return (float32x4_t)__builtin_neon_vmla_nv4sf (__a, __b, (__builtin_neon_sf) __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlaq_n_u16 (uint16x8_t __a, uint16x8_t __b, uint16_t __c)
{
  return (uint16x8_t)__builtin_neon_vmla_nv8hi ((int16x8_t) __a, (int16x8_t) __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlaq_n_u32 (uint32x4_t __a, uint32x4_t __b, uint32_t __c)
{
  return (uint32x4_t)__builtin_neon_vmla_nv4si ((int32x4_t) __a, (int32x4_t) __b, (__builtin_neon_si) __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlal_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return (int32x4_t)__builtin_neon_vmlals_nv4hi (__a, __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlal_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return (int64x2_t)__builtin_neon_vmlals_nv2si (__a, __b, (__builtin_neon_si) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlal_n_u16 (uint32x4_t __a, uint16x4_t __b, uint16_t __c)
{
  return (uint32x4_t)__builtin_neon_vmlalu_nv4hi ((int32x4_t) __a, (int16x4_t) __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlal_n_u32 (uint64x2_t __a, uint32x2_t __b, uint32_t __c)
{
  return (uint64x2_t)__builtin_neon_vmlalu_nv2si ((int64x2_t) __a, (int32x2_t) __b, (__builtin_neon_si) __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlal_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return (int32x4_t)__builtin_neon_vqdmlal_nv4hi (__a, __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlal_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return (int64x2_t)__builtin_neon_vqdmlal_nv2si (__a, __b, (__builtin_neon_si) __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vmls_n_s16 (int16x4_t __a, int16x4_t __b, int16_t __c)
{
  return (int16x4_t)__builtin_neon_vmls_nv4hi (__a, __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vmls_n_s32 (int32x2_t __a, int32x2_t __b, int32_t __c)
{
  return (int32x2_t)__builtin_neon_vmls_nv2si (__a, __b, (__builtin_neon_si) __c);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vmls_n_f32 (float32x2_t __a, float32x2_t __b, float32_t __c)
{
  return (float32x2_t)__builtin_neon_vmls_nv2sf (__a, __b, (__builtin_neon_sf) __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vmls_n_u16 (uint16x4_t __a, uint16x4_t __b, uint16_t __c)
{
  return (uint16x4_t)__builtin_neon_vmls_nv4hi ((int16x4_t) __a, (int16x4_t) __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vmls_n_u32 (uint32x2_t __a, uint32x2_t __b, uint32_t __c)
{
  return (uint32x2_t)__builtin_neon_vmls_nv2si ((int32x2_t) __a, (int32x2_t) __b, (__builtin_neon_si) __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vmlsq_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c)
{
  return (int16x8_t)__builtin_neon_vmls_nv8hi (__a, __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsq_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c)
{
  return (int32x4_t)__builtin_neon_vmls_nv4si (__a, __b, (__builtin_neon_si) __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vmlsq_n_f32 (float32x4_t __a, float32x4_t __b, float32_t __c)
{
  return (float32x4_t)__builtin_neon_vmls_nv4sf (__a, __b, (__builtin_neon_sf) __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vmlsq_n_u16 (uint16x8_t __a, uint16x8_t __b, uint16_t __c)
{
  return (uint16x8_t)__builtin_neon_vmls_nv8hi ((int16x8_t) __a, (int16x8_t) __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsq_n_u32 (uint32x4_t __a, uint32x4_t __b, uint32_t __c)
{
  return (uint32x4_t)__builtin_neon_vmls_nv4si ((int32x4_t) __a, (int32x4_t) __b, (__builtin_neon_si) __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vmlsl_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return (int32x4_t)__builtin_neon_vmlsls_nv4hi (__a, __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vmlsl_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return (int64x2_t)__builtin_neon_vmlsls_nv2si (__a, __b, (__builtin_neon_si) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vmlsl_n_u16 (uint32x4_t __a, uint16x4_t __b, uint16_t __c)
{
  return (uint32x4_t)__builtin_neon_vmlslu_nv4hi ((int32x4_t) __a, (int16x4_t) __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vmlsl_n_u32 (uint64x2_t __a, uint32x2_t __b, uint32_t __c)
{
  return (uint64x2_t)__builtin_neon_vmlslu_nv2si ((int64x2_t) __a, (int32x2_t) __b, (__builtin_neon_si) __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vqdmlsl_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return (int32x4_t)__builtin_neon_vqdmlsl_nv4hi (__a, __b, (__builtin_neon_hi) __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vqdmlsl_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return (int64x2_t)__builtin_neon_vqdmlsl_nv2si (__a, __b, (__builtin_neon_si) __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vext_p64 (poly64x1_t __a, poly64x1_t __b, const int __c)
{
  return (poly64x1_t)__builtin_neon_vextdi (__a, __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vext_s8 (int8x8_t __a, int8x8_t __b, const int __c)
{
  return (int8x8_t)__builtin_neon_vextv8qi (__a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vext_s16 (int16x4_t __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vextv4hi (__a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vext_s32 (int32x2_t __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vextv2si (__a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vext_s64 (int64x1_t __a, int64x1_t __b, const int __c)
{
  return (int64x1_t)__builtin_neon_vextdi (__a, __b, __c);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vext_f32 (float32x2_t __a, float32x2_t __b, const int __c)
{
  return (float32x2_t)__builtin_neon_vextv2sf (__a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vext_u8 (uint8x8_t __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t)__builtin_neon_vextv8qi ((int8x8_t) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vext_u16 (uint16x4_t __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t)__builtin_neon_vextv4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vext_u32 (uint32x2_t __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t)__builtin_neon_vextv2si ((int32x2_t) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vext_u64 (uint64x1_t __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t)__builtin_neon_vextdi ((int64x1_t) __a, (int64x1_t) __b, __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vext_p8 (poly8x8_t __a, poly8x8_t __b, const int __c)
{
  return (poly8x8_t)__builtin_neon_vextv8qi ((int8x8_t) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vext_p16 (poly16x4_t __a, poly16x4_t __b, const int __c)
{
  return (poly16x4_t)__builtin_neon_vextv4hi ((int16x4_t) __a, (int16x4_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vextq_p64 (poly64x2_t __a, poly64x2_t __b, const int __c)
{
  return (poly64x2_t)__builtin_neon_vextv2di ((int64x2_t) __a, (int64x2_t) __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vextq_s8 (int8x16_t __a, int8x16_t __b, const int __c)
{
  return (int8x16_t)__builtin_neon_vextv16qi (__a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vextq_s16 (int16x8_t __a, int16x8_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vextv8hi (__a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vextq_s32 (int32x4_t __a, int32x4_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vextv4si (__a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vextq_s64 (int64x2_t __a, int64x2_t __b, const int __c)
{
  return (int64x2_t)__builtin_neon_vextv2di (__a, __b, __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vextq_f32 (float32x4_t __a, float32x4_t __b, const int __c)
{
  return (float32x4_t)__builtin_neon_vextv4sf (__a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vextq_u8 (uint8x16_t __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t)__builtin_neon_vextv16qi ((int8x16_t) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vextq_u16 (uint16x8_t __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t)__builtin_neon_vextv8hi ((int16x8_t) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vextq_u32 (uint32x4_t __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t)__builtin_neon_vextv4si ((int32x4_t) __a, (int32x4_t) __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vextq_u64 (uint64x2_t __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t)__builtin_neon_vextv2di ((int64x2_t) __a, (int64x2_t) __b, __c);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vextq_p8 (poly8x16_t __a, poly8x16_t __b, const int __c)
{
  return (poly8x16_t)__builtin_neon_vextv16qi ((int8x16_t) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vextq_p16 (poly16x8_t __a, poly16x8_t __b, const int __c)
{
  return (poly16x8_t)__builtin_neon_vextv8hi ((int16x8_t) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrev64_s8 (int8x8_t __a)
{
  return (int8x8_t) __builtin_shuffle (__a, (uint8x8_t) { 7, 6, 5, 4, 3, 2, 1, 0 });
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrev64_s16 (int16x4_t __a)
{
  return (int16x4_t) __builtin_shuffle (__a, (uint16x4_t) { 3, 2, 1, 0 });
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vrev64_s32 (int32x2_t __a)
{
  return (int32x2_t) __builtin_shuffle (__a, (uint32x2_t) { 1, 0 });
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vrev64_f32 (float32x2_t __a)
{
  return (float32x2_t) __builtin_shuffle (__a, (uint32x2_t) { 1, 0 });
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrev64_u8 (uint8x8_t __a)
{
  return (uint8x8_t) __builtin_shuffle (__a, (uint8x8_t) { 7, 6, 5, 4, 3, 2, 1, 0 });
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrev64_u16 (uint16x4_t __a)
{
  return (uint16x4_t) __builtin_shuffle (__a, (uint16x4_t) { 3, 2, 1, 0 });
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vrev64_u32 (uint32x2_t __a)
{
  return (uint32x2_t) __builtin_shuffle (__a, (uint32x2_t) { 1, 0 });
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vrev64_p8 (poly8x8_t __a)
{
  return (poly8x8_t) __builtin_shuffle (__a, (uint8x8_t) { 7, 6, 5, 4, 3, 2, 1, 0 });
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vrev64_p16 (poly16x4_t __a)
{
  return (poly16x4_t) __builtin_shuffle (__a, (uint16x4_t) { 3, 2, 1, 0 });
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrev64q_s8 (int8x16_t __a)
{
  return (int8x16_t) __builtin_shuffle (__a, (uint8x16_t) { 7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8 });
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrev64q_s16 (int16x8_t __a)
{
  return (int16x8_t) __builtin_shuffle (__a, (uint16x8_t) { 3, 2, 1, 0, 7, 6, 5, 4 });
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vrev64q_s32 (int32x4_t __a)
{
  return (int32x4_t) __builtin_shuffle (__a, (uint32x4_t) { 1, 0, 3, 2 });
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vrev64q_f32 (float32x4_t __a)
{
  return (float32x4_t) __builtin_shuffle (__a, (uint32x4_t) { 1, 0, 3, 2 });
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrev64q_u8 (uint8x16_t __a)
{
  return (uint8x16_t) __builtin_shuffle (__a, (uint8x16_t) { 7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8 });
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrev64q_u16 (uint16x8_t __a)
{
  return (uint16x8_t) __builtin_shuffle (__a, (uint16x8_t) { 3, 2, 1, 0, 7, 6, 5, 4 });
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vrev64q_u32 (uint32x4_t __a)
{
  return (uint32x4_t) __builtin_shuffle (__a, (uint32x4_t) { 1, 0, 3, 2 });
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vrev64q_p8 (poly8x16_t __a)
{
  return (poly8x16_t) __builtin_shuffle (__a, (uint8x16_t) { 7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8 });
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vrev64q_p16 (poly16x8_t __a)
{
  return (poly16x8_t) __builtin_shuffle (__a, (uint16x8_t) { 3, 2, 1, 0, 7, 6, 5, 4 });
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrev32_s8 (int8x8_t __a)
{
  return (int8x8_t) __builtin_shuffle (__a, (uint8x8_t) { 3, 2, 1, 0, 7, 6, 5, 4 });
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vrev32_s16 (int16x4_t __a)
{
  return (int16x4_t) __builtin_shuffle (__a, (uint16x4_t) { 1, 0, 3, 2 });
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrev32_u8 (uint8x8_t __a)
{
  return (uint8x8_t) __builtin_shuffle (__a, (uint8x8_t) { 3, 2, 1, 0, 7, 6, 5, 4 });
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vrev32_u16 (uint16x4_t __a)
{
  return (uint16x4_t) __builtin_shuffle (__a, (uint16x4_t) { 1, 0, 3, 2 });
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vrev32_p8 (poly8x8_t __a)
{
  return (poly8x8_t) __builtin_shuffle (__a, (uint8x8_t) { 3, 2, 1, 0, 7, 6, 5, 4 });
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vrev32_p16 (poly16x4_t __a)
{
  return (poly16x4_t) __builtin_shuffle (__a, (uint16x4_t) { 1, 0, 3, 2 });
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrev32q_s8 (int8x16_t __a)
{
  return (int8x16_t) __builtin_shuffle (__a, (uint8x16_t) { 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12 });
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vrev32q_s16 (int16x8_t __a)
{
  return (int16x8_t) __builtin_shuffle (__a, (uint16x8_t) { 1, 0, 3, 2, 5, 4, 7, 6 });
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrev32q_u8 (uint8x16_t __a)
{
  return (uint8x16_t) __builtin_shuffle (__a, (uint8x16_t) { 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12 });
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vrev32q_u16 (uint16x8_t __a)
{
  return (uint16x8_t) __builtin_shuffle (__a, (uint16x8_t) { 1, 0, 3, 2, 5, 4, 7, 6 });
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vrev32q_p8 (poly8x16_t __a)
{
  return (poly8x16_t) __builtin_shuffle (__a, (uint8x16_t) { 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12 });
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vrev32q_p16 (poly16x8_t __a)
{
  return (poly16x8_t) __builtin_shuffle (__a, (uint16x8_t) { 1, 0, 3, 2, 5, 4, 7, 6 });
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vrev16_s8 (int8x8_t __a)
{
  return (int8x8_t) __builtin_shuffle (__a, (uint8x8_t) { 1, 0, 3, 2, 5, 4, 7, 6 });
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vrev16_u8 (uint8x8_t __a)
{
  return (uint8x8_t) __builtin_shuffle (__a, (uint8x8_t) { 1, 0, 3, 2, 5, 4, 7, 6 });
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vrev16_p8 (poly8x8_t __a)
{
  return (poly8x8_t) __builtin_shuffle (__a, (uint8x8_t) { 1, 0, 3, 2, 5, 4, 7, 6 });
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vrev16q_s8 (int8x16_t __a)
{
  return (int8x16_t) __builtin_shuffle (__a, (uint8x16_t) { 1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14 });
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vrev16q_u8 (uint8x16_t __a)
{
  return (uint8x16_t) __builtin_shuffle (__a, (uint8x16_t) { 1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14 });
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vrev16q_p8 (poly8x16_t __a)
{
  return (poly8x16_t) __builtin_shuffle (__a, (uint8x16_t) { 1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14 });
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vbsl_p64 (uint64x1_t __a, poly64x1_t __b, poly64x1_t __c)
{
  return (poly64x1_t)__builtin_neon_vbsldi ((int64x1_t) __a, __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vbsl_s8 (uint8x8_t __a, int8x8_t __b, int8x8_t __c)
{
  return (int8x8_t)__builtin_neon_vbslv8qi ((int8x8_t) __a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vbsl_s16 (uint16x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return (int16x4_t)__builtin_neon_vbslv4hi ((int16x4_t) __a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vbsl_s32 (uint32x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return (int32x2_t)__builtin_neon_vbslv2si ((int32x2_t) __a, __b, __c);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vbsl_s64 (uint64x1_t __a, int64x1_t __b, int64x1_t __c)
{
  return (int64x1_t)__builtin_neon_vbsldi ((int64x1_t) __a, __b, __c);
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vbsl_f32 (uint32x2_t __a, float32x2_t __b, float32x2_t __c)
{
  return (float32x2_t)__builtin_neon_vbslv2sf ((int32x2_t) __a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vbsl_u8 (uint8x8_t __a, uint8x8_t __b, uint8x8_t __c)
{
  return (uint8x8_t)__builtin_neon_vbslv8qi ((int8x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vbsl_u16 (uint16x4_t __a, uint16x4_t __b, uint16x4_t __c)
{
  return (uint16x4_t)__builtin_neon_vbslv4hi ((int16x4_t) __a, (int16x4_t) __b, (int16x4_t) __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vbsl_u32 (uint32x2_t __a, uint32x2_t __b, uint32x2_t __c)
{
  return (uint32x2_t)__builtin_neon_vbslv2si ((int32x2_t) __a, (int32x2_t) __b, (int32x2_t) __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vbsl_u64 (uint64x1_t __a, uint64x1_t __b, uint64x1_t __c)
{
  return (uint64x1_t)__builtin_neon_vbsldi ((int64x1_t) __a, (int64x1_t) __b, (int64x1_t) __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vbsl_p8 (uint8x8_t __a, poly8x8_t __b, poly8x8_t __c)
{
  return (poly8x8_t)__builtin_neon_vbslv8qi ((int8x8_t) __a, (int8x8_t) __b, (int8x8_t) __c);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vbsl_p16 (uint16x4_t __a, poly16x4_t __b, poly16x4_t __c)
{
  return (poly16x4_t)__builtin_neon_vbslv4hi ((int16x4_t) __a, (int16x4_t) __b, (int16x4_t) __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vbslq_p64 (uint64x2_t __a, poly64x2_t __b, poly64x2_t __c)
{
  return (poly64x2_t)__builtin_neon_vbslv2di ((int64x2_t) __a, (int64x2_t) __b, (int64x2_t) __c);
}

#pragma GCC pop_options
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vbslq_s8 (uint8x16_t __a, int8x16_t __b, int8x16_t __c)
{
  return (int8x16_t)__builtin_neon_vbslv16qi ((int8x16_t) __a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vbslq_s16 (uint16x8_t __a, int16x8_t __b, int16x8_t __c)
{
  return (int16x8_t)__builtin_neon_vbslv8hi ((int16x8_t) __a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vbslq_s32 (uint32x4_t __a, int32x4_t __b, int32x4_t __c)
{
  return (int32x4_t)__builtin_neon_vbslv4si ((int32x4_t) __a, __b, __c);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vbslq_s64 (uint64x2_t __a, int64x2_t __b, int64x2_t __c)
{
  return (int64x2_t)__builtin_neon_vbslv2di ((int64x2_t) __a, __b, __c);
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vbslq_f32 (uint32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return (float32x4_t)__builtin_neon_vbslv4sf ((int32x4_t) __a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vbslq_u8 (uint8x16_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return (uint8x16_t)__builtin_neon_vbslv16qi ((int8x16_t) __a, (int8x16_t) __b, (int8x16_t) __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vbslq_u16 (uint16x8_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return (uint16x8_t)__builtin_neon_vbslv8hi ((int16x8_t) __a, (int16x8_t) __b, (int16x8_t) __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vbslq_u32 (uint32x4_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return (uint32x4_t)__builtin_neon_vbslv4si ((int32x4_t) __a, (int32x4_t) __b, (int32x4_t) __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vbslq_u64 (uint64x2_t __a, uint64x2_t __b, uint64x2_t __c)
{
  return (uint64x2_t)__builtin_neon_vbslv2di ((int64x2_t) __a, (int64x2_t) __b, (int64x2_t) __c);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vbslq_p8 (uint8x16_t __a, poly8x16_t __b, poly8x16_t __c)
{
  return (poly8x16_t)__builtin_neon_vbslv16qi ((int8x16_t) __a, (int8x16_t) __b, (int8x16_t) __c);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vbslq_p16 (uint16x8_t __a, poly16x8_t __b, poly16x8_t __c)
{
  return (poly16x8_t)__builtin_neon_vbslv8hi ((int16x8_t) __a, (int16x8_t) __b, (int16x8_t) __c);
}

/* For big-endian, the shuffle masks for ZIP, UZP and TRN must be changed as
   follows. (nelt = the number of elements within a vector.)

   Firstly, a value of N within a mask, becomes (N ^ (nelt - 1)), as gcc vector
   extension's indexing scheme is reversed *within each vector* (relative to the
   neon intrinsics view), but without changing which of the two vectors.

   Secondly, the elements within each mask are reversed, as the mask is itself a
   vector, and will itself be loaded in reverse order (again, relative to the
   neon intrinsics view, i.e. that would result from a "vld1" instruction).  */

__extension__ static __inline int8x8x2_t __attribute__ ((__always_inline__))
vtrn_s8 (int8x8_t __a, int8x8_t __b)
{
  int8x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 9, 1, 11, 3, 13, 5, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 8, 0, 10, 2, 12, 4, 14, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 0, 8, 2, 10, 4, 12, 6, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 1, 9, 3, 11, 5, 13, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline int16x4x2_t __attribute__ ((__always_inline__))
vtrn_s16 (int16x4_t __a, int16x4_t __b)
{
  int16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 5, 1, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 4, 0, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 0, 4, 2, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 1, 5, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline uint8x8x2_t __attribute__ ((__always_inline__))
vtrn_u8 (uint8x8_t __a, uint8x8_t __b)
{
  uint8x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 9, 1, 11, 3, 13, 5, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 8, 0, 10, 2, 12, 4, 14, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 0, 8, 2, 10, 4, 12, 6, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 1, 9, 3, 11, 5, 13, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline uint16x4x2_t __attribute__ ((__always_inline__))
vtrn_u16 (uint16x4_t __a, uint16x4_t __b)
{
  uint16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 5, 1, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 4, 0, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 0, 4, 2, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 1, 5, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline poly8x8x2_t __attribute__ ((__always_inline__))
vtrn_p8 (poly8x8_t __a, poly8x8_t __b)
{
  poly8x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 9, 1, 11, 3, 13, 5, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 8, 0, 10, 2, 12, 4, 14, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 0, 8, 2, 10, 4, 12, 6, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 1, 9, 3, 11, 5, 13, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline poly16x4x2_t __attribute__ ((__always_inline__))
vtrn_p16 (poly16x4_t __a, poly16x4_t __b)
{
  poly16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 5, 1, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 4, 0, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 0, 4, 2, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 1, 5, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline int32x2x2_t __attribute__ ((__always_inline__))
vtrn_s32 (int32x2_t __a, int32x2_t __b)
{
  int32x2x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 3, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 2, 0 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 0, 2 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 1, 3 });
#endif
  return __rv;
}

__extension__ static __inline float32x2x2_t __attribute__ ((__always_inline__))
vtrn_f32 (float32x2_t __a, float32x2_t __b)
{
  float32x2x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 3, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 2, 0 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 0, 2 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 1, 3 });
#endif
  return __rv;
}

__extension__ static __inline uint32x2x2_t __attribute__ ((__always_inline__))
vtrn_u32 (uint32x2_t __a, uint32x2_t __b)
{
  uint32x2x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 3, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 2, 0 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 0, 2 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 1, 3 });
#endif
  return __rv;
}

__extension__ static __inline int8x16x2_t __attribute__ ((__always_inline__))
vtrnq_s8 (int8x16_t __a, int8x16_t __b)
{
  int8x16x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 17, 1, 19, 3, 21, 5, 23, 7, 25, 9, 27, 11, 29, 13, 31, 15 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 16, 0, 18, 2, 20, 4, 22, 6, 24, 8, 26, 10, 28, 12, 30, 14 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 0, 16, 2, 18, 4, 20, 6, 22, 8, 24, 10, 26, 12, 28, 14, 30 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 1, 17, 3, 19, 5, 21, 7, 23, 9, 25, 11, 27, 13, 29, 15, 31 });
#endif
  return __rv;
}

__extension__ static __inline int16x8x2_t __attribute__ ((__always_inline__))
vtrnq_s16 (int16x8_t __a, int16x8_t __b)
{
  int16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 9, 1, 11, 3, 13, 5, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 8, 0, 10, 2, 12, 4, 14, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 0, 8, 2, 10, 4, 12, 6, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 1, 9, 3, 11, 5, 13, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline int32x4x2_t __attribute__ ((__always_inline__))
vtrnq_s32 (int32x4_t __a, int32x4_t __b)
{
  int32x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 5, 1, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 4, 0, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 0, 4, 2, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 1, 5, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline float32x4x2_t __attribute__ ((__always_inline__))
vtrnq_f32 (float32x4_t __a, float32x4_t __b)
{
  float32x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 5, 1, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 4, 0, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 0, 4, 2, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 1, 5, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline uint8x16x2_t __attribute__ ((__always_inline__))
vtrnq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  uint8x16x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 17, 1, 19, 3, 21, 5, 23, 7, 25, 9, 27, 11, 29, 13, 31, 15 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 16, 0, 18, 2, 20, 4, 22, 6, 24, 8, 26, 10, 28, 12, 30, 14 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 0, 16, 2, 18, 4, 20, 6, 22, 8, 24, 10, 26, 12, 28, 14, 30 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 1, 17, 3, 19, 5, 21, 7, 23, 9, 25, 11, 27, 13, 29, 15, 31 });
#endif
  return __rv;
}

__extension__ static __inline uint16x8x2_t __attribute__ ((__always_inline__))
vtrnq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  uint16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 9, 1, 11, 3, 13, 5, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 8, 0, 10, 2, 12, 4, 14, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 0, 8, 2, 10, 4, 12, 6, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 1, 9, 3, 11, 5, 13, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline uint32x4x2_t __attribute__ ((__always_inline__))
vtrnq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  uint32x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 5, 1, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 4, 0, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 0, 4, 2, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 1, 5, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline poly8x16x2_t __attribute__ ((__always_inline__))
vtrnq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  poly8x16x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 17, 1, 19, 3, 21, 5, 23, 7, 25, 9, 27, 11, 29, 13, 31, 15 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 16, 0, 18, 2, 20, 4, 22, 6, 24, 8, 26, 10, 28, 12, 30, 14 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 0, 16, 2, 18, 4, 20, 6, 22, 8, 24, 10, 26, 12, 28, 14, 30 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 1, 17, 3, 19, 5, 21, 7, 23, 9, 25, 11, 27, 13, 29, 15, 31 });
#endif
  return __rv;
}

__extension__ static __inline poly16x8x2_t __attribute__ ((__always_inline__))
vtrnq_p16 (poly16x8_t __a, poly16x8_t __b)
{
  poly16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 9, 1, 11, 3, 13, 5, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 8, 0, 10, 2, 12, 4, 14, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 0, 8, 2, 10, 4, 12, 6, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 1, 9, 3, 11, 5, 13, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline int8x8x2_t __attribute__ ((__always_inline__))
vzip_s8 (int8x8_t __a, int8x8_t __b)
{
  int8x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 12, 4, 13, 5, 14, 6, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 8, 0, 9, 1, 10, 2, 11, 3 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 0, 8, 1, 9, 2, 10, 3, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 4, 12, 5, 13, 6, 14, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline int16x4x2_t __attribute__ ((__always_inline__))
vzip_s16 (int16x4_t __a, int16x4_t __b)
{
  int16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 6, 2, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 4, 0, 5, 1 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 0, 4, 1, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 2, 6, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline uint8x8x2_t __attribute__ ((__always_inline__))
vzip_u8 (uint8x8_t __a, uint8x8_t __b)
{
  uint8x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 12, 4, 13, 5, 14, 6, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 8, 0, 9, 1, 10, 2, 11, 3 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 0, 8, 1, 9, 2, 10, 3, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 4, 12, 5, 13, 6, 14, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline uint16x4x2_t __attribute__ ((__always_inline__))
vzip_u16 (uint16x4_t __a, uint16x4_t __b)
{
  uint16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 6, 2, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 4, 0, 5, 1 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 0, 4, 1, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 2, 6, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline poly8x8x2_t __attribute__ ((__always_inline__))
vzip_p8 (poly8x8_t __a, poly8x8_t __b)
{
  poly8x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 12, 4, 13, 5, 14, 6, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 8, 0, 9, 1, 10, 2, 11, 3 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 0, 8, 1, 9, 2, 10, 3, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 4, 12, 5, 13, 6, 14, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline poly16x4x2_t __attribute__ ((__always_inline__))
vzip_p16 (poly16x4_t __a, poly16x4_t __b)
{
  poly16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 6, 2, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 4, 0, 5, 1 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 0, 4, 1, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 2, 6, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline int32x2x2_t __attribute__ ((__always_inline__))
vzip_s32 (int32x2_t __a, int32x2_t __b)
{
  int32x2x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 3, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 2, 0 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 0, 2 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 1, 3 });
#endif
  return __rv;
}

__extension__ static __inline float32x2x2_t __attribute__ ((__always_inline__))
vzip_f32 (float32x2_t __a, float32x2_t __b)
{
  float32x2x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 3, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 2, 0 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 0, 2 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 1, 3 });
#endif
  return __rv;
}

__extension__ static __inline uint32x2x2_t __attribute__ ((__always_inline__))
vzip_u32 (uint32x2_t __a, uint32x2_t __b)
{
  uint32x2x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 3, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 2, 0 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 0, 2 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 1, 3 });
#endif
  return __rv;
}

__extension__ static __inline int8x16x2_t __attribute__ ((__always_inline__))
vzipq_s8 (int8x16_t __a, int8x16_t __b)
{
  int8x16x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 20, 4, 21, 5, 22, 6, 23, 7, 16, 0, 17, 1, 18, 2, 19, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 28, 12, 29, 13, 30, 14, 31, 15, 24, 8, 25, 9, 26, 10, 27, 11 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 8, 24, 9, 25, 10, 26, 11, 27, 12, 28, 13, 29, 14, 30, 15, 31 });
#endif
  return __rv;
}

__extension__ static __inline int16x8x2_t __attribute__ ((__always_inline__))
vzipq_s16 (int16x8_t __a, int16x8_t __b)
{
  int16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 10, 2, 11, 3, 8, 0, 9, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 14, 6, 15, 7, 12, 4, 13, 5 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 0, 8, 1, 9, 2, 10, 3, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 4, 12, 5, 13, 6, 14, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline int32x4x2_t __attribute__ ((__always_inline__))
vzipq_s32 (int32x4_t __a, int32x4_t __b)
{
  int32x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 5, 1, 4, 0 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 7, 3, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 0, 4, 1, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 2, 6, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline float32x4x2_t __attribute__ ((__always_inline__))
vzipq_f32 (float32x4_t __a, float32x4_t __b)
{
  float32x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 5, 1, 4, 0 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 7, 3, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 0, 4, 1, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 2, 6, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline uint8x16x2_t __attribute__ ((__always_inline__))
vzipq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  uint8x16x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 20, 4, 21, 5, 22, 6, 23, 7, 16, 0, 17, 1, 18, 2, 19, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 28, 12, 29, 13, 30, 14, 31, 15, 24, 8, 25, 9, 26, 10, 27, 11 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 8, 24, 9, 25, 10, 26, 11, 27, 12, 28, 13, 29, 14, 30, 15, 31 });
#endif
  return __rv;
}

__extension__ static __inline uint16x8x2_t __attribute__ ((__always_inline__))
vzipq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  uint16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 10, 2, 11, 3, 8, 0, 9, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 14, 6, 15, 7, 12, 4, 13, 5 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 0, 8, 1, 9, 2, 10, 3, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 4, 12, 5, 13, 6, 14, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline uint32x4x2_t __attribute__ ((__always_inline__))
vzipq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  uint32x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 5, 1, 4, 0 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 7, 3, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 0, 4, 1, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 2, 6, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline poly8x16x2_t __attribute__ ((__always_inline__))
vzipq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  poly8x16x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 20, 4, 21, 5, 22, 6, 23, 7, 16, 0, 17, 1, 18, 2, 19, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 28, 12, 29, 13, 30, 14, 31, 15, 24, 8, 25, 9, 26, 10, 27, 11 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 8, 24, 9, 25, 10, 26, 11, 27, 12, 28, 13, 29, 14, 30, 15, 31 });
#endif
  return __rv;
}

__extension__ static __inline poly16x8x2_t __attribute__ ((__always_inline__))
vzipq_p16 (poly16x8_t __a, poly16x8_t __b)
{
  poly16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 10, 2, 11, 3, 8, 0, 9, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 14, 6, 15, 7, 12, 4, 13, 5 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 0, 8, 1, 9, 2, 10, 3, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 4, 12, 5, 13, 6, 14, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline int8x8x2_t __attribute__ ((__always_inline__))
vuzp_s8 (int8x8_t __a, int8x8_t __b)
{
  int8x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 9, 11, 13, 15, 1, 3, 5, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 8, 10, 12, 14, 0, 2, 4, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 0, 2, 4, 6, 8, 10, 12, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 1, 3, 5, 7, 9, 11, 13, 15 });
#endif
  return __rv;
}

__extension__ static __inline int16x4x2_t __attribute__ ((__always_inline__))
vuzp_s16 (int16x4_t __a, int16x4_t __b)
{
  int16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 5, 7, 1, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 4, 6, 0, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 0, 2, 4, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 1, 3, 5, 7 });
#endif
  return __rv;
}

__extension__ static __inline int32x2x2_t __attribute__ ((__always_inline__))
vuzp_s32 (int32x2_t __a, int32x2_t __b)
{
  int32x2x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 3, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 2, 0 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 0, 2 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 1, 3 });
#endif
  return __rv;
}

__extension__ static __inline float32x2x2_t __attribute__ ((__always_inline__))
vuzp_f32 (float32x2_t __a, float32x2_t __b)
{
  float32x2x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 3, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 2, 0 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 0, 2 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 1, 3 });
#endif
  return __rv;
}

__extension__ static __inline uint8x8x2_t __attribute__ ((__always_inline__))
vuzp_u8 (uint8x8_t __a, uint8x8_t __b)
{
  uint8x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 9, 11, 13, 15, 1, 3, 5, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 8, 10, 12, 14, 0, 2, 4, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 0, 2, 4, 6, 8, 10, 12, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 1, 3, 5, 7, 9, 11, 13, 15 });
#endif
  return __rv;
}

__extension__ static __inline uint16x4x2_t __attribute__ ((__always_inline__))
vuzp_u16 (uint16x4_t __a, uint16x4_t __b)
{
  uint16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 5, 7, 1, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 4, 6, 0, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 0, 2, 4, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 1, 3, 5, 7 });
#endif
  return __rv;
}

__extension__ static __inline uint32x2x2_t __attribute__ ((__always_inline__))
vuzp_u32 (uint32x2_t __a, uint32x2_t __b)
{
  uint32x2x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 3, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 2, 0 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x2_t) { 0, 2 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x2_t) { 1, 3 });
#endif
  return __rv;
}

__extension__ static __inline poly8x8x2_t __attribute__ ((__always_inline__))
vuzp_p8 (poly8x8_t __a, poly8x8_t __b)
{
  poly8x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 9, 11, 13, 15, 1, 3, 5, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 8, 10, 12, 14, 0, 2, 4, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 0, 2, 4, 6, 8, 10, 12, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x8_t)
      { 1, 3, 5, 7, 9, 11, 13, 15 });
#endif
  return __rv;
}

__extension__ static __inline poly16x4x2_t __attribute__ ((__always_inline__))
vuzp_p16 (poly16x4_t __a, poly16x4_t __b)
{
  poly16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 5, 7, 1, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 4, 6, 0, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t) { 0, 2, 4, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t) { 1, 3, 5, 7 });
#endif
  return __rv;
}

__extension__ static __inline int8x16x2_t __attribute__ ((__always_inline__))
vuzpq_s8 (int8x16_t __a, int8x16_t __b)
{
  int8x16x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 9, 11, 13, 15, 1, 3, 5, 7, 25, 27, 29, 31, 17, 19, 21, 23 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 8, 10, 12, 14, 0, 2, 4, 6, 24, 26, 28, 30, 16, 18, 20, 22 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31 });
#endif
  return __rv;
}

__extension__ static __inline int16x8x2_t __attribute__ ((__always_inline__))
vuzpq_s16 (int16x8_t __a, int16x8_t __b)
{
  int16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 5, 7, 1, 3, 13, 15, 9, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 4, 6, 0, 2, 12, 14, 8, 10 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 0, 2, 4, 6, 8, 10, 12, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 1, 3, 5, 7, 9, 11, 13, 15 });
#endif
  return __rv;
}

__extension__ static __inline int32x4x2_t __attribute__ ((__always_inline__))
vuzpq_s32 (int32x4_t __a, int32x4_t __b)
{
  int32x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 3, 1, 7, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 2, 0, 6, 4 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 0, 2, 4, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 1, 3, 5, 7 });
#endif
  return __rv;
}

__extension__ static __inline float32x4x2_t __attribute__ ((__always_inline__))
vuzpq_f32 (float32x4_t __a, float32x4_t __b)
{
  float32x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 3, 1, 7, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 2, 0, 6, 4 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 0, 2, 4, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 1, 3, 5, 7 });
#endif
  return __rv;
}

__extension__ static __inline uint8x16x2_t __attribute__ ((__always_inline__))
vuzpq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  uint8x16x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 9, 11, 13, 15, 1, 3, 5, 7, 25, 27, 29, 31, 17, 19, 21, 23 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 8, 10, 12, 14, 0, 2, 4, 6, 24, 26, 28, 30, 16, 18, 20, 22 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31 });
#endif
  return __rv;
}

__extension__ static __inline uint16x8x2_t __attribute__ ((__always_inline__))
vuzpq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  uint16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 5, 7, 1, 3, 13, 15, 9, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 4, 6, 0, 2, 12, 14, 8, 10 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 0, 2, 4, 6, 8, 10, 12, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 1, 3, 5, 7, 9, 11, 13, 15 });
#endif
  return __rv;
}

__extension__ static __inline uint32x4x2_t __attribute__ ((__always_inline__))
vuzpq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  uint32x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 3, 1, 7, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 2, 0, 6, 4 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint32x4_t) { 0, 2, 4, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint32x4_t) { 1, 3, 5, 7 });
#endif
  return __rv;
}

__extension__ static __inline poly8x16x2_t __attribute__ ((__always_inline__))
vuzpq_p8 (poly8x16_t __a, poly8x16_t __b)
{
  poly8x16x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 9, 11, 13, 15, 1, 3, 5, 7, 25, 27, 29, 31, 17, 19, 21, 23 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 8, 10, 12, 14, 0, 2, 4, 6, 24, 26, 28, 30, 16, 18, 20, 22 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint8x16_t)
      { 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31 });
#endif
  return __rv;
}

__extension__ static __inline poly16x8x2_t __attribute__ ((__always_inline__))
vuzpq_p16 (poly16x8_t __a, poly16x8_t __b)
{
  poly16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 5, 7, 1, 3, 13, 15, 9, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 4, 6, 0, 2, 12, 14, 8, 10 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 0, 2, 4, 6, 8, 10, 12, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
      { 1, 3, 5, 7, 9, 11, 13, 15 });
#endif
  return __rv;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vld1_p64 (const poly64_t * __a)
{
  return (poly64x1_t)__builtin_neon_vld1di ((const __builtin_neon_di *) __a);
}

#pragma GCC pop_options
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vld1_s8 (const int8_t * __a)
{
  return (int8x8_t)__builtin_neon_vld1v8qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vld1_s16 (const int16_t * __a)
{
  return (int16x4_t)__builtin_neon_vld1v4hi ((const __builtin_neon_hi *) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vld1_s32 (const int32_t * __a)
{
  return (int32x2_t)__builtin_neon_vld1v2si ((const __builtin_neon_si *) __a);
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vld1_s64 (const int64_t * __a)
{
  return (int64x1_t)__builtin_neon_vld1di ((const __builtin_neon_di *) __a);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vld1_f16 (const float16_t * __a)
{
  return __builtin_neon_vld1v4hf (__a);
}
#endif

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vld1_f32 (const float32_t * __a)
{
  return (float32x2_t)__builtin_neon_vld1v2sf ((const __builtin_neon_sf *) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vld1_u8 (const uint8_t * __a)
{
  return (uint8x8_t)__builtin_neon_vld1v8qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vld1_u16 (const uint16_t * __a)
{
  return (uint16x4_t)__builtin_neon_vld1v4hi ((const __builtin_neon_hi *) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vld1_u32 (const uint32_t * __a)
{
  return (uint32x2_t)__builtin_neon_vld1v2si ((const __builtin_neon_si *) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vld1_u64 (const uint64_t * __a)
{
  return (uint64x1_t)__builtin_neon_vld1di ((const __builtin_neon_di *) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vld1_p8 (const poly8_t * __a)
{
  return (poly8x8_t)__builtin_neon_vld1v8qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vld1_p16 (const poly16_t * __a)
{
  return (poly16x4_t)__builtin_neon_vld1v4hi ((const __builtin_neon_hi *) __a);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vld1q_p64 (const poly64_t * __a)
{
  return (poly64x2_t)__builtin_neon_vld1v2di ((const __builtin_neon_di *) __a);
}

#pragma GCC pop_options
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vld1q_s8 (const int8_t * __a)
{
  return (int8x16_t)__builtin_neon_vld1v16qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vld1q_s16 (const int16_t * __a)
{
  return (int16x8_t)__builtin_neon_vld1v8hi ((const __builtin_neon_hi *) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vld1q_s32 (const int32_t * __a)
{
  return (int32x4_t)__builtin_neon_vld1v4si ((const __builtin_neon_si *) __a);
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vld1q_s64 (const int64_t * __a)
{
  return (int64x2_t)__builtin_neon_vld1v2di ((const __builtin_neon_di *) __a);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vld1q_f16 (const float16_t * __a)
{
  return __builtin_neon_vld1v8hf (__a);
}
#endif

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vld1q_f32 (const float32_t * __a)
{
  return (float32x4_t)__builtin_neon_vld1v4sf ((const __builtin_neon_sf *) __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vld1q_u8 (const uint8_t * __a)
{
  return (uint8x16_t)__builtin_neon_vld1v16qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vld1q_u16 (const uint16_t * __a)
{
  return (uint16x8_t)__builtin_neon_vld1v8hi ((const __builtin_neon_hi *) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vld1q_u32 (const uint32_t * __a)
{
  return (uint32x4_t)__builtin_neon_vld1v4si ((const __builtin_neon_si *) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vld1q_u64 (const uint64_t * __a)
{
  return (uint64x2_t)__builtin_neon_vld1v2di ((const __builtin_neon_di *) __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vld1q_p8 (const poly8_t * __a)
{
  return (poly8x16_t)__builtin_neon_vld1v16qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vld1q_p16 (const poly16_t * __a)
{
  return (poly16x8_t)__builtin_neon_vld1v8hi ((const __builtin_neon_hi *) __a);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vld1_lane_s8 (const int8_t * __a, int8x8_t __b, const int __c)
{
  return (int8x8_t)__builtin_neon_vld1_lanev8qi ((const __builtin_neon_qi *) __a, __b, __c);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vld1_lane_s16 (const int16_t * __a, int16x4_t __b, const int __c)
{
  return (int16x4_t)__builtin_neon_vld1_lanev4hi ((const __builtin_neon_hi *) __a, __b, __c);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vld1_lane_s32 (const int32_t * __a, int32x2_t __b, const int __c)
{
  return (int32x2_t)__builtin_neon_vld1_lanev2si ((const __builtin_neon_si *) __a, __b, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vld1_lane_f16 (const float16_t * __a, float16x4_t __b, const int __c)
{
  return vset_lane_f16 (*__a, __b, __c);
}
#endif

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vld1_lane_f32 (const float32_t * __a, float32x2_t __b, const int __c)
{
  return (float32x2_t)__builtin_neon_vld1_lanev2sf ((const __builtin_neon_sf *) __a, __b, __c);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vld1_lane_u8 (const uint8_t * __a, uint8x8_t __b, const int __c)
{
  return (uint8x8_t)__builtin_neon_vld1_lanev8qi ((const __builtin_neon_qi *) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vld1_lane_u16 (const uint16_t * __a, uint16x4_t __b, const int __c)
{
  return (uint16x4_t)__builtin_neon_vld1_lanev4hi ((const __builtin_neon_hi *) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vld1_lane_u32 (const uint32_t * __a, uint32x2_t __b, const int __c)
{
  return (uint32x2_t)__builtin_neon_vld1_lanev2si ((const __builtin_neon_si *) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vld1_lane_p8 (const poly8_t * __a, poly8x8_t __b, const int __c)
{
  return (poly8x8_t)__builtin_neon_vld1_lanev8qi ((const __builtin_neon_qi *) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vld1_lane_p16 (const poly16_t * __a, poly16x4_t __b, const int __c)
{
  return (poly16x4_t)__builtin_neon_vld1_lanev4hi ((const __builtin_neon_hi *) __a, (int16x4_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vld1_lane_p64 (const poly64_t * __a, poly64x1_t __b, const int __c)
{
  return (poly64x1_t)__builtin_neon_vld1_lanedi ((const __builtin_neon_di *) __a, __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vld1_lane_s64 (const int64_t * __a, int64x1_t __b, const int __c)
{
  return (int64x1_t)__builtin_neon_vld1_lanedi ((const __builtin_neon_di *) __a, __b, __c);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vld1_lane_u64 (const uint64_t * __a, uint64x1_t __b, const int __c)
{
  return (uint64x1_t)__builtin_neon_vld1_lanedi ((const __builtin_neon_di *) __a, (int64x1_t) __b, __c);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vld1q_lane_s8 (const int8_t * __a, int8x16_t __b, const int __c)
{
  return (int8x16_t)__builtin_neon_vld1_lanev16qi ((const __builtin_neon_qi *) __a, __b, __c);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vld1q_lane_s16 (const int16_t * __a, int16x8_t __b, const int __c)
{
  return (int16x8_t)__builtin_neon_vld1_lanev8hi ((const __builtin_neon_hi *) __a, __b, __c);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vld1q_lane_s32 (const int32_t * __a, int32x4_t __b, const int __c)
{
  return (int32x4_t)__builtin_neon_vld1_lanev4si ((const __builtin_neon_si *) __a, __b, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vld1q_lane_f16 (const float16_t * __a, float16x8_t __b, const int __c)
{
  return vsetq_lane_f16 (*__a, __b, __c);
}
#endif

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vld1q_lane_f32 (const float32_t * __a, float32x4_t __b, const int __c)
{
  return (float32x4_t)__builtin_neon_vld1_lanev4sf ((const __builtin_neon_sf *) __a, __b, __c);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vld1q_lane_u8 (const uint8_t * __a, uint8x16_t __b, const int __c)
{
  return (uint8x16_t)__builtin_neon_vld1_lanev16qi ((const __builtin_neon_qi *) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vld1q_lane_u16 (const uint16_t * __a, uint16x8_t __b, const int __c)
{
  return (uint16x8_t)__builtin_neon_vld1_lanev8hi ((const __builtin_neon_hi *) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vld1q_lane_u32 (const uint32_t * __a, uint32x4_t __b, const int __c)
{
  return (uint32x4_t)__builtin_neon_vld1_lanev4si ((const __builtin_neon_si *) __a, (int32x4_t) __b, __c);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vld1q_lane_p8 (const poly8_t * __a, poly8x16_t __b, const int __c)
{
  return (poly8x16_t)__builtin_neon_vld1_lanev16qi ((const __builtin_neon_qi *) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vld1q_lane_p16 (const poly16_t * __a, poly16x8_t __b, const int __c)
{
  return (poly16x8_t)__builtin_neon_vld1_lanev8hi ((const __builtin_neon_hi *) __a, (int16x8_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vld1q_lane_p64 (const poly64_t * __a, poly64x2_t __b, const int __c)
{
  return (poly64x2_t)__builtin_neon_vld1_lanev2di ((const __builtin_neon_di *) __a, (int64x2_t) __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vld1q_lane_s64 (const int64_t * __a, int64x2_t __b, const int __c)
{
  return (int64x2_t)__builtin_neon_vld1_lanev2di ((const __builtin_neon_di *) __a, __b, __c);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vld1q_lane_u64 (const uint64_t * __a, uint64x2_t __b, const int __c)
{
  return (uint64x2_t)__builtin_neon_vld1_lanev2di ((const __builtin_neon_di *) __a, (int64x2_t) __b, __c);
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vld1_dup_s8 (const int8_t * __a)
{
  return (int8x8_t)__builtin_neon_vld1_dupv8qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vld1_dup_s16 (const int16_t * __a)
{
  return (int16x4_t)__builtin_neon_vld1_dupv4hi ((const __builtin_neon_hi *) __a);
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vld1_dup_s32 (const int32_t * __a)
{
  return (int32x2_t)__builtin_neon_vld1_dupv2si ((const __builtin_neon_si *) __a);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vld1_dup_f16 (const float16_t * __a)
{
  float16_t __f = *__a;
  return (float16x4_t) { __f, __f, __f, __f };
}
#endif

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vld1_dup_f32 (const float32_t * __a)
{
  return (float32x2_t)__builtin_neon_vld1_dupv2sf ((const __builtin_neon_sf *) __a);
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vld1_dup_u8 (const uint8_t * __a)
{
  return (uint8x8_t)__builtin_neon_vld1_dupv8qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vld1_dup_u16 (const uint16_t * __a)
{
  return (uint16x4_t)__builtin_neon_vld1_dupv4hi ((const __builtin_neon_hi *) __a);
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vld1_dup_u32 (const uint32_t * __a)
{
  return (uint32x2_t)__builtin_neon_vld1_dupv2si ((const __builtin_neon_si *) __a);
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vld1_dup_p8 (const poly8_t * __a)
{
  return (poly8x8_t)__builtin_neon_vld1_dupv8qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vld1_dup_p16 (const poly16_t * __a)
{
  return (poly16x4_t)__builtin_neon_vld1_dupv4hi ((const __builtin_neon_hi *) __a);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vld1_dup_p64 (const poly64_t * __a)
{
  return (poly64x1_t)__builtin_neon_vld1_dupdi ((const __builtin_neon_di *) __a);
}

#pragma GCC pop_options
__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vld1_dup_s64 (const int64_t * __a)
{
  return (int64x1_t)__builtin_neon_vld1_dupdi ((const __builtin_neon_di *) __a);
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vld1_dup_u64 (const uint64_t * __a)
{
  return (uint64x1_t)__builtin_neon_vld1_dupdi ((const __builtin_neon_di *) __a);
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vld1q_dup_s8 (const int8_t * __a)
{
  return (int8x16_t)__builtin_neon_vld1_dupv16qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vld1q_dup_s16 (const int16_t * __a)
{
  return (int16x8_t)__builtin_neon_vld1_dupv8hi ((const __builtin_neon_hi *) __a);
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vld1q_dup_s32 (const int32_t * __a)
{
  return (int32x4_t)__builtin_neon_vld1_dupv4si ((const __builtin_neon_si *) __a);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vld1q_dup_f16 (const float16_t * __a)
{
  float16_t __f = *__a;
  return (float16x8_t) { __f, __f, __f, __f, __f, __f, __f, __f };
}
#endif

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vld1q_dup_f32 (const float32_t * __a)
{
  return (float32x4_t)__builtin_neon_vld1_dupv4sf ((const __builtin_neon_sf *) __a);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vld1q_dup_u8 (const uint8_t * __a)
{
  return (uint8x16_t)__builtin_neon_vld1_dupv16qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vld1q_dup_u16 (const uint16_t * __a)
{
  return (uint16x8_t)__builtin_neon_vld1_dupv8hi ((const __builtin_neon_hi *) __a);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vld1q_dup_u32 (const uint32_t * __a)
{
  return (uint32x4_t)__builtin_neon_vld1_dupv4si ((const __builtin_neon_si *) __a);
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vld1q_dup_p8 (const poly8_t * __a)
{
  return (poly8x16_t)__builtin_neon_vld1_dupv16qi ((const __builtin_neon_qi *) __a);
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vld1q_dup_p16 (const poly16_t * __a)
{
  return (poly16x8_t)__builtin_neon_vld1_dupv8hi ((const __builtin_neon_hi *) __a);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vld1q_dup_p64 (const poly64_t * __a)
{
  return (poly64x2_t)__builtin_neon_vld1_dupv2di ((const __builtin_neon_di *) __a);
}

#pragma GCC pop_options
__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vld1q_dup_s64 (const int64_t * __a)
{
  return (int64x2_t)__builtin_neon_vld1_dupv2di ((const __builtin_neon_di *) __a);
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vld1q_dup_u64 (const uint64_t * __a)
{
  return (uint64x2_t)__builtin_neon_vld1_dupv2di ((const __builtin_neon_di *) __a);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_p64 (poly64_t * __a, poly64x1_t __b)
{
  __builtin_neon_vst1di ((__builtin_neon_di *) __a, __b);
}

#pragma GCC pop_options
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_s8 (int8_t * __a, int8x8_t __b)
{
  __builtin_neon_vst1v8qi ((__builtin_neon_qi *) __a, __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_s16 (int16_t * __a, int16x4_t __b)
{
  __builtin_neon_vst1v4hi ((__builtin_neon_hi *) __a, __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_s32 (int32_t * __a, int32x2_t __b)
{
  __builtin_neon_vst1v2si ((__builtin_neon_si *) __a, __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_s64 (int64_t * __a, int64x1_t __b)
{
  __builtin_neon_vst1di ((__builtin_neon_di *) __a, __b);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_f16 (float16_t * __a, float16x4_t __b)
{
  __builtin_neon_vst1v4hf (__a, __b);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_f32 (float32_t * __a, float32x2_t __b)
{
  __builtin_neon_vst1v2sf ((__builtin_neon_sf *) __a, __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_u8 (uint8_t * __a, uint8x8_t __b)
{
  __builtin_neon_vst1v8qi ((__builtin_neon_qi *) __a, (int8x8_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_u16 (uint16_t * __a, uint16x4_t __b)
{
  __builtin_neon_vst1v4hi ((__builtin_neon_hi *) __a, (int16x4_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_u32 (uint32_t * __a, uint32x2_t __b)
{
  __builtin_neon_vst1v2si ((__builtin_neon_si *) __a, (int32x2_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_u64 (uint64_t * __a, uint64x1_t __b)
{
  __builtin_neon_vst1di ((__builtin_neon_di *) __a, (int64x1_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_p8 (poly8_t * __a, poly8x8_t __b)
{
  __builtin_neon_vst1v8qi ((__builtin_neon_qi *) __a, (int8x8_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_p16 (poly16_t * __a, poly16x4_t __b)
{
  __builtin_neon_vst1v4hi ((__builtin_neon_hi *) __a, (int16x4_t) __b);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_p64 (poly64_t * __a, poly64x2_t __b)
{
  __builtin_neon_vst1v2di ((__builtin_neon_di *) __a, (int64x2_t) __b);
}

#pragma GCC pop_options
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_s8 (int8_t * __a, int8x16_t __b)
{
  __builtin_neon_vst1v16qi ((__builtin_neon_qi *) __a, __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_s16 (int16_t * __a, int16x8_t __b)
{
  __builtin_neon_vst1v8hi ((__builtin_neon_hi *) __a, __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_s32 (int32_t * __a, int32x4_t __b)
{
  __builtin_neon_vst1v4si ((__builtin_neon_si *) __a, __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_s64 (int64_t * __a, int64x2_t __b)
{
  __builtin_neon_vst1v2di ((__builtin_neon_di *) __a, __b);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_f16 (float16_t * __a, float16x8_t __b)
{
  __builtin_neon_vst1v8hf (__a, __b);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_f32 (float32_t * __a, float32x4_t __b)
{
  __builtin_neon_vst1v4sf ((__builtin_neon_sf *) __a, __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_u8 (uint8_t * __a, uint8x16_t __b)
{
  __builtin_neon_vst1v16qi ((__builtin_neon_qi *) __a, (int8x16_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_u16 (uint16_t * __a, uint16x8_t __b)
{
  __builtin_neon_vst1v8hi ((__builtin_neon_hi *) __a, (int16x8_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_u32 (uint32_t * __a, uint32x4_t __b)
{
  __builtin_neon_vst1v4si ((__builtin_neon_si *) __a, (int32x4_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_u64 (uint64_t * __a, uint64x2_t __b)
{
  __builtin_neon_vst1v2di ((__builtin_neon_di *) __a, (int64x2_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_p8 (poly8_t * __a, poly8x16_t __b)
{
  __builtin_neon_vst1v16qi ((__builtin_neon_qi *) __a, (int8x16_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_p16 (poly16_t * __a, poly16x8_t __b)
{
  __builtin_neon_vst1v8hi ((__builtin_neon_hi *) __a, (int16x8_t) __b);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_s8 (int8_t * __a, int8x8_t __b, const int __c)
{
  __builtin_neon_vst1_lanev8qi ((__builtin_neon_qi *) __a, __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_s16 (int16_t * __a, int16x4_t __b, const int __c)
{
  __builtin_neon_vst1_lanev4hi ((__builtin_neon_hi *) __a, __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_s32 (int32_t * __a, int32x2_t __b, const int __c)
{
  __builtin_neon_vst1_lanev2si ((__builtin_neon_si *) __a, __b, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_f16 (float16_t * __a, float16x4_t __b, const int __c)
{
  __builtin_neon_vst1_lanev4hf (__a, __b, __c);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_f32 (float32_t * __a, float32x2_t __b, const int __c)
{
  __builtin_neon_vst1_lanev2sf ((__builtin_neon_sf *) __a, __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_u8 (uint8_t * __a, uint8x8_t __b, const int __c)
{
  __builtin_neon_vst1_lanev8qi ((__builtin_neon_qi *) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_u16 (uint16_t * __a, uint16x4_t __b, const int __c)
{
  __builtin_neon_vst1_lanev4hi ((__builtin_neon_hi *) __a, (int16x4_t) __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_u32 (uint32_t * __a, uint32x2_t __b, const int __c)
{
  __builtin_neon_vst1_lanev2si ((__builtin_neon_si *) __a, (int32x2_t) __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_p8 (poly8_t * __a, poly8x8_t __b, const int __c)
{
  __builtin_neon_vst1_lanev8qi ((__builtin_neon_qi *) __a, (int8x8_t) __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_p16 (poly16_t * __a, poly16x4_t __b, const int __c)
{
  __builtin_neon_vst1_lanev4hi ((__builtin_neon_hi *) __a, (int16x4_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_p64 (poly64_t * __a, poly64x1_t __b, const int __c)
{
  __builtin_neon_vst1_lanedi ((__builtin_neon_di *) __a, __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_s64 (int64_t * __a, int64x1_t __b, const int __c)
{
  __builtin_neon_vst1_lanedi ((__builtin_neon_di *) __a, __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1_lane_u64 (uint64_t * __a, uint64x1_t __b, const int __c)
{
  __builtin_neon_vst1_lanedi ((__builtin_neon_di *) __a, (int64x1_t) __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_s8 (int8_t * __a, int8x16_t __b, const int __c)
{
  __builtin_neon_vst1_lanev16qi ((__builtin_neon_qi *) __a, __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_s16 (int16_t * __a, int16x8_t __b, const int __c)
{
  __builtin_neon_vst1_lanev8hi ((__builtin_neon_hi *) __a, __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_s32 (int32_t * __a, int32x4_t __b, const int __c)
{
  __builtin_neon_vst1_lanev4si ((__builtin_neon_si *) __a, __b, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_f16 (float16_t * __a, float16x8_t __b, const int __c)
{
  __builtin_neon_vst1_lanev8hf (__a, __b, __c);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_f32 (float32_t * __a, float32x4_t __b, const int __c)
{
  __builtin_neon_vst1_lanev4sf ((__builtin_neon_sf *) __a, __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_u8 (uint8_t * __a, uint8x16_t __b, const int __c)
{
  __builtin_neon_vst1_lanev16qi ((__builtin_neon_qi *) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_u16 (uint16_t * __a, uint16x8_t __b, const int __c)
{
  __builtin_neon_vst1_lanev8hi ((__builtin_neon_hi *) __a, (int16x8_t) __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_u32 (uint32_t * __a, uint32x4_t __b, const int __c)
{
  __builtin_neon_vst1_lanev4si ((__builtin_neon_si *) __a, (int32x4_t) __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_p8 (poly8_t * __a, poly8x16_t __b, const int __c)
{
  __builtin_neon_vst1_lanev16qi ((__builtin_neon_qi *) __a, (int8x16_t) __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_p16 (poly16_t * __a, poly16x8_t __b, const int __c)
{
  __builtin_neon_vst1_lanev8hi ((__builtin_neon_hi *) __a, (int16x8_t) __b, __c);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_p64 (poly64_t * __a, poly64x2_t __b, const int __c)
{
  __builtin_neon_vst1_lanev2di ((__builtin_neon_di *) __a, (int64x2_t) __b, __c);
}

#pragma GCC pop_options
__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_s64 (int64_t * __a, int64x2_t __b, const int __c)
{
  __builtin_neon_vst1_lanev2di ((__builtin_neon_di *) __a, __b, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst1q_lane_u64 (uint64_t * __a, uint64x2_t __b, const int __c)
{
  __builtin_neon_vst1_lanev2di ((__builtin_neon_di *) __a, (int64x2_t) __b, __c);
}

__extension__ static __inline int8x8x2_t __attribute__ ((__always_inline__))
vld2_s8 (const int8_t * __a)
{
  union { int8x8x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline int16x4x2_t __attribute__ ((__always_inline__))
vld2_s16 (const int16_t * __a)
{
  union { int16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int32x2x2_t __attribute__ ((__always_inline__))
vld2_s32 (const int32_t * __a)
{
  union { int32x2x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4x2_t __attribute__ ((__always_inline__))
vld2_f16 (const float16_t * __a)
{
  union { float16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v4hf (__a);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x2x2_t __attribute__ ((__always_inline__))
vld2_f32 (const float32_t * __a)
{
  union { float32x2x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v2sf ((const __builtin_neon_sf *) __a);
  return __rv.__i;
}

__extension__ static __inline uint8x8x2_t __attribute__ ((__always_inline__))
vld2_u8 (const uint8_t * __a)
{
  union { uint8x8x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint16x4x2_t __attribute__ ((__always_inline__))
vld2_u16 (const uint16_t * __a)
{
  union { uint16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint32x2x2_t __attribute__ ((__always_inline__))
vld2_u32 (const uint32_t * __a)
{
  union { uint32x2x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

__extension__ static __inline poly8x8x2_t __attribute__ ((__always_inline__))
vld2_p8 (const poly8_t * __a)
{
  union { poly8x8x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline poly16x4x2_t __attribute__ ((__always_inline__))
vld2_p16 (const poly16_t * __a)
{
  union { poly16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2v4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1x2_t __attribute__ ((__always_inline__))
vld2_p64 (const poly64_t * __a)
{
  union { poly64x1x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2di ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

#pragma GCC pop_options
__extension__ static __inline int64x1x2_t __attribute__ ((__always_inline__))
vld2_s64 (const int64_t * __a)
{
  union { int64x1x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2di ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline uint64x1x2_t __attribute__ ((__always_inline__))
vld2_u64 (const uint64_t * __a)
{
  union { uint64x1x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2di ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline int8x16x2_t __attribute__ ((__always_inline__))
vld2q_s8 (const int8_t * __a)
{
  union { int8x16x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v16qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline int16x8x2_t __attribute__ ((__always_inline__))
vld2q_s16 (const int16_t * __a)
{
  union { int16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v8hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int32x4x2_t __attribute__ ((__always_inline__))
vld2q_s32 (const int32_t * __a)
{
  union { int32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v4si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8x2_t __attribute__ ((__always_inline__))
vld2q_f16 (const float16_t * __a)
{
  union { float16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v8hf (__a);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x4x2_t __attribute__ ((__always_inline__))
vld2q_f32 (const float32_t * __a)
{
  union { float32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v4sf ((const __builtin_neon_sf *) __a);
  return __rv.__i;
}

__extension__ static __inline uint8x16x2_t __attribute__ ((__always_inline__))
vld2q_u8 (const uint8_t * __a)
{
  union { uint8x16x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v16qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint16x8x2_t __attribute__ ((__always_inline__))
vld2q_u16 (const uint16_t * __a)
{
  union { uint16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v8hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint32x4x2_t __attribute__ ((__always_inline__))
vld2q_u32 (const uint32_t * __a)
{
  union { uint32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v4si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

__extension__ static __inline poly8x16x2_t __attribute__ ((__always_inline__))
vld2q_p8 (const poly8_t * __a)
{
  union { poly8x16x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v16qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline poly16x8x2_t __attribute__ ((__always_inline__))
vld2q_p16 (const poly16_t * __a)
{
  union { poly16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2v8hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int8x8x2_t __attribute__ ((__always_inline__))
vld2_lane_s8 (const int8_t * __a, int8x8x2_t __b, const int __c)
{
  union { int8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { int8x8x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev8qi ((const __builtin_neon_qi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int16x4x2_t __attribute__ ((__always_inline__))
vld2_lane_s16 (const int16_t * __a, int16x4x2_t __b, const int __c)
{
  union { int16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { int16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev4hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int32x2x2_t __attribute__ ((__always_inline__))
vld2_lane_s32 (const int32_t * __a, int32x2x2_t __b, const int __c)
{
  union { int32x2x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { int32x2x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev2si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4x2_t __attribute__ ((__always_inline__))
vld2_lane_f16 (const float16_t * __a, float16x4x2_t __b, const int __c)
{
  union { float16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { float16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev4hf ( __a, __bu.__o, __c);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x2x2_t __attribute__ ((__always_inline__))
vld2_lane_f32 (const float32_t * __a, float32x2x2_t __b, const int __c)
{
  union { float32x2x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { float32x2x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev2sf ((const __builtin_neon_sf *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint8x8x2_t __attribute__ ((__always_inline__))
vld2_lane_u8 (const uint8_t * __a, uint8x8x2_t __b, const int __c)
{
  union { uint8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { uint8x8x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev8qi ((const __builtin_neon_qi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint16x4x2_t __attribute__ ((__always_inline__))
vld2_lane_u16 (const uint16_t * __a, uint16x4x2_t __b, const int __c)
{
  union { uint16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { uint16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev4hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint32x2x2_t __attribute__ ((__always_inline__))
vld2_lane_u32 (const uint32_t * __a, uint32x2x2_t __b, const int __c)
{
  union { uint32x2x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { uint32x2x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev2si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline poly8x8x2_t __attribute__ ((__always_inline__))
vld2_lane_p8 (const poly8_t * __a, poly8x8x2_t __b, const int __c)
{
  union { poly8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { poly8x8x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev8qi ((const __builtin_neon_qi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline poly16x4x2_t __attribute__ ((__always_inline__))
vld2_lane_p16 (const poly16_t * __a, poly16x4x2_t __b, const int __c)
{
  union { poly16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  union { poly16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev4hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int16x8x2_t __attribute__ ((__always_inline__))
vld2q_lane_s16 (const int16_t * __a, int16x8x2_t __b, const int __c)
{
  union { int16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { int16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev8hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int32x4x2_t __attribute__ ((__always_inline__))
vld2q_lane_s32 (const int32_t * __a, int32x4x2_t __b, const int __c)
{
  union { int32x4x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { int32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev4si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8x2_t __attribute__ ((__always_inline__))
vld2q_lane_f16 (const float16_t * __a, float16x8x2_t __b, const int __c)
{
  union { float16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { float16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev8hf (__a, __bu.__o, __c);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x4x2_t __attribute__ ((__always_inline__))
vld2q_lane_f32 (const float32_t * __a, float32x4x2_t __b, const int __c)
{
  union { float32x4x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { float32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev4sf ((const __builtin_neon_sf *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint16x8x2_t __attribute__ ((__always_inline__))
vld2q_lane_u16 (const uint16_t * __a, uint16x8x2_t __b, const int __c)
{
  union { uint16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { uint16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev8hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint32x4x2_t __attribute__ ((__always_inline__))
vld2q_lane_u32 (const uint32_t * __a, uint32x4x2_t __b, const int __c)
{
  union { uint32x4x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { uint32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev4si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline poly16x8x2_t __attribute__ ((__always_inline__))
vld2q_lane_p16 (const poly16_t * __a, poly16x8x2_t __b, const int __c)
{
  union { poly16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { poly16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld2_lanev8hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int8x8x2_t __attribute__ ((__always_inline__))
vld2_dup_s8 (const int8_t * __a)
{
  union { int8x8x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline int16x4x2_t __attribute__ ((__always_inline__))
vld2_dup_s16 (const int16_t * __a)
{
  union { int16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int32x2x2_t __attribute__ ((__always_inline__))
vld2_dup_s32 (const int32_t * __a)
{
  union { int32x2x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4x2_t __attribute__ ((__always_inline__))
vld2_dup_f16 (const float16_t * __a)
{
  union { float16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv4hf (__a);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x2x2_t __attribute__ ((__always_inline__))
vld2_dup_f32 (const float32_t * __a)
{
  union { float32x2x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv2sf ((const __builtin_neon_sf *) __a);
  return __rv.__i;
}

__extension__ static __inline uint8x8x2_t __attribute__ ((__always_inline__))
vld2_dup_u8 (const uint8_t * __a)
{
  union { uint8x8x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint16x4x2_t __attribute__ ((__always_inline__))
vld2_dup_u16 (const uint16_t * __a)
{
  union { uint16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint32x2x2_t __attribute__ ((__always_inline__))
vld2_dup_u32 (const uint32_t * __a)
{
  union { uint32x2x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

__extension__ static __inline poly8x8x2_t __attribute__ ((__always_inline__))
vld2_dup_p8 (const poly8_t * __a)
{
  union { poly8x8x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline poly16x4x2_t __attribute__ ((__always_inline__))
vld2_dup_p16 (const poly16_t * __a)
{
  union { poly16x4x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupv4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1x2_t __attribute__ ((__always_inline__))
vld2_dup_p64 (const poly64_t * __a)
{
  union { poly64x1x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupdi ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

#pragma GCC pop_options
__extension__ static __inline int64x1x2_t __attribute__ ((__always_inline__))
vld2_dup_s64 (const int64_t * __a)
{
  union { int64x1x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupdi ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline uint64x1x2_t __attribute__ ((__always_inline__))
vld2_dup_u64 (const uint64_t * __a)
{
  union { uint64x1x2_t __i; __builtin_neon_ti __o; } __rv;
  __rv.__o = __builtin_neon_vld2_dupdi ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_s8 (int8_t * __a, int8x8x2_t __b)
{
  union { int8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v8qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_s16 (int16_t * __a, int16x4x2_t __b)
{
  union { int16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v4hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_s32 (int32_t * __a, int32x2x2_t __b)
{
  union { int32x2x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v2si ((__builtin_neon_si *) __a, __bu.__o);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_f16 (float16_t * __a, float16x4x2_t __b)
{
  union { float16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v4hf (__a, __bu.__o);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_f32 (float32_t * __a, float32x2x2_t __b)
{
  union { float32x2x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v2sf ((__builtin_neon_sf *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_u8 (uint8_t * __a, uint8x8x2_t __b)
{
  union { uint8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v8qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_u16 (uint16_t * __a, uint16x4x2_t __b)
{
  union { uint16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v4hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_u32 (uint32_t * __a, uint32x2x2_t __b)
{
  union { uint32x2x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v2si ((__builtin_neon_si *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_p8 (poly8_t * __a, poly8x8x2_t __b)
{
  union { poly8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v8qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_p16 (poly16_t * __a, poly16x4x2_t __b)
{
  union { poly16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2v4hi ((__builtin_neon_hi *) __a, __bu.__o);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_p64 (poly64_t * __a, poly64x1x2_t __b)
{
  union { poly64x1x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2di ((__builtin_neon_di *) __a, __bu.__o);
}

#pragma GCC pop_options
__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_s64 (int64_t * __a, int64x1x2_t __b)
{
  union { int64x1x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2di ((__builtin_neon_di *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_u64 (uint64_t * __a, uint64x1x2_t __b)
{
  union { uint64x1x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2di ((__builtin_neon_di *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_s8 (int8_t * __a, int8x16x2_t __b)
{
  union { int8x16x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v16qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_s16 (int16_t * __a, int16x8x2_t __b)
{
  union { int16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v8hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_s32 (int32_t * __a, int32x4x2_t __b)
{
  union { int32x4x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v4si ((__builtin_neon_si *) __a, __bu.__o);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_f16 (float16_t * __a, float16x8x2_t __b)
{
  union { float16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v8hf (__a, __bu.__o);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_f32 (float32_t * __a, float32x4x2_t __b)
{
  union { float32x4x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v4sf ((__builtin_neon_sf *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_u8 (uint8_t * __a, uint8x16x2_t __b)
{
  union { uint8x16x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v16qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_u16 (uint16_t * __a, uint16x8x2_t __b)
{
  union { uint16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v8hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_u32 (uint32_t * __a, uint32x4x2_t __b)
{
  union { uint32x4x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v4si ((__builtin_neon_si *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_p8 (poly8_t * __a, poly8x16x2_t __b)
{
  union { poly8x16x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v16qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_p16 (poly16_t * __a, poly16x8x2_t __b)
{
  union { poly16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2v8hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_s8 (int8_t * __a, int8x8x2_t __b, const int __c)
{
  union { int8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev8qi ((__builtin_neon_qi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_s16 (int16_t * __a, int16x4x2_t __b, const int __c)
{
  union { int16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev4hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_s32 (int32_t * __a, int32x2x2_t __b, const int __c)
{
  union { int32x2x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev2si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_f16 (float16_t * __a, float16x4x2_t __b, const int __c)
{
  union { float16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev4hf (__a, __bu.__o, __c);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_f32 (float32_t * __a, float32x2x2_t __b, const int __c)
{
  union { float32x2x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev2sf ((__builtin_neon_sf *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_u8 (uint8_t * __a, uint8x8x2_t __b, const int __c)
{
  union { uint8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev8qi ((__builtin_neon_qi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_u16 (uint16_t * __a, uint16x4x2_t __b, const int __c)
{
  union { uint16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev4hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_u32 (uint32_t * __a, uint32x2x2_t __b, const int __c)
{
  union { uint32x2x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev2si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_p8 (poly8_t * __a, poly8x8x2_t __b, const int __c)
{
  union { poly8x8x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev8qi ((__builtin_neon_qi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2_lane_p16 (poly16_t * __a, poly16x4x2_t __b, const int __c)
{
  union { poly16x4x2_t __i; __builtin_neon_ti __o; } __bu = { __b };
  __builtin_neon_vst2_lanev4hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_lane_s16 (int16_t * __a, int16x8x2_t __b, const int __c)
{
  union { int16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2_lanev8hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_lane_s32 (int32_t * __a, int32x4x2_t __b, const int __c)
{
  union { int32x4x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2_lanev4si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_lane_f16 (float16_t * __a, float16x8x2_t __b, const int __c)
{
  union { float16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2_lanev8hf (__a, __bu.__o, __c);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_lane_f32 (float32_t * __a, float32x4x2_t __b, const int __c)
{
  union { float32x4x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2_lanev4sf ((__builtin_neon_sf *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_lane_u16 (uint16_t * __a, uint16x8x2_t __b, const int __c)
{
  union { uint16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2_lanev8hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_lane_u32 (uint32_t * __a, uint32x4x2_t __b, const int __c)
{
  union { uint32x4x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2_lanev4si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst2q_lane_p16 (poly16_t * __a, poly16x8x2_t __b, const int __c)
{
  union { poly16x8x2_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst2_lanev8hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline int8x8x3_t __attribute__ ((__always_inline__))
vld3_s8 (const int8_t * __a)
{
  union { int8x8x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline int16x4x3_t __attribute__ ((__always_inline__))
vld3_s16 (const int16_t * __a)
{
  union { int16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int32x2x3_t __attribute__ ((__always_inline__))
vld3_s32 (const int32_t * __a)
{
  union { int32x2x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4x3_t __attribute__ ((__always_inline__))
vld3_f16 (const float16_t * __a)
{
  union { float16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v4hf (__a);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x2x3_t __attribute__ ((__always_inline__))
vld3_f32 (const float32_t * __a)
{
  union { float32x2x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v2sf ((const __builtin_neon_sf *) __a);
  return __rv.__i;
}

__extension__ static __inline uint8x8x3_t __attribute__ ((__always_inline__))
vld3_u8 (const uint8_t * __a)
{
  union { uint8x8x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint16x4x3_t __attribute__ ((__always_inline__))
vld3_u16 (const uint16_t * __a)
{
  union { uint16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint32x2x3_t __attribute__ ((__always_inline__))
vld3_u32 (const uint32_t * __a)
{
  union { uint32x2x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

__extension__ static __inline poly8x8x3_t __attribute__ ((__always_inline__))
vld3_p8 (const poly8_t * __a)
{
  union { poly8x8x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline poly16x4x3_t __attribute__ ((__always_inline__))
vld3_p16 (const poly16_t * __a)
{
  union { poly16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3v4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1x3_t __attribute__ ((__always_inline__))
vld3_p64 (const poly64_t * __a)
{
  union { poly64x1x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3di ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

#pragma GCC pop_options
__extension__ static __inline int64x1x3_t __attribute__ ((__always_inline__))
vld3_s64 (const int64_t * __a)
{
  union { int64x1x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3di ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline uint64x1x3_t __attribute__ ((__always_inline__))
vld3_u64 (const uint64_t * __a)
{
  union { uint64x1x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3di ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline int8x16x3_t __attribute__ ((__always_inline__))
vld3q_s8 (const int8_t * __a)
{
  union { int8x16x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v16qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline int16x8x3_t __attribute__ ((__always_inline__))
vld3q_s16 (const int16_t * __a)
{
  union { int16x8x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v8hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int32x4x3_t __attribute__ ((__always_inline__))
vld3q_s32 (const int32_t * __a)
{
  union { int32x4x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v4si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8x3_t __attribute__ ((__always_inline__))
vld3q_f16 (const float16_t * __a)
{
  union { float16x8x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v8hf (__a);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x4x3_t __attribute__ ((__always_inline__))
vld3q_f32 (const float32_t * __a)
{
  union { float32x4x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v4sf ((const __builtin_neon_sf *) __a);
  return __rv.__i;
}

__extension__ static __inline uint8x16x3_t __attribute__ ((__always_inline__))
vld3q_u8 (const uint8_t * __a)
{
  union { uint8x16x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v16qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint16x8x3_t __attribute__ ((__always_inline__))
vld3q_u16 (const uint16_t * __a)
{
  union { uint16x8x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v8hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint32x4x3_t __attribute__ ((__always_inline__))
vld3q_u32 (const uint32_t * __a)
{
  union { uint32x4x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v4si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

__extension__ static __inline poly8x16x3_t __attribute__ ((__always_inline__))
vld3q_p8 (const poly8_t * __a)
{
  union { poly8x16x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v16qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline poly16x8x3_t __attribute__ ((__always_inline__))
vld3q_p16 (const poly16_t * __a)
{
  union { poly16x8x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3v8hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int8x8x3_t __attribute__ ((__always_inline__))
vld3_lane_s8 (const int8_t * __a, int8x8x3_t __b, const int __c)
{
  union { int8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { int8x8x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev8qi ((const __builtin_neon_qi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int16x4x3_t __attribute__ ((__always_inline__))
vld3_lane_s16 (const int16_t * __a, int16x4x3_t __b, const int __c)
{
  union { int16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { int16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev4hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int32x2x3_t __attribute__ ((__always_inline__))
vld3_lane_s32 (const int32_t * __a, int32x2x3_t __b, const int __c)
{
  union { int32x2x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { int32x2x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev2si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4x3_t __attribute__ ((__always_inline__))
vld3_lane_f16 (const float16_t * __a, float16x4x3_t __b, const int __c)
{
  union { float16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { float16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev4hf (__a, __bu.__o, __c);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x2x3_t __attribute__ ((__always_inline__))
vld3_lane_f32 (const float32_t * __a, float32x2x3_t __b, const int __c)
{
  union { float32x2x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { float32x2x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev2sf ((const __builtin_neon_sf *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint8x8x3_t __attribute__ ((__always_inline__))
vld3_lane_u8 (const uint8_t * __a, uint8x8x3_t __b, const int __c)
{
  union { uint8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { uint8x8x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev8qi ((const __builtin_neon_qi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint16x4x3_t __attribute__ ((__always_inline__))
vld3_lane_u16 (const uint16_t * __a, uint16x4x3_t __b, const int __c)
{
  union { uint16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { uint16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev4hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint32x2x3_t __attribute__ ((__always_inline__))
vld3_lane_u32 (const uint32_t * __a, uint32x2x3_t __b, const int __c)
{
  union { uint32x2x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { uint32x2x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev2si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline poly8x8x3_t __attribute__ ((__always_inline__))
vld3_lane_p8 (const poly8_t * __a, poly8x8x3_t __b, const int __c)
{
  union { poly8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { poly8x8x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev8qi ((const __builtin_neon_qi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline poly16x4x3_t __attribute__ ((__always_inline__))
vld3_lane_p16 (const poly16_t * __a, poly16x4x3_t __b, const int __c)
{
  union { poly16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  union { poly16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev4hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int16x8x3_t __attribute__ ((__always_inline__))
vld3q_lane_s16 (const int16_t * __a, int16x8x3_t __b, const int __c)
{
  union { int16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  union { int16x8x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev8hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int32x4x3_t __attribute__ ((__always_inline__))
vld3q_lane_s32 (const int32_t * __a, int32x4x3_t __b, const int __c)
{
  union { int32x4x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  union { int32x4x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev4si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8x3_t __attribute__ ((__always_inline__))
vld3q_lane_f16 (const float16_t * __a, float16x8x3_t __b, const int __c)
{
  union { float16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  union { float16x8x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev8hf (__a, __bu.__o, __c);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x4x3_t __attribute__ ((__always_inline__))
vld3q_lane_f32 (const float32_t * __a, float32x4x3_t __b, const int __c)
{
  union { float32x4x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  union { float32x4x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev4sf ((const __builtin_neon_sf *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint16x8x3_t __attribute__ ((__always_inline__))
vld3q_lane_u16 (const uint16_t * __a, uint16x8x3_t __b, const int __c)
{
  union { uint16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  union { uint16x8x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev8hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint32x4x3_t __attribute__ ((__always_inline__))
vld3q_lane_u32 (const uint32_t * __a, uint32x4x3_t __b, const int __c)
{
  union { uint32x4x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  union { uint32x4x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev4si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline poly16x8x3_t __attribute__ ((__always_inline__))
vld3q_lane_p16 (const poly16_t * __a, poly16x8x3_t __b, const int __c)
{
  union { poly16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  union { poly16x8x3_t __i; __builtin_neon_ci __o; } __rv;
  __rv.__o = __builtin_neon_vld3_lanev8hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int8x8x3_t __attribute__ ((__always_inline__))
vld3_dup_s8 (const int8_t * __a)
{
  union { int8x8x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline int16x4x3_t __attribute__ ((__always_inline__))
vld3_dup_s16 (const int16_t * __a)
{
  union { int16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int32x2x3_t __attribute__ ((__always_inline__))
vld3_dup_s32 (const int32_t * __a)
{
  union { int32x2x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4x3_t __attribute__ ((__always_inline__))
vld3_dup_f16 (const float16_t * __a)
{
  union { float16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv4hf (__a);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x2x3_t __attribute__ ((__always_inline__))
vld3_dup_f32 (const float32_t * __a)
{
  union { float32x2x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv2sf ((const __builtin_neon_sf *) __a);
  return __rv.__i;
}

__extension__ static __inline uint8x8x3_t __attribute__ ((__always_inline__))
vld3_dup_u8 (const uint8_t * __a)
{
  union { uint8x8x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint16x4x3_t __attribute__ ((__always_inline__))
vld3_dup_u16 (const uint16_t * __a)
{
  union { uint16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint32x2x3_t __attribute__ ((__always_inline__))
vld3_dup_u32 (const uint32_t * __a)
{
  union { uint32x2x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

__extension__ static __inline poly8x8x3_t __attribute__ ((__always_inline__))
vld3_dup_p8 (const poly8_t * __a)
{
  union { poly8x8x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline poly16x4x3_t __attribute__ ((__always_inline__))
vld3_dup_p16 (const poly16_t * __a)
{
  union { poly16x4x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupv4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1x3_t __attribute__ ((__always_inline__))
vld3_dup_p64 (const poly64_t * __a)
{
  union { poly64x1x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupdi ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

#pragma GCC pop_options
__extension__ static __inline int64x1x3_t __attribute__ ((__always_inline__))
vld3_dup_s64 (const int64_t * __a)
{
  union { int64x1x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupdi ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline uint64x1x3_t __attribute__ ((__always_inline__))
vld3_dup_u64 (const uint64_t * __a)
{
  union { uint64x1x3_t __i; __builtin_neon_ei __o; } __rv;
  __rv.__o = __builtin_neon_vld3_dupdi ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_s8 (int8_t * __a, int8x8x3_t __b)
{
  union { int8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v8qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_s16 (int16_t * __a, int16x4x3_t __b)
{
  union { int16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v4hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_s32 (int32_t * __a, int32x2x3_t __b)
{
  union { int32x2x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v2si ((__builtin_neon_si *) __a, __bu.__o);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_f16 (float16_t * __a, float16x4x3_t __b)
{
  union { float16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v4hf (__a, __bu.__o);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_f32 (float32_t * __a, float32x2x3_t __b)
{
  union { float32x2x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v2sf ((__builtin_neon_sf *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_u8 (uint8_t * __a, uint8x8x3_t __b)
{
  union { uint8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v8qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_u16 (uint16_t * __a, uint16x4x3_t __b)
{
  union { uint16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v4hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_u32 (uint32_t * __a, uint32x2x3_t __b)
{
  union { uint32x2x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v2si ((__builtin_neon_si *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_p8 (poly8_t * __a, poly8x8x3_t __b)
{
  union { poly8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v8qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_p16 (poly16_t * __a, poly16x4x3_t __b)
{
  union { poly16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3v4hi ((__builtin_neon_hi *) __a, __bu.__o);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_p64 (poly64_t * __a, poly64x1x3_t __b)
{
  union { poly64x1x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3di ((__builtin_neon_di *) __a, __bu.__o);
}

#pragma GCC pop_options
__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_s64 (int64_t * __a, int64x1x3_t __b)
{
  union { int64x1x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3di ((__builtin_neon_di *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_u64 (uint64_t * __a, uint64x1x3_t __b)
{
  union { uint64x1x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3di ((__builtin_neon_di *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_s8 (int8_t * __a, int8x16x3_t __b)
{
  union { int8x16x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v16qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_s16 (int16_t * __a, int16x8x3_t __b)
{
  union { int16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v8hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_s32 (int32_t * __a, int32x4x3_t __b)
{
  union { int32x4x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v4si ((__builtin_neon_si *) __a, __bu.__o);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_f16 (float16_t * __a, float16x8x3_t __b)
{
  union { float16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v8hf (__a, __bu.__o);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_f32 (float32_t * __a, float32x4x3_t __b)
{
  union { float32x4x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v4sf ((__builtin_neon_sf *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_u8 (uint8_t * __a, uint8x16x3_t __b)
{
  union { uint8x16x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v16qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_u16 (uint16_t * __a, uint16x8x3_t __b)
{
  union { uint16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v8hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_u32 (uint32_t * __a, uint32x4x3_t __b)
{
  union { uint32x4x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v4si ((__builtin_neon_si *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_p8 (poly8_t * __a, poly8x16x3_t __b)
{
  union { poly8x16x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v16qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_p16 (poly16_t * __a, poly16x8x3_t __b)
{
  union { poly16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3v8hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_s8 (int8_t * __a, int8x8x3_t __b, const int __c)
{
  union { int8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev8qi ((__builtin_neon_qi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_s16 (int16_t * __a, int16x4x3_t __b, const int __c)
{
  union { int16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev4hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_s32 (int32_t * __a, int32x2x3_t __b, const int __c)
{
  union { int32x2x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev2si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_f16 (float16_t * __a, float16x4x3_t __b, const int __c)
{
  union { float16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev4hf (__a, __bu.__o, __c);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_f32 (float32_t * __a, float32x2x3_t __b, const int __c)
{
  union { float32x2x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev2sf ((__builtin_neon_sf *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_u8 (uint8_t * __a, uint8x8x3_t __b, const int __c)
{
  union { uint8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev8qi ((__builtin_neon_qi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_u16 (uint16_t * __a, uint16x4x3_t __b, const int __c)
{
  union { uint16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev4hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_u32 (uint32_t * __a, uint32x2x3_t __b, const int __c)
{
  union { uint32x2x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev2si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_p8 (poly8_t * __a, poly8x8x3_t __b, const int __c)
{
  union { poly8x8x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev8qi ((__builtin_neon_qi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3_lane_p16 (poly16_t * __a, poly16x4x3_t __b, const int __c)
{
  union { poly16x4x3_t __i; __builtin_neon_ei __o; } __bu = { __b };
  __builtin_neon_vst3_lanev4hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_lane_s16 (int16_t * __a, int16x8x3_t __b, const int __c)
{
  union { int16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3_lanev8hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_lane_s32 (int32_t * __a, int32x4x3_t __b, const int __c)
{
  union { int32x4x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3_lanev4si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_lane_f16 (float16_t * __a, float16x8x3_t __b, const int __c)
{
  union { float16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3_lanev8hf (__a, __bu.__o, __c);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_lane_f32 (float32_t * __a, float32x4x3_t __b, const int __c)
{
  union { float32x4x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3_lanev4sf ((__builtin_neon_sf *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_lane_u16 (uint16_t * __a, uint16x8x3_t __b, const int __c)
{
  union { uint16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3_lanev8hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_lane_u32 (uint32_t * __a, uint32x4x3_t __b, const int __c)
{
  union { uint32x4x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3_lanev4si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst3q_lane_p16 (poly16_t * __a, poly16x8x3_t __b, const int __c)
{
  union { poly16x8x3_t __i; __builtin_neon_ci __o; } __bu = { __b };
  __builtin_neon_vst3_lanev8hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline int8x8x4_t __attribute__ ((__always_inline__))
vld4_s8 (const int8_t * __a)
{
  union { int8x8x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline int16x4x4_t __attribute__ ((__always_inline__))
vld4_s16 (const int16_t * __a)
{
  union { int16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int32x2x4_t __attribute__ ((__always_inline__))
vld4_s32 (const int32_t * __a)
{
  union { int32x2x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4x4_t __attribute__ ((__always_inline__))
vld4_f16 (const float16_t * __a)
{
  union { float16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v4hf (__a);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x2x4_t __attribute__ ((__always_inline__))
vld4_f32 (const float32_t * __a)
{
  union { float32x2x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v2sf ((const __builtin_neon_sf *) __a);
  return __rv.__i;
}

__extension__ static __inline uint8x8x4_t __attribute__ ((__always_inline__))
vld4_u8 (const uint8_t * __a)
{
  union { uint8x8x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint16x4x4_t __attribute__ ((__always_inline__))
vld4_u16 (const uint16_t * __a)
{
  union { uint16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint32x2x4_t __attribute__ ((__always_inline__))
vld4_u32 (const uint32_t * __a)
{
  union { uint32x2x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

__extension__ static __inline poly8x8x4_t __attribute__ ((__always_inline__))
vld4_p8 (const poly8_t * __a)
{
  union { poly8x8x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline poly16x4x4_t __attribute__ ((__always_inline__))
vld4_p16 (const poly16_t * __a)
{
  union { poly16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1x4_t __attribute__ ((__always_inline__))
vld4_p64 (const poly64_t * __a)
{
  union { poly64x1x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4di ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

#pragma GCC pop_options
__extension__ static __inline int64x1x4_t __attribute__ ((__always_inline__))
vld4_s64 (const int64_t * __a)
{
  union { int64x1x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4di ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline uint64x1x4_t __attribute__ ((__always_inline__))
vld4_u64 (const uint64_t * __a)
{
  union { uint64x1x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4di ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline int8x16x4_t __attribute__ ((__always_inline__))
vld4q_s8 (const int8_t * __a)
{
  union { int8x16x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v16qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline int16x8x4_t __attribute__ ((__always_inline__))
vld4q_s16 (const int16_t * __a)
{
  union { int16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v8hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int32x4x4_t __attribute__ ((__always_inline__))
vld4q_s32 (const int32_t * __a)
{
  union { int32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v4si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8x4_t __attribute__ ((__always_inline__))
vld4q_f16 (const float16_t * __a)
{
  union { float16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v8hf (__a);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x4x4_t __attribute__ ((__always_inline__))
vld4q_f32 (const float32_t * __a)
{
  union { float32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v4sf ((const __builtin_neon_sf *) __a);
  return __rv.__i;
}

__extension__ static __inline uint8x16x4_t __attribute__ ((__always_inline__))
vld4q_u8 (const uint8_t * __a)
{
  union { uint8x16x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v16qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint16x8x4_t __attribute__ ((__always_inline__))
vld4q_u16 (const uint16_t * __a)
{
  union { uint16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v8hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint32x4x4_t __attribute__ ((__always_inline__))
vld4q_u32 (const uint32_t * __a)
{
  union { uint32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v4si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

__extension__ static __inline poly8x16x4_t __attribute__ ((__always_inline__))
vld4q_p8 (const poly8_t * __a)
{
  union { poly8x16x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v16qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline poly16x8x4_t __attribute__ ((__always_inline__))
vld4q_p16 (const poly16_t * __a)
{
  union { poly16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4v8hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int8x8x4_t __attribute__ ((__always_inline__))
vld4_lane_s8 (const int8_t * __a, int8x8x4_t __b, const int __c)
{
  union { int8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { int8x8x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev8qi ((const __builtin_neon_qi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int16x4x4_t __attribute__ ((__always_inline__))
vld4_lane_s16 (const int16_t * __a, int16x4x4_t __b, const int __c)
{
  union { int16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { int16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev4hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int32x2x4_t __attribute__ ((__always_inline__))
vld4_lane_s32 (const int32_t * __a, int32x2x4_t __b, const int __c)
{
  union { int32x2x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { int32x2x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev2si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4x4_t __attribute__ ((__always_inline__))
vld4_lane_f16 (const float16_t * __a, float16x4x4_t __b, const int __c)
{
  union { float16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { float16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev4hf (__a,
					   __bu.__o, __c);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x2x4_t __attribute__ ((__always_inline__))
vld4_lane_f32 (const float32_t * __a, float32x2x4_t __b, const int __c)
{
  union { float32x2x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { float32x2x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev2sf ((const __builtin_neon_sf *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint8x8x4_t __attribute__ ((__always_inline__))
vld4_lane_u8 (const uint8_t * __a, uint8x8x4_t __b, const int __c)
{
  union { uint8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { uint8x8x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev8qi ((const __builtin_neon_qi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint16x4x4_t __attribute__ ((__always_inline__))
vld4_lane_u16 (const uint16_t * __a, uint16x4x4_t __b, const int __c)
{
  union { uint16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { uint16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev4hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint32x2x4_t __attribute__ ((__always_inline__))
vld4_lane_u32 (const uint32_t * __a, uint32x2x4_t __b, const int __c)
{
  union { uint32x2x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { uint32x2x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev2si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline poly8x8x4_t __attribute__ ((__always_inline__))
vld4_lane_p8 (const poly8_t * __a, poly8x8x4_t __b, const int __c)
{
  union { poly8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { poly8x8x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev8qi ((const __builtin_neon_qi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline poly16x4x4_t __attribute__ ((__always_inline__))
vld4_lane_p16 (const poly16_t * __a, poly16x4x4_t __b, const int __c)
{
  union { poly16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  union { poly16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev4hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int16x8x4_t __attribute__ ((__always_inline__))
vld4q_lane_s16 (const int16_t * __a, int16x8x4_t __b, const int __c)
{
  union { int16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  union { int16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev8hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int32x4x4_t __attribute__ ((__always_inline__))
vld4q_lane_s32 (const int32_t * __a, int32x4x4_t __b, const int __c)
{
  union { int32x4x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  union { int32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev4si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8x4_t __attribute__ ((__always_inline__))
vld4q_lane_f16 (const float16_t * __a, float16x8x4_t __b, const int __c)
{
  union { float16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  union { float16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev8hf (__a,
					   __bu.__o, __c);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x4x4_t __attribute__ ((__always_inline__))
vld4q_lane_f32 (const float32_t * __a, float32x4x4_t __b, const int __c)
{
  union { float32x4x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  union { float32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev4sf ((const __builtin_neon_sf *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint16x8x4_t __attribute__ ((__always_inline__))
vld4q_lane_u16 (const uint16_t * __a, uint16x8x4_t __b, const int __c)
{
  union { uint16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  union { uint16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev8hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline uint32x4x4_t __attribute__ ((__always_inline__))
vld4q_lane_u32 (const uint32_t * __a, uint32x4x4_t __b, const int __c)
{
  union { uint32x4x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  union { uint32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev4si ((const __builtin_neon_si *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline poly16x8x4_t __attribute__ ((__always_inline__))
vld4q_lane_p16 (const poly16_t * __a, poly16x8x4_t __b, const int __c)
{
  union { poly16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  union { poly16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_lanev8hi ((const __builtin_neon_hi *) __a, __bu.__o, __c);
  return __rv.__i;
}

__extension__ static __inline int8x8x4_t __attribute__ ((__always_inline__))
vld4_dup_s8 (const int8_t * __a)
{
  union { int8x8x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline int16x4x4_t __attribute__ ((__always_inline__))
vld4_dup_s16 (const int16_t * __a)
{
  union { int16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline int32x2x4_t __attribute__ ((__always_inline__))
vld4_dup_s32 (const int32_t * __a)
{
  union { int32x2x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4x4_t __attribute__ ((__always_inline__))
vld4_dup_f16 (const float16_t * __a)
{
  union { float16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv4hf (__a);
  return __rv.__i;
}
#endif

__extension__ static __inline float32x2x4_t __attribute__ ((__always_inline__))
vld4_dup_f32 (const float32_t * __a)
{
  union { float32x2x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv2sf ((const __builtin_neon_sf *) __a);
  return __rv.__i;
}

__extension__ static __inline uint8x8x4_t __attribute__ ((__always_inline__))
vld4_dup_u8 (const uint8_t * __a)
{
  union { uint8x8x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint16x4x4_t __attribute__ ((__always_inline__))
vld4_dup_u16 (const uint16_t * __a)
{
  union { uint16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

__extension__ static __inline uint32x2x4_t __attribute__ ((__always_inline__))
vld4_dup_u32 (const uint32_t * __a)
{
  union { uint32x2x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv2si ((const __builtin_neon_si *) __a);
  return __rv.__i;
}

__extension__ static __inline poly8x8x4_t __attribute__ ((__always_inline__))
vld4_dup_p8 (const poly8_t * __a)
{
  union { poly8x8x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv8qi ((const __builtin_neon_qi *) __a);
  return __rv.__i;
}

__extension__ static __inline poly16x4x4_t __attribute__ ((__always_inline__))
vld4_dup_p16 (const poly16_t * __a)
{
  union { poly16x4x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupv4hi ((const __builtin_neon_hi *) __a);
  return __rv.__i;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1x4_t __attribute__ ((__always_inline__))
vld4_dup_p64 (const poly64_t * __a)
{
  union { poly64x1x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupdi ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

#pragma GCC pop_options
__extension__ static __inline int64x1x4_t __attribute__ ((__always_inline__))
vld4_dup_s64 (const int64_t * __a)
{
  union { int64x1x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupdi ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline uint64x1x4_t __attribute__ ((__always_inline__))
vld4_dup_u64 (const uint64_t * __a)
{
  union { uint64x1x4_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_neon_vld4_dupdi ((const __builtin_neon_di *) __a);
  return __rv.__i;
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_s8 (int8_t * __a, int8x8x4_t __b)
{
  union { int8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v8qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_s16 (int16_t * __a, int16x4x4_t __b)
{
  union { int16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v4hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_s32 (int32_t * __a, int32x2x4_t __b)
{
  union { int32x2x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v2si ((__builtin_neon_si *) __a, __bu.__o);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_f16 (float16_t * __a, float16x4x4_t __b)
{
  union { float16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v4hf (__a, __bu.__o);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_f32 (float32_t * __a, float32x2x4_t __b)
{
  union { float32x2x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v2sf ((__builtin_neon_sf *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_u8 (uint8_t * __a, uint8x8x4_t __b)
{
  union { uint8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v8qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_u16 (uint16_t * __a, uint16x4x4_t __b)
{
  union { uint16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v4hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_u32 (uint32_t * __a, uint32x2x4_t __b)
{
  union { uint32x2x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v2si ((__builtin_neon_si *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_p8 (poly8_t * __a, poly8x8x4_t __b)
{
  union { poly8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v8qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_p16 (poly16_t * __a, poly16x4x4_t __b)
{
  union { poly16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4v4hi ((__builtin_neon_hi *) __a, __bu.__o);
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_p64 (poly64_t * __a, poly64x1x4_t __b)
{
  union { poly64x1x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4di ((__builtin_neon_di *) __a, __bu.__o);
}

#pragma GCC pop_options
__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_s64 (int64_t * __a, int64x1x4_t __b)
{
  union { int64x1x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4di ((__builtin_neon_di *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_u64 (uint64_t * __a, uint64x1x4_t __b)
{
  union { uint64x1x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4di ((__builtin_neon_di *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_s8 (int8_t * __a, int8x16x4_t __b)
{
  union { int8x16x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v16qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_s16 (int16_t * __a, int16x8x4_t __b)
{
  union { int16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v8hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_s32 (int32_t * __a, int32x4x4_t __b)
{
  union { int32x4x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v4si ((__builtin_neon_si *) __a, __bu.__o);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_f16 (float16_t * __a, float16x8x4_t __b)
{
  union { float16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v8hf (__a, __bu.__o);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_f32 (float32_t * __a, float32x4x4_t __b)
{
  union { float32x4x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v4sf ((__builtin_neon_sf *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_u8 (uint8_t * __a, uint8x16x4_t __b)
{
  union { uint8x16x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v16qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_u16 (uint16_t * __a, uint16x8x4_t __b)
{
  union { uint16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v8hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_u32 (uint32_t * __a, uint32x4x4_t __b)
{
  union { uint32x4x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v4si ((__builtin_neon_si *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_p8 (poly8_t * __a, poly8x16x4_t __b)
{
  union { poly8x16x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v16qi ((__builtin_neon_qi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_p16 (poly16_t * __a, poly16x8x4_t __b)
{
  union { poly16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4v8hi ((__builtin_neon_hi *) __a, __bu.__o);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_s8 (int8_t * __a, int8x8x4_t __b, const int __c)
{
  union { int8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev8qi ((__builtin_neon_qi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_s16 (int16_t * __a, int16x4x4_t __b, const int __c)
{
  union { int16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev4hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_s32 (int32_t * __a, int32x2x4_t __b, const int __c)
{
  union { int32x2x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev2si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_f16 (float16_t * __a, float16x4x4_t __b, const int __c)
{
  union { float16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev4hf (__a, __bu.__o, __c);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_f32 (float32_t * __a, float32x2x4_t __b, const int __c)
{
  union { float32x2x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev2sf ((__builtin_neon_sf *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_u8 (uint8_t * __a, uint8x8x4_t __b, const int __c)
{
  union { uint8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev8qi ((__builtin_neon_qi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_u16 (uint16_t * __a, uint16x4x4_t __b, const int __c)
{
  union { uint16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev4hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_u32 (uint32_t * __a, uint32x2x4_t __b, const int __c)
{
  union { uint32x2x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev2si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_p8 (poly8_t * __a, poly8x8x4_t __b, const int __c)
{
  union { poly8x8x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev8qi ((__builtin_neon_qi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4_lane_p16 (poly16_t * __a, poly16x4x4_t __b, const int __c)
{
  union { poly16x4x4_t __i; __builtin_neon_oi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev4hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_lane_s16 (int16_t * __a, int16x8x4_t __b, const int __c)
{
  union { int16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev8hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_lane_s32 (int32_t * __a, int32x4x4_t __b, const int __c)
{
  union { int32x4x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev4si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_lane_f16 (float16_t * __a, float16x8x4_t __b, const int __c)
{
  union { float16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev8hf (__a, __bu.__o, __c);
}
#endif

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_lane_f32 (float32_t * __a, float32x4x4_t __b, const int __c)
{
  union { float32x4x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev4sf ((__builtin_neon_sf *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_lane_u16 (uint16_t * __a, uint16x8x4_t __b, const int __c)
{
  union { uint16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev8hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_lane_u32 (uint32_t * __a, uint32x4x4_t __b, const int __c)
{
  union { uint32x4x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev4si ((__builtin_neon_si *) __a, __bu.__o, __c);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vst4q_lane_p16 (poly16_t * __a, poly16x8x4_t __b, const int __c)
{
  union { poly16x8x4_t __i; __builtin_neon_xi __o; } __bu = { __b };
  __builtin_neon_vst4_lanev8hi ((__builtin_neon_hi *) __a, __bu.__o, __c);
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

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_p16 (poly16x4_t __a)
{
  return (poly8x8_t) __a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_f16 (float16x4_t __a)
{
  return (poly8x8_t) __a;
}
#endif

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_f32 (float32x2_t __a)
{
  return (poly8x8_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_p64 (poly64x1_t __a)
{
  return (poly8x8_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_s64 (int64x1_t __a)
{
  return (poly8x8_t)__a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_u64 (uint64x1_t __a)
{
  return (poly8x8_t)__a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_s8 (int8x8_t __a)
{
  return (poly8x8_t)__a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_s16 (int16x4_t __a)
{
  return (poly8x8_t)__a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_s32 (int32x2_t __a)
{
  return (poly8x8_t)__a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_u8 (uint8x8_t __a)
{
  return (poly8x8_t)__a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_u16 (uint16x4_t __a)
{
  return (poly8x8_t)__a;
}

__extension__ static __inline poly8x8_t __attribute__ ((__always_inline__))
vreinterpret_p8_u32 (uint32x2_t __a)
{
  return (poly8x8_t)__a;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_p8 (poly8x8_t __a)
{
  return (poly16x4_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_f16 (float16x4_t __a)
{
  return (poly16x4_t) __a;
}
#endif

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_f32 (float32x2_t __a)
{
  return (poly16x4_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_p64 (poly64x1_t __a)
{
  return (poly16x4_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_s64 (int64x1_t __a)
{
  return (poly16x4_t)__a;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_u64 (uint64x1_t __a)
{
  return (poly16x4_t)__a;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_s8 (int8x8_t __a)
{
  return (poly16x4_t)__a;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_s16 (int16x4_t __a)
{
  return (poly16x4_t)__a;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_s32 (int32x2_t __a)
{
  return (poly16x4_t)__a;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_u8 (uint8x8_t __a)
{
  return (poly16x4_t)__a;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_u16 (uint16x4_t __a)
{
  return (poly16x4_t)__a;
}

__extension__ static __inline poly16x4_t __attribute__ ((__always_inline__))
vreinterpret_p16_u32 (uint32x2_t __a)
{
  return (poly16x4_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_p8 (poly8x8_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_p16 (poly16x4_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_f32 (float32x2_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_p64 (poly64x1_t __a)
{
  return (float16x4_t) __a;
}
#pragma GCC pop_options
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_s64 (int64x1_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_u64 (uint64x1_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_s8 (int8x8_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_s16 (int16x4_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_s32 (int32x2_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_u8 (uint8x8_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_u16 (uint16x4_t __a)
{
  return (float16x4_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vreinterpret_f16_u32 (uint32x2_t __a)
{
  return (float16x4_t) __a;
}
#endif

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_p8 (poly8x8_t __a)
{
  return (float32x2_t)__a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_p16 (poly16x4_t __a)
{
  return (float32x2_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_f16 (float16x4_t __a)
{
  return (float32x2_t) __a;
}
#endif

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_p64 (poly64x1_t __a)
{
  return (float32x2_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_s64 (int64x1_t __a)
{
  return (float32x2_t)__a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_u64 (uint64x1_t __a)
{
  return (float32x2_t)__a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_s8 (int8x8_t __a)
{
  return (float32x2_t)__a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_s16 (int16x4_t __a)
{
  return (float32x2_t)__a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_s32 (int32x2_t __a)
{
  return (float32x2_t)__a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_u8 (uint8x8_t __a)
{
  return (float32x2_t)__a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_u16 (uint16x4_t __a)
{
  return (float32x2_t)__a;
}

__extension__ static __inline float32x2_t __attribute__ ((__always_inline__))
vreinterpret_f32_u32 (uint32x2_t __a)
{
  return (float32x2_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_p8 (poly8x8_t __a)
{
  return (poly64x1_t)__a;
}

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_p16 (poly16x4_t __a)
{
  return (poly64x1_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_f16 (float16x4_t __a)
{
  return (poly64x1_t) __a;
}
#endif

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_f32 (float32x2_t __a)
{
  return (poly64x1_t)__a;
}

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_s64 (int64x1_t __a)
{
  return (poly64x1_t)__a;
}

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_u64 (uint64x1_t __a)
{
  return (poly64x1_t)__a;
}

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_s8 (int8x8_t __a)
{
  return (poly64x1_t)__a;
}

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_s16 (int16x4_t __a)
{
  return (poly64x1_t)__a;
}

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_s32 (int32x2_t __a)
{
  return (poly64x1_t)__a;
}

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_u8 (uint8x8_t __a)
{
  return (poly64x1_t)__a;
}

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_u16 (uint16x4_t __a)
{
  return (poly64x1_t)__a;
}

__extension__ static __inline poly64x1_t __attribute__ ((__always_inline__))
vreinterpret_p64_u32 (uint32x2_t __a)
{
  return (poly64x1_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_p8 (poly8x8_t __a)
{
  return (int64x1_t)__a;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_p16 (poly16x4_t __a)
{
  return (int64x1_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_f16 (float16x4_t __a)
{
  return (int64x1_t) __a;
}
#endif

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_f32 (float32x2_t __a)
{
  return (int64x1_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_p64 (poly64x1_t __a)
{
  return (int64x1_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_u64 (uint64x1_t __a)
{
  return (int64x1_t)__a;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_s8 (int8x8_t __a)
{
  return (int64x1_t)__a;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_s16 (int16x4_t __a)
{
  return (int64x1_t)__a;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_s32 (int32x2_t __a)
{
  return (int64x1_t)__a;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_u8 (uint8x8_t __a)
{
  return (int64x1_t)__a;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_u16 (uint16x4_t __a)
{
  return (int64x1_t)__a;
}

__extension__ static __inline int64x1_t __attribute__ ((__always_inline__))
vreinterpret_s64_u32 (uint32x2_t __a)
{
  return (int64x1_t)__a;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_p8 (poly8x8_t __a)
{
  return (uint64x1_t)__a;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_p16 (poly16x4_t __a)
{
  return (uint64x1_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_f16 (float16x4_t __a)
{
  return (uint64x1_t) __a;
}
#endif

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_f32 (float32x2_t __a)
{
  return (uint64x1_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_p64 (poly64x1_t __a)
{
  return (uint64x1_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_s64 (int64x1_t __a)
{
  return (uint64x1_t)__a;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_s8 (int8x8_t __a)
{
  return (uint64x1_t)__a;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_s16 (int16x4_t __a)
{
  return (uint64x1_t)__a;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_s32 (int32x2_t __a)
{
  return (uint64x1_t)__a;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_u8 (uint8x8_t __a)
{
  return (uint64x1_t)__a;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_u16 (uint16x4_t __a)
{
  return (uint64x1_t)__a;
}

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vreinterpret_u64_u32 (uint32x2_t __a)
{
  return (uint64x1_t)__a;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_p8 (poly8x8_t __a)
{
  return (int8x8_t)__a;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_p16 (poly16x4_t __a)
{
  return (int8x8_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_f16 (float16x4_t __a)
{
  return (int8x8_t) __a;
}
#endif

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_f32 (float32x2_t __a)
{
  return (int8x8_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_p64 (poly64x1_t __a)
{
  return (int8x8_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_s64 (int64x1_t __a)
{
  return (int8x8_t)__a;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_u64 (uint64x1_t __a)
{
  return (int8x8_t)__a;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_s16 (int16x4_t __a)
{
  return (int8x8_t)__a;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_s32 (int32x2_t __a)
{
  return (int8x8_t)__a;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_u8 (uint8x8_t __a)
{
  return (int8x8_t)__a;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_u16 (uint16x4_t __a)
{
  return (int8x8_t)__a;
}

__extension__ static __inline int8x8_t __attribute__ ((__always_inline__))
vreinterpret_s8_u32 (uint32x2_t __a)
{
  return (int8x8_t)__a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_p8 (poly8x8_t __a)
{
  return (int16x4_t)__a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_p16 (poly16x4_t __a)
{
  return (int16x4_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_f16 (float16x4_t __a)
{
  return (int16x4_t) __a;
}
#endif

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_f32 (float32x2_t __a)
{
  return (int16x4_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_p64 (poly64x1_t __a)
{
  return (int16x4_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_s64 (int64x1_t __a)
{
  return (int16x4_t)__a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_u64 (uint64x1_t __a)
{
  return (int16x4_t)__a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_s8 (int8x8_t __a)
{
  return (int16x4_t)__a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_s32 (int32x2_t __a)
{
  return (int16x4_t)__a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_u8 (uint8x8_t __a)
{
  return (int16x4_t)__a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_u16 (uint16x4_t __a)
{
  return (int16x4_t)__a;
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vreinterpret_s16_u32 (uint32x2_t __a)
{
  return (int16x4_t)__a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_p8 (poly8x8_t __a)
{
  return (int32x2_t)__a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_p16 (poly16x4_t __a)
{
  return (int32x2_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_f16 (float16x4_t __a)
{
  return (int32x2_t) __a;
}
#endif

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_f32 (float32x2_t __a)
{
  return (int32x2_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_p64 (poly64x1_t __a)
{
  return (int32x2_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_s64 (int64x1_t __a)
{
  return (int32x2_t)__a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_u64 (uint64x1_t __a)
{
  return (int32x2_t)__a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_s8 (int8x8_t __a)
{
  return (int32x2_t)__a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_s16 (int16x4_t __a)
{
  return (int32x2_t)__a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_u8 (uint8x8_t __a)
{
  return (int32x2_t)__a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_u16 (uint16x4_t __a)
{
  return (int32x2_t)__a;
}

__extension__ static __inline int32x2_t __attribute__ ((__always_inline__))
vreinterpret_s32_u32 (uint32x2_t __a)
{
  return (int32x2_t)__a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_p8 (poly8x8_t __a)
{
  return (uint8x8_t)__a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_p16 (poly16x4_t __a)
{
  return (uint8x8_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_f16 (float16x4_t __a)
{
  return (uint8x8_t) __a;
}
#endif

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_f32 (float32x2_t __a)
{
  return (uint8x8_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_p64 (poly64x1_t __a)
{
  return (uint8x8_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_s64 (int64x1_t __a)
{
  return (uint8x8_t)__a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_u64 (uint64x1_t __a)
{
  return (uint8x8_t)__a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_s8 (int8x8_t __a)
{
  return (uint8x8_t)__a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_s16 (int16x4_t __a)
{
  return (uint8x8_t)__a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_s32 (int32x2_t __a)
{
  return (uint8x8_t)__a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_u16 (uint16x4_t __a)
{
  return (uint8x8_t)__a;
}

__extension__ static __inline uint8x8_t __attribute__ ((__always_inline__))
vreinterpret_u8_u32 (uint32x2_t __a)
{
  return (uint8x8_t)__a;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_p8 (poly8x8_t __a)
{
  return (uint16x4_t)__a;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_p16 (poly16x4_t __a)
{
  return (uint16x4_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_f16 (float16x4_t __a)
{
  return (uint16x4_t) __a;
}
#endif

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_f32 (float32x2_t __a)
{
  return (uint16x4_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_p64 (poly64x1_t __a)
{
  return (uint16x4_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_s64 (int64x1_t __a)
{
  return (uint16x4_t)__a;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_u64 (uint64x1_t __a)
{
  return (uint16x4_t)__a;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_s8 (int8x8_t __a)
{
  return (uint16x4_t)__a;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_s16 (int16x4_t __a)
{
  return (uint16x4_t)__a;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_s32 (int32x2_t __a)
{
  return (uint16x4_t)__a;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_u8 (uint8x8_t __a)
{
  return (uint16x4_t)__a;
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vreinterpret_u16_u32 (uint32x2_t __a)
{
  return (uint16x4_t)__a;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_p8 (poly8x8_t __a)
{
  return (uint32x2_t)__a;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_p16 (poly16x4_t __a)
{
  return (uint32x2_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_f16 (float16x4_t __a)
{
  return (uint32x2_t) __a;
}
#endif

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_f32 (float32x2_t __a)
{
  return (uint32x2_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_p64 (poly64x1_t __a)
{
  return (uint32x2_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_s64 (int64x1_t __a)
{
  return (uint32x2_t)__a;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_u64 (uint64x1_t __a)
{
  return (uint32x2_t)__a;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_s8 (int8x8_t __a)
{
  return (uint32x2_t)__a;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_s16 (int16x4_t __a)
{
  return (uint32x2_t)__a;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_s32 (int32x2_t __a)
{
  return (uint32x2_t)__a;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_u8 (uint8x8_t __a)
{
  return (uint32x2_t)__a;
}

__extension__ static __inline uint32x2_t __attribute__ ((__always_inline__))
vreinterpret_u32_u16 (uint16x4_t __a)
{
  return (uint32x2_t)__a;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_p16 (poly16x8_t __a)
{
  return (poly8x16_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_f16 (float16x8_t __a)
{
  return (poly8x16_t) __a;
}
#endif

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_f32 (float32x4_t __a)
{
  return (poly8x16_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_p64 (poly64x2_t __a)
{
  return (poly8x16_t)__a;
}


__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_p128 (poly128_t __a)
{
  return (poly8x16_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_s64 (int64x2_t __a)
{
  return (poly8x16_t)__a;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_u64 (uint64x2_t __a)
{
  return (poly8x16_t)__a;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_s8 (int8x16_t __a)
{
  return (poly8x16_t)__a;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_s16 (int16x8_t __a)
{
  return (poly8x16_t)__a;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_s32 (int32x4_t __a)
{
  return (poly8x16_t)__a;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_u8 (uint8x16_t __a)
{
  return (poly8x16_t)__a;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_u16 (uint16x8_t __a)
{
  return (poly8x16_t)__a;
}

__extension__ static __inline poly8x16_t __attribute__ ((__always_inline__))
vreinterpretq_p8_u32 (uint32x4_t __a)
{
  return (poly8x16_t)__a;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_p8 (poly8x16_t __a)
{
  return (poly16x8_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_f16 (float16x8_t __a)
{
  return (poly16x8_t) __a;
}
#endif

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_f32 (float32x4_t __a)
{
  return (poly16x8_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_p64 (poly64x2_t __a)
{
  return (poly16x8_t)__a;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_p128 (poly128_t __a)
{
  return (poly16x8_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_s64 (int64x2_t __a)
{
  return (poly16x8_t)__a;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_u64 (uint64x2_t __a)
{
  return (poly16x8_t)__a;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_s8 (int8x16_t __a)
{
  return (poly16x8_t)__a;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_s16 (int16x8_t __a)
{
  return (poly16x8_t)__a;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_s32 (int32x4_t __a)
{
  return (poly16x8_t)__a;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_u8 (uint8x16_t __a)
{
  return (poly16x8_t)__a;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_u16 (uint16x8_t __a)
{
  return (poly16x8_t)__a;
}

__extension__ static __inline poly16x8_t __attribute__ ((__always_inline__))
vreinterpretq_p16_u32 (uint32x4_t __a)
{
  return (poly16x8_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_p8 (poly8x16_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_p16 (poly16x8_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_f32 (float32x4_t __a)
{
  return (float16x8_t) __a;
}
#endif

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_p64 (poly64x2_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_p128 (poly128_t __a)
{
  return (float16x8_t) __a;
}
#endif

#pragma GCC pop_options

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_s64 (int64x2_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_u64 (uint64x2_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_s8 (int8x16_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_s16 (int16x8_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_s32 (int32x4_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_u8 (uint8x16_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_u16 (uint16x8_t __a)
{
  return (float16x8_t) __a;
}
#endif

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vreinterpretq_f16_u32 (uint32x4_t __a)
{
  return (float16x8_t) __a;
}
#endif

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_p8 (poly8x16_t __a)
{
  return (float32x4_t)__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_p16 (poly16x8_t __a)
{
  return (float32x4_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_f16 (float16x8_t __a)
{
  return (float32x4_t) __a;
}
#endif

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_p64 (poly64x2_t __a)
{
  return (float32x4_t)__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_p128 (poly128_t __a)
{
  return (float32x4_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_s64 (int64x2_t __a)
{
  return (float32x4_t)__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_u64 (uint64x2_t __a)
{
  return (float32x4_t)__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_s8 (int8x16_t __a)
{
  return (float32x4_t)__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_s16 (int16x8_t __a)
{
  return (float32x4_t)__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_s32 (int32x4_t __a)
{
  return (float32x4_t)__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_u8 (uint8x16_t __a)
{
  return (float32x4_t)__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_u16 (uint16x8_t __a)
{
  return (float32x4_t)__a;
}

__extension__ static __inline float32x4_t __attribute__ ((__always_inline__))
vreinterpretq_f32_u32 (uint32x4_t __a)
{
  return (float32x4_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_p8 (poly8x16_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_p16 (poly16x8_t __a)
{
  return (poly64x2_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_f16 (float16x8_t __a)
{
  return (poly64x2_t) __a;
}
#endif

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_f32 (float32x4_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_p128 (poly128_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_s64 (int64x2_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_u64 (uint64x2_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_s8 (int8x16_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_s16 (int16x8_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_s32 (int32x4_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_u8 (uint8x16_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_u16 (uint16x8_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly64x2_t __attribute__ ((__always_inline__))
vreinterpretq_p64_u32 (uint32x4_t __a)
{
  return (poly64x2_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_p8 (poly8x16_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_p16 (poly16x8_t __a)
{
  return (poly128_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_f16 (float16x8_t __a)
{
  return (poly128_t) __a;
}
#endif

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_f32 (float32x4_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_p64 (poly64x2_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_s64 (int64x2_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_u64 (uint64x2_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_s8 (int8x16_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_s16 (int16x8_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_s32 (int32x4_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_u8 (uint8x16_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_u16 (uint16x8_t __a)
{
  return (poly128_t)__a;
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vreinterpretq_p128_u32 (uint32x4_t __a)
{
  return (poly128_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_p8 (poly8x16_t __a)
{
  return (int64x2_t)__a;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_p16 (poly16x8_t __a)
{
  return (int64x2_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_f16 (float16x8_t __a)
{
  return (int64x2_t) __a;
}
#endif

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_f32 (float32x4_t __a)
{
  return (int64x2_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_p64 (poly64x2_t __a)
{
  return (int64x2_t)__a;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_p128 (poly128_t __a)
{
  return (int64x2_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_u64 (uint64x2_t __a)
{
  return (int64x2_t)__a;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_s8 (int8x16_t __a)
{
  return (int64x2_t)__a;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_s16 (int16x8_t __a)
{
  return (int64x2_t)__a;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_s32 (int32x4_t __a)
{
  return (int64x2_t)__a;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_u8 (uint8x16_t __a)
{
  return (int64x2_t)__a;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_u16 (uint16x8_t __a)
{
  return (int64x2_t)__a;
}

__extension__ static __inline int64x2_t __attribute__ ((__always_inline__))
vreinterpretq_s64_u32 (uint32x4_t __a)
{
  return (int64x2_t)__a;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_p8 (poly8x16_t __a)
{
  return (uint64x2_t)__a;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_p16 (poly16x8_t __a)
{
  return (uint64x2_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_f16 (float16x8_t __a)
{
  return (uint64x2_t) __a;
}
#endif

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_f32 (float32x4_t __a)
{
  return (uint64x2_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_p64 (poly64x2_t __a)
{
  return (uint64x2_t)__a;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_p128 (poly128_t __a)
{
  return (uint64x2_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_s64 (int64x2_t __a)
{
  return (uint64x2_t)__a;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_s8 (int8x16_t __a)
{
  return (uint64x2_t)__a;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_s16 (int16x8_t __a)
{
  return (uint64x2_t)__a;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_s32 (int32x4_t __a)
{
  return (uint64x2_t)__a;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_u8 (uint8x16_t __a)
{
  return (uint64x2_t)__a;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_u16 (uint16x8_t __a)
{
  return (uint64x2_t)__a;
}

__extension__ static __inline uint64x2_t __attribute__ ((__always_inline__))
vreinterpretq_u64_u32 (uint32x4_t __a)
{
  return (uint64x2_t)__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_p8 (poly8x16_t __a)
{
  return (int8x16_t)__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_p16 (poly16x8_t __a)
{
  return (int8x16_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_f16 (float16x8_t __a)
{
  return (int8x16_t) __a;
}
#endif

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_f32 (float32x4_t __a)
{
  return (int8x16_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_p64 (poly64x2_t __a)
{
  return (int8x16_t)__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_p128 (poly128_t __a)
{
  return (int8x16_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_s64 (int64x2_t __a)
{
  return (int8x16_t)__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_u64 (uint64x2_t __a)
{
  return (int8x16_t)__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_s16 (int16x8_t __a)
{
  return (int8x16_t)__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_s32 (int32x4_t __a)
{
  return (int8x16_t)__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_u8 (uint8x16_t __a)
{
  return (int8x16_t)__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_u16 (uint16x8_t __a)
{
  return (int8x16_t)__a;
}

__extension__ static __inline int8x16_t __attribute__ ((__always_inline__))
vreinterpretq_s8_u32 (uint32x4_t __a)
{
  return (int8x16_t)__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_p8 (poly8x16_t __a)
{
  return (int16x8_t)__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_p16 (poly16x8_t __a)
{
  return (int16x8_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_f16 (float16x8_t __a)
{
  return (int16x8_t) __a;
}
#endif

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_f32 (float32x4_t __a)
{
  return (int16x8_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_p64 (poly64x2_t __a)
{
  return (int16x8_t)__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_p128 (poly128_t __a)
{
  return (int16x8_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_s64 (int64x2_t __a)
{
  return (int16x8_t)__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_u64 (uint64x2_t __a)
{
  return (int16x8_t)__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_s8 (int8x16_t __a)
{
  return (int16x8_t)__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_s32 (int32x4_t __a)
{
  return (int16x8_t)__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_u8 (uint8x16_t __a)
{
  return (int16x8_t)__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_u16 (uint16x8_t __a)
{
  return (int16x8_t)__a;
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vreinterpretq_s16_u32 (uint32x4_t __a)
{
  return (int16x8_t)__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_p8 (poly8x16_t __a)
{
  return (int32x4_t)__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_p16 (poly16x8_t __a)
{
  return (int32x4_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_f16 (float16x8_t __a)
{
  return (int32x4_t)__a;
}
#endif

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_f32 (float32x4_t __a)
{
  return (int32x4_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_p64 (poly64x2_t __a)
{
  return (int32x4_t)__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_p128 (poly128_t __a)
{
  return (int32x4_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_s64 (int64x2_t __a)
{
  return (int32x4_t)__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_u64 (uint64x2_t __a)
{
  return (int32x4_t)__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_s8 (int8x16_t __a)
{
  return (int32x4_t)__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_s16 (int16x8_t __a)
{
  return (int32x4_t)__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_u8 (uint8x16_t __a)
{
  return (int32x4_t)__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_u16 (uint16x8_t __a)
{
  return (int32x4_t)__a;
}

__extension__ static __inline int32x4_t __attribute__ ((__always_inline__))
vreinterpretq_s32_u32 (uint32x4_t __a)
{
  return (int32x4_t)__a;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_p8 (poly8x16_t __a)
{
  return (uint8x16_t)__a;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_p16 (poly16x8_t __a)
{
  return (uint8x16_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_f16 (float16x8_t __a)
{
  return (uint8x16_t) __a;
}
#endif

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_f32 (float32x4_t __a)
{
  return (uint8x16_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_p64 (poly64x2_t __a)
{
  return (uint8x16_t)__a;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_p128 (poly128_t __a)
{
  return (uint8x16_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_s64 (int64x2_t __a)
{
  return (uint8x16_t)__a;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_u64 (uint64x2_t __a)
{
  return (uint8x16_t)__a;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_s8 (int8x16_t __a)
{
  return (uint8x16_t)__a;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_s16 (int16x8_t __a)
{
  return (uint8x16_t)__a;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_s32 (int32x4_t __a)
{
  return (uint8x16_t)__a;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_u16 (uint16x8_t __a)
{
  return (uint8x16_t)__a;
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vreinterpretq_u8_u32 (uint32x4_t __a)
{
  return (uint8x16_t)__a;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_p8 (poly8x16_t __a)
{
  return (uint16x8_t)__a;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_p16 (poly16x8_t __a)
{
  return (uint16x8_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_f16 (float16x8_t __a)
{
  return (uint16x8_t) __a;
}
#endif

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_f32 (float32x4_t __a)
{
  return (uint16x8_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_p64 (poly64x2_t __a)
{
  return (uint16x8_t)__a;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_p128 (poly128_t __a)
{
  return (uint16x8_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_s64 (int64x2_t __a)
{
  return (uint16x8_t)__a;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_u64 (uint64x2_t __a)
{
  return (uint16x8_t)__a;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_s8 (int8x16_t __a)
{
  return (uint16x8_t)__a;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_s16 (int16x8_t __a)
{
  return (uint16x8_t)__a;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_s32 (int32x4_t __a)
{
  return (uint16x8_t)__a;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_u8 (uint8x16_t __a)
{
  return (uint16x8_t)__a;
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vreinterpretq_u16_u32 (uint32x4_t __a)
{
  return (uint16x8_t)__a;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_p8 (poly8x16_t __a)
{
  return (uint32x4_t)__a;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_p16 (poly16x8_t __a)
{
  return (uint32x4_t)__a;
}

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_f16 (float16x8_t __a)
{
  return (uint32x4_t) __a;
}
#endif

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_f32 (float32x4_t __a)
{
  return (uint32x4_t)__a;
}

#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_p64 (poly64x2_t __a)
{
  return (uint32x4_t)__a;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_p128 (poly128_t __a)
{
  return (uint32x4_t)__a;
}

#pragma GCC pop_options
__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_s64 (int64x2_t __a)
{
  return (uint32x4_t)__a;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_u64 (uint64x2_t __a)
{
  return (uint32x4_t)__a;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_s8 (int8x16_t __a)
{
  return (uint32x4_t)__a;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_s16 (int16x8_t __a)
{
  return (uint32x4_t)__a;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_s32 (int32x4_t __a)
{
  return (uint32x4_t)__a;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_u8 (uint8x16_t __a)
{
  return (uint32x4_t)__a;
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vreinterpretq_u32_u16 (uint16x8_t __a)
{
  return (uint32x4_t)__a;
}


#pragma GCC push_options
#pragma GCC target ("fpu=crypto-neon-fp-armv8")
__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vldrq_p128 (poly128_t const * __ptr)
{
#ifdef __ARM_BIG_ENDIAN
  poly64_t* __ptmp = (poly64_t*) __ptr;
  poly64_t __d0 = vld1_p64 (__ptmp);
  poly64_t __d1 = vld1_p64 (__ptmp + 1);
  return vreinterpretq_p128_p64 (vcombine_p64 (__d1, __d0));
#else
  return vreinterpretq_p128_p64 (vld1q_p64 ((poly64_t*) __ptr));
#endif
}

__extension__ static __inline void __attribute__ ((__always_inline__))
vstrq_p128 (poly128_t * __ptr, poly128_t __val)
{
#ifdef __ARM_BIG_ENDIAN
  poly64x2_t __tmp = vreinterpretq_p64_p128 (__val);
  poly64_t __d0 = vget_high_p64 (__tmp);
  poly64_t __d1 = vget_low_p64 (__tmp);
  vst1q_p64 ((poly64_t*) __ptr, vcombine_p64 (__d0, __d1));
#else
  vst1q_p64 ((poly64_t*) __ptr, vreinterpretq_p64_p128 (__val));
#endif
}

/* The vceq_p64 intrinsic does not map to a single instruction.
   Instead we emulate it by performing a 32-bit variant of the vceq
   and applying a pairwise min reduction to the result.
   vceq_u32 will produce two 32-bit halves, each of which will contain either
   all ones or all zeros depending on whether the corresponding 32-bit
   halves of the poly64_t were equal.  The whole poly64_t values are equal
   if and only if both halves are equal, i.e. vceq_u32 returns all ones.
   If the result is all zeroes for any half then the whole result is zeroes.
   This is what the pairwise min reduction achieves.  */

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vceq_p64 (poly64x1_t __a, poly64x1_t __b)
{
  uint32x2_t __t_a = vreinterpret_u32_p64 (__a);
  uint32x2_t __t_b = vreinterpret_u32_p64 (__b);
  uint32x2_t __c = vceq_u32 (__t_a, __t_b);
  uint32x2_t __m = vpmin_u32 (__c, __c);
  return vreinterpret_u64_u32 (__m);
}

/* The vtst_p64 intrinsic does not map to a single instruction.
   We emulate it in way similar to vceq_p64 above but here we do
   a reduction with max since if any two corresponding bits
   in the two poly64_t's match, then the whole result must be all ones.  */

__extension__ static __inline uint64x1_t __attribute__ ((__always_inline__))
vtst_p64 (poly64x1_t __a, poly64x1_t __b)
{
  uint32x2_t __t_a = vreinterpret_u32_p64 (__a);
  uint32x2_t __t_b = vreinterpret_u32_p64 (__b);
  uint32x2_t __c = vtst_u32 (__t_a, __t_b);
  uint32x2_t __m = vpmax_u32 (__c, __c);
  return vreinterpret_u64_u32 (__m);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaeseq_u8 (uint8x16_t __data, uint8x16_t __key)
{
  return __builtin_arm_crypto_aese (__data, __key);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaesdq_u8 (uint8x16_t __data, uint8x16_t __key)
{
  return __builtin_arm_crypto_aesd (__data, __key);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaesmcq_u8 (uint8x16_t __data)
{
  return __builtin_arm_crypto_aesmc (__data);
}

__extension__ static __inline uint8x16_t __attribute__ ((__always_inline__))
vaesimcq_u8 (uint8x16_t __data)
{
  return __builtin_arm_crypto_aesimc (__data);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vsha1h_u32 (uint32_t __hash_e)
{
  uint32x4_t __t = vdupq_n_u32 (0);
  __t = vsetq_lane_u32 (__hash_e, __t, 0);
  __t = __builtin_arm_crypto_sha1h (__t);
  return vgetq_lane_u32 (__t, 0);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1cq_u32 (uint32x4_t __hash_abcd, uint32_t __hash_e, uint32x4_t __wk)
{
  uint32x4_t __t = vdupq_n_u32 (0);
  __t = vsetq_lane_u32 (__hash_e, __t, 0);
  return __builtin_arm_crypto_sha1c (__hash_abcd, __t, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1pq_u32 (uint32x4_t __hash_abcd, uint32_t __hash_e, uint32x4_t __wk)
{
  uint32x4_t __t = vdupq_n_u32 (0);
  __t = vsetq_lane_u32 (__hash_e, __t, 0);
  return __builtin_arm_crypto_sha1p (__hash_abcd, __t, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1mq_u32 (uint32x4_t __hash_abcd, uint32_t __hash_e, uint32x4_t __wk)
{
  uint32x4_t __t = vdupq_n_u32 (0);
  __t = vsetq_lane_u32 (__hash_e, __t, 0);
  return __builtin_arm_crypto_sha1m (__hash_abcd, __t, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1su0q_u32 (uint32x4_t __w0_3, uint32x4_t __w4_7, uint32x4_t __w8_11)
{
  return __builtin_arm_crypto_sha1su0 (__w0_3, __w4_7, __w8_11);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha1su1q_u32 (uint32x4_t __tw0_3, uint32x4_t __w12_15)
{
  return __builtin_arm_crypto_sha1su1 (__tw0_3, __w12_15);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha256hq_u32 (uint32x4_t __hash_abcd, uint32x4_t __hash_efgh, uint32x4_t __wk)
{
  return __builtin_arm_crypto_sha256h (__hash_abcd, __hash_efgh, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha256h2q_u32 (uint32x4_t __hash_abcd, uint32x4_t __hash_efgh, uint32x4_t __wk)
{
  return __builtin_arm_crypto_sha256h2 (__hash_abcd, __hash_efgh, __wk);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha256su0q_u32 (uint32x4_t __w0_3, uint32x4_t __w4_7)
{
  return __builtin_arm_crypto_sha256su0 (__w0_3, __w4_7);
}

__extension__ static __inline uint32x4_t __attribute__ ((__always_inline__))
vsha256su1q_u32 (uint32x4_t __tw0_3, uint32x4_t __w8_11, uint32x4_t __w12_15)
{
  return __builtin_arm_crypto_sha256su1 (__tw0_3, __w8_11, __w12_15);
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vmull_p64 (poly64_t __a, poly64_t __b)
{
  return (poly128_t) __builtin_arm_crypto_vmullp64 ((uint64_t) __a, (uint64_t) __b);
}

__extension__ static __inline poly128_t __attribute__ ((__always_inline__))
vmull_high_p64 (poly64x2_t __a, poly64x2_t __b)
{
  poly64_t __t1 = vget_high_p64 (__a);
  poly64_t __t2 = vget_high_p64 (__b);

  return (poly128_t) __builtin_arm_crypto_vmullp64 ((uint64_t) __t1, (uint64_t) __t2);
}

#pragma GCC pop_options

  /* Intrinsics for FP16 instructions.  */
#pragma GCC push_options
#pragma GCC target ("fpu=neon-fp-armv8")
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vabd_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vabdv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vabdq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vabdv8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vabs_f16 (float16x4_t __a)
{
  return __builtin_neon_vabsv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vabsq_f16 (float16x8_t __a)
{
  return __builtin_neon_vabsv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vadd_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vaddv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vaddq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vaddv8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcage_f16 (float16x4_t __a, float16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcagev4hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcageq_f16 (float16x8_t __a, float16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcagev8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcagt_f16 (float16x4_t __a, float16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcagtv4hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcagtq_f16 (float16x8_t __a, float16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcagtv8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcale_f16 (float16x4_t __a, float16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcalev4hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcaleq_f16 (float16x8_t __a, float16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcalev8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcalt_f16 (float16x4_t __a, float16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcaltv4hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcaltq_f16 (float16x8_t __a, float16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcaltv8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vceq_f16 (float16x4_t __a, float16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vceqv4hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vceqq_f16 (float16x8_t __a, float16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vceqv8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vceqz_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vceqzv4hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vceqzq_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vceqzv8hf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcge_f16 (float16x4_t __a, float16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgev4hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgeq_f16 (float16x8_t __a, float16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgev8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgez_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vcgezv4hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgezq_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vcgezv8hf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgt_f16 (float16x4_t __a, float16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcgtv4hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgtq_f16 (float16x8_t __a, float16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcgtv8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcgtz_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vcgtzv4hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcgtzq_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vcgtzv8hf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcle_f16 (float16x4_t __a, float16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vclev4hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcleq_f16 (float16x8_t __a, float16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vclev8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vclez_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vclezv4hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vclezq_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vclezv8hf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vclt_f16 (float16x4_t __a, float16x4_t __b)
{
  return (uint16x4_t)__builtin_neon_vcltv4hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcltq_f16 (float16x8_t __a, float16x8_t __b)
{
  return (uint16x8_t)__builtin_neon_vcltv8hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcltz_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vcltzv4hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcltzq_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vcltzv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vcvt_f16_s16 (int16x4_t __a)
{
  return (float16x4_t)__builtin_neon_vcvtsv4hi (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vcvt_f16_u16 (uint16x4_t __a)
{
  return (float16x4_t)__builtin_neon_vcvtuv4hi ((int16x4_t)__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcvt_s16_f16 (float16x4_t __a)
{
  return (int16x4_t)__builtin_neon_vcvtsv4hf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcvt_u16_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vcvtuv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vcvtq_f16_s16 (int16x8_t __a)
{
  return (float16x8_t)__builtin_neon_vcvtsv8hi (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vcvtq_f16_u16 (uint16x8_t __a)
{
  return (float16x8_t)__builtin_neon_vcvtuv8hi ((int16x8_t)__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vcvtq_s16_f16 (float16x8_t __a)
{
  return (int16x8_t)__builtin_neon_vcvtsv8hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcvtq_u16_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vcvtuv8hf (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcvta_s16_f16 (float16x4_t __a)
{
  return __builtin_neon_vcvtasv4hf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcvta_u16_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vcvtauv4hf (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vcvtaq_s16_f16 (float16x8_t __a)
{
  return __builtin_neon_vcvtasv8hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcvtaq_u16_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vcvtauv8hf (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcvtm_s16_f16 (float16x4_t __a)
{
  return __builtin_neon_vcvtmsv4hf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcvtm_u16_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vcvtmuv4hf (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vcvtmq_s16_f16 (float16x8_t __a)
{
  return __builtin_neon_vcvtmsv8hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcvtmq_u16_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vcvtmuv8hf (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcvtn_s16_f16 (float16x4_t __a)
{
  return __builtin_neon_vcvtnsv4hf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcvtn_u16_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vcvtnuv4hf (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vcvtnq_s16_f16 (float16x8_t __a)
{
  return __builtin_neon_vcvtnsv8hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcvtnq_u16_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vcvtnuv8hf (__a);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcvtp_s16_f16 (float16x4_t __a)
{
  return __builtin_neon_vcvtpsv4hf (__a);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcvtp_u16_f16 (float16x4_t __a)
{
  return (uint16x4_t)__builtin_neon_vcvtpuv4hf (__a);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vcvtpq_s16_f16 (float16x8_t __a)
{
  return __builtin_neon_vcvtpsv8hf (__a);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcvtpq_u16_f16 (float16x8_t __a)
{
  return (uint16x8_t)__builtin_neon_vcvtpuv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vcvt_n_f16_s16 (int16x4_t __a, const int __b)
{
  return __builtin_neon_vcvts_nv4hi (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vcvt_n_f16_u16 (uint16x4_t __a, const int __b)
{
  return __builtin_neon_vcvtu_nv4hi ((int16x4_t)__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vcvtq_n_f16_s16 (int16x8_t __a, const int __b)
{
  return __builtin_neon_vcvts_nv8hi (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vcvtq_n_f16_u16 (uint16x8_t __a, const int __b)
{
  return __builtin_neon_vcvtu_nv8hi ((int16x8_t)__a, __b);
}

__extension__ static __inline int16x4_t __attribute__ ((__always_inline__))
vcvt_n_s16_f16 (float16x4_t __a, const int __b)
{
  return __builtin_neon_vcvts_nv4hf (__a, __b);
}

__extension__ static __inline uint16x4_t __attribute__ ((__always_inline__))
vcvt_n_u16_f16 (float16x4_t __a, const int __b)
{
  return (uint16x4_t)__builtin_neon_vcvtu_nv4hf (__a, __b);
}

__extension__ static __inline int16x8_t __attribute__ ((__always_inline__))
vcvtq_n_s16_f16 (float16x8_t __a, const int __b)
{
  return __builtin_neon_vcvts_nv8hf (__a, __b);
}

__extension__ static __inline uint16x8_t __attribute__ ((__always_inline__))
vcvtq_n_u16_f16 (float16x8_t __a, const int __b)
{
  return (uint16x8_t)__builtin_neon_vcvtu_nv8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vfma_f16 (float16x4_t __a, float16x4_t __b, float16x4_t __c)
{
  return __builtin_neon_vfmav4hf (__a, __b, __c);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vfmaq_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
  return __builtin_neon_vfmav8hf (__a, __b, __c);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vfms_f16 (float16x4_t __a, float16x4_t __b, float16x4_t __c)
{
  return __builtin_neon_vfmsv4hf (__a, __b, __c);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vfmsq_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
  return __builtin_neon_vfmsv8hf (__a, __b, __c);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vmax_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vmaxfv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vmaxq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vmaxfv8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vmaxnm_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vmaxnmv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vmaxnmq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vmaxnmv8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vmin_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vminfv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vminq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vminfv8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vminnm_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vminnmv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vminnmq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vminnmv8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vmul_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vmulfv4hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vmul_lane_f16 (float16x4_t __a, float16x4_t __b, const int __c)
{
  return __builtin_neon_vmul_lanev4hf (__a, __b, __c);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vmul_n_f16 (float16x4_t __a, float16_t __b)
{
  return __builtin_neon_vmul_nv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vmulq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vmulfv8hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vmulq_lane_f16 (float16x8_t __a, float16x4_t __b, const int __c)
{
  return __builtin_neon_vmul_lanev8hf (__a, __b, __c);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vmulq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_neon_vmul_nv8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vneg_f16 (float16x4_t __a)
{
  return __builtin_neon_vnegv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vnegq_f16 (float16x8_t __a)
{
  return __builtin_neon_vnegv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vpadd_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vpaddv4hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vpmax_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vpmaxfv4hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vpmin_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vpminfv4hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrecpe_f16 (float16x4_t __a)
{
  return __builtin_neon_vrecpev4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrecpeq_f16 (float16x8_t __a)
{
  return __builtin_neon_vrecpev8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrnd_f16 (float16x4_t __a)
{
  return __builtin_neon_vrndv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrndq_f16 (float16x8_t __a)
{
  return __builtin_neon_vrndv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrnda_f16 (float16x4_t __a)
{
  return __builtin_neon_vrndav4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrndaq_f16 (float16x8_t __a)
{
  return __builtin_neon_vrndav8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrndm_f16 (float16x4_t __a)
{
  return __builtin_neon_vrndmv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrndmq_f16 (float16x8_t __a)
{
  return __builtin_neon_vrndmv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrndn_f16 (float16x4_t __a)
{
  return __builtin_neon_vrndnv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrndnq_f16 (float16x8_t __a)
{
  return __builtin_neon_vrndnv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrndp_f16 (float16x4_t __a)
{
  return __builtin_neon_vrndpv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrndpq_f16 (float16x8_t __a)
{
  return __builtin_neon_vrndpv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrndx_f16 (float16x4_t __a)
{
  return __builtin_neon_vrndxv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrndxq_f16 (float16x8_t __a)
{
  return __builtin_neon_vrndxv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrsqrte_f16 (float16x4_t __a)
{
  return __builtin_neon_vrsqrtev4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrsqrteq_f16 (float16x8_t __a)
{
  return __builtin_neon_vrsqrtev8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrecps_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vrecpsv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrecpsq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vrecpsv8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrsqrts_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vrsqrtsv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrsqrtsq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vrsqrtsv8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vsub_f16 (float16x4_t __a, float16x4_t __b)
{
  return __builtin_neon_vsubv4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vsubq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_neon_vsubv8hf (__a, __b);
}

#endif /* __ARM_FEATURE_VECTOR_FP16_ARITHMETIC.  */
#pragma GCC pop_options

  /* Half-precision data processing intrinsics.  */
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vbsl_f16 (uint16x4_t __a, float16x4_t __b, float16x4_t __c)
{
  return __builtin_neon_vbslv4hf ((int16x4_t)__a, __b, __c);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vbslq_f16 (uint16x8_t __a, float16x8_t __b, float16x8_t __c)
{
  return __builtin_neon_vbslv8hf ((int16x8_t)__a, __b, __c);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vdup_n_f16 (float16_t __a)
{
  return __builtin_neon_vdup_nv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vdupq_n_f16 (float16_t __a)
{
  return __builtin_neon_vdup_nv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vdup_lane_f16 (float16x4_t __a, const int __b)
{
  return __builtin_neon_vdup_lanev4hf (__a, __b);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vdupq_lane_f16 (float16x4_t __a, const int __b)
{
  return __builtin_neon_vdup_lanev8hf (__a, __b);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vext_f16 (float16x4_t __a, float16x4_t __b, const int __c)
{
  return __builtin_neon_vextv4hf (__a, __b, __c);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vextq_f16 (float16x8_t __a, float16x8_t __b, const int __c)
{
  return __builtin_neon_vextv8hf (__a, __b, __c);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vmov_n_f16 (float16_t __a)
{
  return __builtin_neon_vdup_nv4hf (__a);
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vmovq_n_f16 (float16_t __a)
{
  return __builtin_neon_vdup_nv8hf (__a);
}

__extension__ static __inline float16x4_t __attribute__ ((__always_inline__))
vrev64_f16 (float16x4_t __a)
{
  return (float16x4_t)__builtin_shuffle (__a, (uint16x4_t){ 3, 2, 1, 0 });
}

__extension__ static __inline float16x8_t __attribute__ ((__always_inline__))
vrev64q_f16 (float16x8_t __a)
{
  return
    (float16x8_t)__builtin_shuffle (__a,
				    (uint16x8_t){ 3, 2, 1, 0, 7, 6, 5, 4 });
}

__extension__ static __inline float16x4x2_t __attribute__ ((__always_inline__))
vtrn_f16 (float16x4_t __a, float16x4_t __b)
{
  float16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t){ 5, 1, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t){ 4, 0, 6, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t){ 0, 4, 2, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t){ 1, 5, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline float16x8x2_t __attribute__ ((__always_inline__))
vtrnq_f16 (float16x8_t __a, float16x8_t __b)
{
  float16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b,
				   (uint16x8_t){ 9, 1, 11, 3, 13, 5, 15, 7 });
  __rv.val[1] = __builtin_shuffle (__a, __b,
				   (uint16x8_t){ 8, 0, 10, 2, 12, 4, 14, 6 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b,
				   (uint16x8_t){ 0, 8, 2, 10, 4, 12, 6, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b,
				   (uint16x8_t){ 1, 9, 3, 11, 5, 13, 7, 15 });
#endif
  return __rv;
}

__extension__ static __inline float16x4x2_t __attribute__ ((__always_inline__))
vuzp_f16 (float16x4_t __a, float16x4_t __b)
{
  float16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t){ 5, 7, 1, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t){ 4, 6, 0, 2 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t){ 0, 2, 4, 6 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t){ 1, 3, 5, 7 });
#endif
  return __rv;
}

__extension__ static __inline float16x8x2_t __attribute__ ((__always_inline__))
vuzpq_f16 (float16x8_t __a, float16x8_t __b)
{
  float16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
				   { 5, 7, 1, 3, 13, 15, 9, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
				   { 4, 6, 0, 2, 12, 14, 8, 10 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b,
				   (uint16x8_t){ 0, 2, 4, 6, 8, 10, 12, 14 });
  __rv.val[1] = __builtin_shuffle (__a, __b,
				   (uint16x8_t){ 1, 3, 5, 7, 9, 11, 13, 15 });
#endif
  return __rv;
}

__extension__ static __inline float16x4x2_t __attribute__ ((__always_inline__))
vzip_f16 (float16x4_t __a, float16x4_t __b)
{
  float16x4x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t){ 6, 2, 7, 3 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t){ 4, 0, 5, 1 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x4_t){ 0, 4, 1, 5 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x4_t){ 2, 6, 3, 7 });
#endif
  return __rv;
}

__extension__ static __inline float16x8x2_t __attribute__ ((__always_inline__))
vzipq_f16 (float16x8_t __a, float16x8_t __b)
{
  float16x8x2_t __rv;
#ifdef __ARM_BIG_ENDIAN
  __rv.val[0] = __builtin_shuffle (__a, __b, (uint16x8_t)
				   { 10, 2, 11, 3, 8, 0, 9, 1 });
  __rv.val[1] = __builtin_shuffle (__a, __b, (uint16x8_t)
				   { 14, 6, 15, 7, 12, 4, 13, 5 });
#else
  __rv.val[0] = __builtin_shuffle (__a, __b,
				   (uint16x8_t){ 0, 8, 1, 9, 2, 10, 3, 11 });
  __rv.val[1] = __builtin_shuffle (__a, __b,
				   (uint16x8_t){ 4, 12, 5, 13, 6, 14, 7, 15 });
#endif
  return __rv;
}

#endif

#ifdef __cplusplus
}
#endif

#pragma GCC pop_options

#endif
#endif
