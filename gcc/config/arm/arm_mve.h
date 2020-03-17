/* Arm MVE intrinsics include file.

   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#ifndef _GCC_ARM_MVE_H
#define _GCC_ARM_MVE_H

#if !__ARM_FEATURE_MVE
#error "MVE feature not supported"
#endif

#include <stdint.h>
#ifndef  __cplusplus
#include <stdbool.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */
typedef __fp16 float16_t;
typedef float float32_t;
typedef __simd128_float16_t float16x8_t;
typedef __simd128_float32_t float32x4_t;
#endif

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */
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

#ifndef __ARM_MVE_PRESERVE_USER_NAMESPACE
#define vst4q_s8( __addr, __value) __arm_vst4q_s8( __addr, __value)
#define vst4q_s16( __addr, __value) __arm_vst4q_s16( __addr, __value)
#define vst4q_s32( __addr, __value) __arm_vst4q_s32( __addr, __value)
#define vst4q_u8( __addr, __value) __arm_vst4q_u8( __addr, __value)
#define vst4q_u16( __addr, __value) __arm_vst4q_u16( __addr, __value)
#define vst4q_u32( __addr, __value) __arm_vst4q_u32( __addr, __value)
#define vst4q_f16( __addr, __value) __arm_vst4q_f16( __addr, __value)
#define vst4q_f32( __addr, __value) __arm_vst4q_f32( __addr, __value)
#define vrndxq_f16(__a) __arm_vrndxq_f16(__a)
#define vrndxq_f32(__a) __arm_vrndxq_f32(__a)
#define vrndq_f16(__a) __arm_vrndq_f16(__a)
#define vrndq_f32(__a) __arm_vrndq_f32(__a)
#define vrndpq_f16(__a) __arm_vrndpq_f16(__a)
#define vrndpq_f32(__a) __arm_vrndpq_f32(__a)
#define vrndnq_f16(__a) __arm_vrndnq_f16(__a)
#define vrndnq_f32(__a) __arm_vrndnq_f32(__a)
#define vrndmq_f16(__a) __arm_vrndmq_f16(__a)
#define vrndmq_f32(__a) __arm_vrndmq_f32(__a)
#define vrndaq_f16(__a) __arm_vrndaq_f16(__a)
#define vrndaq_f32(__a) __arm_vrndaq_f32(__a)
#define vrev64q_f16(__a) __arm_vrev64q_f16(__a)
#define vrev64q_f32(__a) __arm_vrev64q_f32(__a)
#define vnegq_f16(__a) __arm_vnegq_f16(__a)
#define vnegq_f32(__a) __arm_vnegq_f32(__a)
#define vdupq_n_f16(__a) __arm_vdupq_n_f16(__a)
#define vdupq_n_f32(__a) __arm_vdupq_n_f32(__a)
#define vabsq_f16(__a) __arm_vabsq_f16(__a)
#define vabsq_f32(__a) __arm_vabsq_f32(__a)
#define vrev32q_f16(__a) __arm_vrev32q_f16(__a)
#define vcvttq_f32_f16(__a) __arm_vcvttq_f32_f16(__a)
#define vcvtbq_f32_f16(__a) __arm_vcvtbq_f32_f16(__a)
#define vcvtq_f16_s16(__a) __arm_vcvtq_f16_s16(__a)
#define vcvtq_f32_s32(__a) __arm_vcvtq_f32_s32(__a)
#define vcvtq_f16_u16(__a) __arm_vcvtq_f16_u16(__a)
#define vcvtq_f32_u32(__a) __arm_vcvtq_f32_u32(__a)
#endif

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q_s8 (int8_t * __addr, int8x16x4_t __value)
{
  union { int8x16x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst4qv16qi ((__builtin_neon_qi *) __addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q_s16 (int16_t * __addr, int16x8x4_t __value)
{
  union { int16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst4qv8hi ((__builtin_neon_hi *) __addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q_s32 (int32_t * __addr, int32x4x4_t __value)
{
  union { int32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst4qv4si ((__builtin_neon_si *) __addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q_u8 (uint8_t * __addr, uint8x16x4_t __value)
{
  union { uint8x16x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst4qv16qi ((__builtin_neon_qi *) __addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q_u16 (uint16_t * __addr, uint16x8x4_t __value)
{
  union { uint16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst4qv8hi ((__builtin_neon_hi *) __addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q_u32 (uint32_t * __addr, uint32x4x4_t __value)
{
  union { uint32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst4qv4si ((__builtin_neon_si *) __addr, __rv.__o);
}

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q_f16 (float16_t * __addr, float16x8x4_t __value)
{
  union { float16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst4qv8hf (__addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q_f32 (float32_t * __addr, float32x4x4_t __value)
{
  union { float32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst4qv4sf (__addr, __rv.__o);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndxq_f16 (float16x8_t __a)
{
  return __builtin_mve_vrndxq_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndxq_f32 (float32x4_t __a)
{
  return __builtin_mve_vrndxq_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndq_f16 (float16x8_t __a)
{
  return __builtin_mve_vrndq_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndq_f32 (float32x4_t __a)
{
  return __builtin_mve_vrndq_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndpq_f16 (float16x8_t __a)
{
  return __builtin_mve_vrndpq_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndpq_f32 (float32x4_t __a)
{
  return __builtin_mve_vrndpq_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndnq_f16 (float16x8_t __a)
{
  return __builtin_mve_vrndnq_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndnq_f32 (float32x4_t __a)
{
  return __builtin_mve_vrndnq_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndmq_f16 (float16x8_t __a)
{
  return __builtin_mve_vrndmq_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndmq_f32 (float32x4_t __a)
{
  return __builtin_mve_vrndmq_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndaq_f16 (float16x8_t __a)
{
  return __builtin_mve_vrndaq_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrndaq_f32 (float32x4_t __a)
{
  return __builtin_mve_vrndaq_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev64q_f16 (float16x8_t __a)
{
  return __builtin_mve_vrev64q_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev64q_f32 (float32x4_t __a)
{
  return __builtin_mve_vrev64q_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vnegq_f16 (float16x8_t __a)
{
  return __builtin_mve_vnegq_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vnegq_f32 (float32x4_t __a)
{
  return __builtin_mve_vnegq_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdupq_n_f16 (float16_t __a)
{
  return __builtin_mve_vdupq_n_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdupq_n_f32 (float32_t __a)
{
  return __builtin_mve_vdupq_n_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabsq_f16 (float16x8_t __a)
{
  return __builtin_mve_vabsq_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabsq_f32 (float32x4_t __a)
{
  return __builtin_mve_vabsq_fv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev32q_f16 (float16x8_t __a)
{
  return __builtin_mve_vrev32q_fv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvttq_f32_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvttq_f32_f16v4sf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtbq_f32_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtbq_f32_f16v4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_f16_s16 (int16x8_t __a)
{
  return __builtin_mve_vcvtq_to_f_sv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_f32_s32 (int32x4_t __a)
{
  return __builtin_mve_vcvtq_to_f_sv4sf (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_f16_u16 (uint16x8_t __a)
{
  return __builtin_mve_vcvtq_to_f_uv8hf (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_f32_u32 (uint32x4_t __a)
{
  return __builtin_mve_vcvtq_to_f_uv4sf (__a);
}

#endif

enum {
    __ARM_mve_type_float16_t = 1,
    __ARM_mve_type_float16_t_ptr,
    __ARM_mve_type_float16_t_const_ptr,
    __ARM_mve_type_float16x8_t,
    __ARM_mve_type_float16x8x2_t,
    __ARM_mve_type_float16x8x4_t,
    __ARM_mve_type_float32_t,
    __ARM_mve_type_float32_t_ptr,
    __ARM_mve_type_float32_t_const_ptr,
    __ARM_mve_type_float32x4_t,
    __ARM_mve_type_float32x4x2_t,
    __ARM_mve_type_float32x4x4_t,
    __ARM_mve_type_int16_t,
    __ARM_mve_type_int16_t_ptr,
    __ARM_mve_type_int16_t_const_ptr,
    __ARM_mve_type_int16x8_t,
    __ARM_mve_type_int16x8x2_t,
    __ARM_mve_type_int16x8x4_t,
    __ARM_mve_type_int32_t,
    __ARM_mve_type_int32_t_ptr,
    __ARM_mve_type_int32_t_const_ptr,
    __ARM_mve_type_int32x4_t,
    __ARM_mve_type_int32x4x2_t,
    __ARM_mve_type_int32x4x4_t,
    __ARM_mve_type_int64_t,
    __ARM_mve_type_int64_t_ptr,
    __ARM_mve_type_int64_t_const_ptr,
    __ARM_mve_type_int64x2_t,
    __ARM_mve_type_int8_t,
    __ARM_mve_type_int8_t_ptr,
    __ARM_mve_type_int8_t_const_ptr,
    __ARM_mve_type_int8x16_t,
    __ARM_mve_type_int8x16x2_t,
    __ARM_mve_type_int8x16x4_t,
    __ARM_mve_type_uint16_t,
    __ARM_mve_type_uint16_t_ptr,
    __ARM_mve_type_uint16_t_const_ptr,
    __ARM_mve_type_uint16x8_t,
    __ARM_mve_type_uint16x8x2_t,
    __ARM_mve_type_uint16x8x4_t,
    __ARM_mve_type_uint32_t,
    __ARM_mve_type_uint32_t_ptr,
    __ARM_mve_type_uint32_t_const_ptr,
    __ARM_mve_type_uint32x4_t,
    __ARM_mve_type_uint32x4x2_t,
    __ARM_mve_type_uint32x4x4_t,
    __ARM_mve_type_uint64_t,
    __ARM_mve_type_uint64_t_ptr,
    __ARM_mve_type_uint64_t_const_ptr,
    __ARM_mve_type_uint64x2_t,
    __ARM_mve_type_uint8_t,
    __ARM_mve_type_uint8_t_ptr,
    __ARM_mve_type_uint8_t_const_ptr,
    __ARM_mve_type_uint8x16_t,
    __ARM_mve_type_uint8x16x2_t,
    __ARM_mve_type_uint8x16x4_t,
    __ARM_mve_unsupported_type
};

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */
#define __ARM_mve_typeid(x) _Generic(x, \
    float16_t: __ARM_mve_type_float16_t, \
    float16_t *: __ARM_mve_type_float16_t_ptr, \
    float16_t const *: __ARM_mve_type_float16_t_const_ptr, \
    float16x8_t: __ARM_mve_type_float16x8_t, \
    float16x8x2_t: __ARM_mve_type_float16x8x2_t, \
    float16x8x4_t: __ARM_mve_type_float16x8x4_t, \
    float32_t: __ARM_mve_type_float32_t, \
    float32_t *: __ARM_mve_type_float32_t_ptr, \
    float32_t const *: __ARM_mve_type_float32_t_const_ptr, \
    float32x4_t: __ARM_mve_type_float32x4_t, \
    float32x4x2_t: __ARM_mve_type_float32x4x2_t, \
    float32x4x4_t: __ARM_mve_type_float32x4x4_t, \
    int16_t: __ARM_mve_type_int16_t, \
    int16_t *: __ARM_mve_type_int16_t_ptr, \
    int16_t const *: __ARM_mve_type_int16_t_const_ptr, \
    int16x8_t: __ARM_mve_type_int16x8_t, \
    int16x8x2_t: __ARM_mve_type_int16x8x2_t, \
    int16x8x4_t: __ARM_mve_type_int16x8x4_t, \
    int32_t: __ARM_mve_type_int32_t, \
    int32_t *: __ARM_mve_type_int32_t_ptr, \
    int32_t const *: __ARM_mve_type_int32_t_const_ptr, \
    int32x4_t: __ARM_mve_type_int32x4_t, \
    int32x4x2_t: __ARM_mve_type_int32x4x2_t, \
    int32x4x4_t: __ARM_mve_type_int32x4x4_t, \
    int64_t: __ARM_mve_type_int64_t, \
    int64_t *: __ARM_mve_type_int64_t_ptr, \
    int64_t const *: __ARM_mve_type_int64_t_const_ptr, \
    int64x2_t: __ARM_mve_type_int64x2_t, \
    int8_t: __ARM_mve_type_int8_t, \
    int8_t *: __ARM_mve_type_int8_t_ptr, \
    int8_t const *: __ARM_mve_type_int8_t_const_ptr, \
    int8x16_t: __ARM_mve_type_int8x16_t, \
    int8x16x2_t: __ARM_mve_type_int8x16x2_t, \
    int8x16x4_t: __ARM_mve_type_int8x16x4_t, \
    uint16_t: __ARM_mve_type_uint16_t, \
    uint16_t *: __ARM_mve_type_uint16_t_ptr, \
    uint16_t const *: __ARM_mve_type_uint16_t_const_ptr, \
    uint16x8_t: __ARM_mve_type_uint16x8_t, \
    uint16x8x2_t: __ARM_mve_type_uint16x8x2_t, \
    uint16x8x4_t: __ARM_mve_type_uint16x8x4_t, \
    uint32_t: __ARM_mve_type_uint32_t, \
    uint32_t *: __ARM_mve_type_uint32_t_ptr, \
    uint32_t const *: __ARM_mve_type_uint32_t_const_ptr, \
    uint32x4_t: __ARM_mve_type_uint32x4_t, \
    uint32x4x2_t: __ARM_mve_type_uint32x4x2_t, \
    uint32x4x4_t: __ARM_mve_type_uint32x4x4_t, \
    uint64_t: __ARM_mve_type_uint64_t, \
    uint64_t *: __ARM_mve_type_uint64_t_ptr, \
    uint64_t const *: __ARM_mve_type_uint64_t_const_ptr, \
    uint64x2_t: __ARM_mve_type_uint64x2_t, \
    uint8_t: __ARM_mve_type_uint8_t, \
    uint8_t *: __ARM_mve_type_uint8_t_ptr, \
    uint8_t const *: __ARM_mve_type_uint8_t_const_ptr, \
    uint8x16_t: __ARM_mve_type_uint8x16_t, \
    uint8x16x2_t: __ARM_mve_type_uint8x16x2_t, \
    uint8x16x4_t: __ARM_mve_type_uint8x16x4_t, \
    default: _Generic(x, \
	signed char: __ARM_mve_type_int8_t, \
	short: __ARM_mve_type_int16_t, \
	int: __ARM_mve_type_int32_t, \
	long: __ARM_mve_type_int32_t, \
	long long: __ARM_mve_type_int64_t, \
	unsigned char: __ARM_mve_type_uint8_t, \
	unsigned short: __ARM_mve_type_uint16_t, \
	unsigned int: __ARM_mve_type_uint32_t, \
	unsigned long: __ARM_mve_type_uint32_t, \
	unsigned long long: __ARM_mve_type_uint64_t, \
	default: __ARM_mve_unsupported_type))
#else
#define __ARM_mve_typeid(x) _Generic(x, \
    int16_t: __ARM_mve_type_int16_t, \
    int16_t *: __ARM_mve_type_int16_t_ptr, \
    int16_t const *: __ARM_mve_type_int16_t_const_ptr, \
    int16x8_t: __ARM_mve_type_int16x8_t, \
    int16x8x2_t: __ARM_mve_type_int16x8x2_t, \
    int16x8x4_t: __ARM_mve_type_int16x8x4_t, \
    int32_t: __ARM_mve_type_int32_t, \
    int32_t *: __ARM_mve_type_int32_t_ptr, \
    int32_t const *: __ARM_mve_type_int32_t_const_ptr, \
    int32x4_t: __ARM_mve_type_int32x4_t, \
    int32x4x2_t: __ARM_mve_type_int32x4x2_t, \
    int32x4x4_t: __ARM_mve_type_int32x4x4_t, \
    int64_t: __ARM_mve_type_int64_t, \
    int64_t *: __ARM_mve_type_int64_t_ptr, \
    int64_t const *: __ARM_mve_type_int64_t_const_ptr, \
    int64x2_t: __ARM_mve_type_int64x2_t, \
    int8_t: __ARM_mve_type_int8_t, \
    int8_t *: __ARM_mve_type_int8_t_ptr, \
    int8_t const *: __ARM_mve_type_int8_t_const_ptr, \
    int8x16_t: __ARM_mve_type_int8x16_t, \
    int8x16x2_t: __ARM_mve_type_int8x16x2_t, \
    int8x16x4_t: __ARM_mve_type_int8x16x4_t, \
    uint16_t: __ARM_mve_type_uint16_t, \
    uint16_t *: __ARM_mve_type_uint16_t_ptr, \
    uint16_t const *: __ARM_mve_type_uint16_t_const_ptr, \
    uint16x8_t: __ARM_mve_type_uint16x8_t, \
    uint16x8x2_t: __ARM_mve_type_uint16x8x2_t, \
    uint16x8x4_t: __ARM_mve_type_uint16x8x4_t, \
    uint32_t: __ARM_mve_type_uint32_t, \
    uint32_t *: __ARM_mve_type_uint32_t_ptr, \
    uint32_t const *: __ARM_mve_type_uint32_t_const_ptr, \
    uint32x4_t: __ARM_mve_type_uint32x4_t, \
    uint32x4x2_t: __ARM_mve_type_uint32x4x2_t, \
    uint32x4x4_t: __ARM_mve_type_uint32x4x4_t, \
    uint64_t: __ARM_mve_type_uint64_t, \
    uint64_t *: __ARM_mve_type_uint64_t_ptr, \
    uint64_t const *: __ARM_mve_type_uint64_t_const_ptr, \
    uint64x2_t: __ARM_mve_type_uint64x2_t, \
    uint8_t: __ARM_mve_type_uint8_t, \
    uint8_t *: __ARM_mve_type_uint8_t_ptr, \
    uint8_t const *: __ARM_mve_type_uint8_t_const_ptr, \
    uint8x16_t: __ARM_mve_type_uint8x16_t, \
    uint8x16x2_t: __ARM_mve_type_uint8x16x2_t, \
    uint8x16x4_t: __ARM_mve_type_uint8x16x4_t, \
    default: _Generic(x, \
	signed char: __ARM_mve_type_int8_t, \
	short: __ARM_mve_type_int16_t, \
	int: __ARM_mve_type_int32_t, \
	long: __ARM_mve_type_int32_t, \
	long long: __ARM_mve_type_int64_t, \
	unsigned char: __ARM_mve_type_uint8_t, \
	unsigned short: __ARM_mve_type_uint16_t, \
	unsigned int: __ARM_mve_type_uint32_t, \
	unsigned long: __ARM_mve_type_uint32_t, \
	unsigned long long: __ARM_mve_type_uint64_t, \
	default: __ARM_mve_unsupported_type))
#endif /* MVE Floating point.  */

extern void *__ARM_undef;
#define __ARM_mve_coerce(param, type) \
    _Generic(param, type: param, default: *(type *)__ARM_undef)

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */

#define vst4q(p0,p1) __arm_vst4q(p0,p1)
#define __arm_vst4q(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x4_t]: __arm_vst4q_s8 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x4_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x4_t]: __arm_vst4q_s16 (__ARM_mve_coerce(__p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x4_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x4_t]: __arm_vst4q_s32 (__ARM_mve_coerce(__p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x4_t]: __arm_vst4q_u8 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x4_t]: __arm_vst4q_u16 (__ARM_mve_coerce(__p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x4_t]: __arm_vst4q_u32 (__ARM_mve_coerce(__p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8x4_t]: __arm_vst4q_f16 (__ARM_mve_coerce(__p0, float16_t *), __ARM_mve_coerce(__p1, float16x8x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4x4_t]: __arm_vst4q_f32 (__ARM_mve_coerce(__p0, float32_t *), __ARM_mve_coerce(__p1, float32x4x4_t)));})

#define vrndxq(p0) __arm_vrndxq(p0)
#define __arm_vrndxq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrndxq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vrndxq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vrndq(p0) __arm_vrndq(p0)
#define __arm_vrndq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrndq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vrndq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vrndpq(p0) __arm_vrndpq(p0)
#define __arm_vrndpq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrndpq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vrndpq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vrndnq(p0) __arm_vrndnq(p0)
#define __arm_vrndnq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrndnq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vrndnq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vrndmq(p0) __arm_vrndmq(p0)
#define __arm_vrndmq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrndmq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vrndmq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vrndaq(p0) __arm_vrndaq(p0)
#define __arm_vrndaq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrndaq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vrndaq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vrev64q(p0) __arm_vrev64q(p0)
#define __arm_vrev64q(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrev64q_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vrev64q_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vnegq(p0) __arm_vnegq(p0)
#define __arm_vnegq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vnegq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vnegq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vabsq(p0) __arm_vabsq(p0)
#define __arm_vabsq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vabsq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vabsq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vrev32q(p0) __arm_vrev32q(p0)
#define __arm_vrev32q(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrev32q_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define vcvtbq_f32(p0) __arm_vcvtbq_f32(p0)
#define __arm_vcvtbq_f32(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vcvtbq_f32_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define vcvttq_f32(p0) __arm_vcvttq_f32(p0)
#define __arm_vcvttq_f32(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vcvttq_f32_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define vcvtq(p0) __arm_vcvtq(p0)
#define __arm_vcvtq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vcvtq_f16_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vcvtq_f32_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vcvtq_f16_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vcvtq_f32_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#else /* MVE Interger.  */

#define vst4q(p0,p1) __arm_vst4q(p0,p1)
#define __arm_vst4q(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x4_t]: __arm_vst4q_s8 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x4_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x4_t]: __arm_vst4q_s16 (__ARM_mve_coerce(__p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x4_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x4_t]: __arm_vst4q_s32 (__ARM_mve_coerce(__p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x4_t]: __arm_vst4q_u8 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x4_t]: __arm_vst4q_u16 (__ARM_mve_coerce(__p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x4_t]: __arm_vst4q_u32 (__ARM_mve_coerce(__p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x4_t)));})

#endif /* MVE Floating point.  */

#ifdef __cplusplus
}
#endif

#endif /* _GCC_ARM_MVE_H.  */
