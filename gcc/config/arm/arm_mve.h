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

typedef uint16_t mve_pred16_t;
typedef __simd128_uint8_t uint8x16_t;
typedef __simd128_uint16_t uint16x8_t;
typedef __simd128_uint32_t uint32x4_t;
typedef __simd128_uint64_t uint64x2_t;
typedef __simd128_int8_t int8x16_t;
typedef __simd128_int16_t int16x8_t;
typedef __simd128_int32_t int32x4_t;
typedef __simd128_int64_t int64x2_t;

#ifdef __cplusplus
}
#endif

#endif /* _GCC_ARM_MVE_H.  */
