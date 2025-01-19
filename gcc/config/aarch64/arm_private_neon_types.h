/* AArch64 type definitions for arm_neon.h
   Do not include this file directly. Use one of arm_neon.h, arm_sme.h,
   or arm_sve.h instead.

   Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#ifndef _GCC_ARM_PRIVATE_NEON_TYPES_H
#define _GCC_ARM_PRIVATE_NEON_TYPES_H

#if !defined(_AARCH64_NEON_H_) && !defined(_ARM_SVE_H_)
#error "This file should not be used standalone. Please include one of arm_neon.h arm_sve.h arm_sme.h instead."
#endif

typedef __Int8x8_t int8x8_t;
typedef __Int16x4_t int16x4_t;
typedef __Int32x2_t int32x2_t;
typedef __Int64x1_t int64x1_t;
typedef __Float16x4_t float16x4_t;
typedef __Float32x2_t float32x2_t;
typedef __Poly8x8_t poly8x8_t;
typedef __Poly16x4_t poly16x4_t;
typedef __Uint8x8_t uint8x8_t;
typedef __Uint16x4_t uint16x4_t;
typedef __Uint32x2_t uint32x2_t;
typedef __Float64x1_t float64x1_t;
typedef __Uint64x1_t uint64x1_t;
typedef __Int8x16_t int8x16_t;
typedef __Int16x8_t int16x8_t;
typedef __Int32x4_t int32x4_t;
typedef __Int64x2_t int64x2_t;
typedef __Float16x8_t float16x8_t;
typedef __Float32x4_t float32x4_t;
typedef __Float64x2_t float64x2_t;
typedef __Poly8x16_t poly8x16_t;
typedef __Poly16x8_t poly16x8_t;
typedef __Poly64x2_t poly64x2_t;
typedef __Poly64x1_t poly64x1_t;
typedef __Uint8x16_t uint8x16_t;
typedef __Uint16x8_t uint16x8_t;
typedef __Uint32x4_t uint32x4_t;
typedef __Uint64x2_t uint64x2_t;

typedef __Poly8_t poly8_t;
typedef __Poly16_t poly16_t;
typedef __Poly64_t poly64_t;
typedef __Poly128_t poly128_t;

typedef __Mfloat8x8_t mfloat8x8_t;
typedef __Mfloat8x16_t mfloat8x16_t;

typedef __fp16 float16_t;
typedef float float32_t;
typedef double float64_t;

typedef __Bfloat16x4_t bfloat16x4_t;
typedef __Bfloat16x8_t bfloat16x8_t;

#endif
