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



#ifdef __cplusplus




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

#endif /* __cplusplus  */
#endif /* __ARM_FEATURE_MVE  */
#endif /* _GCC_ARM_MVE_H.  */
