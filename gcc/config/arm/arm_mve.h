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
#define vdupq_n_s8(__a) __arm_vdupq_n_s8(__a)
#define vdupq_n_s16(__a) __arm_vdupq_n_s16(__a)
#define vdupq_n_s32(__a) __arm_vdupq_n_s32(__a)
#define vabsq_s8(__a) __arm_vabsq_s8(__a)
#define vabsq_s16(__a) __arm_vabsq_s16(__a)
#define vabsq_s32(__a) __arm_vabsq_s32(__a)
#define vclsq_s8(__a) __arm_vclsq_s8(__a)
#define vclsq_s16(__a) __arm_vclsq_s16(__a)
#define vclsq_s32(__a) __arm_vclsq_s32(__a)
#define vclzq_s8(__a) __arm_vclzq_s8(__a)
#define vclzq_s16(__a) __arm_vclzq_s16(__a)
#define vclzq_s32(__a) __arm_vclzq_s32(__a)
#define vnegq_s8(__a) __arm_vnegq_s8(__a)
#define vnegq_s16(__a) __arm_vnegq_s16(__a)
#define vnegq_s32(__a) __arm_vnegq_s32(__a)
#define vaddlvq_s32(__a) __arm_vaddlvq_s32(__a)
#define vaddvq_s8(__a) __arm_vaddvq_s8(__a)
#define vaddvq_s16(__a) __arm_vaddvq_s16(__a)
#define vaddvq_s32(__a) __arm_vaddvq_s32(__a)
#define vmovlbq_s8(__a) __arm_vmovlbq_s8(__a)
#define vmovlbq_s16(__a) __arm_vmovlbq_s16(__a)
#define vmovltq_s8(__a) __arm_vmovltq_s8(__a)
#define vmovltq_s16(__a) __arm_vmovltq_s16(__a)
#define vmvnq_s8(__a) __arm_vmvnq_s8(__a)
#define vmvnq_s16(__a) __arm_vmvnq_s16(__a)
#define vmvnq_s32(__a) __arm_vmvnq_s32(__a)
#define vmvnq_n_s16( __imm) __arm_vmvnq_n_s16( __imm)
#define vmvnq_n_s32( __imm) __arm_vmvnq_n_s32( __imm)
#define vrev16q_s8(__a) __arm_vrev16q_s8(__a)
#define vrev32q_s8(__a) __arm_vrev32q_s8(__a)
#define vrev32q_s16(__a) __arm_vrev32q_s16(__a)
#define vrev64q_s8(__a) __arm_vrev64q_s8(__a)
#define vrev64q_s16(__a) __arm_vrev64q_s16(__a)
#define vrev64q_s32(__a) __arm_vrev64q_s32(__a)
#define vqabsq_s8(__a) __arm_vqabsq_s8(__a)
#define vqabsq_s16(__a) __arm_vqabsq_s16(__a)
#define vqabsq_s32(__a) __arm_vqabsq_s32(__a)
#define vqnegq_s8(__a) __arm_vqnegq_s8(__a)
#define vqnegq_s16(__a) __arm_vqnegq_s16(__a)
#define vqnegq_s32(__a) __arm_vqnegq_s32(__a)
#define vcvtaq_s16_f16(__a) __arm_vcvtaq_s16_f16(__a)
#define vcvtaq_s32_f32(__a) __arm_vcvtaq_s32_f32(__a)
#define vcvtnq_s16_f16(__a) __arm_vcvtnq_s16_f16(__a)
#define vcvtnq_s32_f32(__a) __arm_vcvtnq_s32_f32(__a)
#define vcvtpq_s16_f16(__a) __arm_vcvtpq_s16_f16(__a)
#define vcvtpq_s32_f32(__a) __arm_vcvtpq_s32_f32(__a)
#define vcvtmq_s16_f16(__a) __arm_vcvtmq_s16_f16(__a)
#define vcvtmq_s32_f32(__a) __arm_vcvtmq_s32_f32(__a)
#define vcvtq_s16_f16(__a) __arm_vcvtq_s16_f16(__a)
#define vcvtq_s32_f32(__a) __arm_vcvtq_s32_f32(__a)
#define vrev64q_u8(__a) __arm_vrev64q_u8(__a)
#define vrev64q_u16(__a) __arm_vrev64q_u16(__a)
#define vrev64q_u32(__a) __arm_vrev64q_u32(__a)
#define vmvnq_u8(__a) __arm_vmvnq_u8(__a)
#define vmvnq_u16(__a) __arm_vmvnq_u16(__a)
#define vmvnq_u32(__a) __arm_vmvnq_u32(__a)
#define vdupq_n_u8(__a) __arm_vdupq_n_u8(__a)
#define vdupq_n_u16(__a) __arm_vdupq_n_u16(__a)
#define vdupq_n_u32(__a) __arm_vdupq_n_u32(__a)
#define vclzq_u8(__a) __arm_vclzq_u8(__a)
#define vclzq_u16(__a) __arm_vclzq_u16(__a)
#define vclzq_u32(__a) __arm_vclzq_u32(__a)
#define vaddvq_u8(__a) __arm_vaddvq_u8(__a)
#define vaddvq_u16(__a) __arm_vaddvq_u16(__a)
#define vaddvq_u32(__a) __arm_vaddvq_u32(__a)
#define vrev32q_u8(__a) __arm_vrev32q_u8(__a)
#define vrev32q_u16(__a) __arm_vrev32q_u16(__a)
#define vmovltq_u8(__a) __arm_vmovltq_u8(__a)
#define vmovltq_u16(__a) __arm_vmovltq_u16(__a)
#define vmovlbq_u8(__a) __arm_vmovlbq_u8(__a)
#define vmovlbq_u16(__a) __arm_vmovlbq_u16(__a)
#define vmvnq_n_u16( __imm) __arm_vmvnq_n_u16( __imm)
#define vmvnq_n_u32( __imm) __arm_vmvnq_n_u32( __imm)
#define vrev16q_u8(__a) __arm_vrev16q_u8(__a)
#define vaddlvq_u32(__a) __arm_vaddlvq_u32(__a)
#define vcvtq_u16_f16(__a) __arm_vcvtq_u16_f16(__a)
#define vcvtq_u32_f32(__a) __arm_vcvtq_u32_f32(__a)
#define vcvtpq_u16_f16(__a) __arm_vcvtpq_u16_f16(__a)
#define vcvtpq_u32_f32(__a) __arm_vcvtpq_u32_f32(__a)
#define vcvtnq_u16_f16(__a) __arm_vcvtnq_u16_f16(__a)
#define vcvtmq_u16_f16(__a) __arm_vcvtmq_u16_f16(__a)
#define vcvtmq_u32_f32(__a) __arm_vcvtmq_u32_f32(__a)
#define vcvtaq_u16_f16(__a) __arm_vcvtaq_u16_f16(__a)
#define vcvtaq_u32_f32(__a) __arm_vcvtaq_u32_f32(__a)
#define vctp16q(__a) __arm_vctp16q(__a)
#define vctp32q(__a) __arm_vctp32q(__a)
#define vctp64q(__a) __arm_vctp64q(__a)
#define vctp8q(__a) __arm_vctp8q(__a)
#define vpnot(__a) __arm_vpnot(__a)
#define vsubq_n_f16(__a, __b) __arm_vsubq_n_f16(__a, __b)
#define vsubq_n_f32(__a, __b) __arm_vsubq_n_f32(__a, __b)
#define vbrsrq_n_f16(__a, __b) __arm_vbrsrq_n_f16(__a, __b)
#define vbrsrq_n_f32(__a, __b) __arm_vbrsrq_n_f32(__a, __b)
#define vcvtq_n_f16_s16(__a,  __imm6) __arm_vcvtq_n_f16_s16(__a,  __imm6)
#define vcvtq_n_f32_s32(__a,  __imm6) __arm_vcvtq_n_f32_s32(__a,  __imm6)
#define vcvtq_n_f16_u16(__a,  __imm6) __arm_vcvtq_n_f16_u16(__a,  __imm6)
#define vcvtq_n_f32_u32(__a,  __imm6) __arm_vcvtq_n_f32_u32(__a,  __imm6)
#define vcreateq_f16(__a, __b) __arm_vcreateq_f16(__a, __b)
#define vcreateq_f32(__a, __b) __arm_vcreateq_f32(__a, __b)
#define vcvtq_n_s16_f16(__a,  __imm6) __arm_vcvtq_n_s16_f16(__a,  __imm6)
#define vcvtq_n_s32_f32(__a,  __imm6) __arm_vcvtq_n_s32_f32(__a,  __imm6)
#define vcvtq_n_u16_f16(__a,  __imm6) __arm_vcvtq_n_u16_f16(__a,  __imm6)
#define vcvtq_n_u32_f32(__a,  __imm6) __arm_vcvtq_n_u32_f32(__a,  __imm6)
#define vcreateq_u8(__a, __b) __arm_vcreateq_u8(__a, __b)
#define vcreateq_u16(__a, __b) __arm_vcreateq_u16(__a, __b)
#define vcreateq_u32(__a, __b) __arm_vcreateq_u32(__a, __b)
#define vcreateq_u64(__a, __b) __arm_vcreateq_u64(__a, __b)
#define vcreateq_s8(__a, __b) __arm_vcreateq_s8(__a, __b)
#define vcreateq_s16(__a, __b) __arm_vcreateq_s16(__a, __b)
#define vcreateq_s32(__a, __b) __arm_vcreateq_s32(__a, __b)
#define vcreateq_s64(__a, __b) __arm_vcreateq_s64(__a, __b)
#define vshrq_n_s8(__a,  __imm) __arm_vshrq_n_s8(__a,  __imm)
#define vshrq_n_s16(__a,  __imm) __arm_vshrq_n_s16(__a,  __imm)
#define vshrq_n_s32(__a,  __imm) __arm_vshrq_n_s32(__a,  __imm)
#define vshrq_n_u8(__a,  __imm) __arm_vshrq_n_u8(__a,  __imm)
#define vshrq_n_u16(__a,  __imm) __arm_vshrq_n_u16(__a,  __imm)
#define vshrq_n_u32(__a,  __imm) __arm_vshrq_n_u32(__a,  __imm)
#define vaddlvq_p_s32(__a, __p) __arm_vaddlvq_p_s32(__a, __p)
#define vaddlvq_p_u32(__a, __p) __arm_vaddlvq_p_u32(__a, __p)
#define vcmpneq_s8(__a, __b) __arm_vcmpneq_s8(__a, __b)
#define vcmpneq_s16(__a, __b) __arm_vcmpneq_s16(__a, __b)
#define vcmpneq_s32(__a, __b) __arm_vcmpneq_s32(__a, __b)
#define vcmpneq_u8(__a, __b) __arm_vcmpneq_u8(__a, __b)
#define vcmpneq_u16(__a, __b) __arm_vcmpneq_u16(__a, __b)
#define vcmpneq_u32(__a, __b) __arm_vcmpneq_u32(__a, __b)
#define vshlq_s8(__a, __b) __arm_vshlq_s8(__a, __b)
#define vshlq_s16(__a, __b) __arm_vshlq_s16(__a, __b)
#define vshlq_s32(__a, __b) __arm_vshlq_s32(__a, __b)
#define vshlq_u8(__a, __b) __arm_vshlq_u8(__a, __b)
#define vshlq_u16(__a, __b) __arm_vshlq_u16(__a, __b)
#define vshlq_u32(__a, __b) __arm_vshlq_u32(__a, __b)
#define vsubq_u8(__a, __b) __arm_vsubq_u8(__a, __b)
#define vsubq_n_u8(__a, __b) __arm_vsubq_n_u8(__a, __b)
#define vrmulhq_u8(__a, __b) __arm_vrmulhq_u8(__a, __b)
#define vrhaddq_u8(__a, __b) __arm_vrhaddq_u8(__a, __b)
#define vqsubq_u8(__a, __b) __arm_vqsubq_u8(__a, __b)
#define vqsubq_n_u8(__a, __b) __arm_vqsubq_n_u8(__a, __b)
#define vqaddq_u8(__a, __b) __arm_vqaddq_u8(__a, __b)
#define vqaddq_n_u8(__a, __b) __arm_vqaddq_n_u8(__a, __b)
#define vorrq_u8(__a, __b) __arm_vorrq_u8(__a, __b)
#define vornq_u8(__a, __b) __arm_vornq_u8(__a, __b)
#define vmulq_u8(__a, __b) __arm_vmulq_u8(__a, __b)
#define vmulq_n_u8(__a, __b) __arm_vmulq_n_u8(__a, __b)
#define vmulltq_int_u8(__a, __b) __arm_vmulltq_int_u8(__a, __b)
#define vmullbq_int_u8(__a, __b) __arm_vmullbq_int_u8(__a, __b)
#define vmulhq_u8(__a, __b) __arm_vmulhq_u8(__a, __b)
#define vmladavq_u8(__a, __b) __arm_vmladavq_u8(__a, __b)
#define vminvq_u8(__a, __b) __arm_vminvq_u8(__a, __b)
#define vminq_u8(__a, __b) __arm_vminq_u8(__a, __b)
#define vmaxvq_u8(__a, __b) __arm_vmaxvq_u8(__a, __b)
#define vmaxq_u8(__a, __b) __arm_vmaxq_u8(__a, __b)
#define vhsubq_u8(__a, __b) __arm_vhsubq_u8(__a, __b)
#define vhsubq_n_u8(__a, __b) __arm_vhsubq_n_u8(__a, __b)
#define vhaddq_u8(__a, __b) __arm_vhaddq_u8(__a, __b)
#define vhaddq_n_u8(__a, __b) __arm_vhaddq_n_u8(__a, __b)
#define veorq_u8(__a, __b) __arm_veorq_u8(__a, __b)
#define vcmpneq_n_u8(__a, __b) __arm_vcmpneq_n_u8(__a, __b)
#define vcmphiq_u8(__a, __b) __arm_vcmphiq_u8(__a, __b)
#define vcmphiq_n_u8(__a, __b) __arm_vcmphiq_n_u8(__a, __b)
#define vcmpeqq_u8(__a, __b) __arm_vcmpeqq_u8(__a, __b)
#define vcmpeqq_n_u8(__a, __b) __arm_vcmpeqq_n_u8(__a, __b)
#define vcmpcsq_u8(__a, __b) __arm_vcmpcsq_u8(__a, __b)
#define vcmpcsq_n_u8(__a, __b) __arm_vcmpcsq_n_u8(__a, __b)
#define vcaddq_rot90_u8(__a, __b) __arm_vcaddq_rot90_u8(__a, __b)
#define vcaddq_rot270_u8(__a, __b) __arm_vcaddq_rot270_u8(__a, __b)
#define vbicq_u8(__a, __b) __arm_vbicq_u8(__a, __b)
#define vandq_u8(__a, __b) __arm_vandq_u8(__a, __b)
#define vaddvq_p_u8(__a, __p) __arm_vaddvq_p_u8(__a, __p)
#define vaddvaq_u8(__a, __b) __arm_vaddvaq_u8(__a, __b)
#define vaddq_n_u8(__a, __b) __arm_vaddq_n_u8(__a, __b)
#define vabdq_u8(__a, __b) __arm_vabdq_u8(__a, __b)
#define vshlq_r_u8(__a, __b) __arm_vshlq_r_u8(__a, __b)
#define vrshlq_u8(__a, __b) __arm_vrshlq_u8(__a, __b)
#define vrshlq_n_u8(__a, __b) __arm_vrshlq_n_u8(__a, __b)
#define vqshlq_u8(__a, __b) __arm_vqshlq_u8(__a, __b)
#define vqshlq_r_u8(__a, __b) __arm_vqshlq_r_u8(__a, __b)
#define vqrshlq_u8(__a, __b) __arm_vqrshlq_u8(__a, __b)
#define vqrshlq_n_u8(__a, __b) __arm_vqrshlq_n_u8(__a, __b)
#define vminavq_s8(__a, __b) __arm_vminavq_s8(__a, __b)
#define vminaq_s8(__a, __b) __arm_vminaq_s8(__a, __b)
#define vmaxavq_s8(__a, __b) __arm_vmaxavq_s8(__a, __b)
#define vmaxaq_s8(__a, __b) __arm_vmaxaq_s8(__a, __b)
#define vbrsrq_n_u8(__a, __b) __arm_vbrsrq_n_u8(__a, __b)
#define vshlq_n_u8(__a,  __imm) __arm_vshlq_n_u8(__a,  __imm)
#define vrshrq_n_u8(__a,  __imm) __arm_vrshrq_n_u8(__a,  __imm)
#define vqshlq_n_u8(__a,  __imm) __arm_vqshlq_n_u8(__a,  __imm)
#define vcmpneq_n_s8(__a, __b) __arm_vcmpneq_n_s8(__a, __b)
#define vcmpltq_s8(__a, __b) __arm_vcmpltq_s8(__a, __b)
#define vcmpltq_n_s8(__a, __b) __arm_vcmpltq_n_s8(__a, __b)
#define vcmpleq_s8(__a, __b) __arm_vcmpleq_s8(__a, __b)
#define vcmpleq_n_s8(__a, __b) __arm_vcmpleq_n_s8(__a, __b)
#define vcmpgtq_s8(__a, __b) __arm_vcmpgtq_s8(__a, __b)
#define vcmpgtq_n_s8(__a, __b) __arm_vcmpgtq_n_s8(__a, __b)
#define vcmpgeq_s8(__a, __b) __arm_vcmpgeq_s8(__a, __b)
#define vcmpgeq_n_s8(__a, __b) __arm_vcmpgeq_n_s8(__a, __b)
#define vcmpeqq_s8(__a, __b) __arm_vcmpeqq_s8(__a, __b)
#define vcmpeqq_n_s8(__a, __b) __arm_vcmpeqq_n_s8(__a, __b)
#define vqshluq_n_s8(__a,  __imm) __arm_vqshluq_n_s8(__a,  __imm)
#define vaddvq_p_s8(__a, __p) __arm_vaddvq_p_s8(__a, __p)
#define vsubq_s8(__a, __b) __arm_vsubq_s8(__a, __b)
#define vsubq_n_s8(__a, __b) __arm_vsubq_n_s8(__a, __b)
#define vshlq_r_s8(__a, __b) __arm_vshlq_r_s8(__a, __b)
#define vrshlq_s8(__a, __b) __arm_vrshlq_s8(__a, __b)
#define vrshlq_n_s8(__a, __b) __arm_vrshlq_n_s8(__a, __b)
#define vrmulhq_s8(__a, __b) __arm_vrmulhq_s8(__a, __b)
#define vrhaddq_s8(__a, __b) __arm_vrhaddq_s8(__a, __b)
#define vqsubq_s8(__a, __b) __arm_vqsubq_s8(__a, __b)
#define vqsubq_n_s8(__a, __b) __arm_vqsubq_n_s8(__a, __b)
#define vqshlq_s8(__a, __b) __arm_vqshlq_s8(__a, __b)
#define vqshlq_r_s8(__a, __b) __arm_vqshlq_r_s8(__a, __b)
#define vqrshlq_s8(__a, __b) __arm_vqrshlq_s8(__a, __b)
#define vqrshlq_n_s8(__a, __b) __arm_vqrshlq_n_s8(__a, __b)
#define vqrdmulhq_s8(__a, __b) __arm_vqrdmulhq_s8(__a, __b)
#define vqrdmulhq_n_s8(__a, __b) __arm_vqrdmulhq_n_s8(__a, __b)
#define vqdmulhq_s8(__a, __b) __arm_vqdmulhq_s8(__a, __b)
#define vqdmulhq_n_s8(__a, __b) __arm_vqdmulhq_n_s8(__a, __b)
#define vqaddq_s8(__a, __b) __arm_vqaddq_s8(__a, __b)
#define vqaddq_n_s8(__a, __b) __arm_vqaddq_n_s8(__a, __b)
#define vorrq_s8(__a, __b) __arm_vorrq_s8(__a, __b)
#define vornq_s8(__a, __b) __arm_vornq_s8(__a, __b)
#define vmulq_s8(__a, __b) __arm_vmulq_s8(__a, __b)
#define vmulq_n_s8(__a, __b) __arm_vmulq_n_s8(__a, __b)
#define vmulltq_int_s8(__a, __b) __arm_vmulltq_int_s8(__a, __b)
#define vmullbq_int_s8(__a, __b) __arm_vmullbq_int_s8(__a, __b)
#define vmulhq_s8(__a, __b) __arm_vmulhq_s8(__a, __b)
#define vmlsdavxq_s8(__a, __b) __arm_vmlsdavxq_s8(__a, __b)
#define vmlsdavq_s8(__a, __b) __arm_vmlsdavq_s8(__a, __b)
#define vmladavxq_s8(__a, __b) __arm_vmladavxq_s8(__a, __b)
#define vmladavq_s8(__a, __b) __arm_vmladavq_s8(__a, __b)
#define vminvq_s8(__a, __b) __arm_vminvq_s8(__a, __b)
#define vminq_s8(__a, __b) __arm_vminq_s8(__a, __b)
#define vmaxvq_s8(__a, __b) __arm_vmaxvq_s8(__a, __b)
#define vmaxq_s8(__a, __b) __arm_vmaxq_s8(__a, __b)
#define vhsubq_s8(__a, __b) __arm_vhsubq_s8(__a, __b)
#define vhsubq_n_s8(__a, __b) __arm_vhsubq_n_s8(__a, __b)
#define vhcaddq_rot90_s8(__a, __b) __arm_vhcaddq_rot90_s8(__a, __b)
#define vhcaddq_rot270_s8(__a, __b) __arm_vhcaddq_rot270_s8(__a, __b)
#define vhaddq_s8(__a, __b) __arm_vhaddq_s8(__a, __b)
#define vhaddq_n_s8(__a, __b) __arm_vhaddq_n_s8(__a, __b)
#define veorq_s8(__a, __b) __arm_veorq_s8(__a, __b)
#define vcaddq_rot90_s8(__a, __b) __arm_vcaddq_rot90_s8(__a, __b)
#define vcaddq_rot270_s8(__a, __b) __arm_vcaddq_rot270_s8(__a, __b)
#define vbrsrq_n_s8(__a, __b) __arm_vbrsrq_n_s8(__a, __b)
#define vbicq_s8(__a, __b) __arm_vbicq_s8(__a, __b)
#define vandq_s8(__a, __b) __arm_vandq_s8(__a, __b)
#define vaddvaq_s8(__a, __b) __arm_vaddvaq_s8(__a, __b)
#define vaddq_n_s8(__a, __b) __arm_vaddq_n_s8(__a, __b)
#define vabdq_s8(__a, __b) __arm_vabdq_s8(__a, __b)
#define vshlq_n_s8(__a,  __imm) __arm_vshlq_n_s8(__a,  __imm)
#define vrshrq_n_s8(__a,  __imm) __arm_vrshrq_n_s8(__a,  __imm)
#define vqshlq_n_s8(__a,  __imm) __arm_vqshlq_n_s8(__a,  __imm)
#define vsubq_u16(__a, __b) __arm_vsubq_u16(__a, __b)
#define vsubq_n_u16(__a, __b) __arm_vsubq_n_u16(__a, __b)
#define vrmulhq_u16(__a, __b) __arm_vrmulhq_u16(__a, __b)
#define vrhaddq_u16(__a, __b) __arm_vrhaddq_u16(__a, __b)
#define vqsubq_u16(__a, __b) __arm_vqsubq_u16(__a, __b)
#define vqsubq_n_u16(__a, __b) __arm_vqsubq_n_u16(__a, __b)
#define vqaddq_u16(__a, __b) __arm_vqaddq_u16(__a, __b)
#define vqaddq_n_u16(__a, __b) __arm_vqaddq_n_u16(__a, __b)
#define vorrq_u16(__a, __b) __arm_vorrq_u16(__a, __b)
#define vornq_u16(__a, __b) __arm_vornq_u16(__a, __b)
#define vmulq_u16(__a, __b) __arm_vmulq_u16(__a, __b)
#define vmulq_n_u16(__a, __b) __arm_vmulq_n_u16(__a, __b)
#define vmulltq_int_u16(__a, __b) __arm_vmulltq_int_u16(__a, __b)
#define vmullbq_int_u16(__a, __b) __arm_vmullbq_int_u16(__a, __b)
#define vmulhq_u16(__a, __b) __arm_vmulhq_u16(__a, __b)
#define vmladavq_u16(__a, __b) __arm_vmladavq_u16(__a, __b)
#define vminvq_u16(__a, __b) __arm_vminvq_u16(__a, __b)
#define vminq_u16(__a, __b) __arm_vminq_u16(__a, __b)
#define vmaxvq_u16(__a, __b) __arm_vmaxvq_u16(__a, __b)
#define vmaxq_u16(__a, __b) __arm_vmaxq_u16(__a, __b)
#define vhsubq_u16(__a, __b) __arm_vhsubq_u16(__a, __b)
#define vhsubq_n_u16(__a, __b) __arm_vhsubq_n_u16(__a, __b)
#define vhaddq_u16(__a, __b) __arm_vhaddq_u16(__a, __b)
#define vhaddq_n_u16(__a, __b) __arm_vhaddq_n_u16(__a, __b)
#define veorq_u16(__a, __b) __arm_veorq_u16(__a, __b)
#define vcmpneq_n_u16(__a, __b) __arm_vcmpneq_n_u16(__a, __b)
#define vcmphiq_u16(__a, __b) __arm_vcmphiq_u16(__a, __b)
#define vcmphiq_n_u16(__a, __b) __arm_vcmphiq_n_u16(__a, __b)
#define vcmpeqq_u16(__a, __b) __arm_vcmpeqq_u16(__a, __b)
#define vcmpeqq_n_u16(__a, __b) __arm_vcmpeqq_n_u16(__a, __b)
#define vcmpcsq_u16(__a, __b) __arm_vcmpcsq_u16(__a, __b)
#define vcmpcsq_n_u16(__a, __b) __arm_vcmpcsq_n_u16(__a, __b)
#define vcaddq_rot90_u16(__a, __b) __arm_vcaddq_rot90_u16(__a, __b)
#define vcaddq_rot270_u16(__a, __b) __arm_vcaddq_rot270_u16(__a, __b)
#define vbicq_u16(__a, __b) __arm_vbicq_u16(__a, __b)
#define vandq_u16(__a, __b) __arm_vandq_u16(__a, __b)
#define vaddvq_p_u16(__a, __p) __arm_vaddvq_p_u16(__a, __p)
#define vaddvaq_u16(__a, __b) __arm_vaddvaq_u16(__a, __b)
#define vaddq_n_u16(__a, __b) __arm_vaddq_n_u16(__a, __b)
#define vabdq_u16(__a, __b) __arm_vabdq_u16(__a, __b)
#define vshlq_r_u16(__a, __b) __arm_vshlq_r_u16(__a, __b)
#define vrshlq_u16(__a, __b) __arm_vrshlq_u16(__a, __b)
#define vrshlq_n_u16(__a, __b) __arm_vrshlq_n_u16(__a, __b)
#define vqshlq_u16(__a, __b) __arm_vqshlq_u16(__a, __b)
#define vqshlq_r_u16(__a, __b) __arm_vqshlq_r_u16(__a, __b)
#define vqrshlq_u16(__a, __b) __arm_vqrshlq_u16(__a, __b)
#define vqrshlq_n_u16(__a, __b) __arm_vqrshlq_n_u16(__a, __b)
#define vminavq_s16(__a, __b) __arm_vminavq_s16(__a, __b)
#define vminaq_s16(__a, __b) __arm_vminaq_s16(__a, __b)
#define vmaxavq_s16(__a, __b) __arm_vmaxavq_s16(__a, __b)
#define vmaxaq_s16(__a, __b) __arm_vmaxaq_s16(__a, __b)
#define vbrsrq_n_u16(__a, __b) __arm_vbrsrq_n_u16(__a, __b)
#define vshlq_n_u16(__a,  __imm) __arm_vshlq_n_u16(__a,  __imm)
#define vrshrq_n_u16(__a,  __imm) __arm_vrshrq_n_u16(__a,  __imm)
#define vqshlq_n_u16(__a,  __imm) __arm_vqshlq_n_u16(__a,  __imm)
#define vcmpneq_n_s16(__a, __b) __arm_vcmpneq_n_s16(__a, __b)
#define vcmpltq_s16(__a, __b) __arm_vcmpltq_s16(__a, __b)
#define vcmpltq_n_s16(__a, __b) __arm_vcmpltq_n_s16(__a, __b)
#define vcmpleq_s16(__a, __b) __arm_vcmpleq_s16(__a, __b)
#define vcmpleq_n_s16(__a, __b) __arm_vcmpleq_n_s16(__a, __b)
#define vcmpgtq_s16(__a, __b) __arm_vcmpgtq_s16(__a, __b)
#define vcmpgtq_n_s16(__a, __b) __arm_vcmpgtq_n_s16(__a, __b)
#define vcmpgeq_s16(__a, __b) __arm_vcmpgeq_s16(__a, __b)
#define vcmpgeq_n_s16(__a, __b) __arm_vcmpgeq_n_s16(__a, __b)
#define vcmpeqq_s16(__a, __b) __arm_vcmpeqq_s16(__a, __b)
#define vcmpeqq_n_s16(__a, __b) __arm_vcmpeqq_n_s16(__a, __b)
#define vqshluq_n_s16(__a,  __imm) __arm_vqshluq_n_s16(__a,  __imm)
#define vaddvq_p_s16(__a, __p) __arm_vaddvq_p_s16(__a, __p)
#define vsubq_s16(__a, __b) __arm_vsubq_s16(__a, __b)
#define vsubq_n_s16(__a, __b) __arm_vsubq_n_s16(__a, __b)
#define vshlq_r_s16(__a, __b) __arm_vshlq_r_s16(__a, __b)
#define vrshlq_s16(__a, __b) __arm_vrshlq_s16(__a, __b)
#define vrshlq_n_s16(__a, __b) __arm_vrshlq_n_s16(__a, __b)
#define vrmulhq_s16(__a, __b) __arm_vrmulhq_s16(__a, __b)
#define vrhaddq_s16(__a, __b) __arm_vrhaddq_s16(__a, __b)
#define vqsubq_s16(__a, __b) __arm_vqsubq_s16(__a, __b)
#define vqsubq_n_s16(__a, __b) __arm_vqsubq_n_s16(__a, __b)
#define vqshlq_s16(__a, __b) __arm_vqshlq_s16(__a, __b)
#define vqshlq_r_s16(__a, __b) __arm_vqshlq_r_s16(__a, __b)
#define vqrshlq_s16(__a, __b) __arm_vqrshlq_s16(__a, __b)
#define vqrshlq_n_s16(__a, __b) __arm_vqrshlq_n_s16(__a, __b)
#define vqrdmulhq_s16(__a, __b) __arm_vqrdmulhq_s16(__a, __b)
#define vqrdmulhq_n_s16(__a, __b) __arm_vqrdmulhq_n_s16(__a, __b)
#define vqdmulhq_s16(__a, __b) __arm_vqdmulhq_s16(__a, __b)
#define vqdmulhq_n_s16(__a, __b) __arm_vqdmulhq_n_s16(__a, __b)
#define vqaddq_s16(__a, __b) __arm_vqaddq_s16(__a, __b)
#define vqaddq_n_s16(__a, __b) __arm_vqaddq_n_s16(__a, __b)
#define vorrq_s16(__a, __b) __arm_vorrq_s16(__a, __b)
#define vornq_s16(__a, __b) __arm_vornq_s16(__a, __b)
#define vmulq_s16(__a, __b) __arm_vmulq_s16(__a, __b)
#define vmulq_n_s16(__a, __b) __arm_vmulq_n_s16(__a, __b)
#define vmulltq_int_s16(__a, __b) __arm_vmulltq_int_s16(__a, __b)
#define vmullbq_int_s16(__a, __b) __arm_vmullbq_int_s16(__a, __b)
#define vmulhq_s16(__a, __b) __arm_vmulhq_s16(__a, __b)
#define vmlsdavxq_s16(__a, __b) __arm_vmlsdavxq_s16(__a, __b)
#define vmlsdavq_s16(__a, __b) __arm_vmlsdavq_s16(__a, __b)
#define vmladavxq_s16(__a, __b) __arm_vmladavxq_s16(__a, __b)
#define vmladavq_s16(__a, __b) __arm_vmladavq_s16(__a, __b)
#define vminvq_s16(__a, __b) __arm_vminvq_s16(__a, __b)
#define vminq_s16(__a, __b) __arm_vminq_s16(__a, __b)
#define vmaxvq_s16(__a, __b) __arm_vmaxvq_s16(__a, __b)
#define vmaxq_s16(__a, __b) __arm_vmaxq_s16(__a, __b)
#define vhsubq_s16(__a, __b) __arm_vhsubq_s16(__a, __b)
#define vhsubq_n_s16(__a, __b) __arm_vhsubq_n_s16(__a, __b)
#define vhcaddq_rot90_s16(__a, __b) __arm_vhcaddq_rot90_s16(__a, __b)
#define vhcaddq_rot270_s16(__a, __b) __arm_vhcaddq_rot270_s16(__a, __b)
#define vhaddq_s16(__a, __b) __arm_vhaddq_s16(__a, __b)
#define vhaddq_n_s16(__a, __b) __arm_vhaddq_n_s16(__a, __b)
#define veorq_s16(__a, __b) __arm_veorq_s16(__a, __b)
#define vcaddq_rot90_s16(__a, __b) __arm_vcaddq_rot90_s16(__a, __b)
#define vcaddq_rot270_s16(__a, __b) __arm_vcaddq_rot270_s16(__a, __b)
#define vbrsrq_n_s16(__a, __b) __arm_vbrsrq_n_s16(__a, __b)
#define vbicq_s16(__a, __b) __arm_vbicq_s16(__a, __b)
#define vandq_s16(__a, __b) __arm_vandq_s16(__a, __b)
#define vaddvaq_s16(__a, __b) __arm_vaddvaq_s16(__a, __b)
#define vaddq_n_s16(__a, __b) __arm_vaddq_n_s16(__a, __b)
#define vabdq_s16(__a, __b) __arm_vabdq_s16(__a, __b)
#define vshlq_n_s16(__a,  __imm) __arm_vshlq_n_s16(__a,  __imm)
#define vrshrq_n_s16(__a,  __imm) __arm_vrshrq_n_s16(__a,  __imm)
#define vqshlq_n_s16(__a,  __imm) __arm_vqshlq_n_s16(__a,  __imm)
#define vsubq_u32(__a, __b) __arm_vsubq_u32(__a, __b)
#define vsubq_n_u32(__a, __b) __arm_vsubq_n_u32(__a, __b)
#define vrmulhq_u32(__a, __b) __arm_vrmulhq_u32(__a, __b)
#define vrhaddq_u32(__a, __b) __arm_vrhaddq_u32(__a, __b)
#define vqsubq_u32(__a, __b) __arm_vqsubq_u32(__a, __b)
#define vqsubq_n_u32(__a, __b) __arm_vqsubq_n_u32(__a, __b)
#define vqaddq_u32(__a, __b) __arm_vqaddq_u32(__a, __b)
#define vqaddq_n_u32(__a, __b) __arm_vqaddq_n_u32(__a, __b)
#define vorrq_u32(__a, __b) __arm_vorrq_u32(__a, __b)
#define vornq_u32(__a, __b) __arm_vornq_u32(__a, __b)
#define vmulq_u32(__a, __b) __arm_vmulq_u32(__a, __b)
#define vmulq_n_u32(__a, __b) __arm_vmulq_n_u32(__a, __b)
#define vmulltq_int_u32(__a, __b) __arm_vmulltq_int_u32(__a, __b)
#define vmullbq_int_u32(__a, __b) __arm_vmullbq_int_u32(__a, __b)
#define vmulhq_u32(__a, __b) __arm_vmulhq_u32(__a, __b)
#define vmladavq_u32(__a, __b) __arm_vmladavq_u32(__a, __b)
#define vminvq_u32(__a, __b) __arm_vminvq_u32(__a, __b)
#define vminq_u32(__a, __b) __arm_vminq_u32(__a, __b)
#define vmaxvq_u32(__a, __b) __arm_vmaxvq_u32(__a, __b)
#define vmaxq_u32(__a, __b) __arm_vmaxq_u32(__a, __b)
#define vhsubq_u32(__a, __b) __arm_vhsubq_u32(__a, __b)
#define vhsubq_n_u32(__a, __b) __arm_vhsubq_n_u32(__a, __b)
#define vhaddq_u32(__a, __b) __arm_vhaddq_u32(__a, __b)
#define vhaddq_n_u32(__a, __b) __arm_vhaddq_n_u32(__a, __b)
#define veorq_u32(__a, __b) __arm_veorq_u32(__a, __b)
#define vcmpneq_n_u32(__a, __b) __arm_vcmpneq_n_u32(__a, __b)
#define vcmphiq_u32(__a, __b) __arm_vcmphiq_u32(__a, __b)
#define vcmphiq_n_u32(__a, __b) __arm_vcmphiq_n_u32(__a, __b)
#define vcmpeqq_u32(__a, __b) __arm_vcmpeqq_u32(__a, __b)
#define vcmpeqq_n_u32(__a, __b) __arm_vcmpeqq_n_u32(__a, __b)
#define vcmpcsq_u32(__a, __b) __arm_vcmpcsq_u32(__a, __b)
#define vcmpcsq_n_u32(__a, __b) __arm_vcmpcsq_n_u32(__a, __b)
#define vcaddq_rot90_u32(__a, __b) __arm_vcaddq_rot90_u32(__a, __b)
#define vcaddq_rot270_u32(__a, __b) __arm_vcaddq_rot270_u32(__a, __b)
#define vbicq_u32(__a, __b) __arm_vbicq_u32(__a, __b)
#define vandq_u32(__a, __b) __arm_vandq_u32(__a, __b)
#define vaddvq_p_u32(__a, __p) __arm_vaddvq_p_u32(__a, __p)
#define vaddvaq_u32(__a, __b) __arm_vaddvaq_u32(__a, __b)
#define vaddq_n_u32(__a, __b) __arm_vaddq_n_u32(__a, __b)
#define vabdq_u32(__a, __b) __arm_vabdq_u32(__a, __b)
#define vshlq_r_u32(__a, __b) __arm_vshlq_r_u32(__a, __b)
#define vrshlq_u32(__a, __b) __arm_vrshlq_u32(__a, __b)
#define vrshlq_n_u32(__a, __b) __arm_vrshlq_n_u32(__a, __b)
#define vqshlq_u32(__a, __b) __arm_vqshlq_u32(__a, __b)
#define vqshlq_r_u32(__a, __b) __arm_vqshlq_r_u32(__a, __b)
#define vqrshlq_u32(__a, __b) __arm_vqrshlq_u32(__a, __b)
#define vqrshlq_n_u32(__a, __b) __arm_vqrshlq_n_u32(__a, __b)
#define vminavq_s32(__a, __b) __arm_vminavq_s32(__a, __b)
#define vminaq_s32(__a, __b) __arm_vminaq_s32(__a, __b)
#define vmaxavq_s32(__a, __b) __arm_vmaxavq_s32(__a, __b)
#define vmaxaq_s32(__a, __b) __arm_vmaxaq_s32(__a, __b)
#define vbrsrq_n_u32(__a, __b) __arm_vbrsrq_n_u32(__a, __b)
#define vshlq_n_u32(__a,  __imm) __arm_vshlq_n_u32(__a,  __imm)
#define vrshrq_n_u32(__a,  __imm) __arm_vrshrq_n_u32(__a,  __imm)
#define vqshlq_n_u32(__a,  __imm) __arm_vqshlq_n_u32(__a,  __imm)
#define vcmpneq_n_s32(__a, __b) __arm_vcmpneq_n_s32(__a, __b)
#define vcmpltq_s32(__a, __b) __arm_vcmpltq_s32(__a, __b)
#define vcmpltq_n_s32(__a, __b) __arm_vcmpltq_n_s32(__a, __b)
#define vcmpleq_s32(__a, __b) __arm_vcmpleq_s32(__a, __b)
#define vcmpleq_n_s32(__a, __b) __arm_vcmpleq_n_s32(__a, __b)
#define vcmpgtq_s32(__a, __b) __arm_vcmpgtq_s32(__a, __b)
#define vcmpgtq_n_s32(__a, __b) __arm_vcmpgtq_n_s32(__a, __b)
#define vcmpgeq_s32(__a, __b) __arm_vcmpgeq_s32(__a, __b)
#define vcmpgeq_n_s32(__a, __b) __arm_vcmpgeq_n_s32(__a, __b)
#define vcmpeqq_s32(__a, __b) __arm_vcmpeqq_s32(__a, __b)
#define vcmpeqq_n_s32(__a, __b) __arm_vcmpeqq_n_s32(__a, __b)
#define vqshluq_n_s32(__a,  __imm) __arm_vqshluq_n_s32(__a,  __imm)
#define vaddvq_p_s32(__a, __p) __arm_vaddvq_p_s32(__a, __p)
#define vsubq_s32(__a, __b) __arm_vsubq_s32(__a, __b)
#define vsubq_n_s32(__a, __b) __arm_vsubq_n_s32(__a, __b)
#define vshlq_r_s32(__a, __b) __arm_vshlq_r_s32(__a, __b)
#define vrshlq_s32(__a, __b) __arm_vrshlq_s32(__a, __b)
#define vrshlq_n_s32(__a, __b) __arm_vrshlq_n_s32(__a, __b)
#define vrmulhq_s32(__a, __b) __arm_vrmulhq_s32(__a, __b)
#define vrhaddq_s32(__a, __b) __arm_vrhaddq_s32(__a, __b)
#define vqsubq_s32(__a, __b) __arm_vqsubq_s32(__a, __b)
#define vqsubq_n_s32(__a, __b) __arm_vqsubq_n_s32(__a, __b)
#define vqshlq_s32(__a, __b) __arm_vqshlq_s32(__a, __b)
#define vqshlq_r_s32(__a, __b) __arm_vqshlq_r_s32(__a, __b)
#define vqrshlq_s32(__a, __b) __arm_vqrshlq_s32(__a, __b)
#define vqrshlq_n_s32(__a, __b) __arm_vqrshlq_n_s32(__a, __b)
#define vqrdmulhq_s32(__a, __b) __arm_vqrdmulhq_s32(__a, __b)
#define vqrdmulhq_n_s32(__a, __b) __arm_vqrdmulhq_n_s32(__a, __b)
#define vqdmulhq_s32(__a, __b) __arm_vqdmulhq_s32(__a, __b)
#define vqdmulhq_n_s32(__a, __b) __arm_vqdmulhq_n_s32(__a, __b)
#define vqaddq_s32(__a, __b) __arm_vqaddq_s32(__a, __b)
#define vqaddq_n_s32(__a, __b) __arm_vqaddq_n_s32(__a, __b)
#define vorrq_s32(__a, __b) __arm_vorrq_s32(__a, __b)
#define vornq_s32(__a, __b) __arm_vornq_s32(__a, __b)
#define vmulq_s32(__a, __b) __arm_vmulq_s32(__a, __b)
#define vmulq_n_s32(__a, __b) __arm_vmulq_n_s32(__a, __b)
#define vmulltq_int_s32(__a, __b) __arm_vmulltq_int_s32(__a, __b)
#define vmullbq_int_s32(__a, __b) __arm_vmullbq_int_s32(__a, __b)
#define vmulhq_s32(__a, __b) __arm_vmulhq_s32(__a, __b)
#define vmlsdavxq_s32(__a, __b) __arm_vmlsdavxq_s32(__a, __b)
#define vmlsdavq_s32(__a, __b) __arm_vmlsdavq_s32(__a, __b)
#define vmladavxq_s32(__a, __b) __arm_vmladavxq_s32(__a, __b)
#define vmladavq_s32(__a, __b) __arm_vmladavq_s32(__a, __b)
#define vminvq_s32(__a, __b) __arm_vminvq_s32(__a, __b)
#define vminq_s32(__a, __b) __arm_vminq_s32(__a, __b)
#define vmaxvq_s32(__a, __b) __arm_vmaxvq_s32(__a, __b)
#define vmaxq_s32(__a, __b) __arm_vmaxq_s32(__a, __b)
#define vhsubq_s32(__a, __b) __arm_vhsubq_s32(__a, __b)
#define vhsubq_n_s32(__a, __b) __arm_vhsubq_n_s32(__a, __b)
#define vhcaddq_rot90_s32(__a, __b) __arm_vhcaddq_rot90_s32(__a, __b)
#define vhcaddq_rot270_s32(__a, __b) __arm_vhcaddq_rot270_s32(__a, __b)
#define vhaddq_s32(__a, __b) __arm_vhaddq_s32(__a, __b)
#define vhaddq_n_s32(__a, __b) __arm_vhaddq_n_s32(__a, __b)
#define veorq_s32(__a, __b) __arm_veorq_s32(__a, __b)
#define vcaddq_rot90_s32(__a, __b) __arm_vcaddq_rot90_s32(__a, __b)
#define vcaddq_rot270_s32(__a, __b) __arm_vcaddq_rot270_s32(__a, __b)
#define vbrsrq_n_s32(__a, __b) __arm_vbrsrq_n_s32(__a, __b)
#define vbicq_s32(__a, __b) __arm_vbicq_s32(__a, __b)
#define vandq_s32(__a, __b) __arm_vandq_s32(__a, __b)
#define vaddvaq_s32(__a, __b) __arm_vaddvaq_s32(__a, __b)
#define vaddq_n_s32(__a, __b) __arm_vaddq_n_s32(__a, __b)
#define vabdq_s32(__a, __b) __arm_vabdq_s32(__a, __b)
#define vshlq_n_s32(__a,  __imm) __arm_vshlq_n_s32(__a,  __imm)
#define vrshrq_n_s32(__a,  __imm) __arm_vrshrq_n_s32(__a,  __imm)
#define vqshlq_n_s32(__a,  __imm) __arm_vqshlq_n_s32(__a,  __imm)
#define vqmovntq_u16(__a, __b) __arm_vqmovntq_u16(__a, __b)
#define vqmovnbq_u16(__a, __b) __arm_vqmovnbq_u16(__a, __b)
#define vmulltq_poly_p8(__a, __b) __arm_vmulltq_poly_p8(__a, __b)
#define vmullbq_poly_p8(__a, __b) __arm_vmullbq_poly_p8(__a, __b)
#define vmovntq_u16(__a, __b) __arm_vmovntq_u16(__a, __b)
#define vmovnbq_u16(__a, __b) __arm_vmovnbq_u16(__a, __b)
#define vmlaldavq_u16(__a, __b) __arm_vmlaldavq_u16(__a, __b)
#define vqmovuntq_s16(__a, __b) __arm_vqmovuntq_s16(__a, __b)
#define vqmovunbq_s16(__a, __b) __arm_vqmovunbq_s16(__a, __b)
#define vshlltq_n_u8(__a,  __imm) __arm_vshlltq_n_u8(__a,  __imm)
#define vshllbq_n_u8(__a,  __imm) __arm_vshllbq_n_u8(__a,  __imm)
#define vorrq_n_u16(__a,  __imm) __arm_vorrq_n_u16(__a,  __imm)
#define vbicq_n_u16(__a,  __imm) __arm_vbicq_n_u16(__a,  __imm)
#define vcmpneq_n_f16(__a, __b) __arm_vcmpneq_n_f16(__a, __b)
#define vcmpneq_f16(__a, __b) __arm_vcmpneq_f16(__a, __b)
#define vcmpltq_n_f16(__a, __b) __arm_vcmpltq_n_f16(__a, __b)
#define vcmpltq_f16(__a, __b) __arm_vcmpltq_f16(__a, __b)
#define vcmpleq_n_f16(__a, __b) __arm_vcmpleq_n_f16(__a, __b)
#define vcmpleq_f16(__a, __b) __arm_vcmpleq_f16(__a, __b)
#define vcmpgtq_n_f16(__a, __b) __arm_vcmpgtq_n_f16(__a, __b)
#define vcmpgtq_f16(__a, __b) __arm_vcmpgtq_f16(__a, __b)
#define vcmpgeq_n_f16(__a, __b) __arm_vcmpgeq_n_f16(__a, __b)
#define vcmpgeq_f16(__a, __b) __arm_vcmpgeq_f16(__a, __b)
#define vcmpeqq_n_f16(__a, __b) __arm_vcmpeqq_n_f16(__a, __b)
#define vcmpeqq_f16(__a, __b) __arm_vcmpeqq_f16(__a, __b)
#define vsubq_f16(__a, __b) __arm_vsubq_f16(__a, __b)
#define vqmovntq_s16(__a, __b) __arm_vqmovntq_s16(__a, __b)
#define vqmovnbq_s16(__a, __b) __arm_vqmovnbq_s16(__a, __b)
#define vqdmulltq_s16(__a, __b) __arm_vqdmulltq_s16(__a, __b)
#define vqdmulltq_n_s16(__a, __b) __arm_vqdmulltq_n_s16(__a, __b)
#define vqdmullbq_s16(__a, __b) __arm_vqdmullbq_s16(__a, __b)
#define vqdmullbq_n_s16(__a, __b) __arm_vqdmullbq_n_s16(__a, __b)
#define vorrq_f16(__a, __b) __arm_vorrq_f16(__a, __b)
#define vornq_f16(__a, __b) __arm_vornq_f16(__a, __b)
#define vmulq_n_f16(__a, __b) __arm_vmulq_n_f16(__a, __b)
#define vmulq_f16(__a, __b) __arm_vmulq_f16(__a, __b)
#define vmovntq_s16(__a, __b) __arm_vmovntq_s16(__a, __b)
#define vmovnbq_s16(__a, __b) __arm_vmovnbq_s16(__a, __b)
#define vmlsldavxq_s16(__a, __b) __arm_vmlsldavxq_s16(__a, __b)
#define vmlsldavq_s16(__a, __b) __arm_vmlsldavq_s16(__a, __b)
#define vmlaldavxq_s16(__a, __b) __arm_vmlaldavxq_s16(__a, __b)
#define vmlaldavq_s16(__a, __b) __arm_vmlaldavq_s16(__a, __b)
#define vminnmvq_f16(__a, __b) __arm_vminnmvq_f16(__a, __b)
#define vminnmq_f16(__a, __b) __arm_vminnmq_f16(__a, __b)
#define vminnmavq_f16(__a, __b) __arm_vminnmavq_f16(__a, __b)
#define vminnmaq_f16(__a, __b) __arm_vminnmaq_f16(__a, __b)
#define vmaxnmvq_f16(__a, __b) __arm_vmaxnmvq_f16(__a, __b)
#define vmaxnmq_f16(__a, __b) __arm_vmaxnmq_f16(__a, __b)
#define vmaxnmavq_f16(__a, __b) __arm_vmaxnmavq_f16(__a, __b)
#define vmaxnmaq_f16(__a, __b) __arm_vmaxnmaq_f16(__a, __b)
#define veorq_f16(__a, __b) __arm_veorq_f16(__a, __b)
#define vcmulq_rot90_f16(__a, __b) __arm_vcmulq_rot90_f16(__a, __b)
#define vcmulq_rot270_f16(__a, __b) __arm_vcmulq_rot270_f16(__a, __b)
#define vcmulq_rot180_f16(__a, __b) __arm_vcmulq_rot180_f16(__a, __b)
#define vcmulq_f16(__a, __b) __arm_vcmulq_f16(__a, __b)
#define vcaddq_rot90_f16(__a, __b) __arm_vcaddq_rot90_f16(__a, __b)
#define vcaddq_rot270_f16(__a, __b) __arm_vcaddq_rot270_f16(__a, __b)
#define vbicq_f16(__a, __b) __arm_vbicq_f16(__a, __b)
#define vandq_f16(__a, __b) __arm_vandq_f16(__a, __b)
#define vaddq_n_f16(__a, __b) __arm_vaddq_n_f16(__a, __b)
#define vabdq_f16(__a, __b) __arm_vabdq_f16(__a, __b)
#define vshlltq_n_s8(__a,  __imm) __arm_vshlltq_n_s8(__a,  __imm)
#define vshllbq_n_s8(__a,  __imm) __arm_vshllbq_n_s8(__a,  __imm)
#define vorrq_n_s16(__a,  __imm) __arm_vorrq_n_s16(__a,  __imm)
#define vbicq_n_s16(__a,  __imm) __arm_vbicq_n_s16(__a,  __imm)
#define vqmovntq_u32(__a, __b) __arm_vqmovntq_u32(__a, __b)
#define vqmovnbq_u32(__a, __b) __arm_vqmovnbq_u32(__a, __b)
#define vmulltq_poly_p16(__a, __b) __arm_vmulltq_poly_p16(__a, __b)
#define vmullbq_poly_p16(__a, __b) __arm_vmullbq_poly_p16(__a, __b)
#define vmovntq_u32(__a, __b) __arm_vmovntq_u32(__a, __b)
#define vmovnbq_u32(__a, __b) __arm_vmovnbq_u32(__a, __b)
#define vmlaldavq_u32(__a, __b) __arm_vmlaldavq_u32(__a, __b)
#define vqmovuntq_s32(__a, __b) __arm_vqmovuntq_s32(__a, __b)
#define vqmovunbq_s32(__a, __b) __arm_vqmovunbq_s32(__a, __b)
#define vshlltq_n_u16(__a,  __imm) __arm_vshlltq_n_u16(__a,  __imm)
#define vshllbq_n_u16(__a,  __imm) __arm_vshllbq_n_u16(__a,  __imm)
#define vorrq_n_u32(__a,  __imm) __arm_vorrq_n_u32(__a,  __imm)
#define vbicq_n_u32(__a,  __imm) __arm_vbicq_n_u32(__a,  __imm)
#define vcmpneq_n_f32(__a, __b) __arm_vcmpneq_n_f32(__a, __b)
#define vcmpneq_f32(__a, __b) __arm_vcmpneq_f32(__a, __b)
#define vcmpltq_n_f32(__a, __b) __arm_vcmpltq_n_f32(__a, __b)
#define vcmpltq_f32(__a, __b) __arm_vcmpltq_f32(__a, __b)
#define vcmpleq_n_f32(__a, __b) __arm_vcmpleq_n_f32(__a, __b)
#define vcmpleq_f32(__a, __b) __arm_vcmpleq_f32(__a, __b)
#define vcmpgtq_n_f32(__a, __b) __arm_vcmpgtq_n_f32(__a, __b)
#define vcmpgtq_f32(__a, __b) __arm_vcmpgtq_f32(__a, __b)
#define vcmpgeq_n_f32(__a, __b) __arm_vcmpgeq_n_f32(__a, __b)
#define vcmpgeq_f32(__a, __b) __arm_vcmpgeq_f32(__a, __b)
#define vcmpeqq_n_f32(__a, __b) __arm_vcmpeqq_n_f32(__a, __b)
#define vcmpeqq_f32(__a, __b) __arm_vcmpeqq_f32(__a, __b)
#define vsubq_f32(__a, __b) __arm_vsubq_f32(__a, __b)
#define vqmovntq_s32(__a, __b) __arm_vqmovntq_s32(__a, __b)
#define vqmovnbq_s32(__a, __b) __arm_vqmovnbq_s32(__a, __b)
#define vqdmulltq_s32(__a, __b) __arm_vqdmulltq_s32(__a, __b)
#define vqdmulltq_n_s32(__a, __b) __arm_vqdmulltq_n_s32(__a, __b)
#define vqdmullbq_s32(__a, __b) __arm_vqdmullbq_s32(__a, __b)
#define vqdmullbq_n_s32(__a, __b) __arm_vqdmullbq_n_s32(__a, __b)
#define vorrq_f32(__a, __b) __arm_vorrq_f32(__a, __b)
#define vornq_f32(__a, __b) __arm_vornq_f32(__a, __b)
#define vmulq_n_f32(__a, __b) __arm_vmulq_n_f32(__a, __b)
#define vmulq_f32(__a, __b) __arm_vmulq_f32(__a, __b)
#define vmovntq_s32(__a, __b) __arm_vmovntq_s32(__a, __b)
#define vmovnbq_s32(__a, __b) __arm_vmovnbq_s32(__a, __b)
#define vmlsldavxq_s32(__a, __b) __arm_vmlsldavxq_s32(__a, __b)
#define vmlsldavq_s32(__a, __b) __arm_vmlsldavq_s32(__a, __b)
#define vmlaldavxq_s32(__a, __b) __arm_vmlaldavxq_s32(__a, __b)
#define vmlaldavq_s32(__a, __b) __arm_vmlaldavq_s32(__a, __b)
#define vminnmvq_f32(__a, __b) __arm_vminnmvq_f32(__a, __b)
#define vminnmq_f32(__a, __b) __arm_vminnmq_f32(__a, __b)
#define vminnmavq_f32(__a, __b) __arm_vminnmavq_f32(__a, __b)
#define vminnmaq_f32(__a, __b) __arm_vminnmaq_f32(__a, __b)
#define vmaxnmvq_f32(__a, __b) __arm_vmaxnmvq_f32(__a, __b)
#define vmaxnmq_f32(__a, __b) __arm_vmaxnmq_f32(__a, __b)
#define vmaxnmavq_f32(__a, __b) __arm_vmaxnmavq_f32(__a, __b)
#define vmaxnmaq_f32(__a, __b) __arm_vmaxnmaq_f32(__a, __b)
#define veorq_f32(__a, __b) __arm_veorq_f32(__a, __b)
#define vcmulq_rot90_f32(__a, __b) __arm_vcmulq_rot90_f32(__a, __b)
#define vcmulq_rot270_f32(__a, __b) __arm_vcmulq_rot270_f32(__a, __b)
#define vcmulq_rot180_f32(__a, __b) __arm_vcmulq_rot180_f32(__a, __b)
#define vcmulq_f32(__a, __b) __arm_vcmulq_f32(__a, __b)
#define vcaddq_rot90_f32(__a, __b) __arm_vcaddq_rot90_f32(__a, __b)
#define vcaddq_rot270_f32(__a, __b) __arm_vcaddq_rot270_f32(__a, __b)
#define vbicq_f32(__a, __b) __arm_vbicq_f32(__a, __b)
#define vandq_f32(__a, __b) __arm_vandq_f32(__a, __b)
#define vaddq_n_f32(__a, __b) __arm_vaddq_n_f32(__a, __b)
#define vabdq_f32(__a, __b) __arm_vabdq_f32(__a, __b)
#define vshlltq_n_s16(__a,  __imm) __arm_vshlltq_n_s16(__a,  __imm)
#define vshllbq_n_s16(__a,  __imm) __arm_vshllbq_n_s16(__a,  __imm)
#define vorrq_n_s32(__a,  __imm) __arm_vorrq_n_s32(__a,  __imm)
#define vbicq_n_s32(__a,  __imm) __arm_vbicq_n_s32(__a,  __imm)
#define vrmlaldavhq_u32(__a, __b) __arm_vrmlaldavhq_u32(__a, __b)
#define vctp8q_m(__a, __p) __arm_vctp8q_m(__a, __p)
#define vctp64q_m(__a, __p) __arm_vctp64q_m(__a, __p)
#define vctp32q_m(__a, __p) __arm_vctp32q_m(__a, __p)
#define vctp16q_m(__a, __p) __arm_vctp16q_m(__a, __p)
#define vaddlvaq_u32(__a, __b) __arm_vaddlvaq_u32(__a, __b)
#define vrmlsldavhxq_s32(__a, __b) __arm_vrmlsldavhxq_s32(__a, __b)
#define vrmlsldavhq_s32(__a, __b) __arm_vrmlsldavhq_s32(__a, __b)
#define vrmlaldavhxq_s32(__a, __b) __arm_vrmlaldavhxq_s32(__a, __b)
#define vrmlaldavhq_s32(__a, __b) __arm_vrmlaldavhq_s32(__a, __b)
#define vcvttq_f16_f32(__a, __b) __arm_vcvttq_f16_f32(__a, __b)
#define vcvtbq_f16_f32(__a, __b) __arm_vcvtbq_f16_f32(__a, __b)
#define vaddlvaq_s32(__a, __b) __arm_vaddlvaq_s32(__a, __b)
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

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdupq_n_s8 (int8_t __a)
{
  return __builtin_mve_vdupq_n_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdupq_n_s16 (int16_t __a)
{
  return __builtin_mve_vdupq_n_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdupq_n_s32 (int32_t __a)
{
  return __builtin_mve_vdupq_n_sv4si (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabsq_s8 (int8x16_t __a)
{
  return __builtin_mve_vabsq_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabsq_s16 (int16x8_t __a)
{
  return __builtin_mve_vabsq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabsq_s32 (int32x4_t __a)
{
  return __builtin_mve_vabsq_sv4si (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vclsq_s8 (int8x16_t __a)
{
  return __builtin_mve_vclsq_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vclsq_s16 (int16x8_t __a)
{
  return __builtin_mve_vclsq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vclsq_s32 (int32x4_t __a)
{
  return __builtin_mve_vclsq_sv4si (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vclzq_s8 (int8x16_t __a)
{
  return __builtin_mve_vclzq_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vclzq_s16 (int16x8_t __a)
{
  return __builtin_mve_vclzq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vclzq_s32 (int32x4_t __a)
{
  return __builtin_mve_vclzq_sv4si (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vnegq_s8 (int8x16_t __a)
{
  return __builtin_mve_vnegq_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vnegq_s16 (int16x8_t __a)
{
  return __builtin_mve_vnegq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vnegq_s32 (int32x4_t __a)
{
  return __builtin_mve_vnegq_sv4si (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddlvq_s32 (int32x4_t __a)
{
  return __builtin_mve_vaddlvq_sv4si (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_s8 (int8x16_t __a)
{
  return __builtin_mve_vaddvq_sv16qi (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_s16 (int16x8_t __a)
{
  return __builtin_mve_vaddvq_sv8hi (__a);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_s32 (int32x4_t __a)
{
  return __builtin_mve_vaddvq_sv4si (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovlbq_s8 (int8x16_t __a)
{
  return __builtin_mve_vmovlbq_sv16qi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovlbq_s16 (int16x8_t __a)
{
  return __builtin_mve_vmovlbq_sv8hi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovltq_s8 (int8x16_t __a)
{
  return __builtin_mve_vmovltq_sv16qi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovltq_s16 (int16x8_t __a)
{
  return __builtin_mve_vmovltq_sv8hi (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_s8 (int8x16_t __a)
{
  return __builtin_mve_vmvnq_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_s16 (int16x8_t __a)
{
  return __builtin_mve_vmvnq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_s32 (int32x4_t __a)
{
  return __builtin_mve_vmvnq_sv4si (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_n_s16 (const int16_t __imm)
{
  return __builtin_mve_vmvnq_n_sv8hi (__imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_n_s32 (const int32_t __imm)
{
  return __builtin_mve_vmvnq_n_sv4si (__imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev16q_s8 (int8x16_t __a)
{
  return __builtin_mve_vrev16q_sv16qi (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev32q_s8 (int8x16_t __a)
{
  return __builtin_mve_vrev32q_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev32q_s16 (int16x8_t __a)
{
  return __builtin_mve_vrev32q_sv8hi (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev64q_s8 (int8x16_t __a)
{
  return __builtin_mve_vrev64q_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev64q_s16 (int16x8_t __a)
{
  return __builtin_mve_vrev64q_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev64q_s32 (int32x4_t __a)
{
  return __builtin_mve_vrev64q_sv4si (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqabsq_s8 (int8x16_t __a)
{
  return __builtin_mve_vqabsq_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqabsq_s16 (int16x8_t __a)
{
  return __builtin_mve_vqabsq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqabsq_s32 (int32x4_t __a)
{
  return __builtin_mve_vqabsq_sv4si (__a);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqnegq_s8 (int8x16_t __a)
{
  return __builtin_mve_vqnegq_sv16qi (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqnegq_s16 (int16x8_t __a)
{
  return __builtin_mve_vqnegq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqnegq_s32 (int32x4_t __a)
{
  return __builtin_mve_vqnegq_sv4si (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev64q_u8 (uint8x16_t __a)
{
  return __builtin_mve_vrev64q_uv16qi (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev64q_u16 (uint16x8_t __a)
{
  return __builtin_mve_vrev64q_uv8hi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev64q_u32 (uint32x4_t __a)
{
  return __builtin_mve_vrev64q_uv4si (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_u8 (uint8x16_t __a)
{
  return __builtin_mve_vmvnq_uv16qi (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_u16 (uint16x8_t __a)
{
  return __builtin_mve_vmvnq_uv8hi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_u32 (uint32x4_t __a)
{
  return __builtin_mve_vmvnq_uv4si (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdupq_n_u8 (uint8_t __a)
{
  return __builtin_mve_vdupq_n_uv16qi (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdupq_n_u16 (uint16_t __a)
{
  return __builtin_mve_vdupq_n_uv8hi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdupq_n_u32 (uint32_t __a)
{
  return __builtin_mve_vdupq_n_uv4si (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vclzq_u8 (uint8x16_t __a)
{
  return __builtin_mve_vclzq_uv16qi (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vclzq_u16 (uint16x8_t __a)
{
  return __builtin_mve_vclzq_uv8hi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vclzq_u32 (uint32x4_t __a)
{
  return __builtin_mve_vclzq_uv4si (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_u8 (uint8x16_t __a)
{
  return __builtin_mve_vaddvq_uv16qi (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_u16 (uint16x8_t __a)
{
  return __builtin_mve_vaddvq_uv8hi (__a);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_u32 (uint32x4_t __a)
{
  return __builtin_mve_vaddvq_uv4si (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev32q_u8 (uint8x16_t __a)
{
  return __builtin_mve_vrev32q_uv16qi (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev32q_u16 (uint16x8_t __a)
{
  return __builtin_mve_vrev32q_uv8hi (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovltq_u8 (uint8x16_t __a)
{
  return __builtin_mve_vmovltq_uv16qi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovltq_u16 (uint16x8_t __a)
{
  return __builtin_mve_vmovltq_uv8hi (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovlbq_u8 (uint8x16_t __a)
{
  return __builtin_mve_vmovlbq_uv16qi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovlbq_u16 (uint16x8_t __a)
{
  return __builtin_mve_vmovlbq_uv8hi (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_n_u16 (const int __imm)
{
  return __builtin_mve_vmvnq_n_uv8hi (__imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_n_u32 (const int __imm)
{
  return __builtin_mve_vmvnq_n_uv4si (__imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrev16q_u8 (uint8x16_t __a)
{
  return __builtin_mve_vrev16q_uv16qi (__a);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddlvq_u32 (uint32x4_t __a)
{
  return __builtin_mve_vaddlvq_uv4si (__a);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp16q (uint32_t __a)
{
  return __builtin_mve_vctp16qhi (__a);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp32q (uint32_t __a)
{
  return __builtin_mve_vctp32qhi (__a);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp64q (uint32_t __a)
{
  return __builtin_mve_vctp64qhi (__a);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp8q (uint32_t __a)
{
  return __builtin_mve_vctp8qhi (__a);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpnot (mve_pred16_t __a)
{
  return __builtin_mve_vpnothi (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_u8 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_uv16qi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_u16 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_uv8hi (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_u32 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_uv4si (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_u64 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_uv2di (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_s8 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_sv16qi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_s16 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_sv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_s32 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_sv4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_s64 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_sv2di (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshrq_n_s8 (int8x16_t __a, const int __imm)
{
  return __builtin_mve_vshrq_n_sv16qi (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshrq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vshrq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshrq_n_s32 (int32x4_t __a, const int __imm)
{
  return __builtin_mve_vshrq_n_sv4si (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshrq_n_u8 (uint8x16_t __a, const int __imm)
{
  return __builtin_mve_vshrq_n_uv16qi (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshrq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vshrq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshrq_n_u32 (uint32x4_t __a, const int __imm)
{
  return __builtin_mve_vshrq_n_uv4si (__a, __imm);
}
__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddlvq_p_s32 (int32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vaddlvq_p_sv4si (__a, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddlvq_p_u32 (uint32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vaddlvq_p_uv4si (__a, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcmpneq_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcmpneq_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcmpneq_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vcmpneq_uv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vcmpneq_uv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vcmpneq_uv4si (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vshlq_sv16qi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vshlq_sv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vshlq_sv4si (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vshlq_uv16qi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vshlq_uv8hi (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vshlq_uv4si (__a, __b);
}
__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vsubq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vsubq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmulhq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vrmulhq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrhaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vrhaddq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vqsubq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vqsubq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vqaddq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vqaddq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vorrq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vornq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmulq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vmulq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmulltq_int_uv16qi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmullbq_int_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulhq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmulhq_uv16qi (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmladavq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminvq_u8 (uint8_t __a, uint8x16_t __b)
{
  return __builtin_mve_vminvq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vminq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxvq_u8 (uint8_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmaxvq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmaxq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vhsubq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vhsubq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vhaddq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vhaddq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_veorq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_veorq_uv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vcmpneq_n_uv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmphiq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vcmphiq_uv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmphiq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vcmphiq_n_uv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vcmpeqq_uv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vcmpeqq_n_uv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpcsq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vcmpcsq_uv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpcsq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vcmpcsq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vcaddq_rot90_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vcaddq_rot270_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vbicq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vandq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vandq_uv16qi (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_p_u8 (uint8x16_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vaddvq_p_uv16qi (__a, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvaq_u8 (uint32_t __a, uint8x16_t __b)
{
  return __builtin_mve_vaddvaq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddq_n_u8 (uint8x16_t __a, uint8_t __b)
{
  return __builtin_mve_vaddq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabdq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vabdq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_r_u8 (uint8x16_t __a, int32_t __b)
{
  return __builtin_mve_vshlq_r_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vrshlq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_n_u8 (uint8x16_t __a, int32_t __b)
{
  return __builtin_mve_vrshlq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqshlq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_r_u8 (uint8x16_t __a, int32_t __b)
{
  return __builtin_mve_vqshlq_r_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_u8 (uint8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqrshlq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_n_u8 (uint8x16_t __a, int32_t __b)
{
  return __builtin_mve_vqrshlq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminavq_s8 (uint8_t __a, int8x16_t __b)
{
  return __builtin_mve_vminavq_sv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminaq_s8 (uint8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vminaq_sv16qi (__a, __b);
}

__extension__ extern __inline uint8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxavq_s8 (uint8_t __a, int8x16_t __b)
{
  return __builtin_mve_vmaxavq_sv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxaq_s8 (uint8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmaxaq_sv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_u8 (uint8x16_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_n_u8 (uint8x16_t __a, const int __imm)
{
  return __builtin_mve_vshlq_n_uv16qi (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshrq_n_u8 (uint8x16_t __a, const int __imm)
{
  return __builtin_mve_vrshrq_n_uv16qi (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_n_u8 (uint8x16_t __a, const int __imm)
{
  return __builtin_mve_vqshlq_n_uv16qi (__a, __imm);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vcmpneq_n_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcmpltq_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vcmpltq_n_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcmpleq_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vcmpleq_n_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcmpgtq_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vcmpgtq_n_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcmpgeq_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vcmpgeq_n_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcmpeqq_sv16qi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vcmpeqq_n_sv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_n_s8 (int8x16_t __a, const int __imm)
{
  return __builtin_mve_vqshluq_n_sv16qi (__a, __imm);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_p_s8 (int8x16_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vaddvq_p_sv16qi (__a, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vsubq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vsubq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_r_s8 (int8x16_t __a, int32_t __b)
{
  return __builtin_mve_vshlq_r_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vrshlq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_n_s8 (int8x16_t __a, int32_t __b)
{
  return __builtin_mve_vrshlq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmulhq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vrmulhq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrhaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vrhaddq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqsubq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vqsubq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqshlq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_r_s8 (int8x16_t __a, int32_t __b)
{
  return __builtin_mve_vqshlq_r_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqrshlq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_n_s8 (int8x16_t __a, int32_t __b)
{
  return __builtin_mve_vqrshlq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmulhq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqrdmulhq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmulhq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vqrdmulhq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulhq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqdmulhq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulhq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vqdmulhq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqaddq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vqaddq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vorrq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vornq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmulq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vmulq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmulltq_int_sv16qi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmullbq_int_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulhq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmulhq_sv16qi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmlsdavxq_sv16qi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmlsdavq_sv16qi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmladavxq_sv16qi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmladavq_sv16qi (__a, __b);
}

__extension__ extern __inline int8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminvq_s8 (int8_t __a, int8x16_t __b)
{
  return __builtin_mve_vminvq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vminq_sv16qi (__a, __b);
}

__extension__ extern __inline int8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxvq_s8 (int8_t __a, int8x16_t __b)
{
  return __builtin_mve_vmaxvq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vmaxq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vhsubq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vhsubq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vhcaddq_rot90_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vhcaddq_rot270_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vhaddq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vhaddq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_veorq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_veorq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcaddq_rot90_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcaddq_rot270_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_s8 (int8x16_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vbicq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vandq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vandq_sv16qi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvaq_s8 (int32_t __a, int8x16_t __b)
{
  return __builtin_mve_vaddvaq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddq_n_s8 (int8x16_t __a, int8_t __b)
{
  return __builtin_mve_vaddq_n_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabdq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vabdq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_n_s8 (int8x16_t __a, const int __imm)
{
  return __builtin_mve_vshlq_n_sv16qi (__a, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshrq_n_s8 (int8x16_t __a, const int __imm)
{
  return __builtin_mve_vrshrq_n_sv16qi (__a, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_n_s8 (int8x16_t __a, const int __imm)
{
  return __builtin_mve_vqshlq_n_sv16qi (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vsubq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vsubq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmulhq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vrmulhq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrhaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vrhaddq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vqsubq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vqsubq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vqaddq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vqaddq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vorrq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vornq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmulq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vmulq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmulltq_int_uv8hi (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmullbq_int_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulhq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmulhq_uv8hi (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmladavq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminvq_u16 (uint16_t __a, uint16x8_t __b)
{
  return __builtin_mve_vminvq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vminq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxvq_u16 (uint16_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmaxvq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmaxq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vhsubq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vhsubq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vhaddq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vhaddq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_veorq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_veorq_uv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vcmpneq_n_uv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmphiq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vcmphiq_uv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmphiq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vcmphiq_n_uv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vcmpeqq_uv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vcmpeqq_n_uv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpcsq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vcmpcsq_uv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpcsq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vcmpcsq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vcaddq_rot90_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vcaddq_rot270_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vbicq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vandq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vandq_uv8hi (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_p_u16 (uint16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vaddvq_p_uv8hi (__a, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvaq_u16 (uint32_t __a, uint16x8_t __b)
{
  return __builtin_mve_vaddvaq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddq_n_u16 (uint16x8_t __a, uint16_t __b)
{
  return __builtin_mve_vaddq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabdq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vabdq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_r_u16 (uint16x8_t __a, int32_t __b)
{
  return __builtin_mve_vshlq_r_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vrshlq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_n_u16 (uint16x8_t __a, int32_t __b)
{
  return __builtin_mve_vrshlq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqshlq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_r_u16 (uint16x8_t __a, int32_t __b)
{
  return __builtin_mve_vqshlq_r_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_u16 (uint16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqrshlq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_n_u16 (uint16x8_t __a, int32_t __b)
{
  return __builtin_mve_vqrshlq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminavq_s16 (uint16_t __a, int16x8_t __b)
{
  return __builtin_mve_vminavq_sv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminaq_s16 (uint16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vminaq_sv8hi (__a, __b);
}

__extension__ extern __inline uint16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxavq_s16 (uint16_t __a, int16x8_t __b)
{
  return __builtin_mve_vmaxavq_sv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxaq_s16 (uint16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmaxaq_sv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_u16 (uint16x8_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vshlq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshrq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vrshrq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vqshlq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vcmpneq_n_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcmpltq_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vcmpltq_n_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcmpleq_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vcmpleq_n_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcmpgtq_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vcmpgtq_n_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcmpgeq_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vcmpgeq_n_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcmpeqq_sv8hi (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vcmpeqq_n_sv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vqshluq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_p_s16 (int16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vaddvq_p_sv8hi (__a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vsubq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vsubq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_r_s16 (int16x8_t __a, int32_t __b)
{
  return __builtin_mve_vshlq_r_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vrshlq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_n_s16 (int16x8_t __a, int32_t __b)
{
  return __builtin_mve_vrshlq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vrmulhq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrhaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vrhaddq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqsubq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vqsubq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqshlq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_r_s16 (int16x8_t __a, int32_t __b)
{
  return __builtin_mve_vqshlq_r_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqrshlq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_n_s16 (int16x8_t __a, int32_t __b)
{
  return __builtin_mve_vqrshlq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqrdmulhq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmulhq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vqrdmulhq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqdmulhq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulhq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vqdmulhq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqaddq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vqaddq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vorrq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vornq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmulq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vmulq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmulltq_int_sv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmullbq_int_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulhq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmulhq_sv8hi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmlsdavxq_sv8hi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmlsdavq_sv8hi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmladavxq_sv8hi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmladavq_sv8hi (__a, __b);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminvq_s16 (int16_t __a, int16x8_t __b)
{
  return __builtin_mve_vminvq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vminq_sv8hi (__a, __b);
}

__extension__ extern __inline int16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxvq_s16 (int16_t __a, int16x8_t __b)
{
  return __builtin_mve_vmaxvq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmaxq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vhsubq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vhsubq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vhcaddq_rot90_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vhcaddq_rot270_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vhaddq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vhaddq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_veorq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_veorq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcaddq_rot90_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcaddq_rot270_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_s16 (int16x8_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vbicq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vandq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vandq_sv8hi (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvaq_s16 (int32_t __a, int16x8_t __b)
{
  return __builtin_mve_vaddvaq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vaddq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabdq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vabdq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vshlq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshrq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vrshrq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vqshlq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vsubq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vsubq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmulhq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vrmulhq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrhaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vrhaddq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vqsubq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vqsubq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vqaddq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vqaddq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vorrq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vornq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmulq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vmulq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmulltq_int_uv4si (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmullbq_int_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulhq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmulhq_uv4si (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmladavq_uv4si (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminvq_u32 (uint32_t __a, uint32x4_t __b)
{
  return __builtin_mve_vminvq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vminq_uv4si (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxvq_u32 (uint32_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmaxvq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmaxq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vhsubq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vhsubq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vhaddq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vhaddq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_veorq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_veorq_uv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vcmpneq_n_uv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmphiq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vcmphiq_uv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmphiq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vcmphiq_n_uv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vcmpeqq_uv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vcmpeqq_n_uv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpcsq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vcmpcsq_uv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpcsq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vcmpcsq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vcaddq_rot90_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vcaddq_rot270_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vbicq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vandq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vandq_uv4si (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_p_u32 (uint32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vaddvq_p_uv4si (__a, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvaq_u32 (uint32_t __a, uint32x4_t __b)
{
  return __builtin_mve_vaddvaq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddq_n_u32 (uint32x4_t __a, uint32_t __b)
{
  return __builtin_mve_vaddq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabdq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vabdq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_r_u32 (uint32x4_t __a, int32_t __b)
{
  return __builtin_mve_vshlq_r_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vrshlq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_n_u32 (uint32x4_t __a, int32_t __b)
{
  return __builtin_mve_vrshlq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqshlq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_r_u32 (uint32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqshlq_r_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_u32 (uint32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqrshlq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_n_u32 (uint32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqrshlq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminavq_s32 (uint32_t __a, int32x4_t __b)
{
  return __builtin_mve_vminavq_sv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminaq_s32 (uint32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vminaq_sv4si (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxavq_s32 (uint32_t __a, int32x4_t __b)
{
  return __builtin_mve_vmaxavq_sv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxaq_s32 (uint32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmaxaq_sv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_u32 (uint32x4_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_n_u32 (uint32x4_t __a, const int __imm)
{
  return __builtin_mve_vshlq_n_uv4si (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshrq_n_u32 (uint32x4_t __a, const int __imm)
{
  return __builtin_mve_vrshrq_n_uv4si (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_n_u32 (uint32x4_t __a, const int __imm)
{
  return __builtin_mve_vqshlq_n_uv4si (__a, __imm);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vcmpneq_n_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcmpltq_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vcmpltq_n_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcmpleq_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vcmpleq_n_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcmpgtq_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vcmpgtq_n_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcmpgeq_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vcmpgeq_n_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcmpeqq_sv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vcmpeqq_n_sv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_n_s32 (int32x4_t __a, const int __imm)
{
  return __builtin_mve_vqshluq_n_sv4si (__a, __imm);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvq_p_s32 (int32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vaddvq_p_sv4si (__a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vsubq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vsubq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_r_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vshlq_r_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vrshlq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshlq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vrshlq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vrmulhq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrhaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vrhaddq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqsubq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqsubq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqsubq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqshlq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_r_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqshlq_r_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqrshlq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrshlq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqrshlq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqrdmulhq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmulhq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqrdmulhq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqdmulhq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulhq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqdmulhq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqaddq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqaddq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqaddq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vorrq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vornq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmulq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vmulq_n_sv4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmulltq_int_sv4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmullbq_int_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmulhq_sv4si (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmlsdavxq_sv4si (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmlsdavq_sv4si (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmladavxq_sv4si (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmladavq_sv4si (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminvq_s32 (int32_t __a, int32x4_t __b)
{
  return __builtin_mve_vminvq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vminq_sv4si (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxvq_s32 (int32_t __a, int32x4_t __b)
{
  return __builtin_mve_vmaxvq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmaxq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vhsubq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhsubq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vhsubq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vhcaddq_rot90_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vhcaddq_rot270_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vhaddq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhaddq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vhaddq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_veorq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_veorq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcaddq_rot90_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcaddq_rot270_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vbicq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vandq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vandq_sv4si (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddvaq_s32 (int32_t __a, int32x4_t __b)
{
  return __builtin_mve_vaddvaq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vaddq_n_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabdq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vabdq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlq_n_s32 (int32x4_t __a, const int __imm)
{
  return __builtin_mve_vshlq_n_sv4si (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrshrq_n_s32 (int32x4_t __a, const int __imm)
{
  return __builtin_mve_vrshrq_n_sv4si (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshlq_n_s32 (int32x4_t __a, const int __imm)
{
  return __builtin_mve_vqshlq_n_sv4si (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovntq_u16 (uint8x16_t __a, uint16x8_t __b)
{
  return __builtin_mve_vqmovntq_uv8hi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovnbq_u16 (uint8x16_t __a, uint16x8_t __b)
{
  return __builtin_mve_vqmovnbq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_p8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmulltq_poly_pv16qi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_p8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmullbq_poly_pv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovntq_u16 (uint8x16_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmovntq_uv8hi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovnbq_u16 (uint8x16_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmovnbq_uv8hi (__a, __b);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmlaldavq_uv8hi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovuntq_s16 (uint8x16_t __a, int16x8_t __b)
{
  return __builtin_mve_vqmovuntq_sv8hi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovunbq_s16 (uint8x16_t __a, int16x8_t __b)
{
  return __builtin_mve_vqmovunbq_sv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlltq_n_u8 (uint8x16_t __a, const int __imm)
{
  return __builtin_mve_vshlltq_n_uv16qi (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshllbq_n_u8 (uint8x16_t __a, const int __imm)
{
  return __builtin_mve_vshllbq_n_uv16qi (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vorrq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovntq_s16 (int8x16_t __a, int16x8_t __b)
{
  return __builtin_mve_vqmovntq_sv8hi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovnbq_s16 (int8x16_t __a, int16x8_t __b)
{
  return __builtin_mve_vqmovnbq_sv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqdmulltq_sv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vqdmulltq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqdmullbq_sv8hi (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_n_s16 (int16x8_t __a, int16_t __b)
{
  return __builtin_mve_vqdmullbq_n_sv8hi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovntq_s16 (int8x16_t __a, int16x8_t __b)
{
  return __builtin_mve_vmovntq_sv8hi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovnbq_s16 (int8x16_t __a, int16x8_t __b)
{
  return __builtin_mve_vmovnbq_sv8hi (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavxq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmlsldavxq_sv8hi (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmlsldavq_sv8hi (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavxq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmlaldavxq_sv8hi (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vmlaldavq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlltq_n_s8 (int8x16_t __a, const int __imm)
{
  return __builtin_mve_vshlltq_n_sv16qi (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshllbq_n_s8 (int8x16_t __a, const int __imm)
{
  return __builtin_mve_vshllbq_n_sv16qi (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vorrq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovntq_u32 (uint16x8_t __a, uint32x4_t __b)
{
  return __builtin_mve_vqmovntq_uv4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovnbq_u32 (uint16x8_t __a, uint32x4_t __b)
{
  return __builtin_mve_vqmovnbq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_p16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmulltq_poly_pv8hi (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_p16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmullbq_poly_pv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovntq_u32 (uint16x8_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmovntq_uv4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovnbq_u32 (uint16x8_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmovnbq_uv4si (__a, __b);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmlaldavq_uv4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovuntq_s32 (uint16x8_t __a, int32x4_t __b)
{
  return __builtin_mve_vqmovuntq_sv4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovunbq_s32 (uint16x8_t __a, int32x4_t __b)
{
  return __builtin_mve_vqmovunbq_sv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlltq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vshlltq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshllbq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vshllbq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_n_u32 (uint32x4_t __a, const int __imm)
{
  return __builtin_mve_vorrq_n_uv4si (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_u32 (uint32x4_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_uv4si (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovntq_s32 (int16x8_t __a, int32x4_t __b)
{
  return __builtin_mve_vqmovntq_sv4si (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqmovnbq_s32 (int16x8_t __a, int32x4_t __b)
{
  return __builtin_mve_vqmovnbq_sv4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqdmulltq_sv4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqdmulltq_n_sv4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqdmullbq_sv4si (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_n_s32 (int32x4_t __a, int32_t __b)
{
  return __builtin_mve_vqdmullbq_n_sv4si (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovntq_s32 (int16x8_t __a, int32x4_t __b)
{
  return __builtin_mve_vmovntq_sv4si (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmovnbq_s32 (int16x8_t __a, int32x4_t __b)
{
  return __builtin_mve_vmovnbq_sv4si (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmlsldavxq_sv4si (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmlsldavq_sv4si (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmlaldavxq_sv4si (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vmlaldavq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlltq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vshlltq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshllbq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vshllbq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_n_s32 (int32x4_t __a, const int __imm)
{
  return __builtin_mve_vorrq_n_sv4si (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_s32 (int32x4_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_sv4si (__a, __imm);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vrmlaldavhq_uv4si (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp8q_m (uint32_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vctp8q_mhi (__a, __p);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp64q_m (uint32_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vctp64q_mhi (__a, __p);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp32q_m (uint32_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vctp32q_mhi (__a, __p);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp16q_m (uint32_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vctp16q_mhi (__a, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddlvaq_u32 (uint64_t __a, uint32x4_t __b)
{
  return __builtin_mve_vaddlvaq_uv4si (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vrmlsldavhxq_sv4si (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vrmlsldavhq_sv4si (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vrmlaldavhxq_sv4si (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vrmlaldavhq_sv4si (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddlvaq_s32 (int64_t __a, int32x4_t __b)
{
  return __builtin_mve_vaddlvaq_sv4si (__a, __b);
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

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_s16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtq_from_f_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_s32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtq_from_f_sv4si (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_u16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtq_from_f_uv8hi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_u32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtq_from_f_uv4si (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_u16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtpq_uv8hi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_u32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtpq_uv4si (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_u16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtnq_uv8hi (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_u16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtmq_uv8hi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_u32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtmq_uv4si (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_u16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtaq_uv8hi (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_u32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtaq_uv4si (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_s16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtaq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_s32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtaq_sv4si (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_s16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtnq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_s32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtnq_sv4si (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_s16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtpq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_s32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtpq_sv4si (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_s16_f16 (float16x8_t __a)
{
  return __builtin_mve_vcvtmq_sv8hi (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_s32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtmq_sv4si (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_mve_vsubq_n_fv8hf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_n_f32 (float32x4_t __a, float32_t __b)
{
  return __builtin_mve_vsubq_n_fv4sf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_f16 (float16x8_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_fv8hf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_f32 (float32x4_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_fv4sf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n_f16_s16 (int16x8_t __a, const int __imm6)
{
  return __builtin_mve_vcvtq_n_to_f_sv8hf (__a, __imm6);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n_f32_s32 (int32x4_t __a, const int __imm6)
{
  return __builtin_mve_vcvtq_n_to_f_sv4sf (__a, __imm6);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n_f16_u16 (uint16x8_t __a, const int __imm6)
{
  return __builtin_mve_vcvtq_n_to_f_uv8hf (__a, __imm6);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n_f32_u32 (uint32x4_t __a, const int __imm6)
{
  return __builtin_mve_vcvtq_n_to_f_uv4sf (__a, __imm6);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_f16 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_fv8hf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcreateq_f32 (uint64_t __a, uint64_t __b)
{
  return __builtin_mve_vcreateq_fv4sf (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n_s16_f16 (float16x8_t __a, const int __imm6)
{
  return __builtin_mve_vcvtq_n_from_f_sv8hi (__a, __imm6);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n_s32_f32 (float32x4_t __a, const int __imm6)
{
  return __builtin_mve_vcvtq_n_from_f_sv4si (__a, __imm6);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n_u16_f16 (float16x8_t __a, const int __imm6)
{
  return __builtin_mve_vcvtq_n_from_f_uv8hi (__a, __imm6);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n_u32_f32 (float32x4_t __a, const int __imm6)
{
  return __builtin_mve_vcvtq_n_from_f_uv4si (__a, __imm6);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_mve_vcmpneq_n_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmpneq_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_mve_vcmpltq_n_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmpltq_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_mve_vcmpleq_n_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmpleq_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_mve_vcmpgtq_n_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmpgtq_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_mve_vcmpgeq_n_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmpgeq_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_mve_vcmpeqq_n_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmpeqq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vsubq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vorrq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vornq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_mve_vmulq_n_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vmulq_fv8hf (__a, __b);
}

__extension__ extern __inline float16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminnmvq_f16 (float16_t __a, float16x8_t __b)
{
  return __builtin_mve_vminnmvq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminnmq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vminnmq_fv8hf (__a, __b);
}

__extension__ extern __inline float16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminnmavq_f16 (float16_t __a, float16x8_t __b)
{
  return __builtin_mve_vminnmavq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminnmaq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vminnmaq_fv8hf (__a, __b);
}

__extension__ extern __inline float16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxnmvq_f16 (float16_t __a, float16x8_t __b)
{
  return __builtin_mve_vmaxnmvq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxnmq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vmaxnmq_fv8hf (__a, __b);
}

__extension__ extern __inline float16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxnmavq_f16 (float16_t __a, float16x8_t __b)
{
  return __builtin_mve_vmaxnmavq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxnmaq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vmaxnmaq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_veorq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_veorq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmulq_rot90_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmulq_rot270_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmulq_rot180_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmulq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcaddq_rot90_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcaddq_rot270_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vbicq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vandq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vandq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddq_n_f16 (float16x8_t __a, float16_t __b)
{
  return __builtin_mve_vaddq_n_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabdq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vabdq_fv8hf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_n_f32 (float32x4_t __a, float32_t __b)
{
  return __builtin_mve_vcmpneq_n_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpneq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmpneq_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_n_f32 (float32x4_t __a, float32_t __b)
{
  return __builtin_mve_vcmpltq_n_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpltq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmpltq_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_n_f32 (float32x4_t __a, float32_t __b)
{
  return __builtin_mve_vcmpleq_n_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpleq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmpleq_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_n_f32 (float32x4_t __a, float32_t __b)
{
  return __builtin_mve_vcmpgtq_n_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgtq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmpgtq_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_n_f32 (float32x4_t __a, float32_t __b)
{
  return __builtin_mve_vcmpgeq_n_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpgeq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmpgeq_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_n_f32 (float32x4_t __a, float32_t __b)
{
  return __builtin_mve_vcmpeqq_n_fv4sf (__a, __b);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmpeqq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmpeqq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsubq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vsubq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vorrq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vorrq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vornq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_n_f32 (float32x4_t __a, float32_t __b)
{
  return __builtin_mve_vmulq_n_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vmulq_fv4sf (__a, __b);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminnmvq_f32 (float32_t __a, float32x4_t __b)
{
  return __builtin_mve_vminnmvq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminnmq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vminnmq_fv4sf (__a, __b);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminnmavq_f32 (float32_t __a, float32x4_t __b)
{
  return __builtin_mve_vminnmavq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vminnmaq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vminnmaq_fv4sf (__a, __b);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxnmvq_f32 (float32_t __a, float32x4_t __b)
{
  return __builtin_mve_vmaxnmvq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxnmq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vmaxnmq_fv4sf (__a, __b);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxnmavq_f32 (float32_t __a, float32x4_t __b)
{
  return __builtin_mve_vmaxnmavq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmaxnmaq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vmaxnmaq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_veorq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_veorq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmulq_rot90_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmulq_rot270_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmulq_rot180_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmulq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcaddq_rot90_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcaddq_rot270_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vbicq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vandq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vandq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vaddq_n_f32 (float32x4_t __a, float32_t __b)
{
  return __builtin_mve_vaddq_n_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabdq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vabdq_fv4sf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvttq_f16_f32 (float16x8_t __a, float32x4_t __b)
{
  return __builtin_mve_vcvttq_f16_f32v8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtbq_f16_f32 (float16x8_t __a, float32x4_t __b)
{
  return __builtin_mve_vcvtbq_f16_f32v8hf (__a, __b);
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
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vrev64q_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vrev64q_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vrev64q_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vrev64q_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vrev64q_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vrev64q_u32 (__ARM_mve_coerce(__p0, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrev64q_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vrev64q_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vnegq(p0) __arm_vnegq(p0)
#define __arm_vnegq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vnegq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vnegq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vnegq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vnegq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vnegq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vdupq_n(p0) __arm_vdupq_n(p0)
#define __arm_vdupq_n(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vdupq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vdupq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vabsq(p0) __arm_vabsq(p0)
#define __arm_vabsq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vabsq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vabsq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vabsq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vabsq_f16 (__ARM_mve_coerce(__p0, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vabsq_f32 (__ARM_mve_coerce(__p0, float32x4_t)));})

#define vrev32q(p0) __arm_vrev32q(p0)
#define __arm_vrev32q(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vrev32q_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vrev32q_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vrev32q_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vrev32q_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vrev32q_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define vcvtbq_f32(p0) __arm_vcvtbq_f32(p0)
#define __arm_vcvtbq_f32(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vcvtbq_f32_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define vcvttq_f32(p0) __arm_vcvttq_f32(p0)
#define __arm_vcvttq_f32(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vcvttq_f32_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define vrev16q(p0) __arm_vrev16q(p0)
#define __arm_vrev16q(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vrev16q_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vrev16q_u8 (__ARM_mve_coerce(__p0, uint8x16_t)));})

#define vqabsq(p0) __arm_vqabsq(p0)
#define __arm_vqabsq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqabsq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqabsq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqabsq_s32 (__ARM_mve_coerce(__p0, int32x4_t)));})

#define vqnegq(p0) __arm_vqnegq(p0)
#define __arm_vqnegq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqnegq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqnegq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqnegq_s32 (__ARM_mve_coerce(__p0, int32x4_t)));})

#define vmvnq(p0) __arm_vmvnq(p0)
#define __arm_vmvnq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vmvnq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vmvnq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vmvnq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vmvnq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vmvnq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vmvnq_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define vmovlbq(p0) __arm_vmovlbq(p0)
#define __arm_vmovlbq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vmovlbq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vmovlbq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vmovlbq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vmovlbq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)));})

#define vmovltq(p0) __arm_vmovltq(p0)
#define __arm_vmovltq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vmovltq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vmovltq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vmovltq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vmovltq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)));})

#define vclzq(p0) __arm_vclzq(p0)
#define __arm_vclzq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vclzq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vclzq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vclzq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vclzq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vclzq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vclzq_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define vclsq(p0) __arm_vclsq(p0)
#define __arm_vclsq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vclsq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vclsq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vclsq_s32 (__ARM_mve_coerce(__p0, int32x4_t)));})

#define vcvtq(p0) __arm_vcvtq(p0)
#define __arm_vcvtq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vcvtq_f16_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vcvtq_f32_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vcvtq_f16_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vcvtq_f32_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define vsubq(p0,p1) __arm_vsubq(p0,p1)
#define __arm_vsubq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vsubq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vsubq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vsubq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vsubq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vsubq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vsubq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vsubq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vsubq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vsubq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vsubq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsubq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vsubq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vsubq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsubq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vsubq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vsubq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vshlq(p0,p1) __arm_vshlq(p0,p1)
#define __arm_vshlq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vshlq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vshlq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vshlq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vshlq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vshlq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vshlq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vshrq(p0,p1) __arm_vshrq(p0,p1)
#define __arm_vshrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshrq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshrq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshrq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshrq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshrq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshrq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vcvtq_n(p0,p1) __arm_vcvtq_n(p0,p1)
#define __arm_vcvtq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vcvtq_n_f16_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vcvtq_n_f32_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vcvtq_n_f16_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vcvtq_n_f32_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vorrq(p0,p1) __arm_vorrq(p0,p1)
#define __arm_vorrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vorrq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vorrq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vorrq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vorrq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vorrq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vorrq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vorrq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vorrq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vabdq(p0,p1) __arm_vabdq(p0,p1)
#define __arm_vabdq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vabdq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vabdq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vabdq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vabdq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vabdq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vabdq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vabdq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vabdq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vaddq(p0,p1) __arm_vaddq(p0,p1)
#define __arm_vaddq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vaddq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vaddq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vaddq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vaddq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vaddq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vaddq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vaddq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vaddq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vaddq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vaddq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vaddq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vaddq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vaddq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vaddq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)));})

#define vandq(p0,p1) __arm_vandq(p0,p1)
#define __arm_vandq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vandq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vandq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vandq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vandq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vandq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vandq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vandq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vandq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vbicq(p0,p1) __arm_vbicq(p0,p1)
#define __arm_vbicq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbicq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbicq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbicq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbicq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbicq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbicq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vbicq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vbicq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vornq(p0,p1) __arm_vornq(p0,p1)
#define __arm_vornq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vornq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vornq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vornq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vornq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vornq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vornq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vornq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vornq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vmulq_n(p0,p1) __arm_vmulq_n(p0,p1)
#define __arm_vmulq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vmulq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vmulq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vmulq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vmulq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vmulq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vmulq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vmulq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vmulq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)));})

#define vmulq(p0,p1) __arm_vmulq(p0,p1)
#define __arm_vmulq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vmulq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vmulq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcaddq_rot270(p0,p1) __arm_vcaddq_rot270(p0,p1)
#define __arm_vcaddq_rot270(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot270_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot270_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot270_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot270_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot270_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot270_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcaddq_rot270_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcaddq_rot270_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcmpeqq(p0,p1) __arm_vcmpeqq(p0,p1)
#define __arm_vcmpeqq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpeqq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpeqq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpeqq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vcmpeqq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vcmpeqq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vcmpeqq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vcmpeqq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vcmpeqq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpeqq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpeqq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpeqq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcmpeqq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcmpeqq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcmpeqq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmpeqq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmpeqq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcaddq_rot90(p0,p1) __arm_vcaddq_rot90(p0,p1)
#define __arm_vcaddq_rot90(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot90_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot90_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot90_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot90_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot90_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot90_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcaddq_rot90_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcaddq_rot90_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcmpgeq_n(p0,p1) __arm_vcmpgeq_n(p0,p1)
#define __arm_vcmpgeq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpgeq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpgeq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpgeq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vcmpgeq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vcmpgeq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)));})

#define vcmpgeq(p0,p1) __arm_vcmpgeq(p0,p1)
#define __arm_vcmpgeq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpgeq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpgeq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpgeq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmpgeq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmpgeq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcmpgtq_n(p0,p1) __arm_vcmpgtq_n(p0,p1)
#define __arm_vcmpgtq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpgtq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpgtq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpgtq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vcmpgtq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vcmpgtq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)));})

#define vcmpgtq(p0,p1) __arm_vcmpgtq(p0,p1)
#define __arm_vcmpgtq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpgtq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpgtq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpgtq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmpgtq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmpgtq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcmpleq(p0,p1) __arm_vcmpleq(p0,p1)
#define __arm_vcmpleq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpleq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpleq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpleq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmpleq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmpleq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpleq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpleq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpleq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vcmpleq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vcmpleq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)));})

#define vcmpltq(p0,p1) __arm_vcmpltq(p0,p1)
#define __arm_vcmpltq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpltq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpltq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpltq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpltq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpltq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpltq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmpltq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmpltq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vcmpltq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vcmpltq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)));})

#define vcmpneq(p0,p1) __arm_vcmpneq(p0,p1)
#define __arm_vcmpneq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpneq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpneq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpneq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vcmpneq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vcmpneq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vcmpneq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vcmpneq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vcmpneq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpneq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpneq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpneq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcmpneq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcmpneq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcmpneq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmpneq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmpneq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcmulq(p0,p1) __arm_vcmulq(p0,p1)
#define __arm_vcmulq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcmulq_rot180(p0,p1) __arm_vcmulq_rot180(p0,p1)
#define __arm_vcmulq_rot180(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot180_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot180_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcmulq_rot270(p0,p1) __arm_vcmulq_rot270(p0,p1)
#define __arm_vcmulq_rot270(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot270_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot270_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcmulq_rot90(p0,p1) __arm_vcmulq_rot90(p0,p1)
#define __arm_vcmulq_rot90(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot90_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot90_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define veorq(p0,p1) __arm_veorq(p0,p1)
#define __arm_veorq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_veorq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_veorq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_veorq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_veorq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_veorq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_veorq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_veorq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_veorq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vmaxnmaq(p0,p1) __arm_vmaxnmaq(p0,p1)
#define __arm_vmaxnmaq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vmaxnmaq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vmaxnmaq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vmaxnmavq(p0,p1) __arm_vmaxnmavq(p0,p1)
#define __arm_vmaxnmavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16_t][__ARM_mve_type_float16x8_t]: __arm_vmaxnmavq_f16 (__ARM_mve_coerce(__p0, float16_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32_t][__ARM_mve_type_float32x4_t]: __arm_vmaxnmavq_f32 (__ARM_mve_coerce(__p0, float32_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vmaxnmq(p0,p1) __arm_vmaxnmq(p0,p1)
#define __arm_vmaxnmq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vmaxnmq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vmaxnmq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vmaxnmvq(p0,p1) __arm_vmaxnmvq(p0,p1)
#define __arm_vmaxnmvq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16_t][__ARM_mve_type_float16x8_t]: __arm_vmaxnmvq_f16 (__ARM_mve_coerce(__p0, float16_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32_t][__ARM_mve_type_float32x4_t]: __arm_vmaxnmvq_f32 (__ARM_mve_coerce(__p0, float32_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vmaxnmvq(p0,p1) __arm_vmaxnmvq(p0,p1)
#define __arm_vmaxnmvq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16_t][__ARM_mve_type_float16x8_t]: __arm_vmaxnmvq_f16 (__ARM_mve_coerce(__p0, float16_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32_t][__ARM_mve_type_float32x4_t]: __arm_vmaxnmvq_f32 (__ARM_mve_coerce(__p0, float32_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vminnmaq(p0,p1) __arm_vminnmaq(p0,p1)
#define __arm_vminnmaq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vminnmaq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vminnmaq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vminnmavq(p0,p1) __arm_vminnmavq(p0,p1)
#define __arm_vminnmavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16_t][__ARM_mve_type_float16x8_t]: __arm_vminnmavq_f16 (__ARM_mve_coerce(__p0, float16_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32_t][__ARM_mve_type_float32x4_t]: __arm_vminnmavq_f32 (__ARM_mve_coerce(__p0, float32_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vbrsrq(p0,p1) __arm_vbrsrq(p0,p1)
#define __arm_vbrsrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vbrsrq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vbrsrq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vbrsrq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vbrsrq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vbrsrq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vbrsrq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1), \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vbrsrq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), p1), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vbrsrq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), p1));})

#define vminnmq(p0,p1) __arm_vminnmq(p0,p1)
#define __arm_vminnmq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vminnmq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vminnmq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vminnmvq(p0,p1) __arm_vminnmvq(p0,p1)
#define __arm_vminnmvq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16_t][__ARM_mve_type_float16x8_t]: __arm_vminnmvq_f16 (__ARM_mve_coerce(__p0, float16_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32_t][__ARM_mve_type_float32x4_t]: __arm_vminnmvq_f32 (__ARM_mve_coerce(__p0, float32_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define vcmpgeq(p0,p1) __arm_vcmpgeq(p0,p1)
#define __arm_vcmpgeq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpgeq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpgeq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpgeq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpgeq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpgeq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpgeq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmpgeq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmpgeq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vcmpgeq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vcmpgeq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)));})

#define vshlq_r(p0,p1) __arm_vshlq_r(p0,p1)
#define __arm_vshlq_r(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlq_r_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlq_r_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshlq_r_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlq_r_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlq_r_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshlq_r_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vshlq_n(p0,p1) __arm_vshlq_n(p0,p1)
#define __arm_vshlq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshlq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshlq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vshlltq(p0,p1) __arm_vshlltq(p0,p1)
#define __arm_vshlltq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlltq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlltq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlltq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlltq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1));})

#define vshllbq(p0,p1) __arm_vshllbq(p0,p1)
#define __arm_vshllbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshllbq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshllbq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshllbq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshllbq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1));})

#define vrshrq(p0,p1) __arm_vrshrq(p0,p1)
#define __arm_vrshrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vrshrq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vrshrq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vrshrq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vrshrq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vrshrq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vrshrq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vrshrq(p0,p1) __arm_vrshrq(p0,p1)
#define __arm_vrshrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vrshrq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vrshrq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vrshrq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vrshrq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vrshrq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vrshrq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vrshlq(p0,p1) __arm_vrshlq(p0,p1)
#define __arm_vrshlq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vrshlq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vrshlq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrshlq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vrshlq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vrshlq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrshlq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vrmulhq(p0,p1) __arm_vrmulhq(p0,p1)
#define __arm_vrmulhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vrmulhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vrmulhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrmulhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vrmulhq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vrmulhq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vrmulhq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vrhaddq(p0,p1) __arm_vrhaddq(p0,p1)
#define __arm_vrhaddq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vrhaddq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vrhaddq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrhaddq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vrhaddq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vrhaddq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vrhaddq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vqsubq(p0,p1) __arm_vqsubq(p0,p1)
#define __arm_vqsubq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vqsubq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqsubq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqsubq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vqsubq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vqsubq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vqsubq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqsubq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqsubq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqsubq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vqsubq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vqsubq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vqsubq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vqshluq(p0,p1) __arm_vqshluq(p0,p1)
#define __arm_vqshluq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqshluq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqshluq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqshluq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1));})

#define vqshlq(p0,p1) __arm_vqshlq(p0,p1)
#define __arm_vqshlq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqshlq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqshlq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqshlq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqshlq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqshlq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqshlq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqshlq_r(p0,p1) __arm_vqshlq_r(p0,p1)
#define __arm_vqshlq_r(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqshlq_r_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqshlq_r_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqshlq_r_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vqshlq_r_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vqshlq_r_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vqshlq_r_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vqshlq_n(p0,p1) __arm_vqshlq_n(p0,p1)
#define __arm_vqshlq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqshlq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqshlq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqshlq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vqshlq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vqshlq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vqshlq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vqrshlq(p0,p1) __arm_vqrshlq(p0,p1)
#define __arm_vqrshlq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrshlq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrshlq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrshlq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrshlq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrshlq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrshlq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vqrdmulhq(p0,p1) __arm_vqrdmulhq(p0,p1)
#define __arm_vqrdmulhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmulhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmulhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmulhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vqrdmulhq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqrdmulhq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqrdmulhq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vmlaldavxq(p0,p1) __arm_vmlaldavxq(p0,p1)
#define __arm_vmlaldavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqmovuntq(p0,p1) __arm_vqmovuntq(p0,p1)
#define __arm_vqmovuntq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int16x8_t]: __arm_vqmovuntq_s16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int32x4_t]: __arm_vqmovuntq_s32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqmovntq(p0,p1) __arm_vqmovntq(p0,p1)
#define __arm_vqmovntq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int16x8_t]: __arm_vqmovntq_s16 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32x4_t]: __arm_vqmovntq_s32 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint16x8_t]: __arm_vqmovntq_u16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32x4_t]: __arm_vqmovntq_u32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vqmovnbq(p0,p1) __arm_vqmovnbq(p0,p1)
#define __arm_vqmovnbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int16x8_t]: __arm_vqmovnbq_s16 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32x4_t]: __arm_vqmovnbq_s32 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint16x8_t]: __arm_vqmovnbq_u16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32x4_t]: __arm_vqmovnbq_u32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vqdmulltq(p0,p1) __arm_vqdmulltq(p0,p1)
#define __arm_vqdmulltq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqdmulltq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqdmulltq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmulltq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmulltq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqmovunbq(p0,p1) __arm_vqmovunbq(p0,p1)
#define __arm_vqmovunbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int16x8_t]: __arm_vqmovunbq_s16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int32x4_t]: __arm_vqmovunbq_s32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqdmullbq(p0,p1) __arm_vqdmullbq(p0,p1)
#define __arm_vqdmullbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqdmullbq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqdmullbq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmullbq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmullbq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqdmulhq(p0,p1) __arm_vqdmulhq(p0,p1)
#define __arm_vqdmulhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vqdmulhq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqdmulhq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqdmulhq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmulhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmulhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmulhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqaddq(p0,p1) __arm_vqaddq(p0,p1)
#define __arm_vqaddq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vqaddq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqaddq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqaddq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vqaddq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vqaddq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vqaddq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqaddq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqaddq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqaddq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vqaddq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vqaddq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vqaddq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmulltq_poly(p0,p1) __arm_vmulltq_poly(p0,p1)
#define __arm_vmulltq_poly(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_poly_p8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_poly_p16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define vmullbq_poly(p0,p1) __arm_vmullbq_poly(p0,p1)
#define __arm_vmullbq_poly(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_poly_p8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_poly_p16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define vmulltq_int(p0,p1) __arm_vmulltq_int(p0,p1)
#define __arm_vmulltq_int(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulltq_int_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulltq_int_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulltq_int_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_int_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_int_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulltq_int_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vhaddq(p0,p1) __arm_vhaddq(p0,p1)
#define __arm_vhaddq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vhaddq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vhaddq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vhaddq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vhaddq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vhaddq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vhaddq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhaddq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhaddq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhaddq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vhaddq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vhaddq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vhaddq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vhcaddq_rot270(p0,p1) __arm_vhcaddq_rot270(p0,p1)
#define __arm_vhcaddq_rot270(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot270_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot270_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot270_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vhcaddq_rot90(p0,p1) __arm_vhcaddq_rot90(p0,p1)
#define __arm_vhcaddq_rot90(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot90_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot90_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot90_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vhsubq(p0,p1) __arm_vhsubq(p0,p1)
#define __arm_vhsubq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vhsubq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vhsubq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vhsubq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vhsubq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vhsubq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vhsubq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhsubq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhsubq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhsubq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vhsubq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vhsubq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vhsubq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vminq(p0,p1) __arm_vminq(p0,p1)
#define __arm_vminq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vminq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vminq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vminq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vminq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vminq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vminq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vminaq(p0,p1) __arm_vminaq(p0,p1)
#define __arm_vminaq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vminaq_s8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vminaq_s16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vminaq_s32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vmaxq(p0,p1) __arm_vmaxq(p0,p1)
#define __arm_vmaxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmaxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmaxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmaxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmaxq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmaxq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmaxq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmaxaq(p0,p1) __arm_vmaxaq(p0,p1)
#define __arm_vmaxaq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmaxaq_s8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmaxaq_s16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmaxaq_s32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vmovntq(p0,p1) __arm_vmovntq(p0,p1)
#define __arm_vmovntq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int16x8_t]: __arm_vmovntq_s16 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32x4_t]: __arm_vmovntq_s32 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint16x8_t]: __arm_vmovntq_u16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32x4_t]: __arm_vmovntq_u32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmovnbq(p0,p1) __arm_vmovnbq(p0,p1)
#define __arm_vmovnbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int16x8_t]: __arm_vmovnbq_s16 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32x4_t]: __arm_vmovnbq_s32 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint16x8_t]: __arm_vmovnbq_u16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32x4_t]: __arm_vmovnbq_u32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmulhq(p0,p1) __arm_vmulhq(p0,p1)
#define __arm_vmulhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulhq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulhq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulhq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmullbq_int(p0,p1) __arm_vmullbq_int(p0,p1)
#define __arm_vmullbq_int(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmullbq_int_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmullbq_int_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmullbq_int_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_int_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_int_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmullbq_int_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vcmpgtq(p0,p1) __arm_vcmpgtq(p0,p1)
#define __arm_vcmpgtq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpgtq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpgtq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpgtq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpgtq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpgtq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpgtq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmpgtq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmpgtq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16_t]: __arm_vcmpgtq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32_t]: __arm_vcmpgtq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32_t)));})

#else /* MVE Interger.  srinath*/

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

#define vabsq(p0) __arm_vabsq(p0)
#define __arm_vabsq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vabsq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vabsq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vabsq_s32 (__ARM_mve_coerce(__p0, int32x4_t)));})

#define vclsq(p0) __arm_vclsq(p0)
#define __arm_vclsq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vclsq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vclsq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vclsq_s32 (__ARM_mve_coerce(__p0, int32x4_t)));})

#define vclzq(p0) __arm_vclzq(p0)
#define __arm_vclzq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vclzq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vclzq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vclzq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vclzq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vclzq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vclzq_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define vnegq(p0) __arm_vnegq(p0)
#define __arm_vnegq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vnegq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vnegq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vnegq_s32 (__ARM_mve_coerce(__p0, int32x4_t)));})

#define vaddlvq(p0) __arm_vaddlvq(p0)
#define __arm_vaddlvq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vaddlvq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vaddlvq_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define vaddvq(p0) __arm_vaddvq(p0)
#define __arm_vaddvq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vaddvq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vaddvq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vaddvq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vaddvq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vaddvq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vaddvq_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define vmovlbq(p0) __arm_vmovlbq(p0)
#define __arm_vmovlbq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vmovlbq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vmovlbq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vmovlbq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vmovlbq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)));})

#define vmovltq(p0) __arm_vmovltq(p0)
#define __arm_vmovltq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vmovltq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vmovltq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vmovltq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vmovltq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)));})

#define vmvnq(p0) __arm_vmvnq(p0)
#define __arm_vmvnq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vmvnq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vmvnq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vmvnq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vmvnq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vmvnq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vmvnq_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define vrev16q(p0) __arm_vrev16q(p0)
#define __arm_vrev16q(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vrev16q_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vrev16q_u8 (__ARM_mve_coerce(__p0, uint8x16_t)));})

#define vrev32q(p0) __arm_vrev32q(p0)
#define __arm_vrev32q(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vrev32q_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vrev32q_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vrev32q_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vrev32q_u16 (__ARM_mve_coerce(__p0, uint16x8_t)));})

#define vrev64q(p0) __arm_vrev64q(p0)
#define __arm_vrev64q(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vrev64q_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vrev64q_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vrev64q_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vrev64q_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vrev64q_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vrev64q_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define vqabsq(p0) __arm_vqabsq(p0)
#define __arm_vqabsq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqabsq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqabsq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqabsq_s32 (__ARM_mve_coerce(__p0, int32x4_t)));})

#define vqnegq(p0) __arm_vqnegq(p0)
#define __arm_vqnegq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqnegq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqnegq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqnegq_s32 (__ARM_mve_coerce(__p0, int32x4_t)));})

#define vshrq(p0,p1) __arm_vshrq(p0,p1)
#define __arm_vshrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshrq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshrq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshrq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshrq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshrq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshrq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vaddlvq_p(p0,p1) __arm_vaddlvq_p(p0,p1)
#define __arm_vaddlvq_p(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vaddlvq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vaddlvq_p_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vcmpneq(p0,p1) __arm_vcmpneq(p0,p1)
#define __arm_vcmpneq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpneq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpneq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpneq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcmpneq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcmpneq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcmpneq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vshlq(p0,p1) __arm_vshlq(p0,p1)
#define __arm_vshlq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vshlq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vshlq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vshlq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vshlq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vshlq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vshlq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vsubq(p0,p1) __arm_vsubq(p0,p1)
#define __arm_vsubq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vsubq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vsubq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsubq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vsubq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vsubq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsubq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vsubq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vsubq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vsubq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vsubq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vsubq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vsubq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)));})

#define vshlq_r(p0,p1) __arm_vshlq_r(p0,p1)
#define __arm_vshlq_r(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlq_r_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlq_r_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshlq_r_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlq_r_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlq_r_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshlq_r_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vrshlq(p0,p1) __arm_vrshlq(p0,p1)
#define __arm_vrshlq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32_t]: __arm_vrshlq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vrshlq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vrshlq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrshlq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vrshlq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vrshlq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrshlq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vrmulhq(p0,p1) __arm_vrmulhq(p0,p1)
#define __arm_vrmulhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vrmulhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vrmulhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrmulhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vrmulhq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vrmulhq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vrmulhq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vrhaddq(p0,p1) __arm_vrhaddq(p0,p1)
#define __arm_vrhaddq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vrhaddq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vrhaddq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrhaddq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vrhaddq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vrhaddq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vrhaddq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vqsubq(p0,p1) __arm_vqsubq(p0,p1)
#define __arm_vqsubq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vqsubq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqsubq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqsubq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vqsubq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vqsubq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vqsubq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqsubq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqsubq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqsubq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vqsubq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vqsubq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vqsubq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vqshlq(p0,p1) __arm_vqshlq(p0,p1)
#define __arm_vqshlq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqshlq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqshlq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqshlq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqshlq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqshlq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqshlq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqshlq_r(p0,p1) __arm_vqshlq_r(p0,p1)
#define __arm_vqshlq_r(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqshlq_r_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqshlq_r_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqshlq_r_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vqshlq_r_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vqshlq_r_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vqshlq_r_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vqshluq(p0,p1) __arm_vqshluq(p0,p1)
#define __arm_vqshluq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqshluq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqshluq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqshluq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1));})

#define vrshrq(p0,p1) __arm_vrshrq(p0,p1)
#define __arm_vrshrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vrshrq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vrshrq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vrshrq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vrshrq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vrshrq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vrshrq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vshlq_n(p0,p1) __arm_vshlq_n(p0,p1)
#define __arm_vshlq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshlq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshlq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vqshlq_n(p0,p1) __arm_vqshlq_n(p0,p1)
#define __arm_vqshlq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqshlq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqshlq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqshlq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vqshlq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vqshlq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vqshlq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vqrshlq(p0,p1) __arm_vqrshlq(p0,p1)
#define __arm_vqrshlq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrshlq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrshlq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrshlq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrshlq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrshlq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrshlq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32_t]: __arm_vqrshlq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vqrdmulhq(p0,p1) __arm_vqrdmulhq(p0,p1)
#define __arm_vqrdmulhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmulhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmulhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmulhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vqrdmulhq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqrdmulhq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqrdmulhq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vqdmulhq(p0,p1) __arm_vqdmulhq(p0,p1)
#define __arm_vqdmulhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vqdmulhq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqdmulhq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqdmulhq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmulhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmulhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmulhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqaddq(p0,p1) __arm_vqaddq(p0,p1)
#define __arm_vqaddq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vqaddq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqaddq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqaddq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vqaddq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vqaddq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vqaddq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqaddq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqaddq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqaddq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vqaddq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vqaddq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vqaddq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vorrq(p0,p1) __arm_vorrq(p0,p1)
#define __arm_vorrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vorrq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vorrq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vorrq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vorrq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vorrq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vorrq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vornq(p0,p1) __arm_vornq(p0,p1)
#define __arm_vornq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vornq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vornq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vornq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vornq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vornq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vornq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmulq_n(p0,p1) __arm_vmulq_n(p0,p1)
#define __arm_vmulq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vmulq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vmulq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vmulq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vmulq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vmulq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vmulq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)));})

#define vmulq(p0,p1) __arm_vmulq(p0,p1)
#define __arm_vmulq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmulltq_int(p0,p1) __arm_vmulltq_int(p0,p1)
#define __arm_vmulltq_int(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulltq_int_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulltq_int_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulltq_int_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_int_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_int_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulltq_int_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmullbq_int(p0,p1) __arm_vmullbq_int(p0,p1)
#define __arm_vmullbq_int(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmullbq_int_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmullbq_int_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmullbq_int_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_int_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_int_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmullbq_int_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmulhq(p0,p1) __arm_vmulhq(p0,p1)
#define __arm_vmulhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulhq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulhq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulhq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vminq(p0,p1) __arm_vminq(p0,p1)
#define __arm_vminq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vminq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vminq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vminq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vminq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vminq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vminq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vminaq(p0,p1) __arm_vminaq(p0,p1)
#define __arm_vminaq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vminaq_s8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vminaq_s16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vminaq_s32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vmaxq(p0,p1) __arm_vmaxq(p0,p1)
#define __arm_vmaxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmaxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmaxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmaxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmaxq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmaxq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmaxq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmaxaq(p0,p1) __arm_vmaxaq(p0,p1)
#define __arm_vmaxaq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmaxaq_s8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmaxaq_s16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmaxaq_s32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vhsubq(p0,p1) __arm_vhsubq(p0,p1)
#define __arm_vhsubq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vhsubq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vhsubq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vhsubq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vhsubq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vhsubq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vhsubq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhsubq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhsubq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhsubq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vhsubq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vhsubq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vhsubq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vhcaddq_rot90(p0,p1) __arm_vhcaddq_rot90(p0,p1)
#define __arm_vhcaddq_rot90(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot90_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot90_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot90_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vhcaddq_rot270(p0,p1) __arm_vhcaddq_rot270(p0,p1)
#define __arm_vhcaddq_rot270(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot270_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot270_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot270_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vhaddq(p0,p1) __arm_vhaddq(p0,p1)
#define __arm_vhaddq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vhaddq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vhaddq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vhaddq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vhaddq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vhaddq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vhaddq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhaddq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhaddq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhaddq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vhaddq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vhaddq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vhaddq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define veorq(p0,p1) __arm_veorq(p0,p1)
#define __arm_veorq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_veorq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_veorq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_veorq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_veorq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_veorq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_veorq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vcaddq_rot90(p0,p1) __arm_vcaddq_rot90(p0,p1)
#define __arm_vcaddq_rot90(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot90_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot90_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot90_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot90_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot90_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot90_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vcaddq_rot270(p0,p1) __arm_vcaddq_rot270(p0,p1)
#define __arm_vcaddq_rot270(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot270_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot270_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot270_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot270_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot270_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot270_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vbrsrq(p0,p1) __arm_vbrsrq(p0,p1)
#define __arm_vbrsrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vbrsrq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vbrsrq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vbrsrq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vbrsrq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vbrsrq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vbrsrq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vbicq(p0,p1) __arm_vbicq(p0,p1)
#define __arm_vbicq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbicq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbicq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbicq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbicq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbicq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbicq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vaddq(p0,p1) __arm_vaddq(p0,p1)
#define __arm_vaddq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vaddq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vaddq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vaddq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vaddq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vaddq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vaddq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vaddq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vaddq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vaddq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vaddq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vaddq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vaddq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vandq(p0,p1) __arm_vandq(p0,p1)
#define __arm_vandq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vandq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vandq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vandq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vandq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vandq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vandq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vabdq(p0,p1) __arm_vabdq(p0,p1)
#define __arm_vabdq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vabdq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vabdq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vabdq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vabdq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vabdq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vabdq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vaddvaq(p0,p1) __arm_vaddvaq(p0,p1)
#define __arm_vaddvaq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32_t][__ARM_mve_type_int8x16_t]: __arm_vaddvaq_s8 (__ARM_mve_coerce(__p0, int32_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int32_t][__ARM_mve_type_int16x8_t]: __arm_vaddvaq_s16 (__ARM_mve_coerce(__p0, int32_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32_t][__ARM_mve_type_int32x4_t]: __arm_vaddvaq_s32 (__ARM_mve_coerce(__p0, int32_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t][__ARM_mve_type_uint8x16_t]: __arm_vaddvaq_u8 (__ARM_mve_coerce(__p0, uint32_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint32_t][__ARM_mve_type_uint16x8_t]: __arm_vaddvaq_u16 (__ARM_mve_coerce(__p0, uint32_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32_t][__ARM_mve_type_uint32x4_t]: __arm_vaddvaq_u32 (__ARM_mve_coerce(__p0, uint32_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vaddvq_p(p0,p1) __arm_vaddvq_p(p0,p1)
#define __arm_vaddvq_p(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vaddvq_p_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vaddvq_p_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vaddvq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vaddvq_p_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vaddvq_p_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vaddvq_p_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define vcmpcsq(p0,p1) __arm_vcmpcsq(p0,p1)
#define __arm_vcmpcsq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcmpcsq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcmpcsq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcmpcsq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vcmpcsq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vcmpcsq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vcmpcsq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)));})

#define vcmpeqq(p0,p1) __arm_vcmpeqq(p0,p1)
#define __arm_vcmpeqq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpeqq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpeqq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpeqq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcmpeqq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcmpeqq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcmpeqq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpeqq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpeqq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpeqq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vcmpeqq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vcmpeqq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vcmpeqq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)));})

#define vmlsdavxq(p0,p1) __arm_vmlsdavxq(p0,p1)
#define __arm_vmlsdavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vmlsdavq(p0,p1) __arm_vmlsdavq(p0,p1)
#define __arm_vmlsdavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vmladavxq(p0,p1) __arm_vmladavxq(p0,p1)
#define __arm_vmladavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmladavxq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmladavxq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmladavxq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmladavq(p0,p1) __arm_vmladavq(p0,p1)
#define __arm_vmladavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmladavq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmladavq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmladavq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vminvq(p0,p1) __arm_vminvq(p0,p1)
#define __arm_vminvq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t][__ARM_mve_type_int8x16_t]: __arm_vminvq_s8 (__ARM_mve_coerce(__p0, int8_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16_t][__ARM_mve_type_int16x8_t]: __arm_vminvq_s16 (__ARM_mve_coerce(__p0, int16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32_t][__ARM_mve_type_int32x4_t]: __arm_vminvq_s32 (__ARM_mve_coerce(__p0, int32_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t][__ARM_mve_type_uint8x16_t]: __arm_vminvq_u8 (__ARM_mve_coerce(__p0, uint8_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16_t][__ARM_mve_type_uint16x8_t]: __arm_vminvq_u16 (__ARM_mve_coerce(__p0, uint16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32_t][__ARM_mve_type_uint32x4_t]: __arm_vminvq_u32 (__ARM_mve_coerce(__p0, uint32_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vminavq(p0,p1) __arm_vminavq(p0,p1)
#define __arm_vminavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8_t][__ARM_mve_type_int8x16_t]: __arm_vminavq_s8 (__ARM_mve_coerce(__p0, uint8_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16_t][__ARM_mve_type_int16x8_t]: __arm_vminavq_s16 (__ARM_mve_coerce(__p0, uint16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32_t][__ARM_mve_type_int32x4_t]: __arm_vminavq_s32 (__ARM_mve_coerce(__p0, uint32_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vmaxvq(p0,p1) __arm_vmaxvq(p0,p1)
#define __arm_vmaxvq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t][__ARM_mve_type_int8x16_t]: __arm_vmaxvq_s8 (__ARM_mve_coerce(__p0, int8_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16_t][__ARM_mve_type_int16x8_t]: __arm_vmaxvq_s16 (__ARM_mve_coerce(__p0, int16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32_t][__ARM_mve_type_int32x4_t]: __arm_vmaxvq_s32 (__ARM_mve_coerce(__p0, int32_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t][__ARM_mve_type_uint8x16_t]: __arm_vmaxvq_u8 (__ARM_mve_coerce(__p0, uint8_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16_t][__ARM_mve_type_uint16x8_t]: __arm_vmaxvq_u16 (__ARM_mve_coerce(__p0, uint16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32_t][__ARM_mve_type_uint32x4_t]: __arm_vmaxvq_u32 (__ARM_mve_coerce(__p0, uint32_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmaxavq(p0,p1) __arm_vmaxavq(p0,p1)
#define __arm_vmaxavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8_t][__ARM_mve_type_int8x16_t]: __arm_vmaxavq_s8 (__ARM_mve_coerce(__p0, uint8_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_uint16_t][__ARM_mve_type_int16x8_t]: __arm_vmaxavq_s16 (__ARM_mve_coerce(__p0, uint16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint32_t][__ARM_mve_type_int32x4_t]: __arm_vmaxavq_s32 (__ARM_mve_coerce(__p0, uint32_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vcmpneq(p0,p1) __arm_vcmpneq(p0,p1)
#define __arm_vcmpneq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpneq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpneq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpneq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcmpneq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcmpneq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcmpneq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpneq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpneq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpneq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vcmpneq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vcmpneq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vcmpneq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)));})


#define vqmovntq(p0,p1) __arm_vqmovntq(p0,p1)
#define __arm_vqmovntq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int16x8_t]: __arm_vqmovntq_s16 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32x4_t]: __arm_vqmovntq_s32 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint16x8_t]: __arm_vqmovntq_u16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32x4_t]: __arm_vqmovntq_u32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vqmovnbq(p0,p1) __arm_vqmovnbq(p0,p1)
#define __arm_vqmovnbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int16x8_t]: __arm_vqmovnbq_s16 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32x4_t]: __arm_vqmovnbq_s32 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint16x8_t]: __arm_vqmovnbq_u16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32x4_t]: __arm_vqmovnbq_u32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmulltq_poly(p0,p1) __arm_vmulltq_poly(p0,p1)
#define __arm_vmulltq_poly(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_poly_p8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_poly_p16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define vmullbq_poly(p0,p1) __arm_vmullbq_poly(p0,p1)
#define __arm_vmullbq_poly(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_poly_p8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_poly_p16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define vmovntq(p0,p1) __arm_vmovntq(p0,p1)
#define __arm_vmovntq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int16x8_t]: __arm_vmovntq_s16 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32x4_t]: __arm_vmovntq_s32 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint16x8_t]: __arm_vmovntq_u16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32x4_t]: __arm_vmovntq_u32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmovnbq(p0,p1) __arm_vmovnbq(p0,p1)
#define __arm_vmovnbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int16x8_t]: __arm_vmovnbq_s16 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int32x4_t]: __arm_vmovnbq_s32 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint16x8_t]: __arm_vmovnbq_u16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32x4_t]: __arm_vmovnbq_u32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vmlaldavxq(p0,p1) __arm_vmlaldavxq(p0,p1)
#define __arm_vmlaldavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqmovuntq(p0,p1) __arm_vqmovuntq(p0,p1)
#define __arm_vqmovuntq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int16x8_t]: __arm_vqmovuntq_s16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int32x4_t]: __arm_vqmovuntq_s32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vshlltq(p0,p1) __arm_vshlltq(p0,p1)
#define __arm_vshlltq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlltq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlltq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlltq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlltq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1));})

#define vshllbq(p0,p1) __arm_vshllbq(p0,p1)
#define __arm_vshllbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshllbq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshllbq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshllbq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshllbq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1));})

#define vmlaldavq(p0,p1) __arm_vmlaldavq(p0,p1)
#define __arm_vmlaldavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmlaldavq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmlaldavq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vqmovunbq(p0,p1) __arm_vqmovunbq(p0,p1)
#define __arm_vqmovunbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int16x8_t]: __arm_vqmovunbq_s16 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int32x4_t]: __arm_vqmovunbq_s32 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqdmulltq(p0,p1) __arm_vqdmulltq(p0,p1)
#define __arm_vqdmulltq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqdmulltq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqdmulltq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmulltq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmulltq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vqdmullbq(p0,p1) __arm_vqdmullbq(p0,p1)
#define __arm_vqdmullbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vqdmullbq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vqdmullbq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmullbq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmullbq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vcmpgeq_n(p0,p1) __arm_vcmpgeq_n(p0,p1)
#define __arm_vcmpgeq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpgeq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpgeq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpgeq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vcmpgeq(p0,p1) __arm_vcmpgeq(p0,p1)
#define __arm_vcmpgeq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpgeq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpgeq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpgeq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpgeq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpgeq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpgeq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vcmpgtq(p0,p1) __arm_vcmpgtq(p0,p1)
#define __arm_vcmpgtq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpgtq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpgtq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpgtq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpgtq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpgtq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpgtq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vcmphiq(p0,p1) __arm_vcmphiq(p0,p1)
#define __arm_vcmphiq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcmphiq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcmphiq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcmphiq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8_t]: __arm_vcmphiq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16_t]: __arm_vcmphiq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t]: __arm_vcmphiq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t)));})

#define vcmpleq(p0,p1) __arm_vcmpleq(p0,p1)
#define __arm_vcmpleq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpleq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpleq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpleq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpleq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpleq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpleq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vcmpltq(p0,p1) __arm_vcmpltq(p0,p1)
#define __arm_vcmpltq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcmpltq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcmpltq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcmpltq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8_t]: __arm_vcmpltq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16_t]: __arm_vcmpltq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32_t]: __arm_vcmpltq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32_t)));})

#define vaddlvaq(p0,p1) __arm_vaddlvaq(p0,p1)
#define __arm_vaddlvaq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int64_t][__ARM_mve_type_int32x4_t]: __arm_vaddlvaq_s32 (__ARM_mve_coerce(__p0, int64_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint64_t][__ARM_mve_type_uint32x4_t]: __arm_vaddlvaq_u32 (__ARM_mve_coerce(__p0, uint64_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vrmlaldavhq(p0,p1) __arm_vrmlaldavhq(p0,p1)
#define __arm_vrmlaldavhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrmlaldavhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vrmlaldavhq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define vrmlaldavhxq(p0,p1) __arm_vrmlaldavhxq(p0,p1)
#define __arm_vrmlaldavhxq(p0,p1) __arm_vrmlaldavhxq_s32(p0,p1)

#define vrmlsldavhq(p0,p1) __arm_vrmlsldavhq(p0,p1)
#define __arm_vrmlsldavhq(p0,p1) __arm_vrmlsldavhq_s32(p0,p1)

#define vrmlsldavhq(p0,p1) __arm_vrmlsldavhq(p0,p1)
#define __arm_vrmlsldavhq(p0,p1) __arm_vrmlsldavhq_s32(p0,p1)

#define vrmlsldavhxq(p0,p1) __arm_vrmlsldavhxq(p0,p1)
#define __arm_vrmlsldavhxq(p0,p1) __arm_vrmlsldavhxq_s32(p0,p1)

#define vmlsldavxq(p0,p1) __arm_vmlsldavxq(p0,p1)
#define __arm_vmlsldavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define vmlsldavq(p0,p1) __arm_vmlsldavq(p0,p1)
#define __arm_vmlsldavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#endif /* MVE Floating point.  */

#ifdef __cplusplus
}
#endif

#endif /* _GCC_ARM_MVE_H.  */
