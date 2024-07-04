/* Arm MVE intrinsics include file.

   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#define vst4q(__addr, __value) __arm_vst4q(__addr, __value)
#define vornq(__a, __b) __arm_vornq(__a, __b)
#define vbicq(__a, __b) __arm_vbicq(__a, __b)
#define vbicq_m_n(__a, __imm, __p) __arm_vbicq_m_n(__a, __imm, __p)
#define vshlcq(__a, __b, __imm) __arm_vshlcq(__a, __b, __imm)
#define vbicq_m(__inactive, __a, __b, __p) __arm_vbicq_m(__inactive, __a, __b, __p)
#define vornq_m(__inactive, __a, __b, __p) __arm_vornq_m(__inactive, __a, __b, __p)
#define vstrbq_scatter_offset(__base, __offset, __value) __arm_vstrbq_scatter_offset(__base, __offset, __value)
#define vstrbq(__addr, __value) __arm_vstrbq(__addr, __value)
#define vstrwq_scatter_base(__addr, __offset, __value) __arm_vstrwq_scatter_base(__addr, __offset, __value)
#define vldrbq_gather_offset(__base, __offset) __arm_vldrbq_gather_offset(__base, __offset)
#define vstrbq_p(__addr, __value, __p) __arm_vstrbq_p(__addr, __value, __p)
#define vstrbq_scatter_offset_p(__base, __offset, __value, __p) __arm_vstrbq_scatter_offset_p(__base, __offset, __value, __p)
#define vstrwq_scatter_base_p(__addr, __offset, __value, __p) __arm_vstrwq_scatter_base_p(__addr, __offset, __value, __p)
#define vldrbq_gather_offset_z(__base, __offset, __p) __arm_vldrbq_gather_offset_z(__base, __offset, __p)
#define vldrhq_gather_offset(__base, __offset) __arm_vldrhq_gather_offset(__base, __offset)
#define vldrhq_gather_offset_z(__base, __offset, __p) __arm_vldrhq_gather_offset_z(__base, __offset, __p)
#define vldrhq_gather_shifted_offset(__base, __offset) __arm_vldrhq_gather_shifted_offset(__base, __offset)
#define vldrhq_gather_shifted_offset_z(__base, __offset, __p) __arm_vldrhq_gather_shifted_offset_z(__base, __offset, __p)
#define vldrdq_gather_offset(__base, __offset) __arm_vldrdq_gather_offset(__base, __offset)
#define vldrdq_gather_offset_z(__base, __offset, __p) __arm_vldrdq_gather_offset_z(__base, __offset, __p)
#define vldrdq_gather_shifted_offset(__base, __offset) __arm_vldrdq_gather_shifted_offset(__base, __offset)
#define vldrdq_gather_shifted_offset_z(__base, __offset, __p) __arm_vldrdq_gather_shifted_offset_z(__base, __offset, __p)
#define vldrwq_gather_offset(__base, __offset) __arm_vldrwq_gather_offset(__base, __offset)
#define vldrwq_gather_offset_z(__base, __offset, __p) __arm_vldrwq_gather_offset_z(__base, __offset, __p)
#define vldrwq_gather_shifted_offset(__base, __offset) __arm_vldrwq_gather_shifted_offset(__base, __offset)
#define vldrwq_gather_shifted_offset_z(__base, __offset, __p) __arm_vldrwq_gather_shifted_offset_z(__base, __offset, __p)
#define vstrhq_scatter_offset(__base, __offset, __value) __arm_vstrhq_scatter_offset(__base, __offset, __value)
#define vstrhq_scatter_offset_p(__base, __offset, __value, __p) __arm_vstrhq_scatter_offset_p(__base, __offset, __value, __p)
#define vstrhq_scatter_shifted_offset(__base, __offset, __value) __arm_vstrhq_scatter_shifted_offset(__base, __offset, __value)
#define vstrhq_scatter_shifted_offset_p(__base, __offset, __value, __p) __arm_vstrhq_scatter_shifted_offset_p(__base, __offset, __value, __p)
#define vstrhq(__addr, __value) __arm_vstrhq(__addr, __value)
#define vstrhq_p(__addr, __value, __p) __arm_vstrhq_p(__addr, __value, __p)
#define vstrwq(__addr, __value) __arm_vstrwq(__addr, __value)
#define vstrwq_p(__addr, __value, __p) __arm_vstrwq_p(__addr, __value, __p)
#define vstrdq_scatter_base_p(__addr, __offset, __value, __p) __arm_vstrdq_scatter_base_p(__addr, __offset, __value, __p)
#define vstrdq_scatter_base(__addr, __offset, __value) __arm_vstrdq_scatter_base(__addr, __offset, __value)
#define vstrdq_scatter_offset_p(__base, __offset, __value, __p) __arm_vstrdq_scatter_offset_p(__base, __offset, __value, __p)
#define vstrdq_scatter_offset(__base, __offset, __value) __arm_vstrdq_scatter_offset(__base, __offset, __value)
#define vstrdq_scatter_shifted_offset_p(__base, __offset, __value, __p) __arm_vstrdq_scatter_shifted_offset_p(__base, __offset, __value, __p)
#define vstrdq_scatter_shifted_offset(__base, __offset, __value) __arm_vstrdq_scatter_shifted_offset(__base, __offset, __value)
#define vstrwq_scatter_offset_p(__base, __offset, __value, __p) __arm_vstrwq_scatter_offset_p(__base, __offset, __value, __p)
#define vstrwq_scatter_offset(__base, __offset, __value) __arm_vstrwq_scatter_offset(__base, __offset, __value)
#define vstrwq_scatter_shifted_offset_p(__base, __offset, __value, __p) __arm_vstrwq_scatter_shifted_offset_p(__base, __offset, __value, __p)
#define vstrwq_scatter_shifted_offset(__base, __offset, __value) __arm_vstrwq_scatter_shifted_offset(__base, __offset, __value)
#define vuninitializedq(__v) __arm_vuninitializedq(__v)
#define vddupq_m(__inactive, __a, __imm, __p) __arm_vddupq_m(__inactive, __a, __imm, __p)
#define vddupq_u8(__a, __imm) __arm_vddupq_u8(__a, __imm)
#define vddupq_u32(__a, __imm) __arm_vddupq_u32(__a, __imm)
#define vddupq_u16(__a, __imm) __arm_vddupq_u16(__a, __imm)
#define vdwdupq_m(__inactive, __a, __b, __imm, __p) __arm_vdwdupq_m(__inactive, __a, __b, __imm, __p)
#define vdwdupq_u8(__a, __b, __imm) __arm_vdwdupq_u8(__a, __b, __imm)
#define vdwdupq_u32(__a, __b, __imm) __arm_vdwdupq_u32(__a, __b, __imm)
#define vdwdupq_u16(__a, __b, __imm) __arm_vdwdupq_u16(__a, __b, __imm)
#define vidupq_m(__inactive, __a, __imm, __p) __arm_vidupq_m(__inactive, __a, __imm, __p)
#define vidupq_u8(__a, __imm) __arm_vidupq_u8(__a, __imm)
#define vidupq_u32(__a, __imm) __arm_vidupq_u32(__a, __imm)
#define vidupq_u16(__a, __imm) __arm_vidupq_u16(__a, __imm)
#define viwdupq_m(__inactive, __a, __b, __imm, __p) __arm_viwdupq_m(__inactive, __a, __b, __imm, __p)
#define viwdupq_u8(__a, __b, __imm) __arm_viwdupq_u8(__a, __b, __imm)
#define viwdupq_u32(__a, __b, __imm) __arm_viwdupq_u32(__a, __b, __imm)
#define viwdupq_u16(__a, __b, __imm) __arm_viwdupq_u16(__a, __b, __imm)
#define vstrdq_scatter_base_wb(__addr, __offset, __value) __arm_vstrdq_scatter_base_wb(__addr, __offset, __value)
#define vstrdq_scatter_base_wb_p(__addr, __offset, __value, __p) __arm_vstrdq_scatter_base_wb_p(__addr, __offset, __value, __p)
#define vstrwq_scatter_base_wb_p(__addr, __offset, __value, __p) __arm_vstrwq_scatter_base_wb_p(__addr, __offset, __value, __p)
#define vstrwq_scatter_base_wb(__addr, __offset, __value) __arm_vstrwq_scatter_base_wb(__addr, __offset, __value)
#define vddupq_x_u8(__a, __imm, __p) __arm_vddupq_x_u8(__a, __imm, __p)
#define vddupq_x_u16(__a, __imm, __p) __arm_vddupq_x_u16(__a, __imm, __p)
#define vddupq_x_u32(__a, __imm, __p) __arm_vddupq_x_u32(__a, __imm, __p)
#define vdwdupq_x_u8(__a, __b, __imm, __p) __arm_vdwdupq_x_u8(__a, __b, __imm, __p)
#define vdwdupq_x_u16(__a, __b, __imm, __p) __arm_vdwdupq_x_u16(__a, __b, __imm, __p)
#define vdwdupq_x_u32(__a, __b, __imm, __p) __arm_vdwdupq_x_u32(__a, __b, __imm, __p)
#define vidupq_x_u8(__a, __imm, __p) __arm_vidupq_x_u8(__a, __imm, __p)
#define vidupq_x_u16(__a, __imm, __p) __arm_vidupq_x_u16(__a, __imm, __p)
#define vidupq_x_u32(__a, __imm, __p) __arm_vidupq_x_u32(__a, __imm, __p)
#define viwdupq_x_u8(__a, __b, __imm, __p) __arm_viwdupq_x_u8(__a, __b, __imm, __p)
#define viwdupq_x_u16(__a, __b, __imm, __p) __arm_viwdupq_x_u16(__a, __b, __imm, __p)
#define viwdupq_x_u32(__a, __b, __imm, __p) __arm_viwdupq_x_u32(__a, __b, __imm, __p)
#define vbicq_x(__a, __b, __p) __arm_vbicq_x(__a, __b, __p)
#define vornq_x(__a, __b, __p) __arm_vornq_x(__a, __b, __p)
#define vadciq(__a, __b, __carry_out) __arm_vadciq(__a, __b, __carry_out)
#define vadciq_m(__inactive, __a, __b, __carry_out, __p) __arm_vadciq_m(__inactive, __a, __b, __carry_out, __p)
#define vadcq(__a, __b, __carry) __arm_vadcq(__a, __b, __carry)
#define vadcq_m(__inactive, __a, __b, __carry, __p) __arm_vadcq_m(__inactive, __a, __b, __carry, __p)
#define vsbciq(__a, __b, __carry_out) __arm_vsbciq(__a, __b, __carry_out)
#define vsbciq_m(__inactive, __a, __b, __carry_out, __p) __arm_vsbciq_m(__inactive, __a, __b, __carry_out, __p)
#define vsbcq(__a, __b, __carry) __arm_vsbcq(__a, __b, __carry)
#define vsbcq_m(__inactive, __a, __b, __carry, __p) __arm_vsbcq_m(__inactive, __a, __b, __carry, __p)
#define vst1q_p(__addr, __value, __p) __arm_vst1q_p(__addr, __value, __p)
#define vst2q(__addr, __value) __arm_vst2q(__addr, __value)
#define vld1q_z(__base, __p) __arm_vld1q_z(__base, __p)
#define vld2q(__addr) __arm_vld2q(__addr)
#define vld4q(__addr) __arm_vld4q(__addr)
#define vsetq_lane(__a, __b, __idx) __arm_vsetq_lane(__a, __b, __idx)
#define vgetq_lane(__a, __idx) __arm_vgetq_lane(__a, __idx)
#define vshlcq_m(__a, __b, __imm, __p) __arm_vshlcq_m(__a, __b, __imm, __p)
#define vcvttq_f32(__a) __arm_vcvttq_f32(__a)
#define vcvtbq_f32(__a) __arm_vcvtbq_f32(__a)
#define vcvtq(__a) __arm_vcvtq(__a)
#define vcvtq_n(__a, __imm6) __arm_vcvtq_n(__a, __imm6)
#define vcvtaq_m(__inactive, __a, __p) __arm_vcvtaq_m(__inactive, __a, __p)
#define vcvtq_m(__inactive, __a, __p) __arm_vcvtq_m(__inactive, __a, __p)
#define vcvtbq_m(__a, __b, __p) __arm_vcvtbq_m(__a, __b, __p)
#define vcvttq_m(__a, __b, __p) __arm_vcvttq_m(__a, __b, __p)
#define vcvtmq_m(__inactive, __a, __p) __arm_vcvtmq_m(__inactive, __a, __p)
#define vcvtnq_m(__inactive, __a, __p) __arm_vcvtnq_m(__inactive, __a, __p)
#define vcvtpq_m(__inactive, __a, __p) __arm_vcvtpq_m(__inactive, __a, __p)
#define vcvtq_m_n(__inactive, __a, __imm6, __p) __arm_vcvtq_m_n(__inactive, __a, __imm6, __p)
#define vcvtq_x(__a, __p) __arm_vcvtq_x(__a, __p)
#define vcvtq_x_n(__a, __imm6, __p) __arm_vcvtq_x_n(__a, __imm6, __p)


#define vst4q_s8( __addr, __value) __arm_vst4q_s8( __addr, __value)
#define vst4q_s16( __addr, __value) __arm_vst4q_s16( __addr, __value)
#define vst4q_s32( __addr, __value) __arm_vst4q_s32( __addr, __value)
#define vst4q_u8( __addr, __value) __arm_vst4q_u8( __addr, __value)
#define vst4q_u16( __addr, __value) __arm_vst4q_u16( __addr, __value)
#define vst4q_u32( __addr, __value) __arm_vst4q_u32( __addr, __value)
#define vst4q_f16( __addr, __value) __arm_vst4q_f16( __addr, __value)
#define vst4q_f32( __addr, __value) __arm_vst4q_f32( __addr, __value)
#define vcvttq_f32_f16(__a) __arm_vcvttq_f32_f16(__a)
#define vcvtbq_f32_f16(__a) __arm_vcvtbq_f32_f16(__a)
#define vcvtq_f16_s16(__a) __arm_vcvtq_f16_s16(__a)
#define vcvtq_f32_s32(__a) __arm_vcvtq_f32_s32(__a)
#define vcvtq_f16_u16(__a) __arm_vcvtq_f16_u16(__a)
#define vcvtq_f32_u32(__a) __arm_vcvtq_f32_u32(__a)
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
#define vcvtq_u16_f16(__a) __arm_vcvtq_u16_f16(__a)
#define vcvtq_u32_f32(__a) __arm_vcvtq_u32_f32(__a)
#define vcvtpq_u16_f16(__a) __arm_vcvtpq_u16_f16(__a)
#define vcvtpq_u32_f32(__a) __arm_vcvtpq_u32_f32(__a)
#define vcvtnq_u16_f16(__a) __arm_vcvtnq_u16_f16(__a)
#define vcvtnq_u32_f32(__a) __arm_vcvtnq_u32_f32(__a)
#define vcvtmq_u16_f16(__a) __arm_vcvtmq_u16_f16(__a)
#define vcvtmq_u32_f32(__a) __arm_vcvtmq_u32_f32(__a)
#define vcvtaq_u16_f16(__a) __arm_vcvtaq_u16_f16(__a)
#define vcvtaq_u32_f32(__a) __arm_vcvtaq_u32_f32(__a)
#define vctp16q(__a) __arm_vctp16q(__a)
#define vctp32q(__a) __arm_vctp32q(__a)
#define vctp64q(__a) __arm_vctp64q(__a)
#define vctp8q(__a) __arm_vctp8q(__a)
#define vpnot(__a) __arm_vpnot(__a)
#define vcvtq_n_f16_s16(__a,  __imm6) __arm_vcvtq_n_f16_s16(__a,  __imm6)
#define vcvtq_n_f32_s32(__a,  __imm6) __arm_vcvtq_n_f32_s32(__a,  __imm6)
#define vcvtq_n_f16_u16(__a,  __imm6) __arm_vcvtq_n_f16_u16(__a,  __imm6)
#define vcvtq_n_f32_u32(__a,  __imm6) __arm_vcvtq_n_f32_u32(__a,  __imm6)
#define vcvtq_n_s16_f16(__a,  __imm6) __arm_vcvtq_n_s16_f16(__a,  __imm6)
#define vcvtq_n_s32_f32(__a,  __imm6) __arm_vcvtq_n_s32_f32(__a,  __imm6)
#define vcvtq_n_u16_f16(__a,  __imm6) __arm_vcvtq_n_u16_f16(__a,  __imm6)
#define vcvtq_n_u32_f32(__a,  __imm6) __arm_vcvtq_n_u32_f32(__a,  __imm6)
#define vornq_u8(__a, __b) __arm_vornq_u8(__a, __b)
#define vbicq_u8(__a, __b) __arm_vbicq_u8(__a, __b)
#define vornq_s8(__a, __b) __arm_vornq_s8(__a, __b)
#define vbicq_s8(__a, __b) __arm_vbicq_s8(__a, __b)
#define vornq_u16(__a, __b) __arm_vornq_u16(__a, __b)
#define vbicq_u16(__a, __b) __arm_vbicq_u16(__a, __b)
#define vornq_s16(__a, __b) __arm_vornq_s16(__a, __b)
#define vbicq_s16(__a, __b) __arm_vbicq_s16(__a, __b)
#define vornq_u32(__a, __b) __arm_vornq_u32(__a, __b)
#define vbicq_u32(__a, __b) __arm_vbicq_u32(__a, __b)
#define vornq_s32(__a, __b) __arm_vornq_s32(__a, __b)
#define vbicq_s32(__a, __b) __arm_vbicq_s32(__a, __b)
#define vbicq_n_u16(__a,  __imm) __arm_vbicq_n_u16(__a,  __imm)
#define vornq_f16(__a, __b) __arm_vornq_f16(__a, __b)
#define vbicq_f16(__a, __b) __arm_vbicq_f16(__a, __b)
#define vbicq_n_s16(__a,  __imm) __arm_vbicq_n_s16(__a,  __imm)
#define vbicq_n_u32(__a,  __imm) __arm_vbicq_n_u32(__a,  __imm)
#define vornq_f32(__a, __b) __arm_vornq_f32(__a, __b)
#define vbicq_f32(__a, __b) __arm_vbicq_f32(__a, __b)
#define vbicq_n_s32(__a,  __imm) __arm_vbicq_n_s32(__a,  __imm)
#define vctp8q_m(__a, __p) __arm_vctp8q_m(__a, __p)
#define vctp64q_m(__a, __p) __arm_vctp64q_m(__a, __p)
#define vctp32q_m(__a, __p) __arm_vctp32q_m(__a, __p)
#define vctp16q_m(__a, __p) __arm_vctp16q_m(__a, __p)
#define vcvttq_f16_f32(__a, __b) __arm_vcvttq_f16_f32(__a, __b)
#define vcvtbq_f16_f32(__a, __b) __arm_vcvtbq_f16_f32(__a, __b)
#define vbicq_m_n_s16(__a,  __imm, __p) __arm_vbicq_m_n_s16(__a,  __imm, __p)
#define vbicq_m_n_s32(__a,  __imm, __p) __arm_vbicq_m_n_s32(__a,  __imm, __p)
#define vbicq_m_n_u16(__a,  __imm, __p) __arm_vbicq_m_n_u16(__a,  __imm, __p)
#define vbicq_m_n_u32(__a,  __imm, __p) __arm_vbicq_m_n_u32(__a,  __imm, __p)
#define vcvtaq_m_s16_f16(__inactive, __a, __p) __arm_vcvtaq_m_s16_f16(__inactive, __a, __p)
#define vcvtaq_m_u16_f16(__inactive, __a, __p) __arm_vcvtaq_m_u16_f16(__inactive, __a, __p)
#define vcvtaq_m_s32_f32(__inactive, __a, __p) __arm_vcvtaq_m_s32_f32(__inactive, __a, __p)
#define vcvtaq_m_u32_f32(__inactive, __a, __p) __arm_vcvtaq_m_u32_f32(__inactive, __a, __p)
#define vcvtq_m_f16_s16(__inactive, __a, __p) __arm_vcvtq_m_f16_s16(__inactive, __a, __p)
#define vcvtq_m_f16_u16(__inactive, __a, __p) __arm_vcvtq_m_f16_u16(__inactive, __a, __p)
#define vcvtq_m_f32_s32(__inactive, __a, __p) __arm_vcvtq_m_f32_s32(__inactive, __a, __p)
#define vcvtq_m_f32_u32(__inactive, __a, __p) __arm_vcvtq_m_f32_u32(__inactive, __a, __p)
#define vshlcq_s8(__a,  __b,  __imm) __arm_vshlcq_s8(__a,  __b,  __imm)
#define vshlcq_u8(__a,  __b,  __imm) __arm_vshlcq_u8(__a,  __b,  __imm)
#define vshlcq_s16(__a,  __b,  __imm) __arm_vshlcq_s16(__a,  __b,  __imm)
#define vshlcq_u16(__a,  __b,  __imm) __arm_vshlcq_u16(__a,  __b,  __imm)
#define vshlcq_s32(__a,  __b,  __imm) __arm_vshlcq_s32(__a,  __b,  __imm)
#define vshlcq_u32(__a,  __b,  __imm) __arm_vshlcq_u32(__a,  __b,  __imm)
#define vcvtbq_m_f16_f32(__a, __b, __p) __arm_vcvtbq_m_f16_f32(__a, __b, __p)
#define vcvtbq_m_f32_f16(__inactive, __a, __p) __arm_vcvtbq_m_f32_f16(__inactive, __a, __p)
#define vcvttq_m_f16_f32(__a, __b, __p) __arm_vcvttq_m_f16_f32(__a, __b, __p)
#define vcvttq_m_f32_f16(__inactive, __a, __p) __arm_vcvttq_m_f32_f16(__inactive, __a, __p)
#define vcvtmq_m_s16_f16(__inactive, __a, __p) __arm_vcvtmq_m_s16_f16(__inactive, __a, __p)
#define vcvtnq_m_s16_f16(__inactive, __a, __p) __arm_vcvtnq_m_s16_f16(__inactive, __a, __p)
#define vcvtpq_m_s16_f16(__inactive, __a, __p) __arm_vcvtpq_m_s16_f16(__inactive, __a, __p)
#define vcvtq_m_s16_f16(__inactive, __a, __p) __arm_vcvtq_m_s16_f16(__inactive, __a, __p)
#define vcvtmq_m_u16_f16(__inactive, __a, __p) __arm_vcvtmq_m_u16_f16(__inactive, __a, __p)
#define vcvtnq_m_u16_f16(__inactive, __a, __p) __arm_vcvtnq_m_u16_f16(__inactive, __a, __p)
#define vcvtpq_m_u16_f16(__inactive, __a, __p) __arm_vcvtpq_m_u16_f16(__inactive, __a, __p)
#define vcvtq_m_u16_f16(__inactive, __a, __p) __arm_vcvtq_m_u16_f16(__inactive, __a, __p)
#define vcvtmq_m_s32_f32(__inactive, __a, __p) __arm_vcvtmq_m_s32_f32(__inactive, __a, __p)
#define vcvtnq_m_s32_f32(__inactive, __a, __p) __arm_vcvtnq_m_s32_f32(__inactive, __a, __p)
#define vcvtpq_m_s32_f32(__inactive, __a, __p) __arm_vcvtpq_m_s32_f32(__inactive, __a, __p)
#define vcvtq_m_s32_f32(__inactive, __a, __p) __arm_vcvtq_m_s32_f32(__inactive, __a, __p)
#define vcvtmq_m_u32_f32(__inactive, __a, __p) __arm_vcvtmq_m_u32_f32(__inactive, __a, __p)
#define vcvtnq_m_u32_f32(__inactive, __a, __p) __arm_vcvtnq_m_u32_f32(__inactive, __a, __p)
#define vcvtpq_m_u32_f32(__inactive, __a, __p) __arm_vcvtpq_m_u32_f32(__inactive, __a, __p)
#define vcvtq_m_u32_f32(__inactive, __a, __p) __arm_vcvtq_m_u32_f32(__inactive, __a, __p)
#define vcvtq_m_n_f16_u16(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_f16_u16(__inactive, __a,  __imm6, __p)
#define vcvtq_m_n_f16_s16(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_f16_s16(__inactive, __a,  __imm6, __p)
#define vcvtq_m_n_f32_u32(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_f32_u32(__inactive, __a,  __imm6, __p)
#define vcvtq_m_n_f32_s32(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_f32_s32(__inactive, __a,  __imm6, __p)
#define vbicq_m_s8(__inactive, __a, __b, __p) __arm_vbicq_m_s8(__inactive, __a, __b, __p)
#define vbicq_m_s32(__inactive, __a, __b, __p) __arm_vbicq_m_s32(__inactive, __a, __b, __p)
#define vbicq_m_s16(__inactive, __a, __b, __p) __arm_vbicq_m_s16(__inactive, __a, __b, __p)
#define vbicq_m_u8(__inactive, __a, __b, __p) __arm_vbicq_m_u8(__inactive, __a, __b, __p)
#define vbicq_m_u32(__inactive, __a, __b, __p) __arm_vbicq_m_u32(__inactive, __a, __b, __p)
#define vbicq_m_u16(__inactive, __a, __b, __p) __arm_vbicq_m_u16(__inactive, __a, __b, __p)
#define vornq_m_s8(__inactive, __a, __b, __p) __arm_vornq_m_s8(__inactive, __a, __b, __p)
#define vornq_m_s32(__inactive, __a, __b, __p) __arm_vornq_m_s32(__inactive, __a, __b, __p)
#define vornq_m_s16(__inactive, __a, __b, __p) __arm_vornq_m_s16(__inactive, __a, __b, __p)
#define vornq_m_u8(__inactive, __a, __b, __p) __arm_vornq_m_u8(__inactive, __a, __b, __p)
#define vornq_m_u32(__inactive, __a, __b, __p) __arm_vornq_m_u32(__inactive, __a, __b, __p)
#define vornq_m_u16(__inactive, __a, __b, __p) __arm_vornq_m_u16(__inactive, __a, __b, __p)
#define vbicq_m_f32(__inactive, __a, __b, __p) __arm_vbicq_m_f32(__inactive, __a, __b, __p)
#define vbicq_m_f16(__inactive, __a, __b, __p) __arm_vbicq_m_f16(__inactive, __a, __b, __p)
#define vcvtq_m_n_s32_f32(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_s32_f32(__inactive, __a,  __imm6, __p)
#define vcvtq_m_n_s16_f16(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_s16_f16(__inactive, __a,  __imm6, __p)
#define vcvtq_m_n_u32_f32(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_u32_f32(__inactive, __a,  __imm6, __p)
#define vcvtq_m_n_u16_f16(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_u16_f16(__inactive, __a,  __imm6, __p)
#define vornq_m_f32(__inactive, __a, __b, __p) __arm_vornq_m_f32(__inactive, __a, __b, __p)
#define vornq_m_f16(__inactive, __a, __b, __p) __arm_vornq_m_f16(__inactive, __a, __b, __p)
#define vstrbq_s8( __addr, __value) __arm_vstrbq_s8( __addr, __value)
#define vstrbq_u8( __addr, __value) __arm_vstrbq_u8( __addr, __value)
#define vstrbq_u16( __addr, __value) __arm_vstrbq_u16( __addr, __value)
#define vstrbq_scatter_offset_s8( __base, __offset, __value) __arm_vstrbq_scatter_offset_s8( __base, __offset, __value)
#define vstrbq_scatter_offset_u8( __base, __offset, __value) __arm_vstrbq_scatter_offset_u8( __base, __offset, __value)
#define vstrbq_scatter_offset_u16( __base, __offset, __value) __arm_vstrbq_scatter_offset_u16( __base, __offset, __value)
#define vstrbq_s16( __addr, __value) __arm_vstrbq_s16( __addr, __value)
#define vstrbq_u32( __addr, __value) __arm_vstrbq_u32( __addr, __value)
#define vstrbq_scatter_offset_s16( __base, __offset, __value) __arm_vstrbq_scatter_offset_s16( __base, __offset, __value)
#define vstrbq_scatter_offset_u32( __base, __offset, __value) __arm_vstrbq_scatter_offset_u32( __base, __offset, __value)
#define vstrbq_s32( __addr, __value) __arm_vstrbq_s32( __addr, __value)
#define vstrbq_scatter_offset_s32( __base, __offset, __value) __arm_vstrbq_scatter_offset_s32( __base, __offset, __value)
#define vstrwq_scatter_base_s32(__addr,  __offset, __value) __arm_vstrwq_scatter_base_s32(__addr,  __offset, __value)
#define vstrwq_scatter_base_u32(__addr,  __offset, __value) __arm_vstrwq_scatter_base_u32(__addr,  __offset, __value)
#define vldrbq_gather_offset_u8(__base, __offset) __arm_vldrbq_gather_offset_u8(__base, __offset)
#define vldrbq_gather_offset_s8(__base, __offset) __arm_vldrbq_gather_offset_s8(__base, __offset)
#define vldrbq_s8(__base) __arm_vldrbq_s8(__base)
#define vldrbq_u8(__base) __arm_vldrbq_u8(__base)
#define vldrbq_gather_offset_u16(__base, __offset) __arm_vldrbq_gather_offset_u16(__base, __offset)
#define vldrbq_gather_offset_s16(__base, __offset) __arm_vldrbq_gather_offset_s16(__base, __offset)
#define vldrbq_s16(__base) __arm_vldrbq_s16(__base)
#define vldrbq_u16(__base) __arm_vldrbq_u16(__base)
#define vldrbq_gather_offset_u32(__base, __offset) __arm_vldrbq_gather_offset_u32(__base, __offset)
#define vldrbq_gather_offset_s32(__base, __offset) __arm_vldrbq_gather_offset_s32(__base, __offset)
#define vldrbq_s32(__base) __arm_vldrbq_s32(__base)
#define vldrbq_u32(__base) __arm_vldrbq_u32(__base)
#define vldrwq_gather_base_s32(__addr,  __offset) __arm_vldrwq_gather_base_s32(__addr,  __offset)
#define vldrwq_gather_base_u32(__addr,  __offset) __arm_vldrwq_gather_base_u32(__addr,  __offset)
#define vstrbq_p_s8( __addr, __value, __p) __arm_vstrbq_p_s8( __addr, __value, __p)
#define vstrbq_p_s32( __addr, __value, __p) __arm_vstrbq_p_s32( __addr, __value, __p)
#define vstrbq_p_s16( __addr, __value, __p) __arm_vstrbq_p_s16( __addr, __value, __p)
#define vstrbq_p_u8( __addr, __value, __p) __arm_vstrbq_p_u8( __addr, __value, __p)
#define vstrbq_p_u32( __addr, __value, __p) __arm_vstrbq_p_u32( __addr, __value, __p)
#define vstrbq_p_u16( __addr, __value, __p) __arm_vstrbq_p_u16( __addr, __value, __p)
#define vstrbq_scatter_offset_p_s8( __base, __offset, __value, __p) __arm_vstrbq_scatter_offset_p_s8( __base, __offset, __value, __p)
#define vstrbq_scatter_offset_p_s32( __base, __offset, __value, __p) __arm_vstrbq_scatter_offset_p_s32( __base, __offset, __value, __p)
#define vstrbq_scatter_offset_p_s16( __base, __offset, __value, __p) __arm_vstrbq_scatter_offset_p_s16( __base, __offset, __value, __p)
#define vstrbq_scatter_offset_p_u8( __base, __offset, __value, __p) __arm_vstrbq_scatter_offset_p_u8( __base, __offset, __value, __p)
#define vstrbq_scatter_offset_p_u32( __base, __offset, __value, __p) __arm_vstrbq_scatter_offset_p_u32( __base, __offset, __value, __p)
#define vstrbq_scatter_offset_p_u16( __base, __offset, __value, __p) __arm_vstrbq_scatter_offset_p_u16( __base, __offset, __value, __p)
#define vstrwq_scatter_base_p_s32(__addr,  __offset, __value, __p) __arm_vstrwq_scatter_base_p_s32(__addr,  __offset, __value, __p)
#define vstrwq_scatter_base_p_u32(__addr,  __offset, __value, __p) __arm_vstrwq_scatter_base_p_u32(__addr,  __offset, __value, __p)
#define vldrbq_gather_offset_z_s16(__base, __offset, __p) __arm_vldrbq_gather_offset_z_s16(__base, __offset, __p)
#define vldrbq_gather_offset_z_u8(__base, __offset, __p) __arm_vldrbq_gather_offset_z_u8(__base, __offset, __p)
#define vldrbq_gather_offset_z_s32(__base, __offset, __p) __arm_vldrbq_gather_offset_z_s32(__base, __offset, __p)
#define vldrbq_gather_offset_z_u16(__base, __offset, __p) __arm_vldrbq_gather_offset_z_u16(__base, __offset, __p)
#define vldrbq_gather_offset_z_u32(__base, __offset, __p) __arm_vldrbq_gather_offset_z_u32(__base, __offset, __p)
#define vldrbq_gather_offset_z_s8(__base, __offset, __p) __arm_vldrbq_gather_offset_z_s8(__base, __offset, __p)
#define vldrbq_z_s16(__base, __p) __arm_vldrbq_z_s16(__base, __p)
#define vldrbq_z_u8(__base, __p) __arm_vldrbq_z_u8(__base, __p)
#define vldrbq_z_s8(__base, __p) __arm_vldrbq_z_s8(__base, __p)
#define vldrbq_z_s32(__base, __p) __arm_vldrbq_z_s32(__base, __p)
#define vldrbq_z_u16(__base, __p) __arm_vldrbq_z_u16(__base, __p)
#define vldrbq_z_u32(__base, __p) __arm_vldrbq_z_u32(__base, __p)
#define vldrwq_gather_base_z_u32(__addr,  __offset, __p) __arm_vldrwq_gather_base_z_u32(__addr,  __offset, __p)
#define vldrwq_gather_base_z_s32(__addr,  __offset, __p) __arm_vldrwq_gather_base_z_s32(__addr,  __offset, __p)
#define vldrhq_gather_offset_s32(__base, __offset) __arm_vldrhq_gather_offset_s32(__base, __offset)
#define vldrhq_gather_offset_s16(__base, __offset) __arm_vldrhq_gather_offset_s16(__base, __offset)
#define vldrhq_gather_offset_u32(__base, __offset) __arm_vldrhq_gather_offset_u32(__base, __offset)
#define vldrhq_gather_offset_u16(__base, __offset) __arm_vldrhq_gather_offset_u16(__base, __offset)
#define vldrhq_gather_offset_z_s32(__base, __offset, __p) __arm_vldrhq_gather_offset_z_s32(__base, __offset, __p)
#define vldrhq_gather_offset_z_s16(__base, __offset, __p) __arm_vldrhq_gather_offset_z_s16(__base, __offset, __p)
#define vldrhq_gather_offset_z_u32(__base, __offset, __p) __arm_vldrhq_gather_offset_z_u32(__base, __offset, __p)
#define vldrhq_gather_offset_z_u16(__base, __offset, __p) __arm_vldrhq_gather_offset_z_u16(__base, __offset, __p)
#define vldrhq_gather_shifted_offset_s32(__base, __offset) __arm_vldrhq_gather_shifted_offset_s32(__base, __offset)
#define vldrhq_gather_shifted_offset_s16(__base, __offset) __arm_vldrhq_gather_shifted_offset_s16(__base, __offset)
#define vldrhq_gather_shifted_offset_u32(__base, __offset) __arm_vldrhq_gather_shifted_offset_u32(__base, __offset)
#define vldrhq_gather_shifted_offset_u16(__base, __offset) __arm_vldrhq_gather_shifted_offset_u16(__base, __offset)
#define vldrhq_gather_shifted_offset_z_s32(__base, __offset, __p) __arm_vldrhq_gather_shifted_offset_z_s32(__base, __offset, __p)
#define vldrhq_gather_shifted_offset_z_s16(__base, __offset, __p) __arm_vldrhq_gather_shifted_offset_z_s16(__base, __offset, __p)
#define vldrhq_gather_shifted_offset_z_u32(__base, __offset, __p) __arm_vldrhq_gather_shifted_offset_z_u32(__base, __offset, __p)
#define vldrhq_gather_shifted_offset_z_u16(__base, __offset, __p) __arm_vldrhq_gather_shifted_offset_z_u16(__base, __offset, __p)
#define vldrhq_s32(__base) __arm_vldrhq_s32(__base)
#define vldrhq_s16(__base) __arm_vldrhq_s16(__base)
#define vldrhq_u32(__base) __arm_vldrhq_u32(__base)
#define vldrhq_u16(__base) __arm_vldrhq_u16(__base)
#define vldrhq_z_s32(__base, __p) __arm_vldrhq_z_s32(__base, __p)
#define vldrhq_z_s16(__base, __p) __arm_vldrhq_z_s16(__base, __p)
#define vldrhq_z_u32(__base, __p) __arm_vldrhq_z_u32(__base, __p)
#define vldrhq_z_u16(__base, __p) __arm_vldrhq_z_u16(__base, __p)
#define vldrwq_s32(__base) __arm_vldrwq_s32(__base)
#define vldrwq_u32(__base) __arm_vldrwq_u32(__base)
#define vldrwq_z_s32(__base, __p) __arm_vldrwq_z_s32(__base, __p)
#define vldrwq_z_u32(__base, __p) __arm_vldrwq_z_u32(__base, __p)
#define vldrhq_f16(__base) __arm_vldrhq_f16(__base)
#define vldrhq_z_f16(__base, __p) __arm_vldrhq_z_f16(__base, __p)
#define vldrwq_f32(__base) __arm_vldrwq_f32(__base)
#define vldrwq_z_f32(__base, __p) __arm_vldrwq_z_f32(__base, __p)
#define vldrdq_gather_base_s64(__addr,  __offset) __arm_vldrdq_gather_base_s64(__addr,  __offset)
#define vldrdq_gather_base_u64(__addr,  __offset) __arm_vldrdq_gather_base_u64(__addr,  __offset)
#define vldrdq_gather_base_z_s64(__addr,  __offset, __p) __arm_vldrdq_gather_base_z_s64(__addr,  __offset, __p)
#define vldrdq_gather_base_z_u64(__addr,  __offset, __p) __arm_vldrdq_gather_base_z_u64(__addr,  __offset, __p)
#define vldrdq_gather_offset_s64(__base, __offset) __arm_vldrdq_gather_offset_s64(__base, __offset)
#define vldrdq_gather_offset_u64(__base, __offset) __arm_vldrdq_gather_offset_u64(__base, __offset)
#define vldrdq_gather_offset_z_s64(__base, __offset, __p) __arm_vldrdq_gather_offset_z_s64(__base, __offset, __p)
#define vldrdq_gather_offset_z_u64(__base, __offset, __p) __arm_vldrdq_gather_offset_z_u64(__base, __offset, __p)
#define vldrdq_gather_shifted_offset_s64(__base, __offset) __arm_vldrdq_gather_shifted_offset_s64(__base, __offset)
#define vldrdq_gather_shifted_offset_u64(__base, __offset) __arm_vldrdq_gather_shifted_offset_u64(__base, __offset)
#define vldrdq_gather_shifted_offset_z_s64(__base, __offset, __p) __arm_vldrdq_gather_shifted_offset_z_s64(__base, __offset, __p)
#define vldrdq_gather_shifted_offset_z_u64(__base, __offset, __p) __arm_vldrdq_gather_shifted_offset_z_u64(__base, __offset, __p)
#define vldrhq_gather_offset_f16(__base, __offset) __arm_vldrhq_gather_offset_f16(__base, __offset)
#define vldrhq_gather_offset_z_f16(__base, __offset, __p) __arm_vldrhq_gather_offset_z_f16(__base, __offset, __p)
#define vldrhq_gather_shifted_offset_f16(__base, __offset) __arm_vldrhq_gather_shifted_offset_f16(__base, __offset)
#define vldrhq_gather_shifted_offset_z_f16(__base, __offset, __p) __arm_vldrhq_gather_shifted_offset_z_f16(__base, __offset, __p)
#define vldrwq_gather_base_f32(__addr,  __offset) __arm_vldrwq_gather_base_f32(__addr,  __offset)
#define vldrwq_gather_base_z_f32(__addr,  __offset, __p) __arm_vldrwq_gather_base_z_f32(__addr,  __offset, __p)
#define vldrwq_gather_offset_f32(__base, __offset) __arm_vldrwq_gather_offset_f32(__base, __offset)
#define vldrwq_gather_offset_s32(__base, __offset) __arm_vldrwq_gather_offset_s32(__base, __offset)
#define vldrwq_gather_offset_u32(__base, __offset) __arm_vldrwq_gather_offset_u32(__base, __offset)
#define vldrwq_gather_offset_z_f32(__base, __offset, __p) __arm_vldrwq_gather_offset_z_f32(__base, __offset, __p)
#define vldrwq_gather_offset_z_s32(__base, __offset, __p) __arm_vldrwq_gather_offset_z_s32(__base, __offset, __p)
#define vldrwq_gather_offset_z_u32(__base, __offset, __p) __arm_vldrwq_gather_offset_z_u32(__base, __offset, __p)
#define vldrwq_gather_shifted_offset_f32(__base, __offset) __arm_vldrwq_gather_shifted_offset_f32(__base, __offset)
#define vldrwq_gather_shifted_offset_s32(__base, __offset) __arm_vldrwq_gather_shifted_offset_s32(__base, __offset)
#define vldrwq_gather_shifted_offset_u32(__base, __offset) __arm_vldrwq_gather_shifted_offset_u32(__base, __offset)
#define vldrwq_gather_shifted_offset_z_f32(__base, __offset, __p) __arm_vldrwq_gather_shifted_offset_z_f32(__base, __offset, __p)
#define vldrwq_gather_shifted_offset_z_s32(__base, __offset, __p) __arm_vldrwq_gather_shifted_offset_z_s32(__base, __offset, __p)
#define vldrwq_gather_shifted_offset_z_u32(__base, __offset, __p) __arm_vldrwq_gather_shifted_offset_z_u32(__base, __offset, __p)
#define vstrhq_f16(__addr, __value) __arm_vstrhq_f16(__addr, __value)
#define vstrhq_scatter_offset_s32( __base, __offset, __value) __arm_vstrhq_scatter_offset_s32( __base, __offset, __value)
#define vstrhq_scatter_offset_s16( __base, __offset, __value) __arm_vstrhq_scatter_offset_s16( __base, __offset, __value)
#define vstrhq_scatter_offset_u32( __base, __offset, __value) __arm_vstrhq_scatter_offset_u32( __base, __offset, __value)
#define vstrhq_scatter_offset_u16( __base, __offset, __value) __arm_vstrhq_scatter_offset_u16( __base, __offset, __value)
#define vstrhq_scatter_offset_p_s32( __base, __offset, __value, __p) __arm_vstrhq_scatter_offset_p_s32( __base, __offset, __value, __p)
#define vstrhq_scatter_offset_p_s16( __base, __offset, __value, __p) __arm_vstrhq_scatter_offset_p_s16( __base, __offset, __value, __p)
#define vstrhq_scatter_offset_p_u32( __base, __offset, __value, __p) __arm_vstrhq_scatter_offset_p_u32( __base, __offset, __value, __p)
#define vstrhq_scatter_offset_p_u16( __base, __offset, __value, __p) __arm_vstrhq_scatter_offset_p_u16( __base, __offset, __value, __p)
#define vstrhq_scatter_shifted_offset_s32( __base, __offset, __value) __arm_vstrhq_scatter_shifted_offset_s32( __base, __offset, __value)
#define vstrhq_scatter_shifted_offset_s16( __base, __offset, __value) __arm_vstrhq_scatter_shifted_offset_s16( __base, __offset, __value)
#define vstrhq_scatter_shifted_offset_u32( __base, __offset, __value) __arm_vstrhq_scatter_shifted_offset_u32( __base, __offset, __value)
#define vstrhq_scatter_shifted_offset_u16( __base, __offset, __value) __arm_vstrhq_scatter_shifted_offset_u16( __base, __offset, __value)
#define vstrhq_scatter_shifted_offset_p_s32( __base, __offset, __value, __p) __arm_vstrhq_scatter_shifted_offset_p_s32( __base, __offset, __value, __p)
#define vstrhq_scatter_shifted_offset_p_s16( __base, __offset, __value, __p) __arm_vstrhq_scatter_shifted_offset_p_s16( __base, __offset, __value, __p)
#define vstrhq_scatter_shifted_offset_p_u32( __base, __offset, __value, __p) __arm_vstrhq_scatter_shifted_offset_p_u32( __base, __offset, __value, __p)
#define vstrhq_scatter_shifted_offset_p_u16( __base, __offset, __value, __p) __arm_vstrhq_scatter_shifted_offset_p_u16( __base, __offset, __value, __p)
#define vstrhq_s32(__addr, __value) __arm_vstrhq_s32(__addr, __value)
#define vstrhq_s16(__addr, __value) __arm_vstrhq_s16(__addr, __value)
#define vstrhq_u32(__addr, __value) __arm_vstrhq_u32(__addr, __value)
#define vstrhq_u16(__addr, __value) __arm_vstrhq_u16(__addr, __value)
#define vstrhq_p_f16(__addr, __value, __p) __arm_vstrhq_p_f16(__addr, __value, __p)
#define vstrhq_p_s32(__addr, __value, __p) __arm_vstrhq_p_s32(__addr, __value, __p)
#define vstrhq_p_s16(__addr, __value, __p) __arm_vstrhq_p_s16(__addr, __value, __p)
#define vstrhq_p_u32(__addr, __value, __p) __arm_vstrhq_p_u32(__addr, __value, __p)
#define vstrhq_p_u16(__addr, __value, __p) __arm_vstrhq_p_u16(__addr, __value, __p)
#define vstrwq_f32(__addr, __value) __arm_vstrwq_f32(__addr, __value)
#define vstrwq_s32(__addr, __value) __arm_vstrwq_s32(__addr, __value)
#define vstrwq_u32(__addr, __value) __arm_vstrwq_u32(__addr, __value)
#define vstrwq_p_f32(__addr, __value, __p) __arm_vstrwq_p_f32(__addr, __value, __p)
#define vstrwq_p_s32(__addr, __value, __p) __arm_vstrwq_p_s32(__addr, __value, __p)
#define vstrwq_p_u32(__addr, __value, __p) __arm_vstrwq_p_u32(__addr, __value, __p)
#define vstrdq_scatter_base_p_s64(__addr, __offset, __value, __p) __arm_vstrdq_scatter_base_p_s64(__addr, __offset, __value, __p)
#define vstrdq_scatter_base_p_u64(__addr, __offset, __value, __p) __arm_vstrdq_scatter_base_p_u64(__addr, __offset, __value, __p)
#define vstrdq_scatter_base_s64(__addr, __offset, __value) __arm_vstrdq_scatter_base_s64(__addr, __offset, __value)
#define vstrdq_scatter_base_u64(__addr, __offset, __value) __arm_vstrdq_scatter_base_u64(__addr, __offset, __value)
#define vstrdq_scatter_offset_p_s64(__base, __offset, __value, __p) __arm_vstrdq_scatter_offset_p_s64(__base, __offset, __value, __p)
#define vstrdq_scatter_offset_p_u64(__base, __offset, __value, __p) __arm_vstrdq_scatter_offset_p_u64(__base, __offset, __value, __p)
#define vstrdq_scatter_offset_s64(__base, __offset, __value) __arm_vstrdq_scatter_offset_s64(__base, __offset, __value)
#define vstrdq_scatter_offset_u64(__base, __offset, __value) __arm_vstrdq_scatter_offset_u64(__base, __offset, __value)
#define vstrdq_scatter_shifted_offset_p_s64(__base, __offset, __value, __p) __arm_vstrdq_scatter_shifted_offset_p_s64(__base, __offset, __value, __p)
#define vstrdq_scatter_shifted_offset_p_u64(__base, __offset, __value, __p) __arm_vstrdq_scatter_shifted_offset_p_u64(__base, __offset, __value, __p)
#define vstrdq_scatter_shifted_offset_s64(__base, __offset, __value) __arm_vstrdq_scatter_shifted_offset_s64(__base, __offset, __value)
#define vstrdq_scatter_shifted_offset_u64(__base, __offset, __value) __arm_vstrdq_scatter_shifted_offset_u64(__base, __offset, __value)
#define vstrhq_scatter_offset_f16(__base, __offset, __value) __arm_vstrhq_scatter_offset_f16(__base, __offset, __value)
#define vstrhq_scatter_offset_p_f16(__base, __offset, __value, __p) __arm_vstrhq_scatter_offset_p_f16(__base, __offset, __value, __p)
#define vstrhq_scatter_shifted_offset_f16(__base, __offset, __value) __arm_vstrhq_scatter_shifted_offset_f16(__base, __offset, __value)
#define vstrhq_scatter_shifted_offset_p_f16(__base, __offset, __value, __p) __arm_vstrhq_scatter_shifted_offset_p_f16(__base, __offset, __value, __p)
#define vstrwq_scatter_base_f32(__addr, __offset, __value) __arm_vstrwq_scatter_base_f32(__addr, __offset, __value)
#define vstrwq_scatter_base_p_f32(__addr, __offset, __value, __p) __arm_vstrwq_scatter_base_p_f32(__addr, __offset, __value, __p)
#define vstrwq_scatter_offset_f32(__base, __offset, __value) __arm_vstrwq_scatter_offset_f32(__base, __offset, __value)
#define vstrwq_scatter_offset_p_f32(__base, __offset, __value, __p) __arm_vstrwq_scatter_offset_p_f32(__base, __offset, __value, __p)
#define vstrwq_scatter_offset_p_s32(__base, __offset, __value, __p) __arm_vstrwq_scatter_offset_p_s32(__base, __offset, __value, __p)
#define vstrwq_scatter_offset_p_u32(__base, __offset, __value, __p) __arm_vstrwq_scatter_offset_p_u32(__base, __offset, __value, __p)
#define vstrwq_scatter_offset_s32(__base, __offset, __value) __arm_vstrwq_scatter_offset_s32(__base, __offset, __value)
#define vstrwq_scatter_offset_u32(__base, __offset, __value) __arm_vstrwq_scatter_offset_u32(__base, __offset, __value)
#define vstrwq_scatter_shifted_offset_f32(__base, __offset, __value) __arm_vstrwq_scatter_shifted_offset_f32(__base, __offset, __value)
#define vstrwq_scatter_shifted_offset_p_f32(__base, __offset, __value, __p) __arm_vstrwq_scatter_shifted_offset_p_f32(__base, __offset, __value, __p)
#define vstrwq_scatter_shifted_offset_p_s32(__base, __offset, __value, __p) __arm_vstrwq_scatter_shifted_offset_p_s32(__base, __offset, __value, __p)
#define vstrwq_scatter_shifted_offset_p_u32(__base, __offset, __value, __p) __arm_vstrwq_scatter_shifted_offset_p_u32(__base, __offset, __value, __p)
#define vstrwq_scatter_shifted_offset_s32(__base, __offset, __value) __arm_vstrwq_scatter_shifted_offset_s32(__base, __offset, __value)
#define vstrwq_scatter_shifted_offset_u32(__base, __offset, __value) __arm_vstrwq_scatter_shifted_offset_u32(__base, __offset, __value)
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
#define vddupq_m_n_u8(__inactive, __a,  __imm, __p) __arm_vddupq_m_n_u8(__inactive, __a,  __imm, __p)
#define vddupq_m_n_u32(__inactive, __a,  __imm, __p) __arm_vddupq_m_n_u32(__inactive, __a,  __imm, __p)
#define vddupq_m_n_u16(__inactive, __a,  __imm, __p) __arm_vddupq_m_n_u16(__inactive, __a,  __imm, __p)
#define vddupq_m_wb_u8(__inactive,  __a,  __imm, __p) __arm_vddupq_m_wb_u8(__inactive,  __a,  __imm, __p)
#define vddupq_m_wb_u16(__inactive,  __a,  __imm, __p) __arm_vddupq_m_wb_u16(__inactive,  __a,  __imm, __p)
#define vddupq_m_wb_u32(__inactive,  __a,  __imm, __p) __arm_vddupq_m_wb_u32(__inactive,  __a,  __imm, __p)
#define vddupq_n_u8(__a,  __imm) __arm_vddupq_n_u8(__a,  __imm)
#define vddupq_n_u32(__a,  __imm) __arm_vddupq_n_u32(__a,  __imm)
#define vddupq_n_u16(__a,  __imm) __arm_vddupq_n_u16(__a,  __imm)
#define vddupq_wb_u8( __a,  __imm) __arm_vddupq_wb_u8( __a,  __imm)
#define vddupq_wb_u16( __a,  __imm) __arm_vddupq_wb_u16( __a,  __imm)
#define vddupq_wb_u32( __a,  __imm) __arm_vddupq_wb_u32( __a,  __imm)
#define vdwdupq_m_n_u8(__inactive, __a, __b,  __imm, __p) __arm_vdwdupq_m_n_u8(__inactive, __a, __b,  __imm, __p)
#define vdwdupq_m_n_u32(__inactive, __a, __b,  __imm, __p) __arm_vdwdupq_m_n_u32(__inactive, __a, __b,  __imm, __p)
#define vdwdupq_m_n_u16(__inactive, __a, __b,  __imm, __p) __arm_vdwdupq_m_n_u16(__inactive, __a, __b,  __imm, __p)
#define vdwdupq_m_wb_u8(__inactive,  __a, __b,  __imm, __p) __arm_vdwdupq_m_wb_u8(__inactive,  __a, __b,  __imm, __p)
#define vdwdupq_m_wb_u32(__inactive,  __a, __b,  __imm, __p) __arm_vdwdupq_m_wb_u32(__inactive,  __a, __b,  __imm, __p)
#define vdwdupq_m_wb_u16(__inactive,  __a, __b,  __imm, __p) __arm_vdwdupq_m_wb_u16(__inactive,  __a, __b,  __imm, __p)
#define vdwdupq_n_u8(__a, __b,  __imm) __arm_vdwdupq_n_u8(__a, __b,  __imm)
#define vdwdupq_n_u32(__a, __b,  __imm) __arm_vdwdupq_n_u32(__a, __b,  __imm)
#define vdwdupq_n_u16(__a, __b,  __imm) __arm_vdwdupq_n_u16(__a, __b,  __imm)
#define vdwdupq_wb_u8( __a, __b,  __imm) __arm_vdwdupq_wb_u8( __a, __b,  __imm)
#define vdwdupq_wb_u32( __a, __b,  __imm) __arm_vdwdupq_wb_u32( __a, __b,  __imm)
#define vdwdupq_wb_u16( __a, __b,  __imm) __arm_vdwdupq_wb_u16( __a, __b,  __imm)
#define vidupq_m_n_u8(__inactive, __a,  __imm, __p) __arm_vidupq_m_n_u8(__inactive, __a,  __imm, __p)
#define vidupq_m_n_u32(__inactive, __a,  __imm, __p) __arm_vidupq_m_n_u32(__inactive, __a,  __imm, __p)
#define vidupq_m_n_u16(__inactive, __a,  __imm, __p) __arm_vidupq_m_n_u16(__inactive, __a,  __imm, __p)
#define vidupq_m_wb_u8(__inactive,  __a,  __imm, __p) __arm_vidupq_m_wb_u8(__inactive,  __a,  __imm, __p)
#define vidupq_m_wb_u16(__inactive,  __a,  __imm, __p) __arm_vidupq_m_wb_u16(__inactive,  __a,  __imm, __p)
#define vidupq_m_wb_u32(__inactive,  __a,  __imm, __p) __arm_vidupq_m_wb_u32(__inactive,  __a,  __imm, __p)
#define vidupq_n_u8(__a,  __imm) __arm_vidupq_n_u8(__a,  __imm)
#define vidupq_n_u32(__a,  __imm) __arm_vidupq_n_u32(__a,  __imm)
#define vidupq_n_u16(__a,  __imm) __arm_vidupq_n_u16(__a,  __imm)
#define vidupq_wb_u8( __a,  __imm) __arm_vidupq_wb_u8( __a,  __imm)
#define vidupq_wb_u16( __a,  __imm) __arm_vidupq_wb_u16( __a,  __imm)
#define vidupq_wb_u32( __a,  __imm) __arm_vidupq_wb_u32( __a,  __imm)
#define viwdupq_m_n_u8(__inactive, __a, __b,  __imm, __p) __arm_viwdupq_m_n_u8(__inactive, __a, __b,  __imm, __p)
#define viwdupq_m_n_u32(__inactive, __a, __b,  __imm, __p) __arm_viwdupq_m_n_u32(__inactive, __a, __b,  __imm, __p)
#define viwdupq_m_n_u16(__inactive, __a, __b,  __imm, __p) __arm_viwdupq_m_n_u16(__inactive, __a, __b,  __imm, __p)
#define viwdupq_m_wb_u8(__inactive,  __a, __b,  __imm, __p) __arm_viwdupq_m_wb_u8(__inactive,  __a, __b,  __imm, __p)
#define viwdupq_m_wb_u32(__inactive,  __a, __b,  __imm, __p) __arm_viwdupq_m_wb_u32(__inactive,  __a, __b,  __imm, __p)
#define viwdupq_m_wb_u16(__inactive,  __a, __b,  __imm, __p) __arm_viwdupq_m_wb_u16(__inactive,  __a, __b,  __imm, __p)
#define viwdupq_n_u8(__a, __b,  __imm) __arm_viwdupq_n_u8(__a, __b,  __imm)
#define viwdupq_n_u32(__a, __b,  __imm) __arm_viwdupq_n_u32(__a, __b,  __imm)
#define viwdupq_n_u16(__a, __b,  __imm) __arm_viwdupq_n_u16(__a, __b,  __imm)
#define viwdupq_wb_u8( __a, __b,  __imm) __arm_viwdupq_wb_u8( __a, __b,  __imm)
#define viwdupq_wb_u32( __a, __b,  __imm) __arm_viwdupq_wb_u32( __a, __b,  __imm)
#define viwdupq_wb_u16( __a, __b,  __imm) __arm_viwdupq_wb_u16( __a, __b,  __imm)
#define vldrdq_gather_base_wb_s64(__addr, __offset) __arm_vldrdq_gather_base_wb_s64(__addr, __offset)
#define vldrdq_gather_base_wb_u64(__addr, __offset) __arm_vldrdq_gather_base_wb_u64(__addr, __offset)
#define vldrdq_gather_base_wb_z_s64(__addr, __offset, __p) __arm_vldrdq_gather_base_wb_z_s64(__addr, __offset, __p)
#define vldrdq_gather_base_wb_z_u64(__addr, __offset, __p) __arm_vldrdq_gather_base_wb_z_u64(__addr, __offset, __p)
#define vldrwq_gather_base_wb_f32(__addr, __offset) __arm_vldrwq_gather_base_wb_f32(__addr, __offset)
#define vldrwq_gather_base_wb_s32(__addr, __offset) __arm_vldrwq_gather_base_wb_s32(__addr, __offset)
#define vldrwq_gather_base_wb_u32(__addr, __offset) __arm_vldrwq_gather_base_wb_u32(__addr, __offset)
#define vldrwq_gather_base_wb_z_f32(__addr, __offset, __p) __arm_vldrwq_gather_base_wb_z_f32(__addr, __offset, __p)
#define vldrwq_gather_base_wb_z_s32(__addr, __offset, __p) __arm_vldrwq_gather_base_wb_z_s32(__addr, __offset, __p)
#define vldrwq_gather_base_wb_z_u32(__addr, __offset, __p) __arm_vldrwq_gather_base_wb_z_u32(__addr, __offset, __p)
#define vstrdq_scatter_base_wb_p_s64(__addr, __offset, __value, __p) __arm_vstrdq_scatter_base_wb_p_s64(__addr, __offset, __value, __p)
#define vstrdq_scatter_base_wb_p_u64(__addr, __offset, __value, __p) __arm_vstrdq_scatter_base_wb_p_u64(__addr, __offset, __value, __p)
#define vstrdq_scatter_base_wb_s64(__addr, __offset, __value) __arm_vstrdq_scatter_base_wb_s64(__addr, __offset, __value)
#define vstrdq_scatter_base_wb_u64(__addr, __offset, __value) __arm_vstrdq_scatter_base_wb_u64(__addr, __offset, __value)
#define vstrwq_scatter_base_wb_p_s32(__addr, __offset, __value, __p) __arm_vstrwq_scatter_base_wb_p_s32(__addr, __offset, __value, __p)
#define vstrwq_scatter_base_wb_p_f32(__addr, __offset, __value, __p) __arm_vstrwq_scatter_base_wb_p_f32(__addr, __offset, __value, __p)
#define vstrwq_scatter_base_wb_p_u32(__addr, __offset, __value, __p) __arm_vstrwq_scatter_base_wb_p_u32(__addr, __offset, __value, __p)
#define vstrwq_scatter_base_wb_s32(__addr, __offset, __value) __arm_vstrwq_scatter_base_wb_s32(__addr, __offset, __value)
#define vstrwq_scatter_base_wb_u32(__addr, __offset, __value) __arm_vstrwq_scatter_base_wb_u32(__addr, __offset, __value)
#define vstrwq_scatter_base_wb_f32(__addr, __offset, __value) __arm_vstrwq_scatter_base_wb_f32(__addr, __offset, __value)
#define vddupq_x_n_u8(__a,  __imm, __p) __arm_vddupq_x_n_u8(__a,  __imm, __p)
#define vddupq_x_n_u16(__a,  __imm, __p) __arm_vddupq_x_n_u16(__a,  __imm, __p)
#define vddupq_x_n_u32(__a,  __imm, __p) __arm_vddupq_x_n_u32(__a,  __imm, __p)
#define vddupq_x_wb_u8(__a,  __imm, __p) __arm_vddupq_x_wb_u8(__a,  __imm, __p)
#define vddupq_x_wb_u16(__a,  __imm, __p) __arm_vddupq_x_wb_u16(__a,  __imm, __p)
#define vddupq_x_wb_u32(__a,  __imm, __p) __arm_vddupq_x_wb_u32(__a,  __imm, __p)
#define vdwdupq_x_n_u8(__a, __b,  __imm, __p) __arm_vdwdupq_x_n_u8(__a, __b,  __imm, __p)
#define vdwdupq_x_n_u16(__a, __b,  __imm, __p) __arm_vdwdupq_x_n_u16(__a, __b,  __imm, __p)
#define vdwdupq_x_n_u32(__a, __b,  __imm, __p) __arm_vdwdupq_x_n_u32(__a, __b,  __imm, __p)
#define vdwdupq_x_wb_u8(__a, __b,  __imm, __p) __arm_vdwdupq_x_wb_u8(__a, __b,  __imm, __p)
#define vdwdupq_x_wb_u16(__a, __b,  __imm, __p) __arm_vdwdupq_x_wb_u16(__a, __b,  __imm, __p)
#define vdwdupq_x_wb_u32(__a, __b,  __imm, __p) __arm_vdwdupq_x_wb_u32(__a, __b,  __imm, __p)
#define vidupq_x_n_u8(__a,  __imm, __p) __arm_vidupq_x_n_u8(__a,  __imm, __p)
#define vidupq_x_n_u16(__a,  __imm, __p) __arm_vidupq_x_n_u16(__a,  __imm, __p)
#define vidupq_x_n_u32(__a,  __imm, __p) __arm_vidupq_x_n_u32(__a,  __imm, __p)
#define vidupq_x_wb_u8(__a,  __imm, __p) __arm_vidupq_x_wb_u8(__a,  __imm, __p)
#define vidupq_x_wb_u16(__a,  __imm, __p) __arm_vidupq_x_wb_u16(__a,  __imm, __p)
#define vidupq_x_wb_u32(__a,  __imm, __p) __arm_vidupq_x_wb_u32(__a,  __imm, __p)
#define viwdupq_x_n_u8(__a, __b,  __imm, __p) __arm_viwdupq_x_n_u8(__a, __b,  __imm, __p)
#define viwdupq_x_n_u16(__a, __b,  __imm, __p) __arm_viwdupq_x_n_u16(__a, __b,  __imm, __p)
#define viwdupq_x_n_u32(__a, __b,  __imm, __p) __arm_viwdupq_x_n_u32(__a, __b,  __imm, __p)
#define viwdupq_x_wb_u8(__a, __b,  __imm, __p) __arm_viwdupq_x_wb_u8(__a, __b,  __imm, __p)
#define viwdupq_x_wb_u16(__a, __b,  __imm, __p) __arm_viwdupq_x_wb_u16(__a, __b,  __imm, __p)
#define viwdupq_x_wb_u32(__a, __b,  __imm, __p) __arm_viwdupq_x_wb_u32(__a, __b,  __imm, __p)
#define vbicq_x_s8(__a, __b, __p) __arm_vbicq_x_s8(__a, __b, __p)
#define vbicq_x_s16(__a, __b, __p) __arm_vbicq_x_s16(__a, __b, __p)
#define vbicq_x_s32(__a, __b, __p) __arm_vbicq_x_s32(__a, __b, __p)
#define vbicq_x_u8(__a, __b, __p) __arm_vbicq_x_u8(__a, __b, __p)
#define vbicq_x_u16(__a, __b, __p) __arm_vbicq_x_u16(__a, __b, __p)
#define vbicq_x_u32(__a, __b, __p) __arm_vbicq_x_u32(__a, __b, __p)
#define vornq_x_s8(__a, __b, __p) __arm_vornq_x_s8(__a, __b, __p)
#define vornq_x_s16(__a, __b, __p) __arm_vornq_x_s16(__a, __b, __p)
#define vornq_x_s32(__a, __b, __p) __arm_vornq_x_s32(__a, __b, __p)
#define vornq_x_u8(__a, __b, __p) __arm_vornq_x_u8(__a, __b, __p)
#define vornq_x_u16(__a, __b, __p) __arm_vornq_x_u16(__a, __b, __p)
#define vornq_x_u32(__a, __b, __p) __arm_vornq_x_u32(__a, __b, __p)
#define vcvtaq_x_s16_f16(__a, __p) __arm_vcvtaq_x_s16_f16(__a, __p)
#define vcvtaq_x_s32_f32(__a, __p) __arm_vcvtaq_x_s32_f32(__a, __p)
#define vcvtaq_x_u16_f16(__a, __p) __arm_vcvtaq_x_u16_f16(__a, __p)
#define vcvtaq_x_u32_f32(__a, __p) __arm_vcvtaq_x_u32_f32(__a, __p)
#define vcvtnq_x_s16_f16(__a, __p) __arm_vcvtnq_x_s16_f16(__a, __p)
#define vcvtnq_x_s32_f32(__a, __p) __arm_vcvtnq_x_s32_f32(__a, __p)
#define vcvtnq_x_u16_f16(__a, __p) __arm_vcvtnq_x_u16_f16(__a, __p)
#define vcvtnq_x_u32_f32(__a, __p) __arm_vcvtnq_x_u32_f32(__a, __p)
#define vcvtpq_x_s16_f16(__a, __p) __arm_vcvtpq_x_s16_f16(__a, __p)
#define vcvtpq_x_s32_f32(__a, __p) __arm_vcvtpq_x_s32_f32(__a, __p)
#define vcvtpq_x_u16_f16(__a, __p) __arm_vcvtpq_x_u16_f16(__a, __p)
#define vcvtpq_x_u32_f32(__a, __p) __arm_vcvtpq_x_u32_f32(__a, __p)
#define vcvtmq_x_s16_f16(__a, __p) __arm_vcvtmq_x_s16_f16(__a, __p)
#define vcvtmq_x_s32_f32(__a, __p) __arm_vcvtmq_x_s32_f32(__a, __p)
#define vcvtmq_x_u16_f16(__a, __p) __arm_vcvtmq_x_u16_f16(__a, __p)
#define vcvtmq_x_u32_f32(__a, __p) __arm_vcvtmq_x_u32_f32(__a, __p)
#define vcvtbq_x_f32_f16(__a, __p) __arm_vcvtbq_x_f32_f16(__a, __p)
#define vcvttq_x_f32_f16(__a, __p) __arm_vcvttq_x_f32_f16(__a, __p)
#define vcvtq_x_f16_u16(__a, __p) __arm_vcvtq_x_f16_u16(__a, __p)
#define vcvtq_x_f16_s16(__a, __p) __arm_vcvtq_x_f16_s16(__a, __p)
#define vcvtq_x_f32_s32(__a, __p) __arm_vcvtq_x_f32_s32(__a, __p)
#define vcvtq_x_f32_u32(__a, __p) __arm_vcvtq_x_f32_u32(__a, __p)
#define vcvtq_x_n_f16_s16(__a,  __imm6, __p) __arm_vcvtq_x_n_f16_s16(__a,  __imm6, __p)
#define vcvtq_x_n_f16_u16(__a,  __imm6, __p) __arm_vcvtq_x_n_f16_u16(__a,  __imm6, __p)
#define vcvtq_x_n_f32_s32(__a,  __imm6, __p) __arm_vcvtq_x_n_f32_s32(__a,  __imm6, __p)
#define vcvtq_x_n_f32_u32(__a,  __imm6, __p) __arm_vcvtq_x_n_f32_u32(__a,  __imm6, __p)
#define vcvtq_x_s16_f16(__a, __p) __arm_vcvtq_x_s16_f16(__a, __p)
#define vcvtq_x_s32_f32(__a, __p) __arm_vcvtq_x_s32_f32(__a, __p)
#define vcvtq_x_u16_f16(__a, __p) __arm_vcvtq_x_u16_f16(__a, __p)
#define vcvtq_x_u32_f32(__a, __p) __arm_vcvtq_x_u32_f32(__a, __p)
#define vcvtq_x_n_s16_f16(__a,  __imm6, __p) __arm_vcvtq_x_n_s16_f16(__a,  __imm6, __p)
#define vcvtq_x_n_s32_f32(__a,  __imm6, __p) __arm_vcvtq_x_n_s32_f32(__a,  __imm6, __p)
#define vcvtq_x_n_u16_f16(__a,  __imm6, __p) __arm_vcvtq_x_n_u16_f16(__a,  __imm6, __p)
#define vcvtq_x_n_u32_f32(__a,  __imm6, __p) __arm_vcvtq_x_n_u32_f32(__a,  __imm6, __p)
#define vbicq_x_f16(__a, __b, __p) __arm_vbicq_x_f16(__a, __b, __p)
#define vbicq_x_f32(__a, __b, __p) __arm_vbicq_x_f32(__a, __b, __p)
#define vornq_x_f16(__a, __b, __p) __arm_vornq_x_f16(__a, __b, __p)
#define vornq_x_f32(__a, __b, __p) __arm_vornq_x_f32(__a, __b, __p)
#define vadciq_s32(__a, __b,  __carry_out) __arm_vadciq_s32(__a, __b,  __carry_out)
#define vadciq_u32(__a, __b,  __carry_out) __arm_vadciq_u32(__a, __b,  __carry_out)
#define vadciq_m_s32(__inactive, __a, __b,  __carry_out, __p) __arm_vadciq_m_s32(__inactive, __a, __b,  __carry_out, __p)
#define vadciq_m_u32(__inactive, __a, __b,  __carry_out, __p) __arm_vadciq_m_u32(__inactive, __a, __b,  __carry_out, __p)
#define vadcq_s32(__a, __b,  __carry) __arm_vadcq_s32(__a, __b,  __carry)
#define vadcq_u32(__a, __b,  __carry) __arm_vadcq_u32(__a, __b,  __carry)
#define vadcq_m_s32(__inactive, __a, __b,  __carry, __p) __arm_vadcq_m_s32(__inactive, __a, __b,  __carry, __p)
#define vadcq_m_u32(__inactive, __a, __b,  __carry, __p) __arm_vadcq_m_u32(__inactive, __a, __b,  __carry, __p)
#define vsbciq_s32(__a, __b,  __carry_out) __arm_vsbciq_s32(__a, __b,  __carry_out)
#define vsbciq_u32(__a, __b,  __carry_out) __arm_vsbciq_u32(__a, __b,  __carry_out)
#define vsbciq_m_s32(__inactive, __a, __b,  __carry_out, __p) __arm_vsbciq_m_s32(__inactive, __a, __b,  __carry_out, __p)
#define vsbciq_m_u32(__inactive, __a, __b,  __carry_out, __p) __arm_vsbciq_m_u32(__inactive, __a, __b,  __carry_out, __p)
#define vsbcq_s32(__a, __b,  __carry) __arm_vsbcq_s32(__a, __b,  __carry)
#define vsbcq_u32(__a, __b,  __carry) __arm_vsbcq_u32(__a, __b,  __carry)
#define vsbcq_m_s32(__inactive, __a, __b,  __carry, __p) __arm_vsbcq_m_s32(__inactive, __a, __b,  __carry, __p)
#define vsbcq_m_u32(__inactive, __a, __b,  __carry, __p) __arm_vsbcq_m_u32(__inactive, __a, __b,  __carry, __p)
#define vst1q_p_u8(__addr, __value, __p) __arm_vst1q_p_u8(__addr, __value, __p)
#define vst1q_p_s8(__addr, __value, __p) __arm_vst1q_p_s8(__addr, __value, __p)
#define vst2q_s8(__addr, __value) __arm_vst2q_s8(__addr, __value)
#define vst2q_u8(__addr, __value) __arm_vst2q_u8(__addr, __value)
#define vld1q_z_u8(__base, __p) __arm_vld1q_z_u8(__base, __p)
#define vld1q_z_s8(__base, __p) __arm_vld1q_z_s8(__base, __p)
#define vld2q_s8(__addr) __arm_vld2q_s8(__addr)
#define vld2q_u8(__addr) __arm_vld2q_u8(__addr)
#define vld4q_s8(__addr) __arm_vld4q_s8(__addr)
#define vld4q_u8(__addr) __arm_vld4q_u8(__addr)
#define vst1q_p_u16(__addr, __value, __p) __arm_vst1q_p_u16(__addr, __value, __p)
#define vst1q_p_s16(__addr, __value, __p) __arm_vst1q_p_s16(__addr, __value, __p)
#define vst2q_s16(__addr, __value) __arm_vst2q_s16(__addr, __value)
#define vst2q_u16(__addr, __value) __arm_vst2q_u16(__addr, __value)
#define vld1q_z_u16(__base, __p) __arm_vld1q_z_u16(__base, __p)
#define vld1q_z_s16(__base, __p) __arm_vld1q_z_s16(__base, __p)
#define vld2q_s16(__addr) __arm_vld2q_s16(__addr)
#define vld2q_u16(__addr) __arm_vld2q_u16(__addr)
#define vld4q_s16(__addr) __arm_vld4q_s16(__addr)
#define vld4q_u16(__addr) __arm_vld4q_u16(__addr)
#define vst1q_p_u32(__addr, __value, __p) __arm_vst1q_p_u32(__addr, __value, __p)
#define vst1q_p_s32(__addr, __value, __p) __arm_vst1q_p_s32(__addr, __value, __p)
#define vst2q_s32(__addr, __value) __arm_vst2q_s32(__addr, __value)
#define vst2q_u32(__addr, __value) __arm_vst2q_u32(__addr, __value)
#define vld1q_z_u32(__base, __p) __arm_vld1q_z_u32(__base, __p)
#define vld1q_z_s32(__base, __p) __arm_vld1q_z_s32(__base, __p)
#define vld2q_s32(__addr) __arm_vld2q_s32(__addr)
#define vld2q_u32(__addr) __arm_vld2q_u32(__addr)
#define vld4q_s32(__addr) __arm_vld4q_s32(__addr)
#define vld4q_u32(__addr) __arm_vld4q_u32(__addr)
#define vld4q_f16(__addr) __arm_vld4q_f16(__addr)
#define vld2q_f16(__addr) __arm_vld2q_f16(__addr)
#define vld1q_z_f16(__base, __p) __arm_vld1q_z_f16(__base, __p)
#define vst2q_f16(__addr, __value) __arm_vst2q_f16(__addr, __value)
#define vst1q_p_f16(__addr, __value, __p) __arm_vst1q_p_f16(__addr, __value, __p)
#define vld4q_f32(__addr) __arm_vld4q_f32(__addr)
#define vld2q_f32(__addr) __arm_vld2q_f32(__addr)
#define vld1q_z_f32(__base, __p) __arm_vld1q_z_f32(__base, __p)
#define vst2q_f32(__addr, __value) __arm_vst2q_f32(__addr, __value)
#define vst1q_p_f32(__addr, __value, __p) __arm_vst1q_p_f32(__addr, __value, __p)
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
#define vshlcq_m_s8(__a,  __b,  __imm, __p) __arm_vshlcq_m_s8(__a,  __b,  __imm, __p)
#define vshlcq_m_u8(__a,  __b,  __imm, __p) __arm_vshlcq_m_u8(__a,  __b,  __imm, __p)
#define vshlcq_m_s16(__a,  __b,  __imm, __p) __arm_vshlcq_m_s16(__a,  __b,  __imm, __p)
#define vshlcq_m_u16(__a,  __b,  __imm, __p) __arm_vshlcq_m_u16(__a,  __b,  __imm, __p)
#define vshlcq_m_s32(__a,  __b,  __imm, __p) __arm_vshlcq_m_s32(__a,  __b,  __imm, __p)
#define vshlcq_m_u32(__a,  __b,  __imm, __p) __arm_vshlcq_m_u32(__a,  __b,  __imm, __p)
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

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp16q (uint32_t __a)
{
  return __builtin_mve_vctp16qv8bi (__a);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp32q (uint32_t __a)
{
  return __builtin_mve_vctp32qv4bi (__a);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp64q (uint32_t __a)
{
  return __builtin_mve_vctp64qv2qi (__a);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp8q (uint32_t __a)
{
  return __builtin_mve_vctp8qv16bi (__a);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpnot (mve_pred16_t __a)
{
  return __builtin_mve_vpnotv16bi (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vornq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vbicq_uv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vornq_sv16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vbicq_sv16qi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vornq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vbicq_uv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vornq_sv8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vbicq_sv8hi (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vornq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vbicq_uv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vornq_sv4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vbicq_sv4si (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_u32 (uint32x4_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_uv4si (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_s32 (int32x4_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_sv4si (__a, __imm);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp8q_m (uint32_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vctp8q_mv16bi (__a, __p);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp64q_m (uint32_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vctp64q_mv2qi (__a, __p);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp32q_m (uint32_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vctp32q_mv4bi (__a, __p);
}

__extension__ extern __inline mve_pred16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vctp16q_m (uint32_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vctp16q_mv8bi (__a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_n_s16 (int16x8_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_n_sv8hi (__a, __imm, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_n_s32 (int32x4_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_n_sv4si (__a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_n_u16 (uint16x8_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_n_uv8hi (__a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_n_u32 (uint32x4_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_n_uv4si (__a, __imm, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_s8 (int8x16_t __a, uint32_t * __b, const int __imm)
{
  int8x16_t __res = __builtin_mve_vshlcq_vec_sv16qi (__a, *__b, __imm);
  *__b = __builtin_mve_vshlcq_carry_sv16qi (__a, *__b, __imm);
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_u8 (uint8x16_t __a, uint32_t * __b, const int __imm)
{
  uint8x16_t __res = __builtin_mve_vshlcq_vec_uv16qi (__a, *__b, __imm);
  *__b = __builtin_mve_vshlcq_carry_uv16qi (__a, *__b, __imm);
  return __res;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_s16 (int16x8_t __a, uint32_t * __b, const int __imm)
{
  int16x8_t __res = __builtin_mve_vshlcq_vec_sv8hi (__a, *__b, __imm);
  *__b = __builtin_mve_vshlcq_carry_sv8hi (__a, *__b, __imm);
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_u16 (uint16x8_t __a, uint32_t * __b, const int __imm)
{
  uint16x8_t __res = __builtin_mve_vshlcq_vec_uv8hi (__a, *__b, __imm);
  *__b = __builtin_mve_vshlcq_carry_uv8hi (__a, *__b, __imm);
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_s32 (int32x4_t __a, uint32_t * __b, const int __imm)
{
  int32x4_t __res = __builtin_mve_vshlcq_vec_sv4si (__a, *__b, __imm);
  *__b = __builtin_mve_vshlcq_carry_sv4si (__a, *__b, __imm);
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_u32 (uint32x4_t __a, uint32_t * __b, const int __imm)
{
  uint32x4_t __res = __builtin_mve_vshlcq_vec_uv4si (__a, *__b, __imm);
  *__b = __builtin_mve_vshlcq_carry_uv4si (__a, *__b, __imm);
  return __res;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_u8 (uint8x16_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_uv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_uv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_u16 (uint16x8_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_uv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m_u8 (uint8x16_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_uv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_uv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m_u16 (uint16x8_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_uv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_s8 (int8_t * __base, uint8x16_t __offset, int8x16_t __value)
{
  __builtin_mve_vstrbq_scatter_offset_sv16qi ((__builtin_neon_qi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_s32 (int8_t * __base, uint32x4_t __offset, int32x4_t __value)
{
  __builtin_mve_vstrbq_scatter_offset_sv4si ((__builtin_neon_qi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_s16 (int8_t * __base, uint16x8_t __offset, int16x8_t __value)
{
  __builtin_mve_vstrbq_scatter_offset_sv8hi ((__builtin_neon_qi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_u8 (uint8_t * __base, uint8x16_t __offset, uint8x16_t __value)
{
  __builtin_mve_vstrbq_scatter_offset_uv16qi ((__builtin_neon_qi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_u32 (uint8_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
  __builtin_mve_vstrbq_scatter_offset_uv4si ((__builtin_neon_qi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_u16 (uint8_t * __base, uint16x8_t __offset, uint16x8_t __value)
{
  __builtin_mve_vstrbq_scatter_offset_uv8hi ((__builtin_neon_qi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_s8 (int8_t * __addr, int8x16_t __value)
{
  __builtin_mve_vstrbq_sv16qi ((__builtin_neon_qi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_s32 (int8_t * __addr, int32x4_t __value)
{
  __builtin_mve_vstrbq_sv4si ((__builtin_neon_qi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_s16 (int8_t * __addr, int16x8_t __value)
{
  __builtin_mve_vstrbq_sv8hi ((__builtin_neon_qi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_u8 (uint8_t * __addr, uint8x16_t __value)
{
  __builtin_mve_vstrbq_uv16qi ((__builtin_neon_qi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_u32 (uint8_t * __addr, uint32x4_t __value)
{
  __builtin_mve_vstrbq_uv4si ((__builtin_neon_qi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_u16 (uint8_t * __addr, uint16x8_t __value)
{
  __builtin_mve_vstrbq_uv8hi ((__builtin_neon_qi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_s32 (uint32x4_t __addr, const int __offset, int32x4_t __value)
{
  __builtin_mve_vstrwq_scatter_base_sv4si (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_u32 (uint32x4_t __addr, const int __offset, uint32x4_t __value)
{
  __builtin_mve_vstrwq_scatter_base_uv4si (__addr, __offset, __value);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_u8 (uint8_t const * __base, uint8x16_t __offset)
{
  return __builtin_mve_vldrbq_gather_offset_uv16qi ((__builtin_neon_qi *) __base, __offset);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_s8 (int8_t const * __base, uint8x16_t __offset)
{
  return __builtin_mve_vldrbq_gather_offset_sv16qi ((__builtin_neon_qi *) __base, __offset);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_s8 (int8_t const * __base)
{
  return __builtin_mve_vldrbq_sv16qi ((__builtin_neon_qi *) __base);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_u8 (uint8_t const * __base)
{
  return __builtin_mve_vldrbq_uv16qi ((__builtin_neon_qi *) __base);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_u16 (uint8_t const * __base, uint16x8_t __offset)
{
  return __builtin_mve_vldrbq_gather_offset_uv8hi ((__builtin_neon_qi *) __base, __offset);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_s16 (int8_t const * __base, uint16x8_t __offset)
{
  return __builtin_mve_vldrbq_gather_offset_sv8hi ((__builtin_neon_qi *) __base, __offset);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_s16 (int8_t const * __base)
{
  return __builtin_mve_vldrbq_sv8hi ((__builtin_neon_qi *) __base);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_u16 (uint8_t const * __base)
{
  return __builtin_mve_vldrbq_uv8hi ((__builtin_neon_qi *) __base);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_u32 (uint8_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrbq_gather_offset_uv4si ((__builtin_neon_qi *) __base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_s32 (int8_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrbq_gather_offset_sv4si ((__builtin_neon_qi *) __base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_s32 (int8_t const * __base)
{
  return __builtin_mve_vldrbq_sv4si ((__builtin_neon_qi *) __base);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_u32 (uint8_t const * __base)
{
  return __builtin_mve_vldrbq_uv4si ((__builtin_neon_qi *) __base);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_s32 (uint32x4_t __addr, const int __offset)
{
  return __builtin_mve_vldrwq_gather_base_sv4si (__addr, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_u32 (uint32x4_t __addr, const int __offset)
{
  return __builtin_mve_vldrwq_gather_base_uv4si (__addr, __offset);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p_s8 (int8_t * __addr, int8x16_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_p_sv16qi ((__builtin_neon_qi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p_s32 (int8_t * __addr, int32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_p_sv4si ((__builtin_neon_qi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p_s16 (int8_t * __addr, int16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_p_sv8hi ((__builtin_neon_qi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p_u8 (uint8_t * __addr, uint8x16_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_p_uv16qi ((__builtin_neon_qi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p_u32 (uint8_t * __addr, uint32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_p_uv4si ((__builtin_neon_qi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p_u16 (uint8_t * __addr, uint16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_p_uv8hi ((__builtin_neon_qi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p_s8 (int8_t * __base, uint8x16_t __offset, int8x16_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_scatter_offset_p_sv16qi ((__builtin_neon_qi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p_s32 (int8_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_scatter_offset_p_sv4si ((__builtin_neon_qi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p_s16 (int8_t * __base, uint16x8_t __offset, int16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_scatter_offset_p_sv8hi ((__builtin_neon_qi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p_u8 (uint8_t * __base, uint8x16_t __offset, uint8x16_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_scatter_offset_p_uv16qi ((__builtin_neon_qi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p_u32 (uint8_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_scatter_offset_p_uv4si ((__builtin_neon_qi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p_u16 (uint8_t * __base, uint16x8_t __offset, uint16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrbq_scatter_offset_p_uv8hi ((__builtin_neon_qi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_p_s32 (uint32x4_t __addr, const int __offset, int32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_scatter_base_p_sv4si (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_p_u32 (uint32x4_t __addr, const int __offset, uint32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_scatter_base_p_uv4si (__addr, __offset, __value, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z_s8 (int8_t const * __base, uint8x16_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_gather_offset_z_sv16qi ((__builtin_neon_qi *) __base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z_s32 (int8_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_gather_offset_z_sv4si ((__builtin_neon_qi *) __base, __offset, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z_s16 (int8_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_gather_offset_z_sv8hi ((__builtin_neon_qi *) __base, __offset, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z_u8 (uint8_t const * __base, uint8x16_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_gather_offset_z_uv16qi ((__builtin_neon_qi *) __base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z_u32 (uint8_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_gather_offset_z_uv4si ((__builtin_neon_qi *) __base, __offset, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z_u16 (uint8_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_gather_offset_z_uv8hi ((__builtin_neon_qi *) __base, __offset, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_z_s8 (int8_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_z_sv16qi ((__builtin_neon_qi *) __base, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_z_s32 (int8_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_z_sv4si ((__builtin_neon_qi *) __base, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_z_s16 (int8_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_z_sv8hi ((__builtin_neon_qi *) __base, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_z_u8 (uint8_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_z_uv16qi ((__builtin_neon_qi *) __base, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_z_u32 (uint8_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_z_uv4si ((__builtin_neon_qi *) __base, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_z_u16 (uint8_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrbq_z_uv8hi ((__builtin_neon_qi *) __base, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_z_s32 (uint32x4_t __addr, const int __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_gather_base_z_sv4si (__addr, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_z_u32 (uint32x4_t __addr, const int __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_gather_base_z_uv4si (__addr, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_s32 (int16_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrhq_gather_offset_sv4si ((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_s16 (int16_t const * __base, uint16x8_t __offset)
{
  return __builtin_mve_vldrhq_gather_offset_sv8hi ((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_u32 (uint16_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrhq_gather_offset_uv4si ((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_u16 (uint16_t const * __base, uint16x8_t __offset)
{
  return __builtin_mve_vldrhq_gather_offset_uv8hi ((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z_s32 (int16_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_offset_z_sv4si ((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z_s16 (int16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_offset_z_sv8hi ((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z_u32 (uint16_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_offset_z_uv4si ((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z_u16 (uint16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_offset_z_uv8hi ((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_s32 (int16_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_sv4si ((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_s16 (int16_t const * __base, uint16x8_t __offset)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_sv8hi ((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_u32 (uint16_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_uv4si ((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_u16 (uint16_t const * __base, uint16x8_t __offset)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_uv8hi ((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z_s32 (int16_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_z_sv4si ((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z_s16 (int16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_z_sv8hi ((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z_u32 (uint16_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_z_uv4si ((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z_u16 (uint16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_z_uv8hi ((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_s32 (int16_t const * __base)
{
  return __builtin_mve_vldrhq_sv4si ((__builtin_neon_hi *) __base);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_s16 (int16_t const * __base)
{
  return __builtin_mve_vldrhq_sv8hi ((__builtin_neon_hi *) __base);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_u32 (uint16_t const * __base)
{
  return __builtin_mve_vldrhq_uv4si ((__builtin_neon_hi *) __base);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_u16 (uint16_t const * __base)
{
  return __builtin_mve_vldrhq_uv8hi ((__builtin_neon_hi *) __base);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_z_s32 (int16_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_z_sv4si ((__builtin_neon_hi *) __base, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_z_s16 (int16_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_z_sv8hi ((__builtin_neon_hi *) __base, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_z_u32 (uint16_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_z_uv4si ((__builtin_neon_hi *) __base, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_z_u16 (uint16_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_z_uv8hi ((__builtin_neon_hi *) __base, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_s32 (int32_t const * __base)
{
  return __builtin_mve_vldrwq_sv4si ((__builtin_neon_si *) __base);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_u32 (uint32_t const * __base)
{
  return __builtin_mve_vldrwq_uv4si ((__builtin_neon_si *) __base);
}


__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_z_s32 (int32_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_z_sv4si ((__builtin_neon_si *) __base, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_z_u32 (uint32_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_z_uv4si ((__builtin_neon_si *) __base, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_base_s64 (uint64x2_t __addr, const int __offset)
{
  return __builtin_mve_vldrdq_gather_base_sv2di (__addr, __offset);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_base_u64 (uint64x2_t __addr, const int __offset)
{
  return __builtin_mve_vldrdq_gather_base_uv2di (__addr, __offset);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_base_z_s64 (uint64x2_t __addr, const int __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrdq_gather_base_z_sv2di (__addr, __offset, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_base_z_u64 (uint64x2_t __addr, const int __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrdq_gather_base_z_uv2di (__addr, __offset, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_offset_s64 (int64_t const * __base, uint64x2_t __offset)
{
  return __builtin_mve_vldrdq_gather_offset_sv2di ((__builtin_neon_di *) __base, __offset);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_offset_u64 (uint64_t const * __base, uint64x2_t __offset)
{
  return __builtin_mve_vldrdq_gather_offset_uv2di ((__builtin_neon_di *) __base, __offset);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_offset_z_s64 (int64_t const * __base, uint64x2_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrdq_gather_offset_z_sv2di ((__builtin_neon_di *) __base, __offset, __p);
}


__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_offset_z_u64 (uint64_t const * __base, uint64x2_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrdq_gather_offset_z_uv2di ((__builtin_neon_di *) __base, __offset, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_shifted_offset_s64 (int64_t const * __base, uint64x2_t __offset)
{
  return __builtin_mve_vldrdq_gather_shifted_offset_sv2di ((__builtin_neon_di *) __base, __offset);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_shifted_offset_u64 (uint64_t const * __base, uint64x2_t __offset)
{
  return __builtin_mve_vldrdq_gather_shifted_offset_uv2di ((__builtin_neon_di *) __base, __offset);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_shifted_offset_z_s64 (int64_t const * __base, uint64x2_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrdq_gather_shifted_offset_z_sv2di ((__builtin_neon_di *) __base, __offset, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_shifted_offset_z_u64 (uint64_t const * __base, uint64x2_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrdq_gather_shifted_offset_z_uv2di ((__builtin_neon_di *) __base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset_s32 (int32_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrwq_gather_offset_sv4si ((__builtin_neon_si *) __base, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset_u32 (uint32_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrwq_gather_offset_uv4si ((__builtin_neon_si *) __base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset_z_s32 (int32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_gather_offset_z_sv4si ((__builtin_neon_si *) __base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset_z_u32 (uint32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_gather_offset_z_uv4si ((__builtin_neon_si *) __base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset_s32 (int32_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrwq_gather_shifted_offset_sv4si ((__builtin_neon_si *) __base, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset_u32 (uint32_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrwq_gather_shifted_offset_uv4si ((__builtin_neon_si *) __base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset_z_s32 (int32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_gather_shifted_offset_z_sv4si ((__builtin_neon_si *) __base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset_z_u32 (uint32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_gather_shifted_offset_z_uv4si ((__builtin_neon_si *) __base, __offset, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_s32 (int16_t * __base, uint32x4_t __offset, int32x4_t __value)
{
  __builtin_mve_vstrhq_scatter_offset_sv4si ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_s16 (int16_t * __base, uint16x8_t __offset, int16x8_t __value)
{
  __builtin_mve_vstrhq_scatter_offset_sv8hi ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_u32 (uint16_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
  __builtin_mve_vstrhq_scatter_offset_uv4si ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_u16 (uint16_t * __base, uint16x8_t __offset, uint16x8_t __value)
{
  __builtin_mve_vstrhq_scatter_offset_uv8hi ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p_s32 (int16_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_offset_p_sv4si ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p_s16 (int16_t * __base, uint16x8_t __offset, int16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_offset_p_sv8hi ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p_u32 (uint16_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_offset_p_uv4si ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p_u16 (uint16_t * __base, uint16x8_t __offset, uint16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_offset_p_uv8hi ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_s32 (int16_t * __base, uint32x4_t __offset, int32x4_t __value)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_sv4si ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_s16 (int16_t * __base, uint16x8_t __offset, int16x8_t __value)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_sv8hi ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_u32 (uint16_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_uv4si ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_u16 (uint16_t * __base, uint16x8_t __offset, uint16x8_t __value)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_uv8hi ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p_s32 (int16_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_p_sv4si ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p_s16 (int16_t * __base, uint16x8_t __offset, int16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_p_sv8hi ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p_u32 (uint16_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_p_uv4si ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p_u16 (uint16_t * __base, uint16x8_t __offset, uint16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_p_uv8hi ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_s32 (int16_t * __addr, int32x4_t __value)
{
  __builtin_mve_vstrhq_sv4si ((__builtin_neon_hi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_s16 (int16_t * __addr, int16x8_t __value)
{
  __builtin_mve_vstrhq_sv8hi ((__builtin_neon_hi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_u32 (uint16_t * __addr, uint32x4_t __value)
{
  __builtin_mve_vstrhq_uv4si ((__builtin_neon_hi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_u16 (uint16_t * __addr, uint16x8_t __value)
{
  __builtin_mve_vstrhq_uv8hi ((__builtin_neon_hi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p_s32 (int16_t * __addr, int32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_p_sv4si ((__builtin_neon_hi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p_s16 (int16_t * __addr, int16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_p_sv8hi ((__builtin_neon_hi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p_u32 (uint16_t * __addr, uint32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_p_uv4si ((__builtin_neon_hi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p_u16 (uint16_t * __addr, uint16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_p_uv8hi ((__builtin_neon_hi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_s32 (int32_t * __addr, int32x4_t __value)
{
  __builtin_mve_vstrwq_sv4si ((__builtin_neon_si *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_u32 (uint32_t * __addr, uint32x4_t __value)
{
  __builtin_mve_vstrwq_uv4si ((__builtin_neon_si *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_p_s32 (int32_t * __addr, int32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_p_sv4si ((__builtin_neon_si *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_p_u32 (uint32_t * __addr, uint32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_p_uv4si ((__builtin_neon_si *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_p_s64 (uint64x2_t __addr, const int __offset, int64x2_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrdq_scatter_base_p_sv2di (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_p_u64 (uint64x2_t __addr, const int __offset, uint64x2_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrdq_scatter_base_p_uv2di (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_s64 (uint64x2_t __addr, const int __offset, int64x2_t __value)
{
  __builtin_mve_vstrdq_scatter_base_sv2di (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_u64 (uint64x2_t __addr, const int __offset, uint64x2_t __value)
{
  __builtin_mve_vstrdq_scatter_base_uv2di (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_offset_p_s64 (int64_t * __base, uint64x2_t __offset, int64x2_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrdq_scatter_offset_p_sv2di ((__builtin_neon_di *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_offset_p_u64 (uint64_t * __base, uint64x2_t __offset, uint64x2_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrdq_scatter_offset_p_uv2di ((__builtin_neon_di *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_offset_s64 (int64_t * __base, uint64x2_t __offset, int64x2_t __value)
{
  __builtin_mve_vstrdq_scatter_offset_sv2di ((__builtin_neon_di *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_offset_u64 (uint64_t * __base, uint64x2_t __offset, uint64x2_t __value)
{
  __builtin_mve_vstrdq_scatter_offset_uv2di ((__builtin_neon_di *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_shifted_offset_p_s64 (int64_t * __base, uint64x2_t __offset, int64x2_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrdq_scatter_shifted_offset_p_sv2di ((__builtin_neon_di *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_shifted_offset_p_u64 (uint64_t * __base, uint64x2_t __offset, uint64x2_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrdq_scatter_shifted_offset_p_uv2di ((__builtin_neon_di *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_shifted_offset_s64 (int64_t * __base, uint64x2_t __offset, int64x2_t __value)
{
  __builtin_mve_vstrdq_scatter_shifted_offset_sv2di ((__builtin_neon_di *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_shifted_offset_u64 (uint64_t * __base, uint64x2_t __offset, uint64x2_t __value)
{
  __builtin_mve_vstrdq_scatter_shifted_offset_uv2di ((__builtin_neon_di *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset_p_s32 (int32_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_scatter_offset_p_sv4si ((__builtin_neon_si *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset_p_u32 (uint32_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_scatter_offset_p_uv4si ((__builtin_neon_si *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset_s32 (int32_t * __base, uint32x4_t __offset, int32x4_t __value)
{
  __builtin_mve_vstrwq_scatter_offset_sv4si ((__builtin_neon_si *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset_u32 (uint32_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
  __builtin_mve_vstrwq_scatter_offset_uv4si ((__builtin_neon_si *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset_p_s32 (int32_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_scatter_shifted_offset_p_sv4si ((__builtin_neon_si *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset_p_u32 (uint32_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_scatter_shifted_offset_p_uv4si ((__builtin_neon_si *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset_s32 (int32_t * __base, uint32x4_t __offset, int32x4_t __value)
{
  __builtin_mve_vstrwq_scatter_shifted_offset_sv4si ((__builtin_neon_si *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset_u32 (uint32_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
  __builtin_mve_vstrwq_scatter_shifted_offset_uv4si ((__builtin_neon_si *) __base, __offset, __value);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m_n_u8 (uint8x16_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vddupq_m_n_uv16qi (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m_n_u32 (uint32x4_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vddupq_m_n_uv4si (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m_n_u16 (uint16x8_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vddupq_m_n_uv8hi (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m_wb_u8 (uint8x16_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
  uint8x16_t __res = __builtin_mve_vddupq_m_n_uv16qi (__inactive, * __a, __imm, __p);
  *__a -= __imm * 16u;
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m_wb_u16 (uint16x8_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
  uint16x8_t __res = __builtin_mve_vddupq_m_n_uv8hi (__inactive, *__a, __imm, __p);
  *__a -= __imm * 8u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m_wb_u32 (uint32x4_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
  uint32x4_t __res = __builtin_mve_vddupq_m_n_uv4si (__inactive, *__a, __imm, __p);
  *__a -= __imm * 4u;
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_n_u8 (uint32_t __a, const int __imm)
{
  return __builtin_mve_vddupq_n_uv16qi (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_n_u32 (uint32_t __a, const int __imm)
{
  return __builtin_mve_vddupq_n_uv4si (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_n_u16 (uint32_t __a, const int __imm)
{
  return __builtin_mve_vddupq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m_n_u8 (uint8x16_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_vdwdupq_m_n_uv16qi (__inactive, __a, __c, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m_n_u32 (uint32x4_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_vdwdupq_m_n_uv4si (__inactive, __a, __c, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m_n_u16 (uint16x8_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_vdwdupq_m_n_uv8hi (__inactive, __a, __c, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m_wb_u8 (uint8x16_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint8x16_t __res =  __builtin_mve_vdwdupq_m_n_uv16qi (__inactive, *__a, __c, __imm, __p);
  *__a = __builtin_mve_vdwdupq_m_wb_uv16qi (__inactive, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m_wb_u32 (uint32x4_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint32x4_t __res =  __builtin_mve_vdwdupq_m_n_uv4si (__inactive, *__a, __c, __imm, __p);
  *__a = __builtin_mve_vdwdupq_m_wb_uv4si (__inactive, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m_wb_u16 (uint16x8_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint16x8_t __res =  __builtin_mve_vdwdupq_m_n_uv8hi (__inactive, *__a, __c, __imm, __p);
  *__a = __builtin_mve_vdwdupq_m_wb_uv8hi (__inactive, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_n_u8 (uint32_t __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_vdwdupq_n_uv16qi (__a, __c, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_n_u32 (uint32_t __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_vdwdupq_n_uv4si (__a, __c, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_n_u16 (uint32_t __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_vdwdupq_n_uv8hi (__a, __c, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_wb_u8 (uint32_t * __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint8x16_t __res = __builtin_mve_vdwdupq_n_uv16qi (*__a, __c, __imm);
  *__a = __builtin_mve_vdwdupq_wb_uv16qi (*__a, __c, __imm);
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_wb_u32 (uint32_t * __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint32x4_t __res = __builtin_mve_vdwdupq_n_uv4si (*__a, __c, __imm);
  *__a = __builtin_mve_vdwdupq_wb_uv4si (*__a, __c, __imm);
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_wb_u16 (uint32_t * __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint16x8_t __res = __builtin_mve_vdwdupq_n_uv8hi (*__a, __c, __imm);
  *__a = __builtin_mve_vdwdupq_wb_uv8hi (*__a, __c, __imm);
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m_n_u8 (uint8x16_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vidupq_m_n_uv16qi (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m_n_u32 (uint32x4_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vidupq_m_n_uv4si (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m_n_u16 (uint16x8_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vidupq_m_n_uv8hi (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_n_u8 (uint32_t __a, const int __imm)
{
  return __builtin_mve_vidupq_n_uv16qi (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m_wb_u8 (uint8x16_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
  uint8x16_t __res = __builtin_mve_vidupq_m_n_uv16qi (__inactive, *__a, __imm, __p);
  *__a += __imm * 16u;
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m_wb_u16 (uint16x8_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
  uint16x8_t __res = __builtin_mve_vidupq_m_n_uv8hi (__inactive, *__a, __imm, __p);
  *__a += __imm * 8u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m_wb_u32 (uint32x4_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
  uint32x4_t __res = __builtin_mve_vidupq_m_n_uv4si (__inactive, *__a, __imm, __p);
  *__a += __imm * 4u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_n_u32 (uint32_t __a, const int __imm)
{
  return __builtin_mve_vidupq_n_uv4si (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_n_u16 (uint32_t __a, const int __imm)
{
  return __builtin_mve_vidupq_n_uv8hi (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_wb_u8 (uint32_t * __a, const int __imm)
{
  uint8x16_t __res = __builtin_mve_vidupq_n_uv16qi (*__a, __imm);
  *__a += __imm * 16u;
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_wb_u16 (uint32_t * __a, const int __imm)
{
  uint16x8_t __res = __builtin_mve_vidupq_n_uv8hi (*__a, __imm);
  *__a += __imm * 8u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_wb_u32 (uint32_t * __a, const int __imm)
{
  uint32x4_t __res = __builtin_mve_vidupq_n_uv4si (*__a, __imm);
  *__a += __imm * 4u;
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_wb_u8 (uint32_t * __a, const int __imm)
{
  uint8x16_t __res = __builtin_mve_vddupq_n_uv16qi (*__a, __imm);
  *__a -= __imm * 16u;
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_wb_u16 (uint32_t * __a, const int __imm)
{
  uint16x8_t __res = __builtin_mve_vddupq_n_uv8hi (*__a, __imm);
  *__a -= __imm * 8u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_wb_u32 (uint32_t * __a, const int __imm)
{
  uint32x4_t __res = __builtin_mve_vddupq_n_uv4si (*__a, __imm);
  *__a -= __imm * 4u;
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m_n_u8 (uint8x16_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_viwdupq_m_n_uv16qi (__inactive, __a, __c, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m_n_u32 (uint32x4_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_viwdupq_m_n_uv4si (__inactive, __a, __c, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m_n_u16 (uint16x8_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_viwdupq_m_n_uv8hi (__inactive, __a, __c, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m_wb_u8 (uint8x16_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint8x16_t __res = __builtin_mve_viwdupq_m_n_uv16qi (__inactive, *__a, __c, __imm, __p);
  *__a =  __builtin_mve_viwdupq_m_wb_uv16qi (__inactive, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m_wb_u32 (uint32x4_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint32x4_t __res = __builtin_mve_viwdupq_m_n_uv4si (__inactive, *__a, __c, __imm, __p);
  *__a =  __builtin_mve_viwdupq_m_wb_uv4si (__inactive, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m_wb_u16 (uint16x8_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint16x8_t __res = __builtin_mve_viwdupq_m_n_uv8hi (__inactive, *__a, __c, __imm, __p);
  *__a =  __builtin_mve_viwdupq_m_wb_uv8hi (__inactive, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_n_u8 (uint32_t __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_viwdupq_n_uv16qi (__a, __c, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_n_u32 (uint32_t __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_viwdupq_n_uv4si (__a, __c, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_n_u16 (uint32_t __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_viwdupq_n_uv8hi (__a, __c, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_wb_u8 (uint32_t * __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint8x16_t __res = __builtin_mve_viwdupq_n_uv16qi (*__a, __c, __imm);
  *__a = __builtin_mve_viwdupq_wb_uv16qi (*__a, __c, __imm);
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_wb_u32 (uint32_t * __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint32x4_t __res = __builtin_mve_viwdupq_n_uv4si (*__a, __c, __imm);
  *__a = __builtin_mve_viwdupq_wb_uv4si (*__a, __c, __imm);
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_wb_u16 (uint32_t * __a, uint32_t __b, const int __imm)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint16x8_t __res = __builtin_mve_viwdupq_n_uv8hi (*__a, __c, __imm);
  *__a = __builtin_mve_viwdupq_wb_uv8hi (*__a, __c, __imm);
  return __res;
}


__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_base_wb_s64 (uint64x2_t * __addr, const int __offset)
{
  int64x2_t
  result = __builtin_mve_vldrdq_gather_base_nowb_sv2di (*__addr, __offset);
  *__addr = __builtin_mve_vldrdq_gather_base_wb_sv2di (*__addr, __offset);
  return result;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_base_wb_u64 (uint64x2_t * __addr, const int __offset)
{
  uint64x2_t
  result = __builtin_mve_vldrdq_gather_base_nowb_uv2di (*__addr, __offset);
  *__addr = __builtin_mve_vldrdq_gather_base_wb_uv2di (*__addr, __offset);
  return result;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_base_wb_z_s64 (uint64x2_t * __addr, const int __offset, mve_pred16_t __p)
{
  int64x2_t
  result = __builtin_mve_vldrdq_gather_base_nowb_z_sv2di (*__addr, __offset, __p);
  *__addr = __builtin_mve_vldrdq_gather_base_wb_z_sv2di (*__addr, __offset, __p);
  return result;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_base_wb_z_u64 (uint64x2_t * __addr, const int __offset, mve_pred16_t __p)
{
  uint64x2_t
  result = __builtin_mve_vldrdq_gather_base_nowb_z_uv2di (*__addr, __offset, __p);
  *__addr = __builtin_mve_vldrdq_gather_base_wb_z_uv2di (*__addr, __offset, __p);
  return result;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_wb_s32 (uint32x4_t * __addr, const int __offset)
{
  int32x4_t
  result = __builtin_mve_vldrwq_gather_base_nowb_sv4si (*__addr, __offset);
  *__addr = __builtin_mve_vldrwq_gather_base_wb_sv4si (*__addr, __offset);
  return result;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_wb_u32 (uint32x4_t * __addr, const int __offset)
{
  uint32x4_t
  result = __builtin_mve_vldrwq_gather_base_nowb_uv4si (*__addr, __offset);
  *__addr = __builtin_mve_vldrwq_gather_base_wb_uv4si (*__addr, __offset);
  return result;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_wb_z_s32 (uint32x4_t * __addr, const int __offset, mve_pred16_t __p)
{
  int32x4_t
  result = __builtin_mve_vldrwq_gather_base_nowb_z_sv4si (*__addr, __offset, __p);
  *__addr = __builtin_mve_vldrwq_gather_base_wb_z_sv4si (*__addr, __offset, __p);
  return result;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_wb_z_u32 (uint32x4_t * __addr, const int __offset, mve_pred16_t __p)
{
  uint32x4_t
  result = __builtin_mve_vldrwq_gather_base_nowb_z_uv4si (*__addr, __offset, __p);
  *__addr = __builtin_mve_vldrwq_gather_base_wb_z_uv4si (*__addr, __offset, __p);
  return result;
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_wb_s64 (uint64x2_t * __addr, const int __offset, int64x2_t __value)
{
  *__addr = __builtin_mve_vstrdq_scatter_base_wb_sv2di (*__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_wb_u64 (uint64x2_t * __addr, const int __offset, uint64x2_t __value)
{
  *__addr = __builtin_mve_vstrdq_scatter_base_wb_uv2di (*__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_wb_p_s64 (uint64x2_t * __addr, const int __offset, int64x2_t __value, mve_pred16_t __p)
{
 *__addr =  __builtin_mve_vstrdq_scatter_base_wb_p_sv2di (*__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_wb_p_u64 (uint64x2_t * __addr, const int __offset, uint64x2_t __value, mve_pred16_t __p)
{
  *__addr = __builtin_mve_vstrdq_scatter_base_wb_p_uv2di (*__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb_p_s32 (uint32x4_t * __addr, const int __offset, int32x4_t __value, mve_pred16_t __p)
{
  *__addr = __builtin_mve_vstrwq_scatter_base_wb_p_sv4si (*__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb_p_u32 (uint32x4_t * __addr, const int __offset, uint32x4_t __value, mve_pred16_t __p)
{
  *__addr = __builtin_mve_vstrwq_scatter_base_wb_p_uv4si (*__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb_s32 (uint32x4_t * __addr, const int __offset, int32x4_t __value)
{
  *__addr = __builtin_mve_vstrwq_scatter_base_wb_sv4si (*__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb_u32 (uint32x4_t * __addr, const int __offset, uint32x4_t __value)
{
  *__addr = __builtin_mve_vstrwq_scatter_base_wb_uv4si (*__addr, __offset, __value);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_n_u8 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vddupq_m_n_uv16qi (__arm_vuninitializedq_u8 (), __a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_n_u16 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vddupq_m_n_uv8hi (__arm_vuninitializedq_u16 (), __a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_n_u32 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vddupq_m_n_uv4si (__arm_vuninitializedq_u32 (), __a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_wb_u8 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
  uint8x16_t __arg1 = __arm_vuninitializedq_u8 ();
  uint8x16_t __res = __builtin_mve_vddupq_m_n_uv16qi (__arg1, * __a, __imm, __p);
  *__a -= __imm * 16u;
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_wb_u16 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
  uint16x8_t __arg1 = __arm_vuninitializedq_u16 ();
  uint16x8_t __res = __builtin_mve_vddupq_m_n_uv8hi (__arg1, *__a, __imm, __p);
  *__a -= __imm * 8u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_wb_u32 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
  uint32x4_t __arg1 = __arm_vuninitializedq_u32 ();
  uint32x4_t __res = __builtin_mve_vddupq_m_n_uv4si (__arg1, *__a, __imm, __p);
  *__a -= __imm * 4u;
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_n_u8 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_vdwdupq_m_n_uv16qi (__arm_vuninitializedq_u8 (), __a, __c, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_n_u16 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_vdwdupq_m_n_uv8hi (__arm_vuninitializedq_u16 (), __a, __c, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_n_u32 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_vdwdupq_m_n_uv4si (__arm_vuninitializedq_u32 (), __a, __c, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_wb_u8 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint8x16_t __arg1 = __arm_vuninitializedq_u8 ();
  uint8x16_t __res = __builtin_mve_vdwdupq_m_n_uv16qi (__arg1, *__a, __c, __imm, __p);
  *__a = __builtin_mve_vdwdupq_m_wb_uv16qi (__arg1, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_wb_u16 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint16x8_t __arg1 = __arm_vuninitializedq_u16 ();
  uint16x8_t __res =  __builtin_mve_vdwdupq_m_n_uv8hi (__arg1, *__a, __c, __imm, __p);
  *__a = __builtin_mve_vdwdupq_m_wb_uv8hi (__arg1, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_wb_u32 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint32x4_t __arg1 = __arm_vuninitializedq_u32 ();
  uint32x4_t __res =  __builtin_mve_vdwdupq_m_n_uv4si (__arg1, *__a, __c, __imm, __p);
  *__a = __builtin_mve_vdwdupq_m_wb_uv4si (__arg1, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_n_u8 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vidupq_m_n_uv16qi (__arm_vuninitializedq_u8 (), __a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_n_u16 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vidupq_m_n_uv8hi (__arm_vuninitializedq_u16 (), __a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_n_u32 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vidupq_m_n_uv4si (__arm_vuninitializedq_u32 (), __a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_wb_u8 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
  uint8x16_t __arg1 = __arm_vuninitializedq_u8 ();
  uint8x16_t __res = __builtin_mve_vidupq_m_n_uv16qi (__arg1, *__a, __imm, __p);
  *__a += __imm * 16u;
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_wb_u16 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
  uint16x8_t __arg1 = __arm_vuninitializedq_u16 ();
  uint16x8_t __res = __builtin_mve_vidupq_m_n_uv8hi (__arg1, *__a, __imm, __p);
  *__a += __imm * 8u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_wb_u32 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
  uint32x4_t __arg1 = __arm_vuninitializedq_u32 ();
  uint32x4_t __res = __builtin_mve_vidupq_m_n_uv4si (__arg1, *__a, __imm, __p);
  *__a += __imm * 4u;
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_n_u8 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_viwdupq_m_n_uv16qi (__arm_vuninitializedq_u8 (), __a, __c, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_n_u16 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_viwdupq_m_n_uv8hi (__arm_vuninitializedq_u16 (), __a, __c, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_n_u32 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  return __builtin_mve_viwdupq_m_n_uv4si (__arm_vuninitializedq_u32 (), __a, __c, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_wb_u8 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint8x16_t __arg1 = __arm_vuninitializedq_u8 ();
  uint8x16_t __res = __builtin_mve_viwdupq_m_n_uv16qi (__arg1, *__a, __c, __imm, __p);
  *__a =  __builtin_mve_viwdupq_m_wb_uv16qi (__arg1, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_wb_u16 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint16x8_t __arg1 = __arm_vuninitializedq_u16 ();
  uint16x8_t __res = __builtin_mve_viwdupq_m_n_uv8hi (__arg1, *__a, __c, __imm, __p);
  *__a =  __builtin_mve_viwdupq_m_wb_uv8hi (__arg1, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_wb_u32 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
  uint64_t __c = ((uint64_t) __b) << 32;
  uint32x4_t __arg1 = __arm_vuninitializedq_u32 ();
  uint32x4_t __res = __builtin_mve_viwdupq_m_n_uv4si (__arg1, *__a, __c, __imm, __p);
  *__a =  __builtin_mve_viwdupq_m_wb_uv4si (__arg1, *__a, __c, __imm, __p);
  return __res;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_sv16qi (__arm_vuninitializedq_s8 (), __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_sv4si (__arm_vuninitializedq_s32 (), __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x_u8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_uv16qi (__arm_vuninitializedq_u8 (), __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x_u16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_uv8hi (__arm_vuninitializedq_u16 (), __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_uv4si (__arm_vuninitializedq_u32 (), __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_sv16qi (__arm_vuninitializedq_s8 (), __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_sv4si (__arm_vuninitializedq_s32 (), __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x_u8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_uv16qi (__arm_vuninitializedq_u8 (), __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x_u16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_uv8hi (__arm_vuninitializedq_u16 (), __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_uv4si (__arm_vuninitializedq_u32 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadciq_s32 (int32x4_t __a, int32x4_t __b, unsigned * __carry_out)
{
  int32x4_t __res = __builtin_mve_vadciq_sv4si (__a, __b);
  *__carry_out = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadciq_u32 (uint32x4_t __a, uint32x4_t __b, unsigned * __carry_out)
{
  uint32x4_t __res = __builtin_mve_vadciq_uv4si (__a, __b);
  *__carry_out = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadciq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry_out, mve_pred16_t __p)
{
  int32x4_t __res =  __builtin_mve_vadciq_m_sv4si (__inactive, __a, __b, __p);
  *__carry_out = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadciq_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry_out, mve_pred16_t __p)
{
  uint32x4_t __res = __builtin_mve_vadciq_m_uv4si (__inactive, __a, __b, __p);
  *__carry_out = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq_s32 (int32x4_t __a, int32x4_t __b, unsigned * __carry)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | ((*__carry & 0x1u) << 29));
  int32x4_t __res = __builtin_mve_vadcq_sv4si (__a, __b);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq_u32 (uint32x4_t __a, uint32x4_t __b, unsigned * __carry)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | ((*__carry & 0x1u) << 29));
  uint32x4_t __res = __builtin_mve_vadcq_uv4si (__a, __b);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | ((*__carry & 0x1u) << 29));
  int32x4_t __res = __builtin_mve_vadcq_m_sv4si (__inactive, __a, __b, __p);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | ((*__carry & 0x1u) << 29));
  uint32x4_t __res =  __builtin_mve_vadcq_m_uv4si (__inactive, __a, __b, __p);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbciq_s32 (int32x4_t __a, int32x4_t __b, unsigned * __carry_out)
{
  int32x4_t __res = __builtin_mve_vsbciq_sv4si (__a, __b);
  *__carry_out = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbciq_u32 (uint32x4_t __a, uint32x4_t __b, unsigned * __carry_out)
{
  uint32x4_t __res = __builtin_mve_vsbciq_uv4si (__a, __b);
  *__carry_out = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbciq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry_out, mve_pred16_t __p)
{
  int32x4_t __res = __builtin_mve_vsbciq_m_sv4si (__inactive, __a, __b, __p);
  *__carry_out = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbciq_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry_out, mve_pred16_t __p)
{
  uint32x4_t __res = __builtin_mve_vsbciq_m_uv4si (__inactive, __a, __b, __p);
  *__carry_out = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq_s32 (int32x4_t __a, int32x4_t __b, unsigned * __carry)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | ((*__carry & 0x1u) << 29));
  int32x4_t __res = __builtin_mve_vsbcq_sv4si (__a, __b);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq_u32 (uint32x4_t __a, uint32x4_t __b, unsigned * __carry)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | ((*__carry & 0x1u) << 29));
  uint32x4_t __res =  __builtin_mve_vsbcq_uv4si (__a, __b);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | ((*__carry & 0x1u) << 29));
  int32x4_t __res = __builtin_mve_vsbcq_m_sv4si (__inactive, __a, __b, __p);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | ((*__carry & 0x1u) << 29));
  uint32x4_t __res = __builtin_mve_vsbcq_m_uv4si (__inactive, __a, __b, __p);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p_u8 (uint8_t * __addr, uint8x16_t __value, mve_pred16_t __p)
{
  return __arm_vstrbq_p_u8 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p_s8 (int8_t * __addr, int8x16_t __value, mve_pred16_t __p)
{
  return __arm_vstrbq_p_s8 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_s8 (int8_t * __addr, int8x16x2_t __value)
{
  union { int8x16x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv16qi ((__builtin_neon_qi *) __addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_u8 (uint8_t * __addr, uint8x16x2_t __value)
{
  union { uint8x16x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv16qi ((__builtin_neon_qi *) __addr, __rv.__o);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z_u8 (uint8_t const *__base, mve_pred16_t __p)
{
  return __arm_vldrbq_z_u8 ( __base, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z_s8 (int8_t const *__base, mve_pred16_t __p)
{
  return __arm_vldrbq_z_s8 ( __base, __p);
}

__extension__ extern __inline int8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q_s8 (int8_t const * __addr)
{
  union { int8x16x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_mve_vld2qv16qi ((__builtin_neon_qi *) __addr);
  return __rv.__i;
}

__extension__ extern __inline uint8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q_u8 (uint8_t const * __addr)
{
  union { uint8x16x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_mve_vld2qv16qi ((__builtin_neon_qi *) __addr);
  return __rv.__i;
}

__extension__ extern __inline int8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q_s8 (int8_t const * __addr)
{
  union { int8x16x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_mve_vld4qv16qi ((__builtin_neon_qi *) __addr);
  return __rv.__i;
}

__extension__ extern __inline uint8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q_u8 (uint8_t const * __addr)
{
  union { uint8x16x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_mve_vld4qv16qi ((__builtin_neon_qi *) __addr);
  return __rv.__i;
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p_u16 (uint16_t * __addr, uint16x8_t __value, mve_pred16_t __p)
{
  return __arm_vstrhq_p_u16 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p_s16 (int16_t * __addr, int16x8_t __value, mve_pred16_t __p)
{
  return __arm_vstrhq_p_s16 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_s16 (int16_t * __addr, int16x8x2_t __value)
{
  union { int16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv8hi ((__builtin_neon_hi *) __addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_u16 (uint16_t * __addr, uint16x8x2_t __value)
{
  union { uint16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv8hi ((__builtin_neon_hi *) __addr, __rv.__o);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z_u16 (uint16_t const *__base, mve_pred16_t __p)
{
  return __arm_vldrhq_z_u16 ( __base, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z_s16 (int16_t const *__base, mve_pred16_t __p)
{
  return __arm_vldrhq_z_s16 ( __base, __p);
}

__extension__ extern __inline int16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q_s16 (int16_t const * __addr)
{
  union { int16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_mve_vld2qv8hi ((__builtin_neon_hi *) __addr);
  return __rv.__i;
}

__extension__ extern __inline uint16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q_u16 (uint16_t const * __addr)
{
  union { uint16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_mve_vld2qv8hi ((__builtin_neon_hi *) __addr);
  return __rv.__i;
}

__extension__ extern __inline int16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q_s16 (int16_t const * __addr)
{
  union { int16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_mve_vld4qv8hi ((__builtin_neon_hi *) __addr);
  return __rv.__i;
}

__extension__ extern __inline uint16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q_u16 (uint16_t const * __addr)
{
  union { uint16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_mve_vld4qv8hi ((__builtin_neon_hi *) __addr);
  return __rv.__i;
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p_u32 (uint32_t * __addr, uint32x4_t __value, mve_pred16_t __p)
{
  return __arm_vstrwq_p_u32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p_s32 (int32_t * __addr, int32x4_t __value, mve_pred16_t __p)
{
  return __arm_vstrwq_p_s32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_s32 (int32_t * __addr, int32x4x2_t __value)
{
  union { int32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv4si ((__builtin_neon_si *) __addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_u32 (uint32_t * __addr, uint32x4x2_t __value)
{
  union { uint32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv4si ((__builtin_neon_si *) __addr, __rv.__o);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z_u32 (uint32_t const *__base, mve_pred16_t __p)
{
  return __arm_vldrwq_z_u32 ( __base, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z_s32 (int32_t const *__base, mve_pred16_t __p)
{
  return __arm_vldrwq_z_s32 ( __base, __p);
}

__extension__ extern __inline int32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q_s32 (int32_t const * __addr)
{
  union { int32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_mve_vld2qv4si ((__builtin_neon_si *) __addr);
  return __rv.__i;
}

__extension__ extern __inline uint32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q_u32 (uint32_t const * __addr)
{
  union { uint32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_mve_vld2qv4si ((__builtin_neon_si *) __addr);
  return __rv.__i;
}

__extension__ extern __inline int32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q_s32 (int32_t const * __addr)
{
  union { int32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_mve_vld4qv4si ((__builtin_neon_si *) __addr);
  return __rv.__i;
}

__extension__ extern __inline uint32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q_u32 (uint32_t const * __addr)
{
  union { uint32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_mve_vld4qv4si ((__builtin_neon_si *) __addr);
  return __rv.__i;
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

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m_s8 (int8x16_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
  int8x16_t __res = __builtin_mve_vshlcq_m_vec_sv16qi (__a, *__b, __imm, __p);
  *__b = __builtin_mve_vshlcq_m_carry_sv16qi (__a, *__b, __imm, __p);
  return __res;
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m_u8 (uint8x16_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
  uint8x16_t __res = __builtin_mve_vshlcq_m_vec_uv16qi (__a, *__b, __imm, __p);
  *__b = __builtin_mve_vshlcq_m_carry_uv16qi (__a, *__b, __imm, __p);
  return __res;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m_s16 (int16x8_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
  int16x8_t __res = __builtin_mve_vshlcq_m_vec_sv8hi (__a, *__b, __imm, __p);
  *__b = __builtin_mve_vshlcq_m_carry_sv8hi (__a, *__b, __imm, __p);
  return __res;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m_u16 (uint16x8_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
  uint16x8_t __res = __builtin_mve_vshlcq_m_vec_uv8hi (__a, *__b, __imm, __p);
  *__b = __builtin_mve_vshlcq_m_carry_uv8hi (__a, *__b, __imm, __p);
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m_s32 (int32x4_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
  int32x4_t __res = __builtin_mve_vshlcq_m_vec_sv4si (__a, *__b, __imm, __p);
  *__b = __builtin_mve_vshlcq_m_carry_sv4si (__a, *__b, __imm, __p);
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m_u32 (uint32x4_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
  uint32x4_t __res = __builtin_mve_vshlcq_m_vec_uv4si (__a, *__b, __imm, __p);
  *__b = __builtin_mve_vshlcq_m_carry_uv4si (__a, *__b, __imm, __p);
  return __res;
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

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_u32_f32 (float32x4_t __a)
{
  return __builtin_mve_vcvtnq_uv4si (__a);
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

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vornq_fv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vbicq_fv8hf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vornq_fv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vbicq_fv4sf (__a, __b);
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

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_m_s16_f16 (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtaq_m_sv8hi (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_m_u16_f16 (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtaq_m_uv8hi (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_m_s32_f32 (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtaq_m_sv4si (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_m_u32_f32 (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtaq_m_uv4si (__inactive, __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_f16_s16 (float16x8_t __inactive, int16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_to_f_sv8hf (__inactive, __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_f16_u16 (float16x8_t __inactive, uint16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_to_f_uv8hf (__inactive, __a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_f32_s32 (float32x4_t __inactive, int32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_to_f_sv4sf (__inactive, __a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_f32_u32 (float32x4_t __inactive, uint32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_to_f_uv4sf (__inactive, __a, __p);
}


__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtbq_m_f16_f32 (float16x8_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcvtbq_m_f16_f32v8hf (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtbq_m_f32_f16 (float32x4_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtbq_m_f32_f16v4sf (__inactive, __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvttq_m_f16_f32 (float16x8_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcvttq_m_f16_f32v8hf (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvttq_m_f32_f16 (float32x4_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvttq_m_f32_f16v4sf (__inactive, __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_m_s16_f16 (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtmq_m_sv8hi (__inactive, __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_m_s16_f16 (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtnq_m_sv8hi (__inactive, __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_m_s16_f16 (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtpq_m_sv8hi (__inactive, __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_s16_f16 (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_from_f_sv8hi (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_m_u16_f16 (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtmq_m_uv8hi (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_m_u16_f16 (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtnq_m_uv8hi (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_m_u16_f16 (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtpq_m_uv8hi (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_u16_f16 (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_from_f_uv8hi (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_m_s32_f32 (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtmq_m_sv4si (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_m_s32_f32 (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtnq_m_sv4si (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_m_s32_f32 (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtpq_m_sv4si (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_s32_f32 (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_from_f_sv4si (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_m_u32_f32 (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtmq_m_uv4si (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_m_u32_f32 (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtnq_m_uv4si (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_m_u32_f32 (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtpq_m_uv4si (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_u32_f32 (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_from_f_uv4si (__inactive, __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n_f16_u16 (float16x8_t __inactive, uint16x8_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_to_f_uv8hf (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n_f16_s16 (float16x8_t __inactive, int16x8_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_to_f_sv8hf (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n_f32_u32 (float32x4_t __inactive, uint32x4_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_to_f_uv4sf (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n_f32_s32 (float32x4_t __inactive, int32x4_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_to_f_sv4sf (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_f32 (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_fv4sf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_f16 (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_fv8hf (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n_s32_f32 (int32x4_t __inactive, float32x4_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_from_f_sv4si (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n_s16_f16 (int16x8_t __inactive, float16x8_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_from_f_sv8hi (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n_u32_f32 (uint32x4_t __inactive, float32x4_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_from_f_uv4si (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n_u16_f16 (uint16x8_t __inactive, float16x8_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_from_f_uv8hi (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m_f32 (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_fv4sf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m_f16 (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_fv8hf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_f32 (float32_t const * __base)
{
  return __builtin_mve_vldrwq_fv4sf((__builtin_neon_si *) __base);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_z_f32 (float32_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_z_fv4sf((__builtin_neon_si *) __base, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_z_f16 (float16_t const * __base, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_z_fv8hf((__builtin_neon_hi *) __base, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_f16 (float16_t const * __base)
{
  return __builtin_mve_vldrhq_fv8hf((__builtin_neon_hi *) __base);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_f16 (float16_t const * __base, uint16x8_t __offset)
{
  return __builtin_mve_vldrhq_gather_offset_fv8hf((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z_f16 (float16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_offset_z_fv8hf((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_f16 (float16_t const * __base, uint16x8_t __offset)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_fv8hf ((__builtin_neon_hi *) __base, __offset);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z_f16 (float16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrhq_gather_shifted_offset_z_fv8hf ((__builtin_neon_hi *) __base, __offset, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_f32 (uint32x4_t __addr, const int __offset)
{
  return __builtin_mve_vldrwq_gather_base_fv4sf (__addr, __offset);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_z_f32 (uint32x4_t __addr, const int __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_gather_base_z_fv4sf (__addr, __offset, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset_f32 (float32_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrwq_gather_offset_fv4sf((__builtin_neon_si *) __base, __offset);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset_z_f32 (float32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_gather_offset_z_fv4sf((__builtin_neon_si *) __base, __offset, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset_f32 (float32_t const * __base, uint32x4_t __offset)
{
  return __builtin_mve_vldrwq_gather_shifted_offset_fv4sf ((__builtin_neon_si *) __base, __offset);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset_z_f32 (float32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
  return __builtin_mve_vldrwq_gather_shifted_offset_z_fv4sf ((__builtin_neon_si *) __base, __offset, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_p_f32 (float32_t * __addr, float32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_p_fv4sf ((__builtin_neon_si *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_f32 (float32_t * __addr, float32x4_t __value)
{
  __builtin_mve_vstrwq_fv4sf ((__builtin_neon_si *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_f16 (float16_t * __addr, float16x8_t __value)
{
  __builtin_mve_vstrhq_fv8hf ((__builtin_neon_hi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p_f16 (float16_t * __addr, float16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_p_fv8hf ((__builtin_neon_hi *) __addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_f16 (float16_t * __base, uint16x8_t __offset, float16x8_t __value)
{
  __builtin_mve_vstrhq_scatter_offset_fv8hf ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p_f16 (float16_t * __base, uint16x8_t __offset, float16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_offset_p_fv8hf ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_f16 (float16_t * __base, uint16x8_t __offset, float16x8_t __value)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_fv8hf ((__builtin_neon_hi *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p_f16 (float16_t * __base, uint16x8_t __offset, float16x8_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrhq_scatter_shifted_offset_p_fv8hf ((__builtin_neon_hi *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_f32 (uint32x4_t __addr, const int __offset, float32x4_t __value)
{
  __builtin_mve_vstrwq_scatter_base_fv4sf (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_p_f32 (uint32x4_t __addr, const int __offset, float32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_scatter_base_p_fv4sf (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset_f32 (float32_t * __base, uint32x4_t __offset, float32x4_t __value)
{
  __builtin_mve_vstrwq_scatter_offset_fv4sf ((__builtin_neon_si *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset_p_f32 (float32_t * __base, uint32x4_t __offset, float32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_scatter_offset_p_fv4sf ((__builtin_neon_si *) __base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset_f32 (float32_t * __base, uint32x4_t __offset, float32x4_t __value)
{
  __builtin_mve_vstrwq_scatter_shifted_offset_fv4sf ((__builtin_neon_si *) __base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset_p_f32 (float32_t * __base, uint32x4_t __offset, float32x4_t __value, mve_pred16_t __p)
{
  __builtin_mve_vstrwq_scatter_shifted_offset_p_fv4sf ((__builtin_neon_si *) __base, __offset, __value, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_wb_f32 (uint32x4_t * __addr, const int __offset)
{
  float32x4_t
  result = __builtin_mve_vldrwq_gather_base_nowb_fv4sf (*__addr, __offset);
  *__addr = __builtin_mve_vldrwq_gather_base_wb_fv4sf (*__addr, __offset);
  return result;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_base_wb_z_f32 (uint32x4_t * __addr, const int __offset, mve_pred16_t __p)
{
  float32x4_t
  result = __builtin_mve_vldrwq_gather_base_nowb_z_fv4sf (*__addr, __offset, __p);
  *__addr = __builtin_mve_vldrwq_gather_base_wb_z_fv4sf (*__addr, __offset, __p);
  return result;
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb_f32 (uint32x4_t * __addr, const int __offset, float32x4_t __value)
{
  *__addr = __builtin_mve_vstrwq_scatter_base_wb_fv4sf (*__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb_p_f32 (uint32x4_t * __addr, const int __offset, float32x4_t __value, mve_pred16_t __p)
{
  *__addr = __builtin_mve_vstrwq_scatter_base_wb_p_fv4sf (*__addr, __offset, __value, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_x_s16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtaq_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_x_s32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtaq_m_sv4si (__arm_vuninitializedq_s32 (), __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_x_u16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtaq_m_uv8hi (__arm_vuninitializedq_u16 (), __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_x_u32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtaq_m_uv4si (__arm_vuninitializedq_u32 (), __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_x_s16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtnq_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_x_s32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtnq_m_sv4si (__arm_vuninitializedq_s32 (), __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_x_u16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtnq_m_uv8hi (__arm_vuninitializedq_u16 (), __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_x_u32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtnq_m_uv4si (__arm_vuninitializedq_u32 (), __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_x_s16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtpq_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_x_s32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtpq_m_sv4si (__arm_vuninitializedq_s32 (), __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_x_u16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtpq_m_uv8hi (__arm_vuninitializedq_u16 (), __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_x_u32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtpq_m_uv4si (__arm_vuninitializedq_u32 (), __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_x_s16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtmq_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_x_s32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtmq_m_sv4si (__arm_vuninitializedq_s32 (), __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_x_u16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtmq_m_uv8hi (__arm_vuninitializedq_u16 (), __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_x_u32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtmq_m_uv4si (__arm_vuninitializedq_u32 (), __a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtbq_x_f32_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtbq_m_f32_f16v4sf (__arm_vuninitializedq_f32 (), __a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvttq_x_f32_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvttq_m_f32_f16v4sf (__arm_vuninitializedq_f32 (), __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_f16_u16 (uint16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_to_f_uv8hf (__arm_vuninitializedq_f16 (), __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_f16_s16 (int16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_to_f_sv8hf (__arm_vuninitializedq_f16 (), __a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_f32_s32 (int32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_to_f_sv4sf (__arm_vuninitializedq_f32 (), __a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_f32_u32 (uint32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_to_f_uv4sf (__arm_vuninitializedq_f32 (), __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n_f16_s16 (int16x8_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_to_f_sv8hf (__arm_vuninitializedq_f16 (), __a, __imm6, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n_f16_u16 (uint16x8_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_to_f_uv8hf (__arm_vuninitializedq_f16 (), __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n_f32_s32 (int32x4_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_to_f_sv4sf (__arm_vuninitializedq_f32 (), __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n_f32_u32 (uint32x4_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_to_f_uv4sf (__arm_vuninitializedq_f32 (), __a, __imm6, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_s16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_from_f_sv8hi (__arm_vuninitializedq_s16 (), __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_s32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_from_f_sv4si (__arm_vuninitializedq_s32 (), __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_u16_f16 (float16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_from_f_uv8hi (__arm_vuninitializedq_u16 (), __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_u32_f32 (float32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_from_f_uv4si (__arm_vuninitializedq_u32 (), __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n_s16_f16 (float16x8_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_from_f_sv8hi (__arm_vuninitializedq_s16 (), __a, __imm6, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n_s32_f32 (float32x4_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_from_f_sv4si (__arm_vuninitializedq_s32 (), __a, __imm6, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n_u16_f16 (float16x8_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_from_f_uv8hi (__arm_vuninitializedq_u16 (), __a, __imm6, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n_u32_f32 (float32x4_t __a, const int __imm6, mve_pred16_t __p)
{
  return __builtin_mve_vcvtq_m_n_from_f_uv4si (__arm_vuninitializedq_u32 (), __a, __imm6, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x_f16 (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_fv8hf (__arm_vuninitializedq_f16 (), __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x_f32 (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbicq_m_fv4sf (__arm_vuninitializedq_f32 (), __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x_f16 (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_fv8hf (__arm_vuninitializedq_f16 (), __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x_f32 (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vornq_m_fv4sf (__arm_vuninitializedq_f32 (), __a, __b, __p);
}

__extension__ extern __inline float16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q_f16 (float16_t const * __addr)
{
  union { float16x8x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_mve_vld4qv8hf (__addr);
  return __rv.__i;
}

__extension__ extern __inline float16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q_f16 (float16_t const * __addr)
{
  union { float16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_mve_vld2qv8hf (__addr);
  return __rv.__i;
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z_f16 (float16_t const *__base, mve_pred16_t __p)
{
  return __arm_vldrhq_z_f16 (__base, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_f16 (float16_t * __addr, float16x8x2_t __value)
{
  union { float16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv8hf (__addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p_f16 (float16_t * __addr, float16x8_t __value, mve_pred16_t __p)
{
  return __arm_vstrhq_p_f16 (__addr, __value, __p);
}

__extension__ extern __inline float32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q_f32 (float32_t const * __addr)
{
  union { float32x4x4_t __i; __builtin_neon_xi __o; } __rv;
  __rv.__o = __builtin_mve_vld4qv4sf (__addr);
  return __rv.__i;
}

__extension__ extern __inline float32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q_f32 (float32_t const * __addr)
{
  union { float32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__o = __builtin_mve_vld2qv4sf (__addr);
  return __rv.__i;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z_f32 (float32_t const *__base, mve_pred16_t __p)
{
  return __arm_vldrwq_z_f32 (__base, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_f32 (float32_t * __addr, float32x4x2_t __value)
{
  union { float32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv4sf (__addr, __rv.__o);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p_f32 (float32_t * __addr, float32x4_t __value, mve_pred16_t __p)
{
  return __arm_vstrwq_p_f32 (__addr, __value, __p);
}

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
__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q (int8_t * __addr, int8x16x4_t __value)
{
 __arm_vst4q_s8 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q (int16_t * __addr, int16x8x4_t __value)
{
 __arm_vst4q_s16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q (int32_t * __addr, int32x4x4_t __value)
{
 __arm_vst4q_s32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q (uint8_t * __addr, uint8x16x4_t __value)
{
 __arm_vst4q_u8 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q (uint16_t * __addr, uint16x8x4_t __value)
{
 __arm_vst4q_u16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q (uint32_t * __addr, uint32x4x4_t __value)
{
 __arm_vst4q_u32 (__addr, __value);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vornq_u8 (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vbicq_u8 (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (int8x16_t __a, int8x16_t __b)
{
 return __arm_vornq_s8 (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (int8x16_t __a, int8x16_t __b)
{
 return __arm_vbicq_s8 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vornq_u16 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vbicq_u16 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vornq_s16 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vbicq_s16 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vornq_u32 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vbicq_u32 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vornq_s32 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vbicq_s32 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint16x8_t __a, const int __imm)
{
 return __arm_vbicq_n_u16 (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (int16x8_t __a, const int __imm)
{
 return __arm_vbicq_n_s16 (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint32x4_t __a, const int __imm)
{
 return __arm_vbicq_n_u32 (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (int32x4_t __a, const int __imm)
{
 return __arm_vbicq_n_s32 (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_n (int16x8_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vbicq_m_n_s16 (__a, __imm, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_n (int32x4_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vbicq_m_n_s32 (__a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_n (uint16x8_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vbicq_m_n_u16 (__a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m_n (uint32x4_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vbicq_m_n_u32 (__a, __imm, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq (int8x16_t __a, uint32_t * __b, const int __imm)
{
 return __arm_vshlcq_s8 (__a, __b, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq (uint8x16_t __a, uint32_t * __b, const int __imm)
{
 return __arm_vshlcq_u8 (__a, __b, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq (int16x8_t __a, uint32_t * __b, const int __imm)
{
 return __arm_vshlcq_s16 (__a, __b, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq (uint16x8_t __a, uint32_t * __b, const int __imm)
{
 return __arm_vshlcq_u16 (__a, __b, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq (int32x4_t __a, uint32_t * __b, const int __imm)
{
 return __arm_vshlcq_s32 (__a, __b, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq (uint32x4_t __a, uint32_t * __b, const int __imm)
{
 return __arm_vshlcq_u32 (__a, __b, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m (uint8x16_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_m_u8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_m_u32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m (uint16x8_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_m_u16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vornq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vornq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vornq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m (uint8x16_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vornq_m_u8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vornq_m_u32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m (uint16x8_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vornq_m_u16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset (int8_t * __base, uint8x16_t __offset, int8x16_t __value)
{
 __arm_vstrbq_scatter_offset_s8 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset (int8_t * __base, uint32x4_t __offset, int32x4_t __value)
{
 __arm_vstrbq_scatter_offset_s32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset (int8_t * __base, uint16x8_t __offset, int16x8_t __value)
{
 __arm_vstrbq_scatter_offset_s16 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset (uint8_t * __base, uint8x16_t __offset, uint8x16_t __value)
{
 __arm_vstrbq_scatter_offset_u8 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset (uint8_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
 __arm_vstrbq_scatter_offset_u32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset (uint8_t * __base, uint16x8_t __offset, uint16x8_t __value)
{
 __arm_vstrbq_scatter_offset_u16 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq (int8_t * __addr, int8x16_t __value)
{
 __arm_vstrbq_s8 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq (int8_t * __addr, int32x4_t __value)
{
 __arm_vstrbq_s32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq (int8_t * __addr, int16x8_t __value)
{
 __arm_vstrbq_s16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq (uint8_t * __addr, uint8x16_t __value)
{
 __arm_vstrbq_u8 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq (uint8_t * __addr, uint32x4_t __value)
{
 __arm_vstrbq_u32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq (uint8_t * __addr, uint16x8_t __value)
{
 __arm_vstrbq_u16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base (uint32x4_t __addr, const int __offset, int32x4_t __value)
{
 __arm_vstrwq_scatter_base_s32 (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base (uint32x4_t __addr, const int __offset, uint32x4_t __value)
{
 __arm_vstrwq_scatter_base_u32 (__addr, __offset, __value);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset (uint8_t const * __base, uint8x16_t __offset)
{
 return __arm_vldrbq_gather_offset_u8 (__base, __offset);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset (int8_t const * __base, uint8x16_t __offset)
{
 return __arm_vldrbq_gather_offset_s8 (__base, __offset);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset (uint8_t const * __base, uint16x8_t __offset)
{
 return __arm_vldrbq_gather_offset_u16 (__base, __offset);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset (int8_t const * __base, uint16x8_t __offset)
{
 return __arm_vldrbq_gather_offset_s16 (__base, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset (uint8_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrbq_gather_offset_u32 (__base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset (int8_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrbq_gather_offset_s32 (__base, __offset);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p (int8_t * __addr, int8x16_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_p_s8 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p (int8_t * __addr, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_p_s32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p (int8_t * __addr, int16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_p_s16 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p (uint8_t * __addr, uint8x16_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_p_u8 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p (uint8_t * __addr, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_p_u32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_p (uint8_t * __addr, uint16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_p_u16 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p (int8_t * __base, uint8x16_t __offset, int8x16_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_scatter_offset_p_s8 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p (int8_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_scatter_offset_p_s32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p (int8_t * __base, uint16x8_t __offset, int16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_scatter_offset_p_s16 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p (uint8_t * __base, uint8x16_t __offset, uint8x16_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_scatter_offset_p_u8 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p (uint8_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_scatter_offset_p_u32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrbq_scatter_offset_p (uint8_t * __base, uint16x8_t __offset, uint16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrbq_scatter_offset_p_u16 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_p (uint32x4_t __addr, const int __offset, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_base_p_s32 (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_p (uint32x4_t __addr, const int __offset, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_base_p_u32 (__addr, __offset, __value, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z (int8_t const * __base, uint8x16_t __offset, mve_pred16_t __p)
{
 return __arm_vldrbq_gather_offset_z_s8 (__base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z (int8_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrbq_gather_offset_z_s32 (__base, __offset, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z (int8_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
 return __arm_vldrbq_gather_offset_z_s16 (__base, __offset, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z (uint8_t const * __base, uint8x16_t __offset, mve_pred16_t __p)
{
 return __arm_vldrbq_gather_offset_z_u8 (__base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z (uint8_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrbq_gather_offset_z_u32 (__base, __offset, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrbq_gather_offset_z (uint8_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
 return __arm_vldrbq_gather_offset_z_u16 (__base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset (int16_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrhq_gather_offset_s32 (__base, __offset);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset (int16_t const * __base, uint16x8_t __offset)
{
 return __arm_vldrhq_gather_offset_s16 (__base, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset (uint16_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrhq_gather_offset_u32 (__base, __offset);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset (uint16_t const * __base, uint16x8_t __offset)
{
 return __arm_vldrhq_gather_offset_u16 (__base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z (int16_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_offset_z_s32 (__base, __offset, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z (int16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_offset_z_s16 (__base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z (uint16_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_offset_z_u32 (__base, __offset, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z (uint16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_offset_z_u16 (__base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset (int16_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrhq_gather_shifted_offset_s32 (__base, __offset);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset (int16_t const * __base, uint16x8_t __offset)
{
 return __arm_vldrhq_gather_shifted_offset_s16 (__base, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset (uint16_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrhq_gather_shifted_offset_u32 (__base, __offset);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset (uint16_t const * __base, uint16x8_t __offset)
{
 return __arm_vldrhq_gather_shifted_offset_u16 (__base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z (int16_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_shifted_offset_z_s32 (__base, __offset, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z (int16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_shifted_offset_z_s16 (__base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z (uint16_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_shifted_offset_z_u32 (__base, __offset, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z (uint16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_shifted_offset_z_u16 (__base, __offset, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_offset (int64_t const * __base, uint64x2_t __offset)
{
 return __arm_vldrdq_gather_offset_s64 (__base, __offset);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_offset (uint64_t const * __base, uint64x2_t __offset)
{
 return __arm_vldrdq_gather_offset_u64 (__base, __offset);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_offset_z (int64_t const * __base, uint64x2_t __offset, mve_pred16_t __p)
{
 return __arm_vldrdq_gather_offset_z_s64 (__base, __offset, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_offset_z (uint64_t const * __base, uint64x2_t __offset, mve_pred16_t __p)
{
 return __arm_vldrdq_gather_offset_z_u64 (__base, __offset, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_shifted_offset (int64_t const * __base, uint64x2_t __offset)
{
 return __arm_vldrdq_gather_shifted_offset_s64 (__base, __offset);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_shifted_offset (uint64_t const * __base, uint64x2_t __offset)
{
 return __arm_vldrdq_gather_shifted_offset_u64 (__base, __offset);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_shifted_offset_z (int64_t const * __base, uint64x2_t __offset, mve_pred16_t __p)
{
 return __arm_vldrdq_gather_shifted_offset_z_s64 (__base, __offset, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrdq_gather_shifted_offset_z (uint64_t const * __base, uint64x2_t __offset, mve_pred16_t __p)
{
 return __arm_vldrdq_gather_shifted_offset_z_u64 (__base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset (int32_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrwq_gather_offset_s32 (__base, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset (uint32_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrwq_gather_offset_u32 (__base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset_z (int32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrwq_gather_offset_z_s32 (__base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset_z (uint32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrwq_gather_offset_z_u32 (__base, __offset, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset (int32_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrwq_gather_shifted_offset_s32 (__base, __offset);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset (uint32_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrwq_gather_shifted_offset_u32 (__base, __offset);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset_z (int32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrwq_gather_shifted_offset_z_s32 (__base, __offset, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset_z (uint32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrwq_gather_shifted_offset_z_u32 (__base, __offset, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset (int16_t * __base, uint32x4_t __offset, int32x4_t __value)
{
 __arm_vstrhq_scatter_offset_s32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset (int16_t * __base, uint16x8_t __offset, int16x8_t __value)
{
 __arm_vstrhq_scatter_offset_s16 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset (uint16_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
 __arm_vstrhq_scatter_offset_u32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset (uint16_t * __base, uint16x8_t __offset, uint16x8_t __value)
{
 __arm_vstrhq_scatter_offset_u16 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p (int16_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_offset_p_s32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p (int16_t * __base, uint16x8_t __offset, int16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_offset_p_s16 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p (uint16_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_offset_p_u32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p (uint16_t * __base, uint16x8_t __offset, uint16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_offset_p_u16 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset (int16_t * __base, uint32x4_t __offset, int32x4_t __value)
{
 __arm_vstrhq_scatter_shifted_offset_s32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset (int16_t * __base, uint16x8_t __offset, int16x8_t __value)
{
 __arm_vstrhq_scatter_shifted_offset_s16 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset (uint16_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
 __arm_vstrhq_scatter_shifted_offset_u32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset (uint16_t * __base, uint16x8_t __offset, uint16x8_t __value)
{
 __arm_vstrhq_scatter_shifted_offset_u16 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p (int16_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_shifted_offset_p_s32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p (int16_t * __base, uint16x8_t __offset, int16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_shifted_offset_p_s16 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p (uint16_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_shifted_offset_p_u32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p (uint16_t * __base, uint16x8_t __offset, uint16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_shifted_offset_p_u16 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq (int16_t * __addr, int32x4_t __value)
{
 __arm_vstrhq_s32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq (int16_t * __addr, int16x8_t __value)
{
 __arm_vstrhq_s16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq (uint16_t * __addr, uint32x4_t __value)
{
 __arm_vstrhq_u32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq (uint16_t * __addr, uint16x8_t __value)
{
 __arm_vstrhq_u16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p (int16_t * __addr, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_p_s32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p (int16_t * __addr, int16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_p_s16 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p (uint16_t * __addr, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_p_u32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p (uint16_t * __addr, uint16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_p_u16 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq (int32_t * __addr, int32x4_t __value)
{
 __arm_vstrwq_s32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq (uint32_t * __addr, uint32x4_t __value)
{
 __arm_vstrwq_u32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_p (int32_t * __addr, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_p_s32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_p (uint32_t * __addr, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_p_u32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_p (uint64x2_t __addr, const int __offset, int64x2_t __value, mve_pred16_t __p)
{
 __arm_vstrdq_scatter_base_p_s64 (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_p (uint64x2_t __addr, const int __offset, uint64x2_t __value, mve_pred16_t __p)
{
 __arm_vstrdq_scatter_base_p_u64 (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base (uint64x2_t __addr, const int __offset, int64x2_t __value)
{
 __arm_vstrdq_scatter_base_s64 (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base (uint64x2_t __addr, const int __offset, uint64x2_t __value)
{
 __arm_vstrdq_scatter_base_u64 (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_offset_p (int64_t * __base, uint64x2_t __offset, int64x2_t __value, mve_pred16_t __p)
{
 __arm_vstrdq_scatter_offset_p_s64 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_offset_p (uint64_t * __base, uint64x2_t __offset, uint64x2_t __value, mve_pred16_t __p)
{
 __arm_vstrdq_scatter_offset_p_u64 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_offset (int64_t * __base, uint64x2_t __offset, int64x2_t __value)
{
 __arm_vstrdq_scatter_offset_s64 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_offset (uint64_t * __base, uint64x2_t __offset, uint64x2_t __value)
{
 __arm_vstrdq_scatter_offset_u64 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_shifted_offset_p (int64_t * __base, uint64x2_t __offset, int64x2_t __value, mve_pred16_t __p)
{
 __arm_vstrdq_scatter_shifted_offset_p_s64 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_shifted_offset_p (uint64_t * __base, uint64x2_t __offset, uint64x2_t __value, mve_pred16_t __p)
{
 __arm_vstrdq_scatter_shifted_offset_p_u64 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_shifted_offset (int64_t * __base, uint64x2_t __offset, int64x2_t __value)
{
 __arm_vstrdq_scatter_shifted_offset_s64 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_shifted_offset (uint64_t * __base, uint64x2_t __offset, uint64x2_t __value)
{
 __arm_vstrdq_scatter_shifted_offset_u64 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset_p (int32_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_offset_p_s32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset_p (uint32_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_offset_p_u32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset (int32_t * __base, uint32x4_t __offset, int32x4_t __value)
{
 __arm_vstrwq_scatter_offset_s32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset (uint32_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
 __arm_vstrwq_scatter_offset_u32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset_p (int32_t * __base, uint32x4_t __offset, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_shifted_offset_p_s32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset_p (uint32_t * __base, uint32x4_t __offset, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_shifted_offset_p_u32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset (int32_t * __base, uint32x4_t __offset, int32x4_t __value)
{
 __arm_vstrwq_scatter_shifted_offset_s32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset (uint32_t * __base, uint32x4_t __offset, uint32x4_t __value)
{
 __arm_vstrwq_scatter_shifted_offset_u32 (__base, __offset, __value);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m (uint8x16_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_m_n_u8 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m (uint32x4_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_m_n_u32 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m (uint16x8_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_m_n_u16 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m (uint8x16_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_m_wb_u8 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m (uint16x8_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_m_wb_u16 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_m (uint32x4_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_m_wb_u32 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_u8 (uint32_t __a, const int __imm)
{
 return __arm_vddupq_n_u8 (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_u32 (uint32_t __a, const int __imm)
{
 return __arm_vddupq_n_u32 (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_u16 (uint32_t __a, const int __imm)
{
 return __arm_vddupq_n_u16 (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m (uint8x16_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_m_n_u8 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m (uint32x4_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_m_n_u32 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m (uint16x8_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_m_n_u16 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m (uint8x16_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_m_wb_u8 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m (uint32x4_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_m_wb_u32 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_m (uint16x8_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_m_wb_u16 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_u8 (uint32_t __a, uint32_t __b, const int __imm)
{
 return __arm_vdwdupq_n_u8 (__a, __b, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_u32 (uint32_t __a, uint32_t __b, const int __imm)
{
 return __arm_vdwdupq_n_u32 (__a, __b, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_u16 (uint32_t __a, uint32_t __b, const int __imm)
{
 return __arm_vdwdupq_n_u16 (__a, __b, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_u8 (uint32_t * __a, uint32_t __b, const int __imm)
{
 return __arm_vdwdupq_wb_u8 (__a, __b, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_u32 (uint32_t * __a, uint32_t __b, const int __imm)
{
 return __arm_vdwdupq_wb_u32 (__a, __b, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_u16 (uint32_t * __a, uint32_t __b, const int __imm)
{
 return __arm_vdwdupq_wb_u16 (__a, __b, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m (uint8x16_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_m_n_u8 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m (uint32x4_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_m_n_u32 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m (uint16x8_t __inactive, uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_m_n_u16 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_u8 (uint32_t __a, const int __imm)
{
 return __arm_vidupq_n_u8 (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m (uint8x16_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_m_wb_u8 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m (uint16x8_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_m_wb_u16 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_m (uint32x4_t __inactive, uint32_t * __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_m_wb_u32 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_u32 (uint32_t __a, const int __imm)
{
 return __arm_vidupq_n_u32 (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_u16 (uint32_t __a, const int __imm)
{
 return __arm_vidupq_n_u16 (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_u8 (uint32_t * __a, const int __imm)
{
 return __arm_vidupq_wb_u8 (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_u16 (uint32_t * __a, const int __imm)
{
 return __arm_vidupq_wb_u16 (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_u32 (uint32_t * __a, const int __imm)
{
 return __arm_vidupq_wb_u32 (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_u8 (uint32_t * __a, const int __imm)
{
 return __arm_vddupq_wb_u8 (__a, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_u16 (uint32_t * __a, const int __imm)
{
 return __arm_vddupq_wb_u16 (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_u32 (uint32_t * __a, const int __imm)
{
 return __arm_vddupq_wb_u32 (__a, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m (uint8x16_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_m_n_u8 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m (uint32x4_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_m_n_u32 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m (uint16x8_t __inactive, uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_m_n_u16 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m (uint8x16_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_m_wb_u8 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m (uint32x4_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_m_wb_u32 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_m (uint16x8_t __inactive, uint32_t * __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_m_wb_u16 (__inactive, __a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_u8 (uint32_t __a, uint32_t __b, const int __imm)
{
 return __arm_viwdupq_n_u8 (__a, __b, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_u32 (uint32_t __a, uint32_t __b, const int __imm)
{
 return __arm_viwdupq_n_u32 (__a, __b, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_u16 (uint32_t __a, uint32_t __b, const int __imm)
{
 return __arm_viwdupq_n_u16 (__a, __b, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_u8 (uint32_t * __a, uint32_t __b, const int __imm)
{
 return __arm_viwdupq_wb_u8 (__a, __b, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_u32 (uint32_t * __a, uint32_t __b, const int __imm)
{
 return __arm_viwdupq_wb_u32 (__a, __b, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_u16 (uint32_t * __a, uint32_t __b, const int __imm)
{
 return __arm_viwdupq_wb_u16 (__a, __b, __imm);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_wb (uint64x2_t * __addr, const int __offset, int64x2_t __value)
{
 __arm_vstrdq_scatter_base_wb_s64 (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_wb (uint64x2_t * __addr, const int __offset, uint64x2_t __value)
{
 __arm_vstrdq_scatter_base_wb_u64 (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_wb_p (uint64x2_t * __addr, const int __offset, int64x2_t __value, mve_pred16_t __p)
{
 __arm_vstrdq_scatter_base_wb_p_s64 (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrdq_scatter_base_wb_p (uint64x2_t * __addr, const int __offset, uint64x2_t __value, mve_pred16_t __p)
{
 __arm_vstrdq_scatter_base_wb_p_u64 (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb_p (uint32x4_t * __addr, const int __offset, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_base_wb_p_s32 (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb_p (uint32x4_t * __addr, const int __offset, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_base_wb_p_u32 (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb (uint32x4_t * __addr, const int __offset, int32x4_t __value)
{
 __arm_vstrwq_scatter_base_wb_s32 (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb (uint32x4_t * __addr, const int __offset, uint32x4_t __value)
{
 __arm_vstrwq_scatter_base_wb_u32 (__addr, __offset, __value);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_u8 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_x_n_u8 (__a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_u16 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_x_n_u16 (__a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_u32 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_x_n_u32 (__a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_u8 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_x_wb_u8 (__a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_u16 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_x_wb_u16 (__a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vddupq_x_u32 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
 return __arm_vddupq_x_wb_u32 (__a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_u8 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_x_n_u8 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_u16 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_x_n_u16 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_u32 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_x_n_u32 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_u8 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_x_wb_u8 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_u16 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_x_wb_u16 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vdwdupq_x_u32 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vdwdupq_x_wb_u32 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_u8 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_x_n_u8 (__a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_u16 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_x_n_u16 (__a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_u32 (uint32_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_x_n_u32 (__a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_u8 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_x_wb_u8 (__a, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_u16 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_x_wb_u16 (__a, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vidupq_x_u32 (uint32_t *__a, const int __imm, mve_pred16_t __p)
{
 return __arm_vidupq_x_wb_u32 (__a, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_u8 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_x_n_u8 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_u16 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_x_n_u16 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_u32 (uint32_t __a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_x_n_u32 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_u8 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_x_wb_u8 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_u16 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_x_wb_u16 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_viwdupq_x_u32 (uint32_t *__a, uint32_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_viwdupq_x_wb_u32 (__a, __b, __imm, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_x_s8 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_x_s16 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_x_s32 (__a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_x_u8 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_x_u16 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_x_u32 (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vornq_x_s8 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vornq_x_s16 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vornq_x_s32 (__a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vornq_x_u8 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vornq_x_u16 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vornq_x_u32 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadciq (int32x4_t __a, int32x4_t __b, unsigned * __carry_out)
{
 return __arm_vadciq_s32 (__a, __b, __carry_out);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadciq (uint32x4_t __a, uint32x4_t __b, unsigned * __carry_out)
{
 return __arm_vadciq_u32 (__a, __b, __carry_out);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadciq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry_out, mve_pred16_t __p)
{
 return __arm_vadciq_m_s32 (__inactive, __a, __b, __carry_out, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadciq_m (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry_out, mve_pred16_t __p)
{
 return __arm_vadciq_m_u32 (__inactive, __a, __b, __carry_out, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq (int32x4_t __a, int32x4_t __b, unsigned * __carry)
{
 return __arm_vadcq_s32 (__a, __b, __carry);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq (uint32x4_t __a, uint32x4_t __b, unsigned * __carry)
{
 return __arm_vadcq_u32 (__a, __b, __carry);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
 return __arm_vadcq_m_s32 (__inactive, __a, __b, __carry, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq_m (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
 return __arm_vadcq_m_u32 (__inactive, __a, __b, __carry, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbciq (int32x4_t __a, int32x4_t __b, unsigned * __carry_out)
{
 return __arm_vsbciq_s32 (__a, __b, __carry_out);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbciq (uint32x4_t __a, uint32x4_t __b, unsigned * __carry_out)
{
 return __arm_vsbciq_u32 (__a, __b, __carry_out);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbciq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry_out, mve_pred16_t __p)
{
 return __arm_vsbciq_m_s32 (__inactive, __a, __b, __carry_out, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbciq_m (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry_out, mve_pred16_t __p)
{
 return __arm_vsbciq_m_u32 (__inactive, __a, __b, __carry_out, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq (int32x4_t __a, int32x4_t __b, unsigned * __carry)
{
 return __arm_vsbcq_s32 (__a, __b, __carry);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq (uint32x4_t __a, uint32x4_t __b, unsigned * __carry)
{
 return __arm_vsbcq_u32 (__a, __b, __carry);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
 return __arm_vsbcq_m_s32 (__inactive, __a, __b, __carry, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq_m (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
 return __arm_vsbcq_m_u32 (__inactive, __a, __b, __carry, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p (uint8_t * __addr, uint8x16_t __value, mve_pred16_t __p)
{
 __arm_vst1q_p_u8 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p (int8_t * __addr, int8x16_t __value, mve_pred16_t __p)
{
 __arm_vst1q_p_s8 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (int8_t * __addr, int8x16x2_t __value)
{
 __arm_vst2q_s8 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (uint8_t * __addr, uint8x16x2_t __value)
{
 __arm_vst2q_u8 (__addr, __value);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z (uint8_t const *__base, mve_pred16_t __p)
{
 return __arm_vld1q_z_u8 (__base, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z (int8_t const *__base, mve_pred16_t __p)
{
 return __arm_vld1q_z_s8 (__base, __p);
}

__extension__ extern __inline int8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q (int8_t const * __addr)
{
 return __arm_vld2q_s8 (__addr);
}

__extension__ extern __inline uint8x16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q (uint8_t const * __addr)
{
 return __arm_vld2q_u8 (__addr);
}

__extension__ extern __inline int8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q (int8_t const * __addr)
{
 return __arm_vld4q_s8 (__addr);
}

__extension__ extern __inline uint8x16x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q (uint8_t const * __addr)
{
 return __arm_vld4q_u8 (__addr);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p (uint16_t * __addr, uint16x8_t __value, mve_pred16_t __p)
{
 __arm_vst1q_p_u16 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p (int16_t * __addr, int16x8_t __value, mve_pred16_t __p)
{
 __arm_vst1q_p_s16 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (int16_t * __addr, int16x8x2_t __value)
{
 __arm_vst2q_s16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (uint16_t * __addr, uint16x8x2_t __value)
{
 __arm_vst2q_u16 (__addr, __value);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z (uint16_t const *__base, mve_pred16_t __p)
{
 return __arm_vld1q_z_u16 (__base, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z (int16_t const *__base, mve_pred16_t __p)
{
 return __arm_vld1q_z_s16 (__base, __p);
}

__extension__ extern __inline int16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q (int16_t const * __addr)
{
 return __arm_vld2q_s16 (__addr);
}

__extension__ extern __inline uint16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q (uint16_t const * __addr)
{
 return __arm_vld2q_u16 (__addr);
}

__extension__ extern __inline int16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q (int16_t const * __addr)
{
 return __arm_vld4q_s16 (__addr);
}

__extension__ extern __inline uint16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q (uint16_t const * __addr)
{
 return __arm_vld4q_u16 (__addr);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p (uint32_t * __addr, uint32x4_t __value, mve_pred16_t __p)
{
 __arm_vst1q_p_u32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p (int32_t * __addr, int32x4_t __value, mve_pred16_t __p)
{
 __arm_vst1q_p_s32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (int32_t * __addr, int32x4x2_t __value)
{
 __arm_vst2q_s32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (uint32_t * __addr, uint32x4x2_t __value)
{
 __arm_vst2q_u32 (__addr, __value);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z (uint32_t const *__base, mve_pred16_t __p)
{
 return __arm_vld1q_z_u32 (__base, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z (int32_t const *__base, mve_pred16_t __p)
{
 return __arm_vld1q_z_s32 (__base, __p);
}

__extension__ extern __inline int32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q (int32_t const * __addr)
{
 return __arm_vld2q_s32 (__addr);
}

__extension__ extern __inline uint32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q (uint32_t const * __addr)
{
 return __arm_vld2q_u32 (__addr);
}

__extension__ extern __inline int32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q (int32_t const * __addr)
{
 return __arm_vld4q_s32 (__addr);
}

__extension__ extern __inline uint32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q (uint32_t const * __addr)
{
 return __arm_vld4q_u32 (__addr);
}

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

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m (int8x16_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vshlcq_m_s8 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m (uint8x16_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vshlcq_m_u8 (__a, __b, __imm, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m (int16x8_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vshlcq_m_s16 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m (uint16x8_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vshlcq_m_u16 (__a, __b, __imm, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m (int32x4_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vshlcq_m_s32 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vshlcq_m (uint32x4_t __a, uint32_t * __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vshlcq_m_u32 (__a, __b, __imm, __p);
}

#if (__ARM_FEATURE_MVE & 2)  /* MVE Floating point.  */

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q (float16_t * __addr, float16x8x4_t __value)
{
 __arm_vst4q_f16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst4q (float32_t * __addr, float32x4x4_t __value)
{
 __arm_vst4q_f32 (__addr, __value);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvttq_f32 (float16x8_t __a)
{
 return __arm_vcvttq_f32_f16 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtbq_f32 (float16x8_t __a)
{
 return __arm_vcvtbq_f32_f16 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq (int16x8_t __a)
{
 return __arm_vcvtq_f16_s16 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq (int32x4_t __a)
{
 return __arm_vcvtq_f32_s32 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq (uint16x8_t __a)
{
 return __arm_vcvtq_f16_u16 (__a);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq (uint32x4_t __a)
{
 return __arm_vcvtq_f32_u32 (__a);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n (int16x8_t __a, const int __imm6)
{
 return __arm_vcvtq_n_f16_s16 (__a, __imm6);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n (int32x4_t __a, const int __imm6)
{
 return __arm_vcvtq_n_f32_s32 (__a, __imm6);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n (uint16x8_t __a, const int __imm6)
{
 return __arm_vcvtq_n_f16_u16 (__a, __imm6);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_n (uint32x4_t __a, const int __imm6)
{
 return __arm_vcvtq_n_f32_u32 (__a, __imm6);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (float16x8_t __a, float16x8_t __b)
{
 return __arm_vornq_f16 (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (float16x8_t __a, float16x8_t __b)
{
 return __arm_vbicq_f16 (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (float32x4_t __a, float32x4_t __b)
{
 return __arm_vornq_f32 (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (float32x4_t __a, float32x4_t __b)
{
 return __arm_vbicq_f32 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_m (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtaq_m_s16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_m (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtaq_m_u16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_m (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtaq_m_s32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtaq_m (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtaq_m_u32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m (float16x8_t __inactive, int16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_m_f16_s16 (__inactive, __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m (float16x8_t __inactive, uint16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_m_f16_u16 (__inactive, __a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m (float32x4_t __inactive, int32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_m_f32_s32 (__inactive, __a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m (float32x4_t __inactive, uint32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_m_f32_u32 (__inactive, __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtbq_m (float16x8_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcvtbq_m_f16_f32 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtbq_m (float32x4_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtbq_m_f32_f16 (__inactive, __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvttq_m (float16x8_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcvttq_m_f16_f32 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvttq_m (float32x4_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvttq_m_f32_f16 (__inactive, __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_m (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtmq_m_s16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_m (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtnq_m_s16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_m (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtpq_m_s16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m (int16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_m_s16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_m (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtmq_m_u16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_m (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtnq_m_u16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_m (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtpq_m_u16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m (uint16x8_t __inactive, float16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_m_u16_f16 (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_m (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtmq_m_s32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_m (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtnq_m_s32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_m (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtpq_m_s32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m (int32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_m_s32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtmq_m (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtmq_m_u32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtnq_m (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtnq_m_u32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtpq_m (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtpq_m_u32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m (uint32x4_t __inactive, float32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_m_u32_f32 (__inactive, __a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n (float16x8_t __inactive, uint16x8_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_m_n_f16_u16 (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n (float16x8_t __inactive, int16x8_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_m_n_f16_s16 (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n (float32x4_t __inactive, uint32x4_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_m_n_f32_u32 (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n (float32x4_t __inactive, int32x4_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_m_n_f32_s32 (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_m_f32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_m (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_m_f16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n (int32x4_t __inactive, float32x4_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_m_n_s32_f32 (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n (int16x8_t __inactive, float16x8_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_m_n_s16_f16 (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n (uint32x4_t __inactive, float32x4_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_m_n_u32_f32 (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_m_n (uint16x8_t __inactive, float16x8_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_m_n_u16_f16 (__inactive, __a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vornq_m_f32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_m (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vornq_m_f16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset (float16_t const * __base, uint16x8_t __offset)
{
 return __arm_vldrhq_gather_offset_f16 (__base, __offset);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_offset_z (float16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_offset_z_f16 (__base, __offset, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset (float16_t const * __base, uint16x8_t __offset)
{
 return __arm_vldrhq_gather_shifted_offset_f16 (__base, __offset);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrhq_gather_shifted_offset_z (float16_t const * __base, uint16x8_t __offset, mve_pred16_t __p)
{
 return __arm_vldrhq_gather_shifted_offset_z_f16 (__base, __offset, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset (float32_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrwq_gather_offset_f32 (__base, __offset);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_offset_z (float32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrwq_gather_offset_z_f32 (__base, __offset, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset (float32_t const * __base, uint32x4_t __offset)
{
 return __arm_vldrwq_gather_shifted_offset_f32 (__base, __offset);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vldrwq_gather_shifted_offset_z (float32_t const * __base, uint32x4_t __offset, mve_pred16_t __p)
{
 return __arm_vldrwq_gather_shifted_offset_z_f32 (__base, __offset, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_p (float32_t * __addr, float32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_p_f32 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq (float32_t * __addr, float32x4_t __value)
{
 __arm_vstrwq_f32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq (float16_t * __addr, float16x8_t __value)
{
 __arm_vstrhq_f16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_p (float16_t * __addr, float16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_p_f16 (__addr, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset (float16_t * __base, uint16x8_t __offset, float16x8_t __value)
{
 __arm_vstrhq_scatter_offset_f16 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_offset_p (float16_t * __base, uint16x8_t __offset, float16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_offset_p_f16 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset (float16_t * __base, uint16x8_t __offset, float16x8_t __value)
{
 __arm_vstrhq_scatter_shifted_offset_f16 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrhq_scatter_shifted_offset_p (float16_t * __base, uint16x8_t __offset, float16x8_t __value, mve_pred16_t __p)
{
 __arm_vstrhq_scatter_shifted_offset_p_f16 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base (uint32x4_t __addr, const int __offset, float32x4_t __value)
{
 __arm_vstrwq_scatter_base_f32 (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_p (uint32x4_t __addr, const int __offset, float32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_base_p_f32 (__addr, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset (float32_t * __base, uint32x4_t __offset, float32x4_t __value)
{
 __arm_vstrwq_scatter_offset_f32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_offset_p (float32_t * __base, uint32x4_t __offset, float32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_offset_p_f32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset (float32_t * __base, uint32x4_t __offset, float32x4_t __value)
{
 __arm_vstrwq_scatter_shifted_offset_f32 (__base, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_shifted_offset_p (float32_t * __base, uint32x4_t __offset, float32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_shifted_offset_p_f32 (__base, __offset, __value, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb (uint32x4_t * __addr, const int __offset, float32x4_t __value)
{
 __arm_vstrwq_scatter_base_wb_f32 (__addr, __offset, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vstrwq_scatter_base_wb_p (uint32x4_t * __addr, const int __offset, float32x4_t __value, mve_pred16_t __p)
{
 __arm_vstrwq_scatter_base_wb_p_f32 (__addr, __offset, __value, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x (uint16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_x_f16_u16 (__a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x (int16x8_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_x_f16_s16 (__a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x (int32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_x_f32_s32 (__a, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x (uint32x4_t __a, mve_pred16_t __p)
{
 return __arm_vcvtq_x_f32_u32 (__a, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n (int16x8_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_x_n_f16_s16 (__a, __imm6, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n (uint16x8_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_x_n_f16_u16 (__a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n (int32x4_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_x_n_f32_s32 (__a, __imm6, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcvtq_x_n (uint32x4_t __a, const int __imm6, mve_pred16_t __p)
{
 return __arm_vcvtq_x_n_f32_u32 (__a, __imm6, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_x_f16 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_x (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vbicq_x_f32 (__a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vornq_x_f16 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_x (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vornq_x_f32 (__a, __b, __p);
}

__extension__ extern __inline float16x8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q (float16_t const * __addr)
{
 return __arm_vld4q_f16 (__addr);
}

__extension__ extern __inline float16x8x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q (float16_t const * __addr)
{
 return __arm_vld2q_f16 (__addr);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z (float16_t const *__base, mve_pred16_t __p)
{
 return __arm_vld1q_z_f16 (__base, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (float16_t * __addr, float16x8x2_t __value)
{
 __arm_vst2q_f16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p (float16_t * __addr, float16x8_t __value, mve_pred16_t __p)
{
 __arm_vst1q_p_f16 (__addr, __value, __p);
}

__extension__ extern __inline float32x4x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld4q (float32_t const * __addr)
{
 return __arm_vld4q_f32 (__addr);
}

__extension__ extern __inline float32x4x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld2q (float32_t const * __addr)
{
 return __arm_vld2q_f32 (__addr);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_z (float32_t const *__base, mve_pred16_t __p)
{
 return __arm_vld1q_z_f32 (__base, __p);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (float32_t * __addr, float32x4x2_t __value)
{
 __arm_vst2q_f32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_p (float32_t * __addr, float32x4_t __value, mve_pred16_t __p)
{
 __arm_vst1q_p_f32 (__addr, __value, __p);
}

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

#define __arm_vst4q(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x4_t]: __arm_vst4q_s8 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x4_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x4_t]: __arm_vst4q_s16 (__ARM_mve_coerce_s16_ptr(__p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x4_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x4_t]: __arm_vst4q_s32 (__ARM_mve_coerce_s32_ptr(__p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x4_t]: __arm_vst4q_u8 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x4_t]: __arm_vst4q_u16 (__ARM_mve_coerce_u16_ptr(__p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x4_t]: __arm_vst4q_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8x4_t]: __arm_vst4q_f16 (__ARM_mve_coerce_f16_ptr(__p0, float16_t *), __ARM_mve_coerce(__p1, float16x8x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4x4_t]: __arm_vst4q_f32 (__ARM_mve_coerce_f32_ptr(__p0, float32_t *), __ARM_mve_coerce(__p1, float32x4x4_t)));})

#define __arm_vcvtbq_f32(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vcvtbq_f32_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define __arm_vcvttq_f32(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vcvttq_f32_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define __arm_vcvtq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vcvtq_f16_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vcvtq_f32_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vcvtq_f16_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vcvtq_f32_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define __arm_vcvtq_n(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vcvtq_n_f16_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vcvtq_n_f32_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vcvtq_n_f16_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vcvtq_n_f32_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define __arm_vbicq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vbicq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce_i_scalar (__p1, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vbicq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce_i_scalar (__p1, int)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vbicq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce_i_scalar (__p1, int)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vbicq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce_i_scalar (__p1, int)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbicq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbicq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbicq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbicq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbicq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbicq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vbicq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vbicq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

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

#define __arm_vbicq_m_n(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vbicq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1, p2), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vbicq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1, p2), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vbicq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1, p2), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vbicq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1, p2));})

#define __arm_vshlcq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlcq_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1, p2), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlcq_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1, p2), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshlcq_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1, p2), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlcq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1, p2), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlcq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1, p2), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshlcq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1, p2));})

#define __arm_vcvtaq_m(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtaq_m_s16_f16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtaq_m_s32_f32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtaq_m_u16_f16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtaq_m_u32_f32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vcvtq_m(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcvtq_m_f16_s16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcvtq_m_f32_s32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcvtq_m_f16_u16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcvtq_m_f32_u32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtq_m_s16_f16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtq_m_s32_f32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtq_m_u16_f16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtq_m_u32_f32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vcvtq_m_n(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtq_m_n_s16_f16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtq_m_n_s32_f32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtq_m_n_u16_f16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtq_m_n_u32_f32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcvtq_m_n_f16_s16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcvtq_m_n_f32_s32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcvtq_m_n_f16_u16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcvtq_m_n_f32_u32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2, p3));})

#define __arm_vcvtbq_m(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float16x8_t]: __arm_vcvtbq_m_f32_f16 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float32x4_t]: __arm_vcvtbq_m_f16_f32 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vcvttq_m(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float16x8_t]: __arm_vcvttq_m_f32_f16 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float32x4_t]: __arm_vcvttq_m_f16_f32 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vcvtmq_m(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtmq_m_s16_f16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtmq_m_s32_f32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtmq_m_u16_f16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtmq_m_u32_f32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vcvtnq_m(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtnq_m_s16_f16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtnq_m_s32_f32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtnq_m_u16_f16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtnq_m_u32_f32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vcvtpq_m(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtpq_m_s16_f16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtpq_m_s32_f32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcvtpq_m_u16_f16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcvtpq_m_u32_f32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vbicq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbicq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbicq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbicq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbicq_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbicq_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbicq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vbicq_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vbicq_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vornq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vornq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vornq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vornq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vornq_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vornq_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vornq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vornq_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vornq_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vld1q_z(p0,p1) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld1q_z_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), p1), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld1q_z_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), p1), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld1q_z_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld1q_z_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), p1), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld1q_z_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld1q_z_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), p1), \
  int (*)[__ARM_mve_type_float16_t_ptr]: __arm_vld1q_z_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), p1), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vld1q_z_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), p1)))

#define __arm_vld2q(p0) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld2q_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld2q_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld2q_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld2q_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld2q_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld2q_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *)), \
  int (*)[__ARM_mve_type_float16_t_ptr]: __arm_vld2q_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *)), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vld2q_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *))))

#define __arm_vld4q(p0) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld4q_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld4q_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld4q_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld4q_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld4q_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld4q_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *)), \
  int (*)[__ARM_mve_type_float16_t_ptr]: __arm_vld4q_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *)), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vld4q_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *))))

#define __arm_vldrhq_gather_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define __arm_vldrhq_gather_offset_z(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_z_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_z_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2));})

#define __arm_vldrhq_gather_shifted_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define __arm_vldrhq_gather_shifted_offset_z(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_z_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_z_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2));})

#define __arm_vldrwq_gather_offset(p0,p1) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_offset_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_offset_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), p1), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vldrwq_gather_offset_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), p1)))

#define __arm_vldrwq_gather_offset_z(p0,p1,p2) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_offset_z_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_offset_z_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), p1, p2), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vldrwq_gather_offset_z_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), p1, p2)))

#define __arm_vldrwq_gather_shifted_offset(p0,p1) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_shifted_offset_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_shifted_offset_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), p1), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vldrwq_gather_shifted_offset_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), p1)))

#define __arm_vldrwq_gather_shifted_offset_z(p0,p1,p2) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), p1, p2), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), p1, p2)))

#define __arm_vst1q_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vst1q_p_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vst1q_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vst1q_p_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vst1q_p_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vst1q_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vst1q_p_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8_t]: __arm_vst1q_p_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vst1q_p_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vst2q(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x2_t]: __arm_vst2q_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x2_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x2_t]: __arm_vst2q_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x2_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x2_t]: __arm_vst2q_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x2_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x2_t]: __arm_vst2q_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x2_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x2_t]: __arm_vst2q_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x2_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x2_t]: __arm_vst2q_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x2_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8x2_t]: __arm_vst2q_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, float16x8x2_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4x2_t]: __arm_vst2q_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), __ARM_mve_coerce(__p1, float32x4x2_t)));})

#define __arm_vstrhq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrhq_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrhq_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8_t]: __arm_vstrhq_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, float16x8_t)));})

#define __arm_vstrhq_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrhq_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrhq_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8_t]: __arm_vstrhq_p_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, float16x8_t), p2));})

#define __arm_vstrhq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_offset_p_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3));})

#define __arm_vstrhq_scatter_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_offset_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t)));})

#define __arm_vstrhq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3));})

#define __arm_vstrhq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_shifted_offset_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t)));})

#define __arm_vstrwq_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_p_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_p_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_p_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vstrwq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), __ARM_mve_coerce(__p1, float32x4_t)));})

#define __arm_vstrhq_scatter_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_offset_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t)));})

#define __arm_vstrhq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_offset_p_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3));})

#define __arm_vstrhq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_shifted_offset_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t)));})

#define __arm_vstrhq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_f16 (__ARM_mve_coerce_f16_ptr(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3));})

#define __arm_vstrwq_scatter_base(p0,p1,p2) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_base_s32 (p0, p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_base_u32 (p0, p1, __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_base_f32 (p0, p1, __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vstrwq_scatter_base_p(p0,p1,p2,p3) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_base_p_s32(p0, p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_base_p_u32(p0, p1, __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_base_p_f32(p0, p1, __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vstrwq_scatter_offset(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_offset_s32 (__ARM_mve_coerce_s32_ptr(__p0, int32_t *), p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_offset_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_offset_f32 (__ARM_mve_coerce_f32_ptr(__p0, float32_t *), p1, __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vstrwq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_offset_p_s32 (__ARM_mve_coerce_s32_ptr(__p0, int32_t *), p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_offset_p_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_offset_p_f32 (__ARM_mve_coerce_f32_ptr(__p0, float32_t *), p1, __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vstrwq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_shifted_offset_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), __p1, __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vstrwq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), __p1, __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vstrwq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), __p1, __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vstrwq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_shifted_offset_f32 (__ARM_mve_coerce_f32_ptr(p0, float32_t *), __p1, __ARM_mve_coerce(__p2, float32x4_t)));})

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

#define __arm_vstrwq_scatter_base_wb(p0,p1,p2) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_base_wb_s32 (p0, p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_base_wb_u32 (p0, p1, __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_base_wb_f32 (p0, p1, __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vstrwq_scatter_base_wb_p(p0,p1,p2,p3) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_base_wb_p_s32 (p0, p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_base_wb_p_u32 (p0, p1, __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_base_wb_p_f32 (p0, p1, __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vbicq_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbicq_x_s8   (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbicq_x_s16  (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbicq_x_s32  (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbicq_x_u8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbicq_x_u16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbicq_x_u32 (__ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vbicq_x_f16 (__ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vbicq_x_f32 (__ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcvtq_x(p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vcvtq_x_f16_s16 (__ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vcvtq_x_f32_s32 (__ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vcvtq_x_f16_u16 (__ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vcvtq_x_f32_u32 (__ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vcvtq_x_n(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vcvtq_x_n_f16_s16 (__ARM_mve_coerce(__p1, int16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vcvtq_x_n_f32_s32 (__ARM_mve_coerce(__p1, int32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vcvtq_x_n_f16_u16 (__ARM_mve_coerce(__p1, uint16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vcvtq_x_n_f32_u32 (__ARM_mve_coerce(__p1, uint32x4_t), p2, p3));})

#define __arm_vornq_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vornq_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vornq_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vornq_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vornq_x_u8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vornq_x_u16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vornq_x_u32 (__ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vornq_x_f16 (__ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vornq_x_f32 (__ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

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

#define __arm_vstrwq_scatter_base_wb(p0,p1,p2) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_base_wb_s32 (p0, p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_base_wb_u32 (p0, p1, __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrwq_scatter_base_wb_p(p0,p1,p2,p3) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_base_wb_p_s32 (p0, p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_base_wb_p_u32 (p0, p1, __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vst4q(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x4_t]: __arm_vst4q_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x4_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x4_t]: __arm_vst4q_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x4_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x4_t]: __arm_vst4q_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x4_t]: __arm_vst4q_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x4_t]: __arm_vst4q_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x4_t]: __arm_vst4q_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x4_t)));})

#define __arm_vornq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vornq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vornq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vornq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vornq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vornq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vornq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vbicq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vbicq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce_i_scalar (__p1, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vbicq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce_i_scalar (__p1, int)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vbicq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce_i_scalar (__p1, int)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vbicq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce_i_scalar (__p1, int)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbicq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbicq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbicq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbicq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbicq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbicq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vshlcq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlcq_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1, p2), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlcq_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1, p2), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshlcq_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1, p2), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlcq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1, p2), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlcq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1, p2), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshlcq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1, p2));})

#define __arm_vbicq_m_n(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vbicq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1, p2), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vbicq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1, p2), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vbicq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1, p2), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vbicq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1, p2));})

#define __arm_vbicq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbicq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbicq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbicq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbicq_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbicq_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbicq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vornq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vornq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vornq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vornq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vornq_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vornq_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vornq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrwq_scatter_base(p0,p1,p2) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_base_s32(p0, p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_base_u32(p0, p1, __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vldrbq_gather_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_s16 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_s32 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_u16 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_u32 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vstrwq_scatter_base_p(p0,p1,p2,p3) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_base_p_s32 (p0, p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_base_p_u32 (p0, p1, __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vldrhq_gather_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vldrhq_gather_offset_z(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_z_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_z_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vldrhq_gather_shifted_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vldrhq_gather_shifted_offset_z(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_z_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_z_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vldrwq_gather_offset(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_offset_s32 (__ARM_mve_coerce_s32_ptr(__p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_offset_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1));})

#define __arm_vldrwq_gather_offset_z(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_offset_z_s32 (__ARM_mve_coerce_s32_ptr(__p0, int32_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_offset_z_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, p2));})

#define __arm_vldrwq_gather_shifted_offset(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_shifted_offset_s32 (__ARM_mve_coerce_s32_ptr(__p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_shifted_offset_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1));})

#define __arm_vldrwq_gather_shifted_offset_z(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_s32 (__ARM_mve_coerce_s32_ptr(__p0, int32_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, p2));})

#define __arm_vst1q_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vst1q_p_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vst1q_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vst1q_p_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vst1q_p_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vst1q_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vst1q_p_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vst2q(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x2_t]: __arm_vst2q_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x2_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x2_t]: __arm_vst2q_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x2_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x2_t]: __arm_vst2q_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x2_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x2_t]: __arm_vst2q_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x2_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x2_t]: __arm_vst2q_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x2_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x2_t]: __arm_vst2q_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x2_t)));})

#define __arm_vstrhq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrhq_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrhq_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vstrhq_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrhq_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrhq_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vstrhq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrhq_scatter_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrhq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrhq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})


#define __arm_vstrwq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vstrwq_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_p_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_p_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vstrdq_scatter_base_p(p0,p1,p2,p3) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_base_p_s64 (p0, p1, __ARM_mve_coerce(__p2, int64x2_t), p3), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_base_p_u64 (p0, p1, __ARM_mve_coerce(__p2, uint64x2_t), p3));})

#define __arm_vstrdq_scatter_base(p0,p1,p2) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_base_s64 (p0, p1, __ARM_mve_coerce(__p2, int64x2_t)), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_base_u64 (p0, p1, __ARM_mve_coerce(__p2, uint64x2_t)));})

#define __arm_vstrhq_scatter_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrhq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrhq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrhq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrwq_scatter_offset(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_offset_s32 (__ARM_mve_coerce_s32_ptr(__p0, int32_t *), p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_offset_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrwq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_offset_p_s32 (__ARM_mve_coerce_s32_ptr(__p0, int32_t *), p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_offset_p_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrwq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrwq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t), p3));})

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

#define __arm_vornq_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vornq_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vornq_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vornq_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vornq_x_u8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vornq_x_u16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vornq_x_u32 (__ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vbicq_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbicq_x_s8   (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbicq_x_s16  (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbicq_x_s32  (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbicq_x_u8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbicq_x_u16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbicq_x_u32 (__ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vld1q_z(p0,p1) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld1q_z_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), p1), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld1q_z_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), p1), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld1q_z_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld1q_z_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), p1), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld1q_z_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld1q_z_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), p1)))

#define __arm_vld2q(p0) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld2q_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld2q_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld2q_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld2q_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld2q_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld2q_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *))))


#define __arm_vld4q(p0) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld4q_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld4q_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld4q_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld4q_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld4q_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld4q_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *))))

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



#define __arm_vdwdupq_x_u8(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_x_n_u8 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_x_wb_u8 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4));})

#define __arm_vdwdupq_x_u16(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_x_n_u16 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_x_wb_u16 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4));})

#define __arm_vdwdupq_x_u32(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_x_n_u32 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_x_wb_u32 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4));})

#define __arm_viwdupq_x_u8(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_x_n_u8 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_x_wb_u8 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4));})

#define __arm_viwdupq_x_u16(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_x_n_u16 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_x_wb_u16 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4));})

#define __arm_viwdupq_x_u32(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_x_n_u32 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_x_wb_u32 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4));})

#define __arm_vidupq_x_u8(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_x_n_u8 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_x_wb_u8 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3));})

#define __arm_vddupq_x_u8(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_x_n_u8 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_x_wb_u8 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3));})

#define __arm_vidupq_x_u16(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_x_n_u16 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_x_wb_u16 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3));})

#define __arm_vddupq_x_u16(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_x_n_u16 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_x_wb_u16 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3));})

#define __arm_vidupq_x_u32(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_x_n_u32 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_x_wb_u32 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3));})

#define __arm_vddupq_x_u32(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_x_n_u32 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_x_wb_u32 (__ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3));})

#define __arm_vadciq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vadciq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vadciq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vstrdq_scatter_base_wb_p(p0,p1,p2,p3) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_base_wb_p_s64 (p0, p1, __ARM_mve_coerce(__p2, int64x2_t), p3), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_base_wb_p_u64 (p0, p1, __ARM_mve_coerce(__p2, uint64x2_t), p3));})

#define __arm_vstrdq_scatter_base_wb(p0,p1,p2) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_base_wb_s64 (p0, p1, __ARM_mve_coerce(__p2, int64x2_t)), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_base_wb_u64 (p0, p1, __ARM_mve_coerce(__p2, uint64x2_t)));})

#define __arm_vldrdq_gather_offset(p0,p1) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr]: __arm_vldrdq_gather_offset_s64 (__ARM_mve_coerce_s64_ptr(p0, int64_t *), p1), \
  int (*)[__ARM_mve_type_uint64_t_ptr]: __arm_vldrdq_gather_offset_u64 (__ARM_mve_coerce_u64_ptr(p0, uint64_t *), p1)))

#define __arm_vldrdq_gather_offset_z(p0,p1,p2) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr]: __arm_vldrdq_gather_offset_z_s64 (__ARM_mve_coerce_s64_ptr(p0, int64_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint64_t_ptr]: __arm_vldrdq_gather_offset_z_u64 (__ARM_mve_coerce_u64_ptr(p0, uint64_t *), p1, p2)))

#define __arm_vldrdq_gather_shifted_offset(p0,p1) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr]: __arm_vldrdq_gather_shifted_offset_s64 (__ARM_mve_coerce_s64_ptr(p0, int64_t *), p1), \
  int (*)[__ARM_mve_type_uint64_t_ptr]: __arm_vldrdq_gather_shifted_offset_u64 (__ARM_mve_coerce_u64_ptr(p0, uint64_t *), p1)))

#define __arm_vldrdq_gather_shifted_offset_z(p0,p1,p2) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr]: __arm_vldrdq_gather_shifted_offset_z_s64 (__ARM_mve_coerce_s64_ptr(p0, int64_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint64_t_ptr]: __arm_vldrdq_gather_shifted_offset_z_u64 (__ARM_mve_coerce_u64_ptr(p0, uint64_t *), p1, p2)))

#define __arm_vadciq_m(p0,p1,p2,p3,p4) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vadciq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vadciq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3, p4));})

#define __arm_vadciq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vadciq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vadciq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vadcq_m(p0,p1,p2,p3,p4) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vadcq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vadcq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3, p4));})

#define __arm_vadcq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vadcq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vadcq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vsbciq_m(p0,p1,p2,p3,p4) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsbciq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsbciq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3, p4));})

#define __arm_vsbciq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsbciq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsbciq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vsbcq_m(p0,p1,p2,p3,p4) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsbcq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsbcq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3, p4));})

#define __arm_vsbcq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsbcq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsbcq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vldrbq_gather_offset_z(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_z_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_z_s16 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_z_s32 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_z_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_z_u16 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_z_u32 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vldrbq_gather_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_s8(__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_s16(__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_s32(__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_u8(__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_u16(__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_u32(__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vidupq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
 __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
 int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vidupq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vidupq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vidupq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_m_wb_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3), \
 int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_m_wb_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3), \
 int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_m_wb_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3));})

#define __arm_vddupq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
 __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
 int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vddupq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vddupq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vddupq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_m_wb_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3), \
 int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_m_wb_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3), \
 int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_m_wb_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3));})

#define __arm_vidupq_u16(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_n_u16 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_wb_u16 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1));})

#define __arm_vidupq_u32(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_n_u32 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_wb_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1));})

#define __arm_vidupq_u8(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_n_u8 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_wb_u8 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1));})

#define __arm_vddupq_u16(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_n_u16 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_wb_u16 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1));})

#define __arm_vddupq_u32(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_n_u32 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_wb_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1));})

#define __arm_vddupq_u8(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_n_u8 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_wb_u8 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1));})

#define __arm_viwdupq_m(p0,p1,p2,p3,p4) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_viwdupq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce_i_scalar(__p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_viwdupq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce_i_scalar(__p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_viwdupq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce_i_scalar(__p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_m_wb_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_m_wb_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_m_wb_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4));})

#define __arm_viwdupq_u16(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_n_u16 (__ARM_mve_coerce_i_scalar(__p0, int), p1, (const int) p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_wb_u16 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, (const int) p2));})

#define __arm_viwdupq_u32(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_n_u32 (__ARM_mve_coerce_i_scalar(__p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_wb_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, p2));})

#define __arm_viwdupq_u8(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_n_u8 (__ARM_mve_coerce_i_scalar(__p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_wb_u8 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, p2));})

#define __arm_vdwdupq_m(p0,p1,p2,p3,p4) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vdwdupq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce_i_scalar(__p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vdwdupq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce_i_scalar(__p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vdwdupq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce_i_scalar(__p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_m_wb_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_m_wb_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_m_wb_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce_u32_ptr(__p1, uint32_t *), p2, p3, p4));})

#define __arm_vdwdupq_u16(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_n_u16 (__ARM_mve_coerce_i_scalar(__p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_wb_u16 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, p2));})

#define __arm_vdwdupq_u32(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_n_u32 (__ARM_mve_coerce_i_scalar(__p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_wb_u32 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, p2));})

#define __arm_vdwdupq_u8(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_n_u8 (__ARM_mve_coerce_i_scalar(__p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_wb_u8 (__ARM_mve_coerce_u32_ptr(__p0, uint32_t *), p1, p2));})

#define __arm_vshlcq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlcq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlcq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshlcq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlcq_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlcq_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshlcq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1, p2, p3));})

#define __arm_vstrbq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vstrbq_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrbq_s16 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrbq_s32 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vstrbq_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrbq_u16 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrbq_u32 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vstrbq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vstrbq_p_s8 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrbq_p_s16 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrbq_p_s32 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vstrbq_p_u8 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrbq_p_u16 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrbq_p_u32 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vstrdq_scatter_base(p0,p1,p2) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_base_s64 (p0, p1, __ARM_mve_coerce(__p2, int64x2_t)), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_base_u64 (p0, p1, __ARM_mve_coerce(__p2, uint64x2_t)));})

#define __arm_vstrdq_scatter_base_p(p0,p1,p2,p3) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_base_p_s64 (p0, p1, __ARM_mve_coerce(__p2, int64x2_t), p3), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_base_p_u64 (p0, p1, __ARM_mve_coerce(__p2, uint64x2_t), p3));})

#define __arm_vstrbq_scatter_offset(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vstrbq_scatter_offset_s8 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrbq_scatter_offset_s16 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrbq_scatter_offset_s32 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vstrbq_scatter_offset_u8 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrbq_scatter_offset_u16 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrbq_scatter_offset_u32 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrbq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vstrbq_scatter_offset_p_s8 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrbq_scatter_offset_p_s16 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrbq_scatter_offset_p_s32 (__ARM_mve_coerce_s8_ptr(__p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vstrbq_scatter_offset_p_u8 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrbq_scatter_offset_p_u16 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrbq_scatter_offset_p_u32 (__ARM_mve_coerce_u8_ptr(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrdq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr][__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_offset_p_s64 (__ARM_mve_coerce_s64_ptr(__p0, int64_t *), p1, __ARM_mve_coerce(__p2, int64x2_t), p3), \
  int (*)[__ARM_mve_type_uint64_t_ptr][__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_offset_p_u64 (__ARM_mve_coerce_u64_ptr(__p0, uint64_t *), p1, __ARM_mve_coerce(__p2, uint64x2_t), p3));})

#define __arm_vstrdq_scatter_offset(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr][__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_offset_s64 (__ARM_mve_coerce_s64_ptr(__p0, int64_t *), p1, __ARM_mve_coerce(__p2, int64x2_t)), \
  int (*)[__ARM_mve_type_uint64_t_ptr][__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_offset_u64 (__ARM_mve_coerce_u64_ptr(__p0, uint64_t *), p1, __ARM_mve_coerce(__p2, uint64x2_t)));})

#define __arm_vstrdq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr][__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_shifted_offset_p_s64 (__ARM_mve_coerce_s64_ptr(__p0, int64_t *), p1, __ARM_mve_coerce(__p2, int64x2_t), p3), \
  int (*)[__ARM_mve_type_uint64_t_ptr][__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_shifted_offset_p_u64 (__ARM_mve_coerce_u64_ptr(__p0, uint64_t *), p1, __ARM_mve_coerce(__p2, uint64x2_t), p3));})

#define __arm_vstrdq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr][__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_shifted_offset_s64 (__ARM_mve_coerce_s64_ptr(__p0, int64_t *), p1, __ARM_mve_coerce(__p2, int64x2_t)), \
  int (*)[__ARM_mve_type_uint64_t_ptr][__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_shifted_offset_u64 (__ARM_mve_coerce_u64_ptr(__p0, uint64_t *), p1, __ARM_mve_coerce(__p2, uint64x2_t)));})

#endif /* __cplusplus  */
#endif /* __ARM_FEATURE_MVE  */
#endif /* _GCC_ARM_MVE_H.  */
