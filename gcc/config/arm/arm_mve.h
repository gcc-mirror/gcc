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
#define vstrbq_scatter_offset(__base, __offset, __value) __arm_vstrbq_scatter_offset(__base, __offset, __value)
#define vstrwq_scatter_base(__addr, __offset, __value) __arm_vstrwq_scatter_base(__addr, __offset, __value)
#define vldrbq_gather_offset(__base, __offset) __arm_vldrbq_gather_offset(__base, __offset)
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
#define vstrdq_scatter_base_wb(__addr, __offset, __value) __arm_vstrdq_scatter_base_wb(__addr, __offset, __value)
#define vstrdq_scatter_base_wb_p(__addr, __offset, __value, __p) __arm_vstrdq_scatter_base_wb_p(__addr, __offset, __value, __p)
#define vstrwq_scatter_base_wb_p(__addr, __offset, __value, __p) __arm_vstrwq_scatter_base_wb_p(__addr, __offset, __value, __p)
#define vstrwq_scatter_base_wb(__addr, __offset, __value) __arm_vstrwq_scatter_base_wb(__addr, __offset, __value)
#define vst2q(__addr, __value) __arm_vst2q(__addr, __value)
#define vld2q(__addr) __arm_vld2q(__addr)
#define vld4q(__addr) __arm_vld4q(__addr)
#define vsetq_lane(__a, __b, __idx) __arm_vsetq_lane(__a, __b, __idx)
#define vgetq_lane(__a, __idx) __arm_vgetq_lane(__a, __idx)


#define vst4q_s8( __addr, __value) __arm_vst4q_s8( __addr, __value)
#define vst4q_s16( __addr, __value) __arm_vst4q_s16( __addr, __value)
#define vst4q_s32( __addr, __value) __arm_vst4q_s32( __addr, __value)
#define vst4q_u8( __addr, __value) __arm_vst4q_u8( __addr, __value)
#define vst4q_u16( __addr, __value) __arm_vst4q_u16( __addr, __value)
#define vst4q_u32( __addr, __value) __arm_vst4q_u32( __addr, __value)
#define vst4q_f16( __addr, __value) __arm_vst4q_f16( __addr, __value)
#define vst4q_f32( __addr, __value) __arm_vst4q_f32( __addr, __value)
#define vpnot(__a) __arm_vpnot(__a)
#define vstrbq_scatter_offset_s8( __base, __offset, __value) __arm_vstrbq_scatter_offset_s8( __base, __offset, __value)
#define vstrbq_scatter_offset_u8( __base, __offset, __value) __arm_vstrbq_scatter_offset_u8( __base, __offset, __value)
#define vstrbq_scatter_offset_u16( __base, __offset, __value) __arm_vstrbq_scatter_offset_u16( __base, __offset, __value)
#define vstrbq_scatter_offset_s16( __base, __offset, __value) __arm_vstrbq_scatter_offset_s16( __base, __offset, __value)
#define vstrbq_scatter_offset_u32( __base, __offset, __value) __arm_vstrbq_scatter_offset_u32( __base, __offset, __value)
#define vstrbq_scatter_offset_s32( __base, __offset, __value) __arm_vstrbq_scatter_offset_s32( __base, __offset, __value)
#define vstrwq_scatter_base_s32(__addr,  __offset, __value) __arm_vstrwq_scatter_base_s32(__addr,  __offset, __value)
#define vstrwq_scatter_base_u32(__addr,  __offset, __value) __arm_vstrwq_scatter_base_u32(__addr,  __offset, __value)
#define vldrbq_gather_offset_u8(__base, __offset) __arm_vldrbq_gather_offset_u8(__base, __offset)
#define vldrbq_gather_offset_s8(__base, __offset) __arm_vldrbq_gather_offset_s8(__base, __offset)
#define vldrbq_gather_offset_u16(__base, __offset) __arm_vldrbq_gather_offset_u16(__base, __offset)
#define vldrbq_gather_offset_s16(__base, __offset) __arm_vldrbq_gather_offset_s16(__base, __offset)
#define vldrbq_gather_offset_u32(__base, __offset) __arm_vldrbq_gather_offset_u32(__base, __offset)
#define vldrbq_gather_offset_s32(__base, __offset) __arm_vldrbq_gather_offset_s32(__base, __offset)
#define vldrwq_gather_base_s32(__addr,  __offset) __arm_vldrwq_gather_base_s32(__addr,  __offset)
#define vldrwq_gather_base_u32(__addr,  __offset) __arm_vldrwq_gather_base_u32(__addr,  __offset)
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
#define vst2q_s8(__addr, __value) __arm_vst2q_s8(__addr, __value)
#define vst2q_u8(__addr, __value) __arm_vst2q_u8(__addr, __value)
#define vld2q_s8(__addr) __arm_vld2q_s8(__addr)
#define vld2q_u8(__addr) __arm_vld2q_u8(__addr)
#define vld4q_s8(__addr) __arm_vld4q_s8(__addr)
#define vld4q_u8(__addr) __arm_vld4q_u8(__addr)
#define vst2q_s16(__addr, __value) __arm_vst2q_s16(__addr, __value)
#define vst2q_u16(__addr, __value) __arm_vst2q_u16(__addr, __value)
#define vld2q_s16(__addr) __arm_vld2q_s16(__addr)
#define vld2q_u16(__addr) __arm_vld2q_u16(__addr)
#define vld4q_s16(__addr) __arm_vld4q_s16(__addr)
#define vld4q_u16(__addr) __arm_vld4q_u16(__addr)
#define vst2q_s32(__addr, __value) __arm_vst2q_s32(__addr, __value)
#define vst2q_u32(__addr, __value) __arm_vst2q_u32(__addr, __value)
#define vld2q_s32(__addr) __arm_vld2q_s32(__addr)
#define vld2q_u32(__addr) __arm_vld2q_u32(__addr)
#define vld4q_s32(__addr) __arm_vld4q_s32(__addr)
#define vld4q_u32(__addr) __arm_vld4q_u32(__addr)
#define vld4q_f16(__addr) __arm_vld4q_f16(__addr)
#define vld2q_f16(__addr) __arm_vld2q_f16(__addr)
#define vst2q_f16(__addr, __value) __arm_vst2q_f16(__addr, __value)
#define vld4q_f32(__addr) __arm_vld4q_f32(__addr)
#define vld2q_f32(__addr) __arm_vld2q_f32(__addr)
#define vst2q_f32(__addr, __value) __arm_vst2q_f32(__addr, __value)
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
__arm_vpnot (mve_pred16_t __a)
{
  return __builtin_mve_vpnotv16bi (__a);
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

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_f16 (float16_t * __addr, float16x8x2_t __value)
{
  union { float16x8x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv8hf (__addr, __rv.__o);
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

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q_f32 (float32_t * __addr, float32x4x2_t __value)
{
  union { float32x4x2_t __i; __builtin_neon_oi __o; } __rv;
  __rv.__i = __value;
  __builtin_mve_vst2qv4sf (__addr, __rv.__o);
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

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (float16_t * __addr, float16x8x2_t __value)
{
 __arm_vst2q_f16 (__addr, __value);
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

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst2q (float32_t * __addr, float32x4x2_t __value)
{
 __arm_vst2q_f32 (__addr, __value);
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

#define __arm_vst2q(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x2_t]: __arm_vst2q_s8 (__ARM_mve_coerce_s8_ptr(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x2_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x2_t]: __arm_vst2q_s16 (__ARM_mve_coerce_s16_ptr(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x2_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x2_t]: __arm_vst2q_s32 (__ARM_mve_coerce_s32_ptr(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x2_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x2_t]: __arm_vst2q_u8 (__ARM_mve_coerce_u8_ptr(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x2_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x2_t]: __arm_vst2q_u16 (__ARM_mve_coerce_u16_ptr(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x2_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x2_t]: __arm_vst2q_u32 (__ARM_mve_coerce_u32_ptr(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x2_t)));})

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
