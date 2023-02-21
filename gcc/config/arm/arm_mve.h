/* Arm MVE intrinsics include file.

   Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
#define vmvnq(__a) __arm_vmvnq(__a)
#define vornq(__a, __b) __arm_vornq(__a, __b)
#define vmulltq_int(__a, __b) __arm_vmulltq_int(__a, __b)
#define vmullbq_int(__a, __b) __arm_vmullbq_int(__a, __b)
#define vmladavq(__a, __b) __arm_vmladavq(__a, __b)
#define vcaddq_rot90(__a, __b) __arm_vcaddq_rot90(__a, __b)
#define vcaddq_rot270(__a, __b) __arm_vcaddq_rot270(__a, __b)
#define vbicq(__a, __b) __arm_vbicq(__a, __b)
#define vbrsrq(__a, __b) __arm_vbrsrq(__a, __b)
#define vqshluq(__a, __imm) __arm_vqshluq(__a, __imm)
#define vmlsdavxq(__a, __b) __arm_vmlsdavxq(__a, __b)
#define vmlsdavq(__a, __b) __arm_vmlsdavq(__a, __b)
#define vmladavxq(__a, __b) __arm_vmladavxq(__a, __b)
#define vhcaddq_rot90(__a, __b) __arm_vhcaddq_rot90(__a, __b)
#define vhcaddq_rot270(__a, __b) __arm_vhcaddq_rot270(__a, __b)
#define vmulltq_poly(__a, __b) __arm_vmulltq_poly(__a, __b)
#define vmullbq_poly(__a, __b) __arm_vmullbq_poly(__a, __b)
#define vmlaldavq(__a, __b) __arm_vmlaldavq(__a, __b)
#define vqdmulltq(__a, __b) __arm_vqdmulltq(__a, __b)
#define vqdmullbq(__a, __b) __arm_vqdmullbq(__a, __b)
#define vmlsldavxq(__a, __b) __arm_vmlsldavxq(__a, __b)
#define vmlsldavq(__a, __b) __arm_vmlsldavq(__a, __b)
#define vmlaldavxq(__a, __b) __arm_vmlaldavxq(__a, __b)
#define vrmlaldavhq(__a, __b) __arm_vrmlaldavhq(__a, __b)
#define vrmlsldavhxq(__a, __b) __arm_vrmlsldavhxq(__a, __b)
#define vrmlsldavhq(__a, __b) __arm_vrmlsldavhq(__a, __b)
#define vrmlaldavhxq(__a, __b) __arm_vrmlaldavhxq(__a, __b)
#define vabavq(__a, __b, __c) __arm_vabavq(__a, __b, __c)
#define vbicq_m_n(__a, __imm, __p) __arm_vbicq_m_n(__a, __imm, __p)
#define vrmlaldavhaq(__a, __b, __c) __arm_vrmlaldavhaq(__a, __b, __c)
#define vshlcq(__a, __b, __imm) __arm_vshlcq(__a, __b, __imm)
#define vpselq(__a, __b, __p) __arm_vpselq(__a, __b, __p)
#define vqrdmlashq(__a, __b, __c) __arm_vqrdmlashq(__a, __b, __c)
#define vqrdmlahq(__a, __b, __c) __arm_vqrdmlahq(__a, __b, __c)
#define vqdmlashq(__a, __b, __c) __arm_vqdmlashq(__a, __b, __c)
#define vqdmlahq(__a, __b, __c) __arm_vqdmlahq(__a, __b, __c)
#define vmvnq_m(__inactive, __a, __p) __arm_vmvnq_m(__inactive, __a, __p)
#define vmlasq(__a, __b, __c) __arm_vmlasq(__a, __b, __c)
#define vmlaq(__a, __b, __c) __arm_vmlaq(__a, __b, __c)
#define vmladavq_p(__a, __b, __p) __arm_vmladavq_p(__a, __b, __p)
#define vmladavaq(__a, __b, __c) __arm_vmladavaq(__a, __b, __c)
#define vsriq(__a, __b, __imm) __arm_vsriq(__a, __b, __imm)
#define vsliq(__a, __b, __imm) __arm_vsliq(__a, __b, __imm)
#define vmlsdavxq_p(__a, __b, __p) __arm_vmlsdavxq_p(__a, __b, __p)
#define vmlsdavq_p(__a, __b, __p) __arm_vmlsdavq_p(__a, __b, __p)
#define vmladavxq_p(__a, __b, __p) __arm_vmladavxq_p(__a, __b, __p)
#define vqrdmlsdhxq(__inactive, __a, __b) __arm_vqrdmlsdhxq(__inactive, __a, __b)
#define vqrdmlsdhq(__inactive, __a, __b) __arm_vqrdmlsdhq(__inactive, __a, __b)
#define vqrdmladhxq(__inactive, __a, __b) __arm_vqrdmladhxq(__inactive, __a, __b)
#define vqrdmladhq(__inactive, __a, __b) __arm_vqrdmladhq(__inactive, __a, __b)
#define vqdmlsdhxq(__inactive, __a, __b) __arm_vqdmlsdhxq(__inactive, __a, __b)
#define vqdmlsdhq(__inactive, __a, __b) __arm_vqdmlsdhq(__inactive, __a, __b)
#define vqdmladhxq(__inactive, __a, __b) __arm_vqdmladhxq(__inactive, __a, __b)
#define vqdmladhq(__inactive, __a, __b) __arm_vqdmladhq(__inactive, __a, __b)
#define vmlsdavaxq(__a, __b, __c) __arm_vmlsdavaxq(__a, __b, __c)
#define vmlsdavaq(__a, __b, __c) __arm_vmlsdavaq(__a, __b, __c)
#define vmladavaxq(__a, __b, __c) __arm_vmladavaxq(__a, __b, __c)
#define vrmlaldavhaxq(__a, __b, __c) __arm_vrmlaldavhaxq(__a, __b, __c)
#define vrmlsldavhaq(__a, __b, __c) __arm_vrmlsldavhaq(__a, __b, __c)
#define vrmlsldavhaxq(__a, __b, __c) __arm_vrmlsldavhaxq(__a, __b, __c)
#define vrmlaldavhq_p(__a, __b, __p) __arm_vrmlaldavhq_p(__a, __b, __p)
#define vrmlaldavhxq_p(__a, __b, __p) __arm_vrmlaldavhxq_p(__a, __b, __p)
#define vrmlsldavhq_p(__a, __b, __p) __arm_vrmlsldavhq_p(__a, __b, __p)
#define vrmlsldavhxq_p(__a, __b, __p) __arm_vrmlsldavhxq_p(__a, __b, __p)
#define vmlaldavaq(__a, __b, __c) __arm_vmlaldavaq(__a, __b, __c)
#define vmlaldavaxq(__a, __b, __c) __arm_vmlaldavaxq(__a, __b, __c)
#define vmlsldavaq(__a, __b, __c) __arm_vmlsldavaq(__a, __b, __c)
#define vmlsldavaxq(__a, __b, __c) __arm_vmlsldavaxq(__a, __b, __c)
#define vmlaldavq_p(__a, __b, __p) __arm_vmlaldavq_p(__a, __b, __p)
#define vmlaldavxq_p(__a, __b, __p) __arm_vmlaldavxq_p(__a, __b, __p)
#define vmlsldavq_p(__a, __b, __p) __arm_vmlsldavq_p(__a, __b, __p)
#define vmlsldavxq_p(__a, __b, __p) __arm_vmlsldavxq_p(__a, __b, __p)
#define vsriq_m(__a, __b, __imm, __p) __arm_vsriq_m(__a, __b, __imm, __p)
#define vqshluq_m(__inactive, __a, __imm, __p) __arm_vqshluq_m(__inactive, __a, __imm, __p)
#define vabavq_p(__a, __b, __c, __p) __arm_vabavq_p(__a, __b, __c, __p)
#define vbicq_m(__inactive, __a, __b, __p) __arm_vbicq_m(__inactive, __a, __b, __p)
#define vbrsrq_m(__inactive, __a, __b, __p) __arm_vbrsrq_m(__inactive, __a, __b, __p)
#define vcaddq_rot270_m(__inactive, __a, __b, __p) __arm_vcaddq_rot270_m(__inactive, __a, __b, __p)
#define vcaddq_rot90_m(__inactive, __a, __b, __p) __arm_vcaddq_rot90_m(__inactive, __a, __b, __p)
#define vhcaddq_rot270_m(__inactive, __a, __b, __p) __arm_vhcaddq_rot270_m(__inactive, __a, __b, __p)
#define vhcaddq_rot90_m(__inactive, __a, __b, __p) __arm_vhcaddq_rot90_m(__inactive, __a, __b, __p)
#define vmladavaq_p(__a, __b, __c, __p) __arm_vmladavaq_p(__a, __b, __c, __p)
#define vmladavaxq_p(__a, __b, __c, __p) __arm_vmladavaxq_p(__a, __b, __c, __p)
#define vmlaq_m(__a, __b, __c, __p) __arm_vmlaq_m(__a, __b, __c, __p)
#define vmlasq_m(__a, __b, __c, __p) __arm_vmlasq_m(__a, __b, __c, __p)
#define vmlsdavaq_p(__a, __b, __c, __p) __arm_vmlsdavaq_p(__a, __b, __c, __p)
#define vmlsdavaxq_p(__a, __b, __c, __p) __arm_vmlsdavaxq_p(__a, __b, __c, __p)
#define vmullbq_int_m(__inactive, __a, __b, __p) __arm_vmullbq_int_m(__inactive, __a, __b, __p)
#define vmulltq_int_m(__inactive, __a, __b, __p) __arm_vmulltq_int_m(__inactive, __a, __b, __p)
#define vornq_m(__inactive, __a, __b, __p) __arm_vornq_m(__inactive, __a, __b, __p)
#define vqdmladhq_m(__inactive, __a, __b, __p) __arm_vqdmladhq_m(__inactive, __a, __b, __p)
#define vqdmlashq_m(__a, __b, __c, __p) __arm_vqdmlashq_m(__a, __b, __c, __p)
#define vqdmladhxq_m(__inactive, __a, __b, __p) __arm_vqdmladhxq_m(__inactive, __a, __b, __p)
#define vqdmlahq_m(__a, __b, __c, __p) __arm_vqdmlahq_m(__a, __b, __c, __p)
#define vqdmlsdhq_m(__inactive, __a, __b, __p) __arm_vqdmlsdhq_m(__inactive, __a, __b, __p)
#define vqdmlsdhxq_m(__inactive, __a, __b, __p) __arm_vqdmlsdhxq_m(__inactive, __a, __b, __p)
#define vqrdmladhq_m(__inactive, __a, __b, __p) __arm_vqrdmladhq_m(__inactive, __a, __b, __p)
#define vqrdmladhxq_m(__inactive, __a, __b, __p) __arm_vqrdmladhxq_m(__inactive, __a, __b, __p)
#define vqrdmlahq_m(__a, __b, __c, __p) __arm_vqrdmlahq_m(__a, __b, __c, __p)
#define vqrdmlashq_m(__a, __b, __c, __p) __arm_vqrdmlashq_m(__a, __b, __c, __p)
#define vqrdmlsdhq_m(__inactive, __a, __b, __p) __arm_vqrdmlsdhq_m(__inactive, __a, __b, __p)
#define vqrdmlsdhxq_m(__inactive, __a, __b, __p) __arm_vqrdmlsdhxq_m(__inactive, __a, __b, __p)
#define vsliq_m(__a, __b, __imm, __p) __arm_vsliq_m(__a, __b, __imm, __p)
#define vmlaldavaq_p(__a, __b, __c, __p) __arm_vmlaldavaq_p(__a, __b, __c, __p)
#define vmlaldavaxq_p(__a, __b, __c, __p) __arm_vmlaldavaxq_p(__a, __b, __c, __p)
#define vmlsldavaq_p(__a, __b, __c, __p) __arm_vmlsldavaq_p(__a, __b, __c, __p)
#define vmlsldavaxq_p(__a, __b, __c, __p) __arm_vmlsldavaxq_p(__a, __b, __c, __p)
#define vmullbq_poly_m(__inactive, __a, __b, __p) __arm_vmullbq_poly_m(__inactive, __a, __b, __p)
#define vmulltq_poly_m(__inactive, __a, __b, __p) __arm_vmulltq_poly_m(__inactive, __a, __b, __p)
#define vqdmullbq_m(__inactive, __a, __b, __p) __arm_vqdmullbq_m(__inactive, __a, __b, __p)
#define vqdmulltq_m(__inactive, __a, __b, __p) __arm_vqdmulltq_m(__inactive, __a, __b, __p)
#define vrmlaldavhaq_p(__a, __b, __c, __p) __arm_vrmlaldavhaq_p(__a, __b, __c, __p)
#define vrmlaldavhaxq_p(__a, __b, __c, __p) __arm_vrmlaldavhaxq_p(__a, __b, __c, __p)
#define vrmlsldavhaq_p(__a, __b, __c, __p) __arm_vrmlsldavhaq_p(__a, __b, __c, __p)
#define vrmlsldavhaxq_p(__a, __b, __c, __p) __arm_vrmlsldavhaxq_p(__a, __b, __c, __p)
#define vstrbq_scatter_offset(__base, __offset, __value) __arm_vstrbq_scatter_offset(__base, __offset, __value)
#define vstrbq(__addr, __value) __arm_vstrbq(__addr, __value)
#define vstrwq_scatter_base(__addr, __offset, __value) __arm_vstrwq_scatter_base(__addr, __offset, __value)
#define vldrbq_gather_offset(__base, __offset) __arm_vldrbq_gather_offset(__base, __offset)
#define vstrbq_p(__addr, __value, __p) __arm_vstrbq_p(__addr, __value, __p)
#define vstrbq_scatter_offset_p(__base, __offset, __value, __p) __arm_vstrbq_scatter_offset_p(__base, __offset, __value, __p)
#define vstrwq_scatter_base_p(__addr, __offset, __value, __p) __arm_vstrwq_scatter_base_p(__addr, __offset, __value, __p)
#define vldrbq_gather_offset_z(__base, __offset, __p) __arm_vldrbq_gather_offset_z(__base, __offset, __p)
#define vld1q(__base) __arm_vld1q(__base)
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
#define vst1q(__addr, __value) __arm_vst1q(__addr, __value)
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
#define vmullbq_poly_x(__a, __b, __p) __arm_vmullbq_poly_x(__a, __b, __p)
#define vmullbq_int_x(__a, __b, __p) __arm_vmullbq_int_x(__a, __b, __p)
#define vmulltq_poly_x(__a, __b, __p) __arm_vmulltq_poly_x(__a, __b, __p)
#define vmulltq_int_x(__a, __b, __p) __arm_vmulltq_int_x(__a, __b, __p)
#define vcaddq_rot90_x(__a, __b, __p) __arm_vcaddq_rot90_x(__a, __b, __p)
#define vcaddq_rot270_x(__a, __b, __p) __arm_vcaddq_rot270_x(__a, __b, __p)
#define vhcaddq_rot90_x(__a, __b, __p) __arm_vhcaddq_rot90_x(__a, __b, __p)
#define vhcaddq_rot270_x(__a, __b, __p) __arm_vhcaddq_rot270_x(__a, __b, __p)
#define vbicq_x(__a, __b, __p) __arm_vbicq_x(__a, __b, __p)
#define vbrsrq_x(__a, __b, __p) __arm_vbrsrq_x(__a, __b, __p)
#define vmvnq_x(__a, __p) __arm_vmvnq_x(__a, __p)
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
#define vcmulq_rot90(__a, __b) __arm_vcmulq_rot90(__a, __b)
#define vcmulq_rot270(__a, __b) __arm_vcmulq_rot270(__a, __b)
#define vcmulq_rot180(__a, __b) __arm_vcmulq_rot180(__a, __b)
#define vcmulq(__a, __b) __arm_vcmulq(__a, __b)
#define vcvtaq_m(__inactive, __a, __p) __arm_vcvtaq_m(__inactive, __a, __p)
#define vcvtq_m(__inactive, __a, __p) __arm_vcvtq_m(__inactive, __a, __p)
#define vcvtbq_m(__a, __b, __p) __arm_vcvtbq_m(__a, __b, __p)
#define vcvttq_m(__a, __b, __p) __arm_vcvttq_m(__a, __b, __p)
#define vcmlaq(__a, __b, __c) __arm_vcmlaq(__a, __b, __c)
#define vcmlaq_rot180(__a, __b, __c) __arm_vcmlaq_rot180(__a, __b, __c)
#define vcmlaq_rot270(__a, __b, __c) __arm_vcmlaq_rot270(__a, __b, __c)
#define vcmlaq_rot90(__a, __b, __c) __arm_vcmlaq_rot90(__a, __b, __c)
#define vfmaq(__a, __b, __c) __arm_vfmaq(__a, __b, __c)
#define vfmasq(__a, __b, __c) __arm_vfmasq(__a, __b, __c)
#define vfmsq(__a, __b, __c) __arm_vfmsq(__a, __b, __c)
#define vcvtmq_m(__inactive, __a, __p) __arm_vcvtmq_m(__inactive, __a, __p)
#define vcvtnq_m(__inactive, __a, __p) __arm_vcvtnq_m(__inactive, __a, __p)
#define vcvtpq_m(__inactive, __a, __p) __arm_vcvtpq_m(__inactive, __a, __p)
#define vcvtq_m_n(__inactive, __a, __imm6, __p) __arm_vcvtq_m_n(__inactive, __a, __imm6, __p)
#define vcmlaq_m(__a, __b, __c, __p) __arm_vcmlaq_m(__a, __b, __c, __p)
#define vcmlaq_rot180_m(__a, __b, __c, __p) __arm_vcmlaq_rot180_m(__a, __b, __c, __p)
#define vcmlaq_rot270_m(__a, __b, __c, __p) __arm_vcmlaq_rot270_m(__a, __b, __c, __p)
#define vcmlaq_rot90_m(__a, __b, __c, __p) __arm_vcmlaq_rot90_m(__a, __b, __c, __p)
#define vcmulq_m(__inactive, __a, __b, __p) __arm_vcmulq_m(__inactive, __a, __b, __p)
#define vcmulq_rot180_m(__inactive, __a, __b, __p) __arm_vcmulq_rot180_m(__inactive, __a, __b, __p)
#define vcmulq_rot270_m(__inactive, __a, __b, __p) __arm_vcmulq_rot270_m(__inactive, __a, __b, __p)
#define vcmulq_rot90_m(__inactive, __a, __b, __p) __arm_vcmulq_rot90_m(__inactive, __a, __b, __p)
#define vfmaq_m(__a, __b, __c, __p) __arm_vfmaq_m(__a, __b, __c, __p)
#define vfmasq_m(__a, __b, __c, __p) __arm_vfmasq_m(__a, __b, __c, __p)
#define vfmsq_m(__a, __b, __c, __p) __arm_vfmsq_m(__a, __b, __c, __p)
#define vcmulq_x(__a, __b, __p) __arm_vcmulq_x(__a, __b, __p)
#define vcmulq_rot90_x(__a, __b, __p) __arm_vcmulq_rot90_x(__a, __b, __p)
#define vcmulq_rot180_x(__a, __b, __p) __arm_vcmulq_rot180_x(__a, __b, __p)
#define vcmulq_rot270_x(__a, __b, __p) __arm_vcmulq_rot270_x(__a, __b, __p)
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
#define vmvnq_s8(__a) __arm_vmvnq_s8(__a)
#define vmvnq_s16(__a) __arm_vmvnq_s16(__a)
#define vmvnq_s32(__a) __arm_vmvnq_s32(__a)
#define vmvnq_n_s16( __imm) __arm_vmvnq_n_s16( __imm)
#define vmvnq_n_s32( __imm) __arm_vmvnq_n_s32( __imm)
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
#define vmvnq_u8(__a) __arm_vmvnq_u8(__a)
#define vmvnq_u16(__a) __arm_vmvnq_u16(__a)
#define vmvnq_u32(__a) __arm_vmvnq_u32(__a)
#define vmvnq_n_u16( __imm) __arm_vmvnq_n_u16( __imm)
#define vmvnq_n_u32( __imm) __arm_vmvnq_n_u32( __imm)
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
#define vbrsrq_n_f16(__a, __b) __arm_vbrsrq_n_f16(__a, __b)
#define vbrsrq_n_f32(__a, __b) __arm_vbrsrq_n_f32(__a, __b)
#define vcvtq_n_f16_s16(__a,  __imm6) __arm_vcvtq_n_f16_s16(__a,  __imm6)
#define vcvtq_n_f32_s32(__a,  __imm6) __arm_vcvtq_n_f32_s32(__a,  __imm6)
#define vcvtq_n_f16_u16(__a,  __imm6) __arm_vcvtq_n_f16_u16(__a,  __imm6)
#define vcvtq_n_f32_u32(__a,  __imm6) __arm_vcvtq_n_f32_u32(__a,  __imm6)
#define vcvtq_n_s16_f16(__a,  __imm6) __arm_vcvtq_n_s16_f16(__a,  __imm6)
#define vcvtq_n_s32_f32(__a,  __imm6) __arm_vcvtq_n_s32_f32(__a,  __imm6)
#define vcvtq_n_u16_f16(__a,  __imm6) __arm_vcvtq_n_u16_f16(__a,  __imm6)
#define vcvtq_n_u32_f32(__a,  __imm6) __arm_vcvtq_n_u32_f32(__a,  __imm6)
#define vornq_u8(__a, __b) __arm_vornq_u8(__a, __b)
#define vmulltq_int_u8(__a, __b) __arm_vmulltq_int_u8(__a, __b)
#define vmullbq_int_u8(__a, __b) __arm_vmullbq_int_u8(__a, __b)
#define vmladavq_u8(__a, __b) __arm_vmladavq_u8(__a, __b)
#define vcaddq_rot90_u8(__a, __b) __arm_vcaddq_rot90_u8(__a, __b)
#define vcaddq_rot270_u8(__a, __b) __arm_vcaddq_rot270_u8(__a, __b)
#define vbicq_u8(__a, __b) __arm_vbicq_u8(__a, __b)
#define vbrsrq_n_u8(__a, __b) __arm_vbrsrq_n_u8(__a, __b)
#define vqshluq_n_s8(__a,  __imm) __arm_vqshluq_n_s8(__a,  __imm)
#define vornq_s8(__a, __b) __arm_vornq_s8(__a, __b)
#define vmulltq_int_s8(__a, __b) __arm_vmulltq_int_s8(__a, __b)
#define vmullbq_int_s8(__a, __b) __arm_vmullbq_int_s8(__a, __b)
#define vmlsdavxq_s8(__a, __b) __arm_vmlsdavxq_s8(__a, __b)
#define vmlsdavq_s8(__a, __b) __arm_vmlsdavq_s8(__a, __b)
#define vmladavxq_s8(__a, __b) __arm_vmladavxq_s8(__a, __b)
#define vmladavq_s8(__a, __b) __arm_vmladavq_s8(__a, __b)
#define vhcaddq_rot90_s8(__a, __b) __arm_vhcaddq_rot90_s8(__a, __b)
#define vhcaddq_rot270_s8(__a, __b) __arm_vhcaddq_rot270_s8(__a, __b)
#define vcaddq_rot90_s8(__a, __b) __arm_vcaddq_rot90_s8(__a, __b)
#define vcaddq_rot270_s8(__a, __b) __arm_vcaddq_rot270_s8(__a, __b)
#define vbrsrq_n_s8(__a, __b) __arm_vbrsrq_n_s8(__a, __b)
#define vbicq_s8(__a, __b) __arm_vbicq_s8(__a, __b)
#define vornq_u16(__a, __b) __arm_vornq_u16(__a, __b)
#define vmulltq_int_u16(__a, __b) __arm_vmulltq_int_u16(__a, __b)
#define vmullbq_int_u16(__a, __b) __arm_vmullbq_int_u16(__a, __b)
#define vmladavq_u16(__a, __b) __arm_vmladavq_u16(__a, __b)
#define vcaddq_rot90_u16(__a, __b) __arm_vcaddq_rot90_u16(__a, __b)
#define vcaddq_rot270_u16(__a, __b) __arm_vcaddq_rot270_u16(__a, __b)
#define vbicq_u16(__a, __b) __arm_vbicq_u16(__a, __b)
#define vbrsrq_n_u16(__a, __b) __arm_vbrsrq_n_u16(__a, __b)
#define vqshluq_n_s16(__a,  __imm) __arm_vqshluq_n_s16(__a,  __imm)
#define vornq_s16(__a, __b) __arm_vornq_s16(__a, __b)
#define vmulltq_int_s16(__a, __b) __arm_vmulltq_int_s16(__a, __b)
#define vmullbq_int_s16(__a, __b) __arm_vmullbq_int_s16(__a, __b)
#define vmlsdavxq_s16(__a, __b) __arm_vmlsdavxq_s16(__a, __b)
#define vmlsdavq_s16(__a, __b) __arm_vmlsdavq_s16(__a, __b)
#define vmladavxq_s16(__a, __b) __arm_vmladavxq_s16(__a, __b)
#define vmladavq_s16(__a, __b) __arm_vmladavq_s16(__a, __b)
#define vhcaddq_rot90_s16(__a, __b) __arm_vhcaddq_rot90_s16(__a, __b)
#define vhcaddq_rot270_s16(__a, __b) __arm_vhcaddq_rot270_s16(__a, __b)
#define vcaddq_rot90_s16(__a, __b) __arm_vcaddq_rot90_s16(__a, __b)
#define vcaddq_rot270_s16(__a, __b) __arm_vcaddq_rot270_s16(__a, __b)
#define vbrsrq_n_s16(__a, __b) __arm_vbrsrq_n_s16(__a, __b)
#define vbicq_s16(__a, __b) __arm_vbicq_s16(__a, __b)
#define vornq_u32(__a, __b) __arm_vornq_u32(__a, __b)
#define vmulltq_int_u32(__a, __b) __arm_vmulltq_int_u32(__a, __b)
#define vmullbq_int_u32(__a, __b) __arm_vmullbq_int_u32(__a, __b)
#define vmladavq_u32(__a, __b) __arm_vmladavq_u32(__a, __b)
#define vcaddq_rot90_u32(__a, __b) __arm_vcaddq_rot90_u32(__a, __b)
#define vcaddq_rot270_u32(__a, __b) __arm_vcaddq_rot270_u32(__a, __b)
#define vbicq_u32(__a, __b) __arm_vbicq_u32(__a, __b)
#define vbrsrq_n_u32(__a, __b) __arm_vbrsrq_n_u32(__a, __b)
#define vqshluq_n_s32(__a,  __imm) __arm_vqshluq_n_s32(__a,  __imm)
#define vornq_s32(__a, __b) __arm_vornq_s32(__a, __b)
#define vmulltq_int_s32(__a, __b) __arm_vmulltq_int_s32(__a, __b)
#define vmullbq_int_s32(__a, __b) __arm_vmullbq_int_s32(__a, __b)
#define vmlsdavxq_s32(__a, __b) __arm_vmlsdavxq_s32(__a, __b)
#define vmlsdavq_s32(__a, __b) __arm_vmlsdavq_s32(__a, __b)
#define vmladavxq_s32(__a, __b) __arm_vmladavxq_s32(__a, __b)
#define vmladavq_s32(__a, __b) __arm_vmladavq_s32(__a, __b)
#define vhcaddq_rot90_s32(__a, __b) __arm_vhcaddq_rot90_s32(__a, __b)
#define vhcaddq_rot270_s32(__a, __b) __arm_vhcaddq_rot270_s32(__a, __b)
#define vcaddq_rot90_s32(__a, __b) __arm_vcaddq_rot90_s32(__a, __b)
#define vcaddq_rot270_s32(__a, __b) __arm_vcaddq_rot270_s32(__a, __b)
#define vbrsrq_n_s32(__a, __b) __arm_vbrsrq_n_s32(__a, __b)
#define vbicq_s32(__a, __b) __arm_vbicq_s32(__a, __b)
#define vmulltq_poly_p8(__a, __b) __arm_vmulltq_poly_p8(__a, __b)
#define vmullbq_poly_p8(__a, __b) __arm_vmullbq_poly_p8(__a, __b)
#define vmlaldavq_u16(__a, __b) __arm_vmlaldavq_u16(__a, __b)
#define vbicq_n_u16(__a,  __imm) __arm_vbicq_n_u16(__a,  __imm)
#define vqdmulltq_s16(__a, __b) __arm_vqdmulltq_s16(__a, __b)
#define vqdmulltq_n_s16(__a, __b) __arm_vqdmulltq_n_s16(__a, __b)
#define vqdmullbq_s16(__a, __b) __arm_vqdmullbq_s16(__a, __b)
#define vqdmullbq_n_s16(__a, __b) __arm_vqdmullbq_n_s16(__a, __b)
#define vornq_f16(__a, __b) __arm_vornq_f16(__a, __b)
#define vmlsldavxq_s16(__a, __b) __arm_vmlsldavxq_s16(__a, __b)
#define vmlsldavq_s16(__a, __b) __arm_vmlsldavq_s16(__a, __b)
#define vmlaldavxq_s16(__a, __b) __arm_vmlaldavxq_s16(__a, __b)
#define vmlaldavq_s16(__a, __b) __arm_vmlaldavq_s16(__a, __b)
#define vcmulq_rot90_f16(__a, __b) __arm_vcmulq_rot90_f16(__a, __b)
#define vcmulq_rot270_f16(__a, __b) __arm_vcmulq_rot270_f16(__a, __b)
#define vcmulq_rot180_f16(__a, __b) __arm_vcmulq_rot180_f16(__a, __b)
#define vcmulq_f16(__a, __b) __arm_vcmulq_f16(__a, __b)
#define vcaddq_rot90_f16(__a, __b) __arm_vcaddq_rot90_f16(__a, __b)
#define vcaddq_rot270_f16(__a, __b) __arm_vcaddq_rot270_f16(__a, __b)
#define vbicq_f16(__a, __b) __arm_vbicq_f16(__a, __b)
#define vbicq_n_s16(__a,  __imm) __arm_vbicq_n_s16(__a,  __imm)
#define vmulltq_poly_p16(__a, __b) __arm_vmulltq_poly_p16(__a, __b)
#define vmullbq_poly_p16(__a, __b) __arm_vmullbq_poly_p16(__a, __b)
#define vmlaldavq_u32(__a, __b) __arm_vmlaldavq_u32(__a, __b)
#define vbicq_n_u32(__a,  __imm) __arm_vbicq_n_u32(__a,  __imm)
#define vqdmulltq_s32(__a, __b) __arm_vqdmulltq_s32(__a, __b)
#define vqdmulltq_n_s32(__a, __b) __arm_vqdmulltq_n_s32(__a, __b)
#define vqdmullbq_s32(__a, __b) __arm_vqdmullbq_s32(__a, __b)
#define vqdmullbq_n_s32(__a, __b) __arm_vqdmullbq_n_s32(__a, __b)
#define vornq_f32(__a, __b) __arm_vornq_f32(__a, __b)
#define vmlsldavxq_s32(__a, __b) __arm_vmlsldavxq_s32(__a, __b)
#define vmlsldavq_s32(__a, __b) __arm_vmlsldavq_s32(__a, __b)
#define vmlaldavxq_s32(__a, __b) __arm_vmlaldavxq_s32(__a, __b)
#define vmlaldavq_s32(__a, __b) __arm_vmlaldavq_s32(__a, __b)
#define vcmulq_rot90_f32(__a, __b) __arm_vcmulq_rot90_f32(__a, __b)
#define vcmulq_rot270_f32(__a, __b) __arm_vcmulq_rot270_f32(__a, __b)
#define vcmulq_rot180_f32(__a, __b) __arm_vcmulq_rot180_f32(__a, __b)
#define vcmulq_f32(__a, __b) __arm_vcmulq_f32(__a, __b)
#define vcaddq_rot90_f32(__a, __b) __arm_vcaddq_rot90_f32(__a, __b)
#define vcaddq_rot270_f32(__a, __b) __arm_vcaddq_rot270_f32(__a, __b)
#define vbicq_f32(__a, __b) __arm_vbicq_f32(__a, __b)
#define vbicq_n_s32(__a,  __imm) __arm_vbicq_n_s32(__a,  __imm)
#define vrmlaldavhq_u32(__a, __b) __arm_vrmlaldavhq_u32(__a, __b)
#define vctp8q_m(__a, __p) __arm_vctp8q_m(__a, __p)
#define vctp64q_m(__a, __p) __arm_vctp64q_m(__a, __p)
#define vctp32q_m(__a, __p) __arm_vctp32q_m(__a, __p)
#define vctp16q_m(__a, __p) __arm_vctp16q_m(__a, __p)
#define vrmlsldavhxq_s32(__a, __b) __arm_vrmlsldavhxq_s32(__a, __b)
#define vrmlsldavhq_s32(__a, __b) __arm_vrmlsldavhq_s32(__a, __b)
#define vrmlaldavhxq_s32(__a, __b) __arm_vrmlaldavhxq_s32(__a, __b)
#define vrmlaldavhq_s32(__a, __b) __arm_vrmlaldavhq_s32(__a, __b)
#define vcvttq_f16_f32(__a, __b) __arm_vcvttq_f16_f32(__a, __b)
#define vcvtbq_f16_f32(__a, __b) __arm_vcvtbq_f16_f32(__a, __b)
#define vabavq_s8(__a, __b, __c) __arm_vabavq_s8(__a, __b, __c)
#define vabavq_s16(__a, __b, __c) __arm_vabavq_s16(__a, __b, __c)
#define vabavq_s32(__a, __b, __c) __arm_vabavq_s32(__a, __b, __c)
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
#define vrmlaldavhaq_s32(__a, __b, __c) __arm_vrmlaldavhaq_s32(__a, __b, __c)
#define vrmlaldavhaq_u32(__a, __b, __c) __arm_vrmlaldavhaq_u32(__a, __b, __c)
#define vshlcq_s8(__a,  __b,  __imm) __arm_vshlcq_s8(__a,  __b,  __imm)
#define vshlcq_u8(__a,  __b,  __imm) __arm_vshlcq_u8(__a,  __b,  __imm)
#define vshlcq_s16(__a,  __b,  __imm) __arm_vshlcq_s16(__a,  __b,  __imm)
#define vshlcq_u16(__a,  __b,  __imm) __arm_vshlcq_u16(__a,  __b,  __imm)
#define vshlcq_s32(__a,  __b,  __imm) __arm_vshlcq_s32(__a,  __b,  __imm)
#define vshlcq_u32(__a,  __b,  __imm) __arm_vshlcq_u32(__a,  __b,  __imm)
#define vabavq_u8(__a, __b, __c) __arm_vabavq_u8(__a, __b, __c)
#define vabavq_u16(__a, __b, __c) __arm_vabavq_u16(__a, __b, __c)
#define vabavq_u32(__a, __b, __c) __arm_vabavq_u32(__a, __b, __c)
#define vpselq_u8(__a, __b, __p) __arm_vpselq_u8(__a, __b, __p)
#define vpselq_s8(__a, __b, __p) __arm_vpselq_s8(__a, __b, __p)
#define vmvnq_m_u8(__inactive, __a, __p) __arm_vmvnq_m_u8(__inactive, __a, __p)
#define vmlasq_n_u8(__a, __b, __c) __arm_vmlasq_n_u8(__a, __b, __c)
#define vmlaq_n_u8(__a, __b, __c) __arm_vmlaq_n_u8(__a, __b, __c)
#define vmladavq_p_u8(__a, __b, __p) __arm_vmladavq_p_u8(__a, __b, __p)
#define vmladavaq_u8(__a, __b, __c) __arm_vmladavaq_u8(__a, __b, __c)
#define vsriq_n_u8(__a, __b,  __imm) __arm_vsriq_n_u8(__a, __b,  __imm)
#define vsliq_n_u8(__a, __b,  __imm) __arm_vsliq_n_u8(__a, __b,  __imm)
#define vmvnq_m_s8(__inactive, __a, __p) __arm_vmvnq_m_s8(__inactive, __a, __p)
#define vmlsdavxq_p_s8(__a, __b, __p) __arm_vmlsdavxq_p_s8(__a, __b, __p)
#define vmlsdavq_p_s8(__a, __b, __p) __arm_vmlsdavq_p_s8(__a, __b, __p)
#define vmladavxq_p_s8(__a, __b, __p) __arm_vmladavxq_p_s8(__a, __b, __p)
#define vmladavq_p_s8(__a, __b, __p) __arm_vmladavq_p_s8(__a, __b, __p)
#define vqrdmlsdhxq_s8(__inactive, __a, __b) __arm_vqrdmlsdhxq_s8(__inactive, __a, __b)
#define vqrdmlsdhq_s8(__inactive, __a, __b) __arm_vqrdmlsdhq_s8(__inactive, __a, __b)
#define vqrdmlashq_n_s8(__a, __b, __c) __arm_vqrdmlashq_n_s8(__a, __b, __c)
#define vqrdmlahq_n_s8(__a, __b, __c) __arm_vqrdmlahq_n_s8(__a, __b, __c)
#define vqrdmladhxq_s8(__inactive, __a, __b) __arm_vqrdmladhxq_s8(__inactive, __a, __b)
#define vqrdmladhq_s8(__inactive, __a, __b) __arm_vqrdmladhq_s8(__inactive, __a, __b)
#define vqdmlsdhxq_s8(__inactive, __a, __b) __arm_vqdmlsdhxq_s8(__inactive, __a, __b)
#define vqdmlsdhq_s8(__inactive, __a, __b) __arm_vqdmlsdhq_s8(__inactive, __a, __b)
#define vqdmlahq_n_s8(__a, __b, __c) __arm_vqdmlahq_n_s8(__a, __b, __c)
#define vqdmlashq_n_s8(__a, __b, __c) __arm_vqdmlashq_n_s8(__a, __b, __c)
#define vqdmladhxq_s8(__inactive, __a, __b) __arm_vqdmladhxq_s8(__inactive, __a, __b)
#define vqdmladhq_s8(__inactive, __a, __b) __arm_vqdmladhq_s8(__inactive, __a, __b)
#define vmlsdavaxq_s8(__a, __b, __c) __arm_vmlsdavaxq_s8(__a, __b, __c)
#define vmlsdavaq_s8(__a, __b, __c) __arm_vmlsdavaq_s8(__a, __b, __c)
#define vmlasq_n_s8(__a, __b, __c) __arm_vmlasq_n_s8(__a, __b, __c)
#define vmlaq_n_s8(__a, __b, __c) __arm_vmlaq_n_s8(__a, __b, __c)
#define vmladavaxq_s8(__a, __b, __c) __arm_vmladavaxq_s8(__a, __b, __c)
#define vmladavaq_s8(__a, __b, __c) __arm_vmladavaq_s8(__a, __b, __c)
#define vsriq_n_s8(__a, __b,  __imm) __arm_vsriq_n_s8(__a, __b,  __imm)
#define vsliq_n_s8(__a, __b,  __imm) __arm_vsliq_n_s8(__a, __b,  __imm)
#define vpselq_u16(__a, __b, __p) __arm_vpselq_u16(__a, __b, __p)
#define vpselq_s16(__a, __b, __p) __arm_vpselq_s16(__a, __b, __p)
#define vmvnq_m_u16(__inactive, __a, __p) __arm_vmvnq_m_u16(__inactive, __a, __p)
#define vmlasq_n_u16(__a, __b, __c) __arm_vmlasq_n_u16(__a, __b, __c)
#define vmlaq_n_u16(__a, __b, __c) __arm_vmlaq_n_u16(__a, __b, __c)
#define vmladavq_p_u16(__a, __b, __p) __arm_vmladavq_p_u16(__a, __b, __p)
#define vmladavaq_u16(__a, __b, __c) __arm_vmladavaq_u16(__a, __b, __c)
#define vsriq_n_u16(__a, __b,  __imm) __arm_vsriq_n_u16(__a, __b,  __imm)
#define vsliq_n_u16(__a, __b,  __imm) __arm_vsliq_n_u16(__a, __b,  __imm)
#define vmvnq_m_s16(__inactive, __a, __p) __arm_vmvnq_m_s16(__inactive, __a, __p)
#define vmlsdavxq_p_s16(__a, __b, __p) __arm_vmlsdavxq_p_s16(__a, __b, __p)
#define vmlsdavq_p_s16(__a, __b, __p) __arm_vmlsdavq_p_s16(__a, __b, __p)
#define vmladavxq_p_s16(__a, __b, __p) __arm_vmladavxq_p_s16(__a, __b, __p)
#define vmladavq_p_s16(__a, __b, __p) __arm_vmladavq_p_s16(__a, __b, __p)
#define vqrdmlsdhxq_s16(__inactive, __a, __b) __arm_vqrdmlsdhxq_s16(__inactive, __a, __b)
#define vqrdmlsdhq_s16(__inactive, __a, __b) __arm_vqrdmlsdhq_s16(__inactive, __a, __b)
#define vqrdmlashq_n_s16(__a, __b, __c) __arm_vqrdmlashq_n_s16(__a, __b, __c)
#define vqrdmlahq_n_s16(__a, __b, __c) __arm_vqrdmlahq_n_s16(__a, __b, __c)
#define vqrdmladhxq_s16(__inactive, __a, __b) __arm_vqrdmladhxq_s16(__inactive, __a, __b)
#define vqrdmladhq_s16(__inactive, __a, __b) __arm_vqrdmladhq_s16(__inactive, __a, __b)
#define vqdmlsdhxq_s16(__inactive, __a, __b) __arm_vqdmlsdhxq_s16(__inactive, __a, __b)
#define vqdmlsdhq_s16(__inactive, __a, __b) __arm_vqdmlsdhq_s16(__inactive, __a, __b)
#define vqdmlashq_n_s16(__a, __b, __c) __arm_vqdmlashq_n_s16(__a, __b, __c)
#define vqdmlahq_n_s16(__a, __b, __c) __arm_vqdmlahq_n_s16(__a, __b, __c)
#define vqdmladhxq_s16(__inactive, __a, __b) __arm_vqdmladhxq_s16(__inactive, __a, __b)
#define vqdmladhq_s16(__inactive, __a, __b) __arm_vqdmladhq_s16(__inactive, __a, __b)
#define vmlsdavaxq_s16(__a, __b, __c) __arm_vmlsdavaxq_s16(__a, __b, __c)
#define vmlsdavaq_s16(__a, __b, __c) __arm_vmlsdavaq_s16(__a, __b, __c)
#define vmlasq_n_s16(__a, __b, __c) __arm_vmlasq_n_s16(__a, __b, __c)
#define vmlaq_n_s16(__a, __b, __c) __arm_vmlaq_n_s16(__a, __b, __c)
#define vmladavaxq_s16(__a, __b, __c) __arm_vmladavaxq_s16(__a, __b, __c)
#define vmladavaq_s16(__a, __b, __c) __arm_vmladavaq_s16(__a, __b, __c)
#define vsriq_n_s16(__a, __b,  __imm) __arm_vsriq_n_s16(__a, __b,  __imm)
#define vsliq_n_s16(__a, __b,  __imm) __arm_vsliq_n_s16(__a, __b,  __imm)
#define vpselq_u32(__a, __b, __p) __arm_vpselq_u32(__a, __b, __p)
#define vpselq_s32(__a, __b, __p) __arm_vpselq_s32(__a, __b, __p)
#define vmvnq_m_u32(__inactive, __a, __p) __arm_vmvnq_m_u32(__inactive, __a, __p)
#define vmlasq_n_u32(__a, __b, __c) __arm_vmlasq_n_u32(__a, __b, __c)
#define vmlaq_n_u32(__a, __b, __c) __arm_vmlaq_n_u32(__a, __b, __c)
#define vmladavq_p_u32(__a, __b, __p) __arm_vmladavq_p_u32(__a, __b, __p)
#define vmladavaq_u32(__a, __b, __c) __arm_vmladavaq_u32(__a, __b, __c)
#define vsriq_n_u32(__a, __b,  __imm) __arm_vsriq_n_u32(__a, __b,  __imm)
#define vsliq_n_u32(__a, __b,  __imm) __arm_vsliq_n_u32(__a, __b,  __imm)
#define vmvnq_m_s32(__inactive, __a, __p) __arm_vmvnq_m_s32(__inactive, __a, __p)
#define vmlsdavxq_p_s32(__a, __b, __p) __arm_vmlsdavxq_p_s32(__a, __b, __p)
#define vmlsdavq_p_s32(__a, __b, __p) __arm_vmlsdavq_p_s32(__a, __b, __p)
#define vmladavxq_p_s32(__a, __b, __p) __arm_vmladavxq_p_s32(__a, __b, __p)
#define vmladavq_p_s32(__a, __b, __p) __arm_vmladavq_p_s32(__a, __b, __p)
#define vqrdmlsdhxq_s32(__inactive, __a, __b) __arm_vqrdmlsdhxq_s32(__inactive, __a, __b)
#define vqrdmlsdhq_s32(__inactive, __a, __b) __arm_vqrdmlsdhq_s32(__inactive, __a, __b)
#define vqrdmlashq_n_s32(__a, __b, __c) __arm_vqrdmlashq_n_s32(__a, __b, __c)
#define vqrdmlahq_n_s32(__a, __b, __c) __arm_vqrdmlahq_n_s32(__a, __b, __c)
#define vqrdmladhxq_s32(__inactive, __a, __b) __arm_vqrdmladhxq_s32(__inactive, __a, __b)
#define vqrdmladhq_s32(__inactive, __a, __b) __arm_vqrdmladhq_s32(__inactive, __a, __b)
#define vqdmlsdhxq_s32(__inactive, __a, __b) __arm_vqdmlsdhxq_s32(__inactive, __a, __b)
#define vqdmlsdhq_s32(__inactive, __a, __b) __arm_vqdmlsdhq_s32(__inactive, __a, __b)
#define vqdmlashq_n_s32(__a, __b, __c) __arm_vqdmlashq_n_s32(__a, __b, __c)
#define vqdmlahq_n_s32(__a, __b, __c) __arm_vqdmlahq_n_s32(__a, __b, __c)
#define vqdmladhxq_s32(__inactive, __a, __b) __arm_vqdmladhxq_s32(__inactive, __a, __b)
#define vqdmladhq_s32(__inactive, __a, __b) __arm_vqdmladhq_s32(__inactive, __a, __b)
#define vmlsdavaxq_s32(__a, __b, __c) __arm_vmlsdavaxq_s32(__a, __b, __c)
#define vmlsdavaq_s32(__a, __b, __c) __arm_vmlsdavaq_s32(__a, __b, __c)
#define vmlasq_n_s32(__a, __b, __c) __arm_vmlasq_n_s32(__a, __b, __c)
#define vmlaq_n_s32(__a, __b, __c) __arm_vmlaq_n_s32(__a, __b, __c)
#define vmladavaxq_s32(__a, __b, __c) __arm_vmladavaxq_s32(__a, __b, __c)
#define vmladavaq_s32(__a, __b, __c) __arm_vmladavaq_s32(__a, __b, __c)
#define vsriq_n_s32(__a, __b,  __imm) __arm_vsriq_n_s32(__a, __b,  __imm)
#define vsliq_n_s32(__a, __b,  __imm) __arm_vsliq_n_s32(__a, __b,  __imm)
#define vpselq_u64(__a, __b, __p) __arm_vpselq_u64(__a, __b, __p)
#define vpselq_s64(__a, __b, __p) __arm_vpselq_s64(__a, __b, __p)
#define vrmlaldavhaxq_s32(__a, __b, __c) __arm_vrmlaldavhaxq_s32(__a, __b, __c)
#define vrmlsldavhaq_s32(__a, __b, __c) __arm_vrmlsldavhaq_s32(__a, __b, __c)
#define vrmlsldavhaxq_s32(__a, __b, __c) __arm_vrmlsldavhaxq_s32(__a, __b, __c)
#define vcvtbq_m_f16_f32(__a, __b, __p) __arm_vcvtbq_m_f16_f32(__a, __b, __p)
#define vcvtbq_m_f32_f16(__inactive, __a, __p) __arm_vcvtbq_m_f32_f16(__inactive, __a, __p)
#define vcvttq_m_f16_f32(__a, __b, __p) __arm_vcvttq_m_f16_f32(__a, __b, __p)
#define vcvttq_m_f32_f16(__inactive, __a, __p) __arm_vcvttq_m_f32_f16(__inactive, __a, __p)
#define vrmlaldavhq_p_s32(__a, __b, __p) __arm_vrmlaldavhq_p_s32(__a, __b, __p)
#define vrmlaldavhxq_p_s32(__a, __b, __p) __arm_vrmlaldavhxq_p_s32(__a, __b, __p)
#define vrmlsldavhq_p_s32(__a, __b, __p) __arm_vrmlsldavhq_p_s32(__a, __b, __p)
#define vrmlsldavhxq_p_s32(__a, __b, __p) __arm_vrmlsldavhxq_p_s32(__a, __b, __p)
#define vrmlaldavhq_p_u32(__a, __b, __p) __arm_vrmlaldavhq_p_u32(__a, __b, __p)
#define vmvnq_m_n_s16(__inactive,  __imm, __p) __arm_vmvnq_m_n_s16(__inactive,  __imm, __p)
#define vcmlaq_f16(__a, __b, __c) __arm_vcmlaq_f16(__a, __b, __c)
#define vcmlaq_rot180_f16(__a, __b, __c) __arm_vcmlaq_rot180_f16(__a, __b, __c)
#define vcmlaq_rot270_f16(__a, __b, __c) __arm_vcmlaq_rot270_f16(__a, __b, __c)
#define vcmlaq_rot90_f16(__a, __b, __c) __arm_vcmlaq_rot90_f16(__a, __b, __c)
#define vfmaq_f16(__a, __b, __c) __arm_vfmaq_f16(__a, __b, __c)
#define vfmaq_n_f16(__a, __b, __c) __arm_vfmaq_n_f16(__a, __b, __c)
#define vfmasq_n_f16(__a, __b, __c) __arm_vfmasq_n_f16(__a, __b, __c)
#define vfmsq_f16(__a, __b, __c) __arm_vfmsq_f16(__a, __b, __c)
#define vmlaldavaq_s16(__a, __b, __c) __arm_vmlaldavaq_s16(__a, __b, __c)
#define vmlaldavaxq_s16(__a, __b, __c) __arm_vmlaldavaxq_s16(__a, __b, __c)
#define vmlsldavaq_s16(__a, __b, __c) __arm_vmlsldavaq_s16(__a, __b, __c)
#define vmlsldavaxq_s16(__a, __b, __c) __arm_vmlsldavaxq_s16(__a, __b, __c)
#define vcvtmq_m_s16_f16(__inactive, __a, __p) __arm_vcvtmq_m_s16_f16(__inactive, __a, __p)
#define vcvtnq_m_s16_f16(__inactive, __a, __p) __arm_vcvtnq_m_s16_f16(__inactive, __a, __p)
#define vcvtpq_m_s16_f16(__inactive, __a, __p) __arm_vcvtpq_m_s16_f16(__inactive, __a, __p)
#define vcvtq_m_s16_f16(__inactive, __a, __p) __arm_vcvtq_m_s16_f16(__inactive, __a, __p)
#define vmlaldavq_p_s16(__a, __b, __p) __arm_vmlaldavq_p_s16(__a, __b, __p)
#define vmlaldavxq_p_s16(__a, __b, __p) __arm_vmlaldavxq_p_s16(__a, __b, __p)
#define vmlsldavq_p_s16(__a, __b, __p) __arm_vmlsldavq_p_s16(__a, __b, __p)
#define vmlsldavxq_p_s16(__a, __b, __p) __arm_vmlsldavxq_p_s16(__a, __b, __p)
#define vpselq_f16(__a, __b, __p) __arm_vpselq_f16(__a, __b, __p)
#define vmvnq_m_n_u16(__inactive,  __imm, __p) __arm_vmvnq_m_n_u16(__inactive,  __imm, __p)
#define vcvtmq_m_u16_f16(__inactive, __a, __p) __arm_vcvtmq_m_u16_f16(__inactive, __a, __p)
#define vcvtnq_m_u16_f16(__inactive, __a, __p) __arm_vcvtnq_m_u16_f16(__inactive, __a, __p)
#define vcvtpq_m_u16_f16(__inactive, __a, __p) __arm_vcvtpq_m_u16_f16(__inactive, __a, __p)
#define vcvtq_m_u16_f16(__inactive, __a, __p) __arm_vcvtq_m_u16_f16(__inactive, __a, __p)
#define vmlaldavaq_u16(__a, __b, __c) __arm_vmlaldavaq_u16(__a, __b, __c)
#define vmlaldavq_p_u16(__a, __b, __p) __arm_vmlaldavq_p_u16(__a, __b, __p)
#define vmvnq_m_n_s32(__inactive,  __imm, __p) __arm_vmvnq_m_n_s32(__inactive,  __imm, __p)
#define vcmlaq_f32(__a, __b, __c) __arm_vcmlaq_f32(__a, __b, __c)
#define vcmlaq_rot180_f32(__a, __b, __c) __arm_vcmlaq_rot180_f32(__a, __b, __c)
#define vcmlaq_rot270_f32(__a, __b, __c) __arm_vcmlaq_rot270_f32(__a, __b, __c)
#define vcmlaq_rot90_f32(__a, __b, __c) __arm_vcmlaq_rot90_f32(__a, __b, __c)
#define vfmaq_f32(__a, __b, __c) __arm_vfmaq_f32(__a, __b, __c)
#define vfmaq_n_f32(__a, __b, __c) __arm_vfmaq_n_f32(__a, __b, __c)
#define vfmasq_n_f32(__a, __b, __c) __arm_vfmasq_n_f32(__a, __b, __c)
#define vfmsq_f32(__a, __b, __c) __arm_vfmsq_f32(__a, __b, __c)
#define vmlaldavaq_s32(__a, __b, __c) __arm_vmlaldavaq_s32(__a, __b, __c)
#define vmlaldavaxq_s32(__a, __b, __c) __arm_vmlaldavaxq_s32(__a, __b, __c)
#define vmlsldavaq_s32(__a, __b, __c) __arm_vmlsldavaq_s32(__a, __b, __c)
#define vmlsldavaxq_s32(__a, __b, __c) __arm_vmlsldavaxq_s32(__a, __b, __c)
#define vcvtmq_m_s32_f32(__inactive, __a, __p) __arm_vcvtmq_m_s32_f32(__inactive, __a, __p)
#define vcvtnq_m_s32_f32(__inactive, __a, __p) __arm_vcvtnq_m_s32_f32(__inactive, __a, __p)
#define vcvtpq_m_s32_f32(__inactive, __a, __p) __arm_vcvtpq_m_s32_f32(__inactive, __a, __p)
#define vcvtq_m_s32_f32(__inactive, __a, __p) __arm_vcvtq_m_s32_f32(__inactive, __a, __p)
#define vmlaldavq_p_s32(__a, __b, __p) __arm_vmlaldavq_p_s32(__a, __b, __p)
#define vmlaldavxq_p_s32(__a, __b, __p) __arm_vmlaldavxq_p_s32(__a, __b, __p)
#define vmlsldavq_p_s32(__a, __b, __p) __arm_vmlsldavq_p_s32(__a, __b, __p)
#define vmlsldavxq_p_s32(__a, __b, __p) __arm_vmlsldavxq_p_s32(__a, __b, __p)
#define vpselq_f32(__a, __b, __p) __arm_vpselq_f32(__a, __b, __p)
#define vmvnq_m_n_u32(__inactive,  __imm, __p) __arm_vmvnq_m_n_u32(__inactive,  __imm, __p)
#define vcvtmq_m_u32_f32(__inactive, __a, __p) __arm_vcvtmq_m_u32_f32(__inactive, __a, __p)
#define vcvtnq_m_u32_f32(__inactive, __a, __p) __arm_vcvtnq_m_u32_f32(__inactive, __a, __p)
#define vcvtpq_m_u32_f32(__inactive, __a, __p) __arm_vcvtpq_m_u32_f32(__inactive, __a, __p)
#define vcvtq_m_u32_f32(__inactive, __a, __p) __arm_vcvtq_m_u32_f32(__inactive, __a, __p)
#define vmlaldavaq_u32(__a, __b, __c) __arm_vmlaldavaq_u32(__a, __b, __c)
#define vmlaldavq_p_u32(__a, __b, __p) __arm_vmlaldavq_p_u32(__a, __b, __p)
#define vsriq_m_n_s8(__a, __b,  __imm, __p) __arm_vsriq_m_n_s8(__a, __b,  __imm, __p)
#define vcvtq_m_n_f16_u16(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_f16_u16(__inactive, __a,  __imm6, __p)
#define vqshluq_m_n_s8(__inactive, __a,  __imm, __p) __arm_vqshluq_m_n_s8(__inactive, __a,  __imm, __p)
#define vabavq_p_s8(__a, __b, __c, __p) __arm_vabavq_p_s8(__a, __b, __c, __p)
#define vsriq_m_n_u8(__a, __b,  __imm, __p) __arm_vsriq_m_n_u8(__a, __b,  __imm, __p)
#define vabavq_p_u8(__a, __b, __c, __p) __arm_vabavq_p_u8(__a, __b, __c, __p)
#define vcvtq_m_n_f16_s16(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_f16_s16(__inactive, __a,  __imm6, __p)
#define vsriq_m_n_s16(__a, __b,  __imm, __p) __arm_vsriq_m_n_s16(__a, __b,  __imm, __p)
#define vcvtq_m_n_f32_u32(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_f32_u32(__inactive, __a,  __imm6, __p)
#define vqshluq_m_n_s16(__inactive, __a,  __imm, __p) __arm_vqshluq_m_n_s16(__inactive, __a,  __imm, __p)
#define vabavq_p_s16(__a, __b, __c, __p) __arm_vabavq_p_s16(__a, __b, __c, __p)
#define vsriq_m_n_u16(__a, __b,  __imm, __p) __arm_vsriq_m_n_u16(__a, __b,  __imm, __p)
#define vabavq_p_u16(__a, __b, __c, __p) __arm_vabavq_p_u16(__a, __b, __c, __p)
#define vcvtq_m_n_f32_s32(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_f32_s32(__inactive, __a,  __imm6, __p)
#define vsriq_m_n_s32(__a, __b,  __imm, __p) __arm_vsriq_m_n_s32(__a, __b,  __imm, __p)
#define vqshluq_m_n_s32(__inactive, __a,  __imm, __p) __arm_vqshluq_m_n_s32(__inactive, __a,  __imm, __p)
#define vabavq_p_s32(__a, __b, __c, __p) __arm_vabavq_p_s32(__a, __b, __c, __p)
#define vsriq_m_n_u32(__a, __b,  __imm, __p) __arm_vsriq_m_n_u32(__a, __b,  __imm, __p)
#define vabavq_p_u32(__a, __b, __c, __p) __arm_vabavq_p_u32(__a, __b, __c, __p)
#define vbicq_m_s8(__inactive, __a, __b, __p) __arm_vbicq_m_s8(__inactive, __a, __b, __p)
#define vbicq_m_s32(__inactive, __a, __b, __p) __arm_vbicq_m_s32(__inactive, __a, __b, __p)
#define vbicq_m_s16(__inactive, __a, __b, __p) __arm_vbicq_m_s16(__inactive, __a, __b, __p)
#define vbicq_m_u8(__inactive, __a, __b, __p) __arm_vbicq_m_u8(__inactive, __a, __b, __p)
#define vbicq_m_u32(__inactive, __a, __b, __p) __arm_vbicq_m_u32(__inactive, __a, __b, __p)
#define vbicq_m_u16(__inactive, __a, __b, __p) __arm_vbicq_m_u16(__inactive, __a, __b, __p)
#define vbrsrq_m_n_s8(__inactive, __a, __b, __p) __arm_vbrsrq_m_n_s8(__inactive, __a, __b, __p)
#define vbrsrq_m_n_s32(__inactive, __a, __b, __p) __arm_vbrsrq_m_n_s32(__inactive, __a, __b, __p)
#define vbrsrq_m_n_s16(__inactive, __a, __b, __p) __arm_vbrsrq_m_n_s16(__inactive, __a, __b, __p)
#define vbrsrq_m_n_u8(__inactive, __a, __b, __p) __arm_vbrsrq_m_n_u8(__inactive, __a, __b, __p)
#define vbrsrq_m_n_u32(__inactive, __a, __b, __p) __arm_vbrsrq_m_n_u32(__inactive, __a, __b, __p)
#define vbrsrq_m_n_u16(__inactive, __a, __b, __p) __arm_vbrsrq_m_n_u16(__inactive, __a, __b, __p)
#define vcaddq_rot270_m_s8(__inactive, __a, __b, __p) __arm_vcaddq_rot270_m_s8(__inactive, __a, __b, __p)
#define vcaddq_rot270_m_s32(__inactive, __a, __b, __p) __arm_vcaddq_rot270_m_s32(__inactive, __a, __b, __p)
#define vcaddq_rot270_m_s16(__inactive, __a, __b, __p) __arm_vcaddq_rot270_m_s16(__inactive, __a, __b, __p)
#define vcaddq_rot270_m_u8(__inactive, __a, __b, __p) __arm_vcaddq_rot270_m_u8(__inactive, __a, __b, __p)
#define vcaddq_rot270_m_u32(__inactive, __a, __b, __p) __arm_vcaddq_rot270_m_u32(__inactive, __a, __b, __p)
#define vcaddq_rot270_m_u16(__inactive, __a, __b, __p) __arm_vcaddq_rot270_m_u16(__inactive, __a, __b, __p)
#define vcaddq_rot90_m_s8(__inactive, __a, __b, __p) __arm_vcaddq_rot90_m_s8(__inactive, __a, __b, __p)
#define vcaddq_rot90_m_s32(__inactive, __a, __b, __p) __arm_vcaddq_rot90_m_s32(__inactive, __a, __b, __p)
#define vcaddq_rot90_m_s16(__inactive, __a, __b, __p) __arm_vcaddq_rot90_m_s16(__inactive, __a, __b, __p)
#define vcaddq_rot90_m_u8(__inactive, __a, __b, __p) __arm_vcaddq_rot90_m_u8(__inactive, __a, __b, __p)
#define vcaddq_rot90_m_u32(__inactive, __a, __b, __p) __arm_vcaddq_rot90_m_u32(__inactive, __a, __b, __p)
#define vcaddq_rot90_m_u16(__inactive, __a, __b, __p) __arm_vcaddq_rot90_m_u16(__inactive, __a, __b, __p)
#define vhcaddq_rot270_m_s8(__inactive, __a, __b, __p) __arm_vhcaddq_rot270_m_s8(__inactive, __a, __b, __p)
#define vhcaddq_rot270_m_s32(__inactive, __a, __b, __p) __arm_vhcaddq_rot270_m_s32(__inactive, __a, __b, __p)
#define vhcaddq_rot270_m_s16(__inactive, __a, __b, __p) __arm_vhcaddq_rot270_m_s16(__inactive, __a, __b, __p)
#define vhcaddq_rot90_m_s8(__inactive, __a, __b, __p) __arm_vhcaddq_rot90_m_s8(__inactive, __a, __b, __p)
#define vhcaddq_rot90_m_s32(__inactive, __a, __b, __p) __arm_vhcaddq_rot90_m_s32(__inactive, __a, __b, __p)
#define vhcaddq_rot90_m_s16(__inactive, __a, __b, __p) __arm_vhcaddq_rot90_m_s16(__inactive, __a, __b, __p)
#define vmladavaq_p_s8(__a, __b, __c, __p) __arm_vmladavaq_p_s8(__a, __b, __c, __p)
#define vmladavaq_p_s32(__a, __b, __c, __p) __arm_vmladavaq_p_s32(__a, __b, __c, __p)
#define vmladavaq_p_s16(__a, __b, __c, __p) __arm_vmladavaq_p_s16(__a, __b, __c, __p)
#define vmladavaq_p_u8(__a, __b, __c, __p) __arm_vmladavaq_p_u8(__a, __b, __c, __p)
#define vmladavaq_p_u32(__a, __b, __c, __p) __arm_vmladavaq_p_u32(__a, __b, __c, __p)
#define vmladavaq_p_u16(__a, __b, __c, __p) __arm_vmladavaq_p_u16(__a, __b, __c, __p)
#define vmladavaxq_p_s8(__a, __b, __c, __p) __arm_vmladavaxq_p_s8(__a, __b, __c, __p)
#define vmladavaxq_p_s32(__a, __b, __c, __p) __arm_vmladavaxq_p_s32(__a, __b, __c, __p)
#define vmladavaxq_p_s16(__a, __b, __c, __p) __arm_vmladavaxq_p_s16(__a, __b, __c, __p)
#define vmlaq_m_n_s8(__a, __b, __c, __p) __arm_vmlaq_m_n_s8(__a, __b, __c, __p)
#define vmlaq_m_n_s32(__a, __b, __c, __p) __arm_vmlaq_m_n_s32(__a, __b, __c, __p)
#define vmlaq_m_n_s16(__a, __b, __c, __p) __arm_vmlaq_m_n_s16(__a, __b, __c, __p)
#define vmlaq_m_n_u8(__a, __b, __c, __p) __arm_vmlaq_m_n_u8(__a, __b, __c, __p)
#define vmlaq_m_n_u32(__a, __b, __c, __p) __arm_vmlaq_m_n_u32(__a, __b, __c, __p)
#define vmlaq_m_n_u16(__a, __b, __c, __p) __arm_vmlaq_m_n_u16(__a, __b, __c, __p)
#define vmlasq_m_n_s8(__a, __b, __c, __p) __arm_vmlasq_m_n_s8(__a, __b, __c, __p)
#define vmlasq_m_n_s32(__a, __b, __c, __p) __arm_vmlasq_m_n_s32(__a, __b, __c, __p)
#define vmlasq_m_n_s16(__a, __b, __c, __p) __arm_vmlasq_m_n_s16(__a, __b, __c, __p)
#define vmlasq_m_n_u8(__a, __b, __c, __p) __arm_vmlasq_m_n_u8(__a, __b, __c, __p)
#define vmlasq_m_n_u32(__a, __b, __c, __p) __arm_vmlasq_m_n_u32(__a, __b, __c, __p)
#define vmlasq_m_n_u16(__a, __b, __c, __p) __arm_vmlasq_m_n_u16(__a, __b, __c, __p)
#define vmlsdavaq_p_s8(__a, __b, __c, __p) __arm_vmlsdavaq_p_s8(__a, __b, __c, __p)
#define vmlsdavaq_p_s32(__a, __b, __c, __p) __arm_vmlsdavaq_p_s32(__a, __b, __c, __p)
#define vmlsdavaq_p_s16(__a, __b, __c, __p) __arm_vmlsdavaq_p_s16(__a, __b, __c, __p)
#define vmlsdavaxq_p_s8(__a, __b, __c, __p) __arm_vmlsdavaxq_p_s8(__a, __b, __c, __p)
#define vmlsdavaxq_p_s32(__a, __b, __c, __p) __arm_vmlsdavaxq_p_s32(__a, __b, __c, __p)
#define vmlsdavaxq_p_s16(__a, __b, __c, __p) __arm_vmlsdavaxq_p_s16(__a, __b, __c, __p)
#define vmullbq_int_m_s8(__inactive, __a, __b, __p) __arm_vmullbq_int_m_s8(__inactive, __a, __b, __p)
#define vmullbq_int_m_s32(__inactive, __a, __b, __p) __arm_vmullbq_int_m_s32(__inactive, __a, __b, __p)
#define vmullbq_int_m_s16(__inactive, __a, __b, __p) __arm_vmullbq_int_m_s16(__inactive, __a, __b, __p)
#define vmullbq_int_m_u8(__inactive, __a, __b, __p) __arm_vmullbq_int_m_u8(__inactive, __a, __b, __p)
#define vmullbq_int_m_u32(__inactive, __a, __b, __p) __arm_vmullbq_int_m_u32(__inactive, __a, __b, __p)
#define vmullbq_int_m_u16(__inactive, __a, __b, __p) __arm_vmullbq_int_m_u16(__inactive, __a, __b, __p)
#define vmulltq_int_m_s8(__inactive, __a, __b, __p) __arm_vmulltq_int_m_s8(__inactive, __a, __b, __p)
#define vmulltq_int_m_s32(__inactive, __a, __b, __p) __arm_vmulltq_int_m_s32(__inactive, __a, __b, __p)
#define vmulltq_int_m_s16(__inactive, __a, __b, __p) __arm_vmulltq_int_m_s16(__inactive, __a, __b, __p)
#define vmulltq_int_m_u8(__inactive, __a, __b, __p) __arm_vmulltq_int_m_u8(__inactive, __a, __b, __p)
#define vmulltq_int_m_u32(__inactive, __a, __b, __p) __arm_vmulltq_int_m_u32(__inactive, __a, __b, __p)
#define vmulltq_int_m_u16(__inactive, __a, __b, __p) __arm_vmulltq_int_m_u16(__inactive, __a, __b, __p)
#define vornq_m_s8(__inactive, __a, __b, __p) __arm_vornq_m_s8(__inactive, __a, __b, __p)
#define vornq_m_s32(__inactive, __a, __b, __p) __arm_vornq_m_s32(__inactive, __a, __b, __p)
#define vornq_m_s16(__inactive, __a, __b, __p) __arm_vornq_m_s16(__inactive, __a, __b, __p)
#define vornq_m_u8(__inactive, __a, __b, __p) __arm_vornq_m_u8(__inactive, __a, __b, __p)
#define vornq_m_u32(__inactive, __a, __b, __p) __arm_vornq_m_u32(__inactive, __a, __b, __p)
#define vornq_m_u16(__inactive, __a, __b, __p) __arm_vornq_m_u16(__inactive, __a, __b, __p)
#define vqdmladhq_m_s8(__inactive, __a, __b, __p) __arm_vqdmladhq_m_s8(__inactive, __a, __b, __p)
#define vqdmladhq_m_s32(__inactive, __a, __b, __p) __arm_vqdmladhq_m_s32(__inactive, __a, __b, __p)
#define vqdmladhq_m_s16(__inactive, __a, __b, __p) __arm_vqdmladhq_m_s16(__inactive, __a, __b, __p)
#define vqdmladhxq_m_s8(__inactive, __a, __b, __p) __arm_vqdmladhxq_m_s8(__inactive, __a, __b, __p)
#define vqdmladhxq_m_s32(__inactive, __a, __b, __p) __arm_vqdmladhxq_m_s32(__inactive, __a, __b, __p)
#define vqdmladhxq_m_s16(__inactive, __a, __b, __p) __arm_vqdmladhxq_m_s16(__inactive, __a, __b, __p)
#define vqdmlashq_m_n_s8(__a, __b, __c, __p) __arm_vqdmlashq_m_n_s8(__a, __b, __c, __p)
#define vqdmlashq_m_n_s32(__a, __b, __c, __p) __arm_vqdmlashq_m_n_s32(__a, __b, __c, __p)
#define vqdmlashq_m_n_s16(__a, __b, __c, __p) __arm_vqdmlashq_m_n_s16(__a, __b, __c, __p)
#define vqdmlahq_m_n_s8(__a, __b, __c, __p) __arm_vqdmlahq_m_n_s8(__a, __b, __c, __p)
#define vqdmlahq_m_n_s32(__a, __b, __c, __p) __arm_vqdmlahq_m_n_s32(__a, __b, __c, __p)
#define vqdmlahq_m_n_s16(__a, __b, __c, __p) __arm_vqdmlahq_m_n_s16(__a, __b, __c, __p)
#define vqdmlsdhq_m_s8(__inactive, __a, __b, __p) __arm_vqdmlsdhq_m_s8(__inactive, __a, __b, __p)
#define vqdmlsdhq_m_s32(__inactive, __a, __b, __p) __arm_vqdmlsdhq_m_s32(__inactive, __a, __b, __p)
#define vqdmlsdhq_m_s16(__inactive, __a, __b, __p) __arm_vqdmlsdhq_m_s16(__inactive, __a, __b, __p)
#define vqdmlsdhxq_m_s8(__inactive, __a, __b, __p) __arm_vqdmlsdhxq_m_s8(__inactive, __a, __b, __p)
#define vqdmlsdhxq_m_s32(__inactive, __a, __b, __p) __arm_vqdmlsdhxq_m_s32(__inactive, __a, __b, __p)
#define vqdmlsdhxq_m_s16(__inactive, __a, __b, __p) __arm_vqdmlsdhxq_m_s16(__inactive, __a, __b, __p)
#define vqrdmladhq_m_s8(__inactive, __a, __b, __p) __arm_vqrdmladhq_m_s8(__inactive, __a, __b, __p)
#define vqrdmladhq_m_s32(__inactive, __a, __b, __p) __arm_vqrdmladhq_m_s32(__inactive, __a, __b, __p)
#define vqrdmladhq_m_s16(__inactive, __a, __b, __p) __arm_vqrdmladhq_m_s16(__inactive, __a, __b, __p)
#define vqrdmladhxq_m_s8(__inactive, __a, __b, __p) __arm_vqrdmladhxq_m_s8(__inactive, __a, __b, __p)
#define vqrdmladhxq_m_s32(__inactive, __a, __b, __p) __arm_vqrdmladhxq_m_s32(__inactive, __a, __b, __p)
#define vqrdmladhxq_m_s16(__inactive, __a, __b, __p) __arm_vqrdmladhxq_m_s16(__inactive, __a, __b, __p)
#define vqrdmlahq_m_n_s8(__a, __b, __c, __p) __arm_vqrdmlahq_m_n_s8(__a, __b, __c, __p)
#define vqrdmlahq_m_n_s32(__a, __b, __c, __p) __arm_vqrdmlahq_m_n_s32(__a, __b, __c, __p)
#define vqrdmlahq_m_n_s16(__a, __b, __c, __p) __arm_vqrdmlahq_m_n_s16(__a, __b, __c, __p)
#define vqrdmlashq_m_n_s8(__a, __b, __c, __p) __arm_vqrdmlashq_m_n_s8(__a, __b, __c, __p)
#define vqrdmlashq_m_n_s32(__a, __b, __c, __p) __arm_vqrdmlashq_m_n_s32(__a, __b, __c, __p)
#define vqrdmlashq_m_n_s16(__a, __b, __c, __p) __arm_vqrdmlashq_m_n_s16(__a, __b, __c, __p)
#define vqrdmlsdhq_m_s8(__inactive, __a, __b, __p) __arm_vqrdmlsdhq_m_s8(__inactive, __a, __b, __p)
#define vqrdmlsdhq_m_s32(__inactive, __a, __b, __p) __arm_vqrdmlsdhq_m_s32(__inactive, __a, __b, __p)
#define vqrdmlsdhq_m_s16(__inactive, __a, __b, __p) __arm_vqrdmlsdhq_m_s16(__inactive, __a, __b, __p)
#define vqrdmlsdhxq_m_s8(__inactive, __a, __b, __p) __arm_vqrdmlsdhxq_m_s8(__inactive, __a, __b, __p)
#define vqrdmlsdhxq_m_s32(__inactive, __a, __b, __p) __arm_vqrdmlsdhxq_m_s32(__inactive, __a, __b, __p)
#define vqrdmlsdhxq_m_s16(__inactive, __a, __b, __p) __arm_vqrdmlsdhxq_m_s16(__inactive, __a, __b, __p)
#define vsliq_m_n_s8(__a, __b,  __imm, __p) __arm_vsliq_m_n_s8(__a, __b,  __imm, __p)
#define vsliq_m_n_s32(__a, __b,  __imm, __p) __arm_vsliq_m_n_s32(__a, __b,  __imm, __p)
#define vsliq_m_n_s16(__a, __b,  __imm, __p) __arm_vsliq_m_n_s16(__a, __b,  __imm, __p)
#define vsliq_m_n_u8(__a, __b,  __imm, __p) __arm_vsliq_m_n_u8(__a, __b,  __imm, __p)
#define vsliq_m_n_u32(__a, __b,  __imm, __p) __arm_vsliq_m_n_u32(__a, __b,  __imm, __p)
#define vsliq_m_n_u16(__a, __b,  __imm, __p) __arm_vsliq_m_n_u16(__a, __b,  __imm, __p)
#define vmlaldavaq_p_s32(__a, __b, __c, __p) __arm_vmlaldavaq_p_s32(__a, __b, __c, __p)
#define vmlaldavaq_p_s16(__a, __b, __c, __p) __arm_vmlaldavaq_p_s16(__a, __b, __c, __p)
#define vmlaldavaq_p_u32(__a, __b, __c, __p) __arm_vmlaldavaq_p_u32(__a, __b, __c, __p)
#define vmlaldavaq_p_u16(__a, __b, __c, __p) __arm_vmlaldavaq_p_u16(__a, __b, __c, __p)
#define vmlaldavaxq_p_s32(__a, __b, __c, __p) __arm_vmlaldavaxq_p_s32(__a, __b, __c, __p)
#define vmlaldavaxq_p_s16(__a, __b, __c, __p) __arm_vmlaldavaxq_p_s16(__a, __b, __c, __p)
#define vmlsldavaq_p_s32(__a, __b, __c, __p) __arm_vmlsldavaq_p_s32(__a, __b, __c, __p)
#define vmlsldavaq_p_s16(__a, __b, __c, __p) __arm_vmlsldavaq_p_s16(__a, __b, __c, __p)
#define vmlsldavaxq_p_s32(__a, __b, __c, __p) __arm_vmlsldavaxq_p_s32(__a, __b, __c, __p)
#define vmlsldavaxq_p_s16(__a, __b, __c, __p) __arm_vmlsldavaxq_p_s16(__a, __b, __c, __p)
#define vmullbq_poly_m_p8(__inactive, __a, __b, __p) __arm_vmullbq_poly_m_p8(__inactive, __a, __b, __p)
#define vmullbq_poly_m_p16(__inactive, __a, __b, __p) __arm_vmullbq_poly_m_p16(__inactive, __a, __b, __p)
#define vmulltq_poly_m_p8(__inactive, __a, __b, __p) __arm_vmulltq_poly_m_p8(__inactive, __a, __b, __p)
#define vmulltq_poly_m_p16(__inactive, __a, __b, __p) __arm_vmulltq_poly_m_p16(__inactive, __a, __b, __p)
#define vqdmullbq_m_n_s32(__inactive, __a, __b, __p) __arm_vqdmullbq_m_n_s32(__inactive, __a, __b, __p)
#define vqdmullbq_m_n_s16(__inactive, __a, __b, __p) __arm_vqdmullbq_m_n_s16(__inactive, __a, __b, __p)
#define vqdmullbq_m_s32(__inactive, __a, __b, __p) __arm_vqdmullbq_m_s32(__inactive, __a, __b, __p)
#define vqdmullbq_m_s16(__inactive, __a, __b, __p) __arm_vqdmullbq_m_s16(__inactive, __a, __b, __p)
#define vqdmulltq_m_n_s32(__inactive, __a, __b, __p) __arm_vqdmulltq_m_n_s32(__inactive, __a, __b, __p)
#define vqdmulltq_m_n_s16(__inactive, __a, __b, __p) __arm_vqdmulltq_m_n_s16(__inactive, __a, __b, __p)
#define vqdmulltq_m_s32(__inactive, __a, __b, __p) __arm_vqdmulltq_m_s32(__inactive, __a, __b, __p)
#define vqdmulltq_m_s16(__inactive, __a, __b, __p) __arm_vqdmulltq_m_s16(__inactive, __a, __b, __p)
#define vrmlaldavhaq_p_s32(__a, __b, __c, __p) __arm_vrmlaldavhaq_p_s32(__a, __b, __c, __p)
#define vrmlaldavhaq_p_u32(__a, __b, __c, __p) __arm_vrmlaldavhaq_p_u32(__a, __b, __c, __p)
#define vrmlaldavhaxq_p_s32(__a, __b, __c, __p) __arm_vrmlaldavhaxq_p_s32(__a, __b, __c, __p)
#define vrmlsldavhaq_p_s32(__a, __b, __c, __p) __arm_vrmlsldavhaq_p_s32(__a, __b, __c, __p)
#define vrmlsldavhaxq_p_s32(__a, __b, __c, __p) __arm_vrmlsldavhaxq_p_s32(__a, __b, __c, __p)
#define vbicq_m_f32(__inactive, __a, __b, __p) __arm_vbicq_m_f32(__inactive, __a, __b, __p)
#define vbicq_m_f16(__inactive, __a, __b, __p) __arm_vbicq_m_f16(__inactive, __a, __b, __p)
#define vbrsrq_m_n_f32(__inactive, __a, __b, __p) __arm_vbrsrq_m_n_f32(__inactive, __a, __b, __p)
#define vbrsrq_m_n_f16(__inactive, __a, __b, __p) __arm_vbrsrq_m_n_f16(__inactive, __a, __b, __p)
#define vcaddq_rot270_m_f32(__inactive, __a, __b, __p) __arm_vcaddq_rot270_m_f32(__inactive, __a, __b, __p)
#define vcaddq_rot270_m_f16(__inactive, __a, __b, __p) __arm_vcaddq_rot270_m_f16(__inactive, __a, __b, __p)
#define vcaddq_rot90_m_f32(__inactive, __a, __b, __p) __arm_vcaddq_rot90_m_f32(__inactive, __a, __b, __p)
#define vcaddq_rot90_m_f16(__inactive, __a, __b, __p) __arm_vcaddq_rot90_m_f16(__inactive, __a, __b, __p)
#define vcmlaq_m_f32(__a, __b, __c, __p) __arm_vcmlaq_m_f32(__a, __b, __c, __p)
#define vcmlaq_m_f16(__a, __b, __c, __p) __arm_vcmlaq_m_f16(__a, __b, __c, __p)
#define vcmlaq_rot180_m_f32(__a, __b, __c, __p) __arm_vcmlaq_rot180_m_f32(__a, __b, __c, __p)
#define vcmlaq_rot180_m_f16(__a, __b, __c, __p) __arm_vcmlaq_rot180_m_f16(__a, __b, __c, __p)
#define vcmlaq_rot270_m_f32(__a, __b, __c, __p) __arm_vcmlaq_rot270_m_f32(__a, __b, __c, __p)
#define vcmlaq_rot270_m_f16(__a, __b, __c, __p) __arm_vcmlaq_rot270_m_f16(__a, __b, __c, __p)
#define vcmlaq_rot90_m_f32(__a, __b, __c, __p) __arm_vcmlaq_rot90_m_f32(__a, __b, __c, __p)
#define vcmlaq_rot90_m_f16(__a, __b, __c, __p) __arm_vcmlaq_rot90_m_f16(__a, __b, __c, __p)
#define vcmulq_m_f32(__inactive, __a, __b, __p) __arm_vcmulq_m_f32(__inactive, __a, __b, __p)
#define vcmulq_m_f16(__inactive, __a, __b, __p) __arm_vcmulq_m_f16(__inactive, __a, __b, __p)
#define vcmulq_rot180_m_f32(__inactive, __a, __b, __p) __arm_vcmulq_rot180_m_f32(__inactive, __a, __b, __p)
#define vcmulq_rot180_m_f16(__inactive, __a, __b, __p) __arm_vcmulq_rot180_m_f16(__inactive, __a, __b, __p)
#define vcmulq_rot270_m_f32(__inactive, __a, __b, __p) __arm_vcmulq_rot270_m_f32(__inactive, __a, __b, __p)
#define vcmulq_rot270_m_f16(__inactive, __a, __b, __p) __arm_vcmulq_rot270_m_f16(__inactive, __a, __b, __p)
#define vcmulq_rot90_m_f32(__inactive, __a, __b, __p) __arm_vcmulq_rot90_m_f32(__inactive, __a, __b, __p)
#define vcmulq_rot90_m_f16(__inactive, __a, __b, __p) __arm_vcmulq_rot90_m_f16(__inactive, __a, __b, __p)
#define vcvtq_m_n_s32_f32(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_s32_f32(__inactive, __a,  __imm6, __p)
#define vcvtq_m_n_s16_f16(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_s16_f16(__inactive, __a,  __imm6, __p)
#define vcvtq_m_n_u32_f32(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_u32_f32(__inactive, __a,  __imm6, __p)
#define vcvtq_m_n_u16_f16(__inactive, __a,  __imm6, __p) __arm_vcvtq_m_n_u16_f16(__inactive, __a,  __imm6, __p)
#define vfmaq_m_f32(__a, __b, __c, __p) __arm_vfmaq_m_f32(__a, __b, __c, __p)
#define vfmaq_m_f16(__a, __b, __c, __p) __arm_vfmaq_m_f16(__a, __b, __c, __p)
#define vfmaq_m_n_f32(__a, __b, __c, __p) __arm_vfmaq_m_n_f32(__a, __b, __c, __p)
#define vfmaq_m_n_f16(__a, __b, __c, __p) __arm_vfmaq_m_n_f16(__a, __b, __c, __p)
#define vfmasq_m_n_f32(__a, __b, __c, __p) __arm_vfmasq_m_n_f32(__a, __b, __c, __p)
#define vfmasq_m_n_f16(__a, __b, __c, __p) __arm_vfmasq_m_n_f16(__a, __b, __c, __p)
#define vfmsq_m_f32(__a, __b, __c, __p) __arm_vfmsq_m_f32(__a, __b, __c, __p)
#define vfmsq_m_f16(__a, __b, __c, __p) __arm_vfmsq_m_f16(__a, __b, __c, __p)
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
#define vld1q_s8(__base) __arm_vld1q_s8(__base)
#define vld1q_s32(__base) __arm_vld1q_s32(__base)
#define vld1q_s16(__base) __arm_vld1q_s16(__base)
#define vld1q_u8(__base) __arm_vld1q_u8(__base)
#define vld1q_u32(__base) __arm_vld1q_u32(__base)
#define vld1q_u16(__base) __arm_vld1q_u16(__base)
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
#define vld1q_f32(__base) __arm_vld1q_f32(__base)
#define vld1q_f16(__base) __arm_vld1q_f16(__base)
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
#define vst1q_f32(__addr, __value) __arm_vst1q_f32(__addr, __value)
#define vst1q_f16(__addr, __value) __arm_vst1q_f16(__addr, __value)
#define vst1q_s8(__addr, __value) __arm_vst1q_s8(__addr, __value)
#define vst1q_s32(__addr, __value) __arm_vst1q_s32(__addr, __value)
#define vst1q_s16(__addr, __value) __arm_vst1q_s16(__addr, __value)
#define vst1q_u8(__addr, __value) __arm_vst1q_u8(__addr, __value)
#define vst1q_u32(__addr, __value) __arm_vst1q_u32(__addr, __value)
#define vst1q_u16(__addr, __value) __arm_vst1q_u16(__addr, __value)
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
#define vmullbq_poly_x_p8(__a, __b, __p) __arm_vmullbq_poly_x_p8(__a, __b, __p)
#define vmullbq_poly_x_p16(__a, __b, __p) __arm_vmullbq_poly_x_p16(__a, __b, __p)
#define vmullbq_int_x_s8(__a, __b, __p) __arm_vmullbq_int_x_s8(__a, __b, __p)
#define vmullbq_int_x_s16(__a, __b, __p) __arm_vmullbq_int_x_s16(__a, __b, __p)
#define vmullbq_int_x_s32(__a, __b, __p) __arm_vmullbq_int_x_s32(__a, __b, __p)
#define vmullbq_int_x_u8(__a, __b, __p) __arm_vmullbq_int_x_u8(__a, __b, __p)
#define vmullbq_int_x_u16(__a, __b, __p) __arm_vmullbq_int_x_u16(__a, __b, __p)
#define vmullbq_int_x_u32(__a, __b, __p) __arm_vmullbq_int_x_u32(__a, __b, __p)
#define vmulltq_poly_x_p8(__a, __b, __p) __arm_vmulltq_poly_x_p8(__a, __b, __p)
#define vmulltq_poly_x_p16(__a, __b, __p) __arm_vmulltq_poly_x_p16(__a, __b, __p)
#define vmulltq_int_x_s8(__a, __b, __p) __arm_vmulltq_int_x_s8(__a, __b, __p)
#define vmulltq_int_x_s16(__a, __b, __p) __arm_vmulltq_int_x_s16(__a, __b, __p)
#define vmulltq_int_x_s32(__a, __b, __p) __arm_vmulltq_int_x_s32(__a, __b, __p)
#define vmulltq_int_x_u8(__a, __b, __p) __arm_vmulltq_int_x_u8(__a, __b, __p)
#define vmulltq_int_x_u16(__a, __b, __p) __arm_vmulltq_int_x_u16(__a, __b, __p)
#define vmulltq_int_x_u32(__a, __b, __p) __arm_vmulltq_int_x_u32(__a, __b, __p)
#define vcaddq_rot90_x_s8(__a, __b, __p) __arm_vcaddq_rot90_x_s8(__a, __b, __p)
#define vcaddq_rot90_x_s16(__a, __b, __p) __arm_vcaddq_rot90_x_s16(__a, __b, __p)
#define vcaddq_rot90_x_s32(__a, __b, __p) __arm_vcaddq_rot90_x_s32(__a, __b, __p)
#define vcaddq_rot90_x_u8(__a, __b, __p) __arm_vcaddq_rot90_x_u8(__a, __b, __p)
#define vcaddq_rot90_x_u16(__a, __b, __p) __arm_vcaddq_rot90_x_u16(__a, __b, __p)
#define vcaddq_rot90_x_u32(__a, __b, __p) __arm_vcaddq_rot90_x_u32(__a, __b, __p)
#define vcaddq_rot270_x_s8(__a, __b, __p) __arm_vcaddq_rot270_x_s8(__a, __b, __p)
#define vcaddq_rot270_x_s16(__a, __b, __p) __arm_vcaddq_rot270_x_s16(__a, __b, __p)
#define vcaddq_rot270_x_s32(__a, __b, __p) __arm_vcaddq_rot270_x_s32(__a, __b, __p)
#define vcaddq_rot270_x_u8(__a, __b, __p) __arm_vcaddq_rot270_x_u8(__a, __b, __p)
#define vcaddq_rot270_x_u16(__a, __b, __p) __arm_vcaddq_rot270_x_u16(__a, __b, __p)
#define vcaddq_rot270_x_u32(__a, __b, __p) __arm_vcaddq_rot270_x_u32(__a, __b, __p)
#define vhcaddq_rot90_x_s8(__a, __b, __p) __arm_vhcaddq_rot90_x_s8(__a, __b, __p)
#define vhcaddq_rot90_x_s16(__a, __b, __p) __arm_vhcaddq_rot90_x_s16(__a, __b, __p)
#define vhcaddq_rot90_x_s32(__a, __b, __p) __arm_vhcaddq_rot90_x_s32(__a, __b, __p)
#define vhcaddq_rot270_x_s8(__a, __b, __p) __arm_vhcaddq_rot270_x_s8(__a, __b, __p)
#define vhcaddq_rot270_x_s16(__a, __b, __p) __arm_vhcaddq_rot270_x_s16(__a, __b, __p)
#define vhcaddq_rot270_x_s32(__a, __b, __p) __arm_vhcaddq_rot270_x_s32(__a, __b, __p)
#define vbicq_x_s8(__a, __b, __p) __arm_vbicq_x_s8(__a, __b, __p)
#define vbicq_x_s16(__a, __b, __p) __arm_vbicq_x_s16(__a, __b, __p)
#define vbicq_x_s32(__a, __b, __p) __arm_vbicq_x_s32(__a, __b, __p)
#define vbicq_x_u8(__a, __b, __p) __arm_vbicq_x_u8(__a, __b, __p)
#define vbicq_x_u16(__a, __b, __p) __arm_vbicq_x_u16(__a, __b, __p)
#define vbicq_x_u32(__a, __b, __p) __arm_vbicq_x_u32(__a, __b, __p)
#define vbrsrq_x_n_s8(__a, __b, __p) __arm_vbrsrq_x_n_s8(__a, __b, __p)
#define vbrsrq_x_n_s16(__a, __b, __p) __arm_vbrsrq_x_n_s16(__a, __b, __p)
#define vbrsrq_x_n_s32(__a, __b, __p) __arm_vbrsrq_x_n_s32(__a, __b, __p)
#define vbrsrq_x_n_u8(__a, __b, __p) __arm_vbrsrq_x_n_u8(__a, __b, __p)
#define vbrsrq_x_n_u16(__a, __b, __p) __arm_vbrsrq_x_n_u16(__a, __b, __p)
#define vbrsrq_x_n_u32(__a, __b, __p) __arm_vbrsrq_x_n_u32(__a, __b, __p)
#define vmvnq_x_s8(__a, __p) __arm_vmvnq_x_s8(__a, __p)
#define vmvnq_x_s16(__a, __p) __arm_vmvnq_x_s16(__a, __p)
#define vmvnq_x_s32(__a, __p) __arm_vmvnq_x_s32(__a, __p)
#define vmvnq_x_u8(__a, __p) __arm_vmvnq_x_u8(__a, __p)
#define vmvnq_x_u16(__a, __p) __arm_vmvnq_x_u16(__a, __p)
#define vmvnq_x_u32(__a, __p) __arm_vmvnq_x_u32(__a, __p)
#define vmvnq_x_n_s16( __imm, __p) __arm_vmvnq_x_n_s16( __imm, __p)
#define vmvnq_x_n_s32( __imm, __p) __arm_vmvnq_x_n_s32( __imm, __p)
#define vmvnq_x_n_u16( __imm, __p) __arm_vmvnq_x_n_u16( __imm, __p)
#define vmvnq_x_n_u32( __imm, __p) __arm_vmvnq_x_n_u32( __imm, __p)
#define vornq_x_s8(__a, __b, __p) __arm_vornq_x_s8(__a, __b, __p)
#define vornq_x_s16(__a, __b, __p) __arm_vornq_x_s16(__a, __b, __p)
#define vornq_x_s32(__a, __b, __p) __arm_vornq_x_s32(__a, __b, __p)
#define vornq_x_u8(__a, __b, __p) __arm_vornq_x_u8(__a, __b, __p)
#define vornq_x_u16(__a, __b, __p) __arm_vornq_x_u16(__a, __b, __p)
#define vornq_x_u32(__a, __b, __p) __arm_vornq_x_u32(__a, __b, __p)
#define vcaddq_rot90_x_f16(__a, __b, __p) __arm_vcaddq_rot90_x_f16(__a, __b, __p)
#define vcaddq_rot90_x_f32(__a, __b, __p) __arm_vcaddq_rot90_x_f32(__a, __b, __p)
#define vcaddq_rot270_x_f16(__a, __b, __p) __arm_vcaddq_rot270_x_f16(__a, __b, __p)
#define vcaddq_rot270_x_f32(__a, __b, __p) __arm_vcaddq_rot270_x_f32(__a, __b, __p)
#define vcmulq_x_f16(__a, __b, __p) __arm_vcmulq_x_f16(__a, __b, __p)
#define vcmulq_x_f32(__a, __b, __p) __arm_vcmulq_x_f32(__a, __b, __p)
#define vcmulq_rot90_x_f16(__a, __b, __p) __arm_vcmulq_rot90_x_f16(__a, __b, __p)
#define vcmulq_rot90_x_f32(__a, __b, __p) __arm_vcmulq_rot90_x_f32(__a, __b, __p)
#define vcmulq_rot180_x_f16(__a, __b, __p) __arm_vcmulq_rot180_x_f16(__a, __b, __p)
#define vcmulq_rot180_x_f32(__a, __b, __p) __arm_vcmulq_rot180_x_f32(__a, __b, __p)
#define vcmulq_rot270_x_f16(__a, __b, __p) __arm_vcmulq_rot270_x_f16(__a, __b, __p)
#define vcmulq_rot270_x_f32(__a, __b, __p) __arm_vcmulq_rot270_x_f32(__a, __b, __p)
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
#define vbrsrq_x_n_f16(__a, __b, __p) __arm_vbrsrq_x_n_f16(__a, __b, __p)
#define vbrsrq_x_n_f32(__a, __b, __p) __arm_vbrsrq_x_n_f32(__a, __b, __p)
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

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vmladavq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)
    __builtin_mve_vcaddq_rot90v16qi ((int8x16_t)__a, (int8x16_t)__b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return (uint8x16_t)
    __builtin_mve_vcaddq_rot270v16qi ((int8x16_t)__a, (int8x16_t)__b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return __builtin_mve_vbicq_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_u8 (uint8x16_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_uv16qi (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_n_s8 (int8x16_t __a, const int __imm)
{
  return __builtin_mve_vqshluq_n_sv16qi (__a, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vornq_sv16qi (__a, __b);
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
__arm_vcaddq_rot90_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcaddq_rot90v16qi (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_s8 (int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vcaddq_rot270v16qi (__a, __b);
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

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vornq_uv8hi (__a, __b);
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

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmladavq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)
    __builtin_mve_vcaddq_rot90v8hi ((int16x8_t)__a, (int16x8_t)__b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return (uint16x8_t)
    __builtin_mve_vcaddq_rot270v8hi ((int16x8_t)__a, (int16x8_t)__b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vbicq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_u16 (uint16x8_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vqshluq_n_sv8hi (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vornq_sv8hi (__a, __b);
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
__arm_vcaddq_rot90_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcaddq_rot90v8hi (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_s16 (int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vcaddq_rot270v8hi (__a, __b);
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

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vornq_uv4si (__a, __b);
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

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmladavq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)
    __builtin_mve_vcaddq_rot90v4si ((int32x4_t)__a, (int32x4_t)__b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return (uint32x4_t)
    __builtin_mve_vcaddq_rot270v4si ((int32x4_t)__a, (int32x4_t)__b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vbicq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_n_u32 (uint32x4_t __a, int32_t __b)
{
  return __builtin_mve_vbrsrq_n_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_n_s32 (int32x4_t __a, const int __imm)
{
  return __builtin_mve_vqshluq_n_sv4si (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vornq_sv4si (__a, __b);
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
__arm_vcaddq_rot90_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcaddq_rot90v4si (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_s32 (int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vcaddq_rot270v4si (__a, __b);
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

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return __builtin_mve_vmlaldavq_uv8hi (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_u16 (uint16x8_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_uv8hi (__a, __imm);
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
__arm_vbicq_n_s16 (int16x8_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_sv8hi (__a, __imm);
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

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return __builtin_mve_vmlaldavq_uv4si (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq_n_u32 (uint32x4_t __a, const int __imm)
{
  return __builtin_mve_vbicq_n_uv4si (__a, __imm);
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

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_s8 (uint32_t __a, int8x16_t __b, int8x16_t __c)
{
  return __builtin_mve_vabavq_sv16qi (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_s16 (uint32_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_mve_vabavq_sv8hi (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_s32 (uint32_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vabavq_sv4si (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_u8 (uint32_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return __builtin_mve_vabavq_uv16qi(__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_u16 (uint32_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_mve_vabavq_uv8hi(__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_u32 (uint32_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_mve_vabavq_uv4si(__a, __b, __c);
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

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaq_s32 (int64_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vrmlaldavhaq_sv4si (__a, __b, __c);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaq_u32 (uint64_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_mve_vrmlaldavhaq_uv4si (__a, __b, __c);
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

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_u8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_uv16qi (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_sv16qi (__a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_u8 (uint8x16_t __inactive, uint8x16_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_uv16qi (__inactive, __a, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_n_u8 (uint8x16_t __a, uint8x16_t __b, uint8_t __c)
{
  return __builtin_mve_vmlasq_n_uv16qi (__a, __b, __c);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_n_u8 (uint8x16_t __a, uint8x16_t __b, uint8_t __c)
{
  return __builtin_mve_vmlaq_n_uv16qi (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p_u8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmladavq_p_uv16qi (__a, __b, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_u8 (uint32_t __a, uint8x16_t __b, uint8x16_t __c)
{
  return __builtin_mve_vmladavaq_uv16qi (__a, __b, __c);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __imm)
{
  return __builtin_mve_vsriq_n_uv16qi (__a, __b, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __imm)
{
  return __builtin_mve_vsliq_n_uv16qi (__a, __b, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_s8 (int8x16_t __inactive, int8x16_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_sv16qi (__inactive, __a, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq_p_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavxq_p_sv16qi (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq_p_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavq_p_sv16qi (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq_p_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmladavxq_p_sv16qi (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmladavq_p_sv16qi (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqrdmlsdhxq_sv16qi (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqrdmlsdhq_sv16qi (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c)
{
  return __builtin_mve_vqrdmlashq_n_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c)
{
  return __builtin_mve_vqdmlashq_n_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c)
{
  return __builtin_mve_vqrdmlahq_n_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqrdmladhxq_sv16qi (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqrdmladhq_sv16qi (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqdmlsdhxq_sv16qi (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqdmlsdhq_sv16qi (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c)
{
  return __builtin_mve_vqdmlahq_n_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqdmladhxq_sv16qi (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
  return __builtin_mve_vqdmladhq_sv16qi (__inactive, __a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq_s8 (int32_t __a, int8x16_t __b, int8x16_t __c)
{
  return __builtin_mve_vmlsdavaxq_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq_s8 (int32_t __a, int8x16_t __b, int8x16_t __c)
{
  return __builtin_mve_vmlsdavaq_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c)
{
  return __builtin_mve_vmlasq_n_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c)
{
  return __builtin_mve_vmlaq_n_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq_s8 (int32_t __a, int8x16_t __b, int8x16_t __c)
{
  return __builtin_mve_vmladavaxq_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_s8 (int32_t __a, int8x16_t __b, int8x16_t __c)
{
  return __builtin_mve_vmladavaq_sv16qi (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_n_s8 (int8x16_t __a, int8x16_t __b, const int __imm)
{
  return __builtin_mve_vsriq_n_sv16qi (__a, __b, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_n_s8 (int8x16_t __a, int8x16_t __b, const int __imm)
{
  return __builtin_mve_vsliq_n_sv16qi (__a, __b, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_u16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_uv8hi (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_sv8hi (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_u16 (uint16x8_t __inactive, uint16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_uv8hi (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_n_u16 (uint16x8_t __a, uint16x8_t __b, uint16_t __c)
{
  return __builtin_mve_vmlasq_n_uv8hi (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_n_u16 (uint16x8_t __a, uint16x8_t __b, uint16_t __c)
{
  return __builtin_mve_vmlaq_n_uv8hi (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p_u16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmladavq_p_uv8hi (__a, __b, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_u16 (uint32_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_mve_vmladavaq_uv8hi (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __imm)
{
  return __builtin_mve_vsriq_n_uv8hi (__a, __b, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __imm)
{
  return __builtin_mve_vsliq_n_uv8hi (__a, __b, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_s16 (int16x8_t __inactive, int16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_sv8hi (__inactive, __a, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq_p_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavxq_p_sv8hi (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq_p_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavq_p_sv8hi (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq_p_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmladavxq_p_sv8hi (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmladavq_p_sv8hi (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqrdmlsdhxq_sv8hi (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqrdmlsdhq_sv8hi (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_mve_vqrdmlashq_n_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_mve_vqdmlashq_n_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_mve_vqrdmlahq_n_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqrdmladhxq_sv8hi (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqrdmladhq_sv8hi (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqdmlsdhxq_sv8hi (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqdmlsdhq_sv8hi (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_mve_vqdmlahq_n_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqdmladhxq_sv8hi (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
  return __builtin_mve_vqdmladhq_sv8hi (__inactive, __a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq_s16 (int32_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_mve_vmlsdavaxq_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq_s16 (int32_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_mve_vmlsdavaq_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_mve_vmlasq_n_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c)
{
  return __builtin_mve_vmlaq_n_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq_s16 (int32_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_mve_vmladavaxq_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_s16 (int32_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_mve_vmladavaq_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_n_s16 (int16x8_t __a, int16x8_t __b, const int __imm)
{
  return __builtin_mve_vsriq_n_sv8hi (__a, __b, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_n_s16 (int16x8_t __a, int16x8_t __b, const int __imm)
{
  return __builtin_mve_vsliq_n_sv8hi (__a, __b, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_uv4si (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_sv4si (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_u32 (uint32x4_t __inactive, uint32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_uv4si (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_n_u32 (uint32x4_t __a, uint32x4_t __b, uint32_t __c)
{
  return __builtin_mve_vmlasq_n_uv4si (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_n_u32 (uint32x4_t __a, uint32x4_t __b, uint32_t __c)
{
  return __builtin_mve_vmlaq_n_uv4si (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmladavq_p_uv4si (__a, __b, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_u32 (uint32_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_mve_vmladavaq_uv4si (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __imm)
{
  return __builtin_mve_vsriq_n_uv4si (__a, __b, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __imm)
{
  return __builtin_mve_vsliq_n_uv4si (__a, __b, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_s32 (int32x4_t __inactive, int32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_sv4si (__inactive, __a, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavxq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmladavxq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmladavq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqrdmlsdhxq_sv4si (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqrdmlsdhq_sv4si (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_mve_vqrdmlashq_n_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_mve_vqdmlashq_n_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_mve_vqrdmlahq_n_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqrdmladhxq_sv4si (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqrdmladhq_sv4si (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqdmlsdhxq_sv4si (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqdmlsdhq_sv4si (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_mve_vqdmlahq_n_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqdmladhxq_sv4si (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
  return __builtin_mve_vqdmladhq_sv4si (__inactive, __a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq_s32 (int32_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vmlsdavaxq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq_s32 (int32_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vmlsdavaq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_mve_vmlasq_n_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c)
{
  return __builtin_mve_vmlaq_n_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq_s32 (int32_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vmladavaxq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_s32 (int32_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vmladavaq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_n_s32 (int32x4_t __a, int32x4_t __b, const int __imm)
{
  return __builtin_mve_vsriq_n_sv4si (__a, __b, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_n_s32 (int32x4_t __a, int32x4_t __b, const int __imm)
{
  return __builtin_mve_vsliq_n_sv4si (__a, __b, __imm);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_u64 (uint64x2_t __a, uint64x2_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_uv2di (__a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_s64 (int64x2_t __a, int64x2_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_sv2di (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaxq_s32 (int64_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vrmlaldavhaxq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhaq_s32 (int64_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vrmlsldavhaq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhaxq_s32 (int64_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vrmlsldavhaxq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vrmlaldavhq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhxq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vrmlaldavhxq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vrmlsldavhq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhxq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vrmlsldavhxq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhq_p_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vrmlaldavhq_p_uv4si (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_n_s16 (int16x8_t __inactive, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_n_sv8hi (__inactive, __imm, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_s16 (int64_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_mve_vmlaldavaq_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaxq_s16 (int64_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_mve_vmlaldavaxq_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaq_s16 (int64_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_mve_vmlsldavaq_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaxq_s16 (int64_t __a, int16x8_t __b, int16x8_t __c)
{
  return __builtin_mve_vmlsldavaxq_sv8hi (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_p_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavq_p_sv8hi (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavxq_p_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavxq_p_sv8hi (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavq_p_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsldavq_p_sv8hi (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavxq_p_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsldavxq_p_sv8hi (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_n_u16 (uint16x8_t __inactive, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_n_uv8hi (__inactive, __imm, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_u16 (uint64_t __a, uint16x8_t __b, uint16x8_t __c)
{
  return __builtin_mve_vmlaldavaq_uv8hi (__a, __b, __c);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_p_u16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavq_p_uv8hi (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_n_s32 (int32x4_t __inactive, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_n_sv4si (__inactive, __imm, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_s32 (int64_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vmlaldavaq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaxq_s32 (int64_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vmlaldavaxq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaq_s32 (int64_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vmlsldavaq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaxq_s32 (int64_t __a, int32x4_t __b, int32x4_t __c)
{
  return __builtin_mve_vmlsldavaxq_sv4si (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavxq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavxq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsldavq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavxq_p_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlsldavxq_p_sv4si (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m_n_u32 (uint32x4_t __inactive, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_n_uv4si (__inactive, __imm, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_u32 (uint64_t __a, uint32x4_t __b, uint32x4_t __c)
{
  return __builtin_mve_vmlaldavaq_uv4si (__a, __b, __c);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_p_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavq_p_uv4si (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m_n_s8 (int8x16_t __a, int8x16_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsriq_m_n_sv16qi (__a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_m_n_s8 (uint8x16_t __inactive, int8x16_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vqshluq_m_n_sv16qi (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p_s8 (uint32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vabavq_p_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsriq_m_n_uv16qi (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p_u8 (uint32_t __a, uint8x16_t __b, uint8x16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vabavq_p_uv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m_n_s16 (int16x8_t __a, int16x8_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsriq_m_n_sv8hi (__a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_m_n_s16 (uint16x8_t __inactive, int16x8_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vqshluq_m_n_sv8hi (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p_s16 (uint32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vabavq_p_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsriq_m_n_uv8hi (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p_u16 (uint32_t __a, uint16x8_t __b, uint16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vabavq_p_uv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m_n_s32 (int32x4_t __a, int32x4_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsriq_m_n_sv4si (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_m_n_s32 (uint32x4_t __inactive, int32x4_t __a, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vqshluq_m_n_sv4si (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p_s32 (uint32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vabavq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsriq_m_n_uv4si (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p_u32 (uint32_t __a, uint32x4_t __b, uint32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vabavq_p_uv4si (__a, __b, __c, __p);
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
__arm_vbrsrq_m_n_s8 (int8x16_t __inactive, int8x16_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m_n_s32 (int32x4_t __inactive, int32x4_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m_n_s16 (int16x8_t __inactive, int16x8_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m_n_u8 (uint8x16_t __inactive, uint8x16_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_uv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m_n_u32 (uint32x4_t __inactive, uint32x4_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_uv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m_n_u16 (uint16x8_t __inactive, uint16x8_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_uv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m_u8 (uint8x16_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_uv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_uv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m_u16 (uint16x8_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_uv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m_u8 (uint8x16_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_uv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_uv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m_u16 (uint16x8_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_uv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot270_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot270_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot270_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot90_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot90_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot90_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p_s8 (int32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmladavaq_p_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p_s32 (int32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmladavaq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p_s16 (int32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmladavaq_p_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p_u8 (uint32_t __a, uint8x16_t __b, uint8x16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmladavaq_p_uv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p_u32 (uint32_t __a, uint32x4_t __b, uint32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmladavaq_p_uv4si (__a, __b, __c, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p_u16 (uint32_t __a, uint16x8_t __b, uint16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmladavaq_p_uv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq_p_s8 (int32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmladavaxq_p_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq_p_s32 (int32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmladavaxq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq_p_s16 (int32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmladavaxq_p_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaq_m_n_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaq_m_n_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaq_m_n_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m_n_u8 (uint8x16_t __a, uint8x16_t __b, uint8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaq_m_n_uv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m_n_u32 (uint32x4_t __a, uint32x4_t __b, uint32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaq_m_n_uv4si (__a, __b, __c, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m_n_u16 (uint16x8_t __a, uint16x8_t __b, uint16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaq_m_n_uv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlasq_m_n_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlasq_m_n_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlasq_m_n_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m_n_u8 (uint8x16_t __a, uint8x16_t __b, uint8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlasq_m_n_uv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m_n_u32 (uint32x4_t __a, uint32x4_t __b, uint32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlasq_m_n_uv4si (__a, __b, __c, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m_n_u16 (uint16x8_t __a, uint16x8_t __b, uint16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlasq_m_n_uv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq_p_s8 (int32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavaq_p_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq_p_s32 (int32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavaq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq_p_s16 (int32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavaq_p_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq_p_s8 (int32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavaxq_p_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq_p_s32 (int32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavaxq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq_p_s16 (int32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsdavaxq_p_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m_s8 (int16x8_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m_s32 (int64x2_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m_s16 (int32x4_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m_u8 (uint16x8_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_uv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m_u32 (uint64x2_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_uv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m_u16 (uint32x4_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_uv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m_s8 (int16x8_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m_s32 (int64x2_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m_s16 (int32x4_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m_u8 (uint16x8_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_uv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m_u32 (uint64x2_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_uv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m_u16 (uint32x4_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_uv8hi (__inactive, __a, __b, __p);
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

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmladhq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmladhq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmladhq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmladhxq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmladhxq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmladhxq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq_m_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlahq_m_n_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq_m_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlahq_m_n_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq_m_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlahq_m_n_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlsdhq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlsdhq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlsdhq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlsdhxq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlsdhxq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlsdhxq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmladhq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmladhq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmladhq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmladhxq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmladhxq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmladhxq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq_m_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlahq_m_n_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq_m_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlahq_m_n_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq_m_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlahq_m_n_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq_m_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlashq_m_n_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq_m_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlashq_m_n_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq_m_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlashq_m_n_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq_m_n_s8 (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlashq_m_n_sv16qi (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq_m_n_s16 (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlashq_m_n_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq_m_n_s32 (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vqdmlashq_m_n_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlsdhq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlsdhq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlsdhq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq_m_s8 (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlsdhxq_m_sv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlsdhxq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq_m_s16 (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqrdmlsdhxq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m_n_s8 (int8x16_t __a, int8x16_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsliq_m_n_sv16qi (__a, __b, __imm, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m_n_s32 (int32x4_t __a, int32x4_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsliq_m_n_sv4si (__a, __b, __imm, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m_n_s16 (int16x8_t __a, int16x8_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsliq_m_n_sv8hi (__a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m_n_u8 (uint8x16_t __a, uint8x16_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsliq_m_n_uv16qi (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m_n_u32 (uint32x4_t __a, uint32x4_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsliq_m_n_uv4si (__a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m_n_u16 (uint16x8_t __a, uint16x8_t __b, const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vsliq_m_n_uv8hi (__a, __b, __imm, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_p_s32 (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavaq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_p_s16 (int64_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavaq_p_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_p_u32 (uint64_t __a, uint32x4_t __b, uint32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavaq_p_uv4si (__a, __b, __c, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_p_u16 (uint64_t __a, uint16x8_t __b, uint16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavaq_p_uv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaxq_p_s32 (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavaxq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaxq_p_s16 (int64_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlaldavaxq_p_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaq_p_s32 (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsldavaq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaq_p_s16 (int64_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsldavaq_p_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaxq_p_s32 (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsldavaxq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaxq_p_s16 (int64_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vmlsldavaxq_p_sv8hi (__a, __b, __c, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_m_p8 (uint16x8_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_poly_m_pv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_m_p16 (uint32x4_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_poly_m_pv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_m_p8 (uint16x8_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_poly_m_pv16qi (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_m_p16 (uint32x4_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_poly_m_pv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_m_n_s32 (int64x2_t __inactive, int32x4_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmullbq_m_n_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_m_n_s16 (int32x4_t __inactive, int16x8_t __a, int16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmullbq_m_n_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_m_s32 (int64x2_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmullbq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_m_s16 (int32x4_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmullbq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_m_n_s32 (int64x2_t __inactive, int32x4_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmulltq_m_n_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_m_n_s16 (int32x4_t __inactive, int16x8_t __a, int16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmulltq_m_n_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_m_s32 (int64x2_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmulltq_m_sv4si (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_m_s16 (int32x4_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vqdmulltq_m_sv8hi (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaq_p_s32 (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vrmlaldavhaq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaq_p_u32 (uint64_t __a, uint32x4_t __b, uint32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vrmlaldavhaq_p_uv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaxq_p_s32 (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vrmlaldavhaxq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhaq_p_s32 (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vrmlsldavhaq_p_sv4si (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhaxq_p_s32 (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vrmlsldavhaxq_p_sv4si (__a, __b, __c, __p);
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

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_s8 (int8_t const * __base)
{
  return __builtin_mve_vld1q_sv16qi ((__builtin_neon_qi *) __base);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_s32 (int32_t const * __base)
{
  return __builtin_mve_vld1q_sv4si ((__builtin_neon_si *) __base);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_s16 (int16_t const * __base)
{
  return __builtin_mve_vld1q_sv8hi ((__builtin_neon_hi *) __base);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_u8 (uint8_t const * __base)
{
  return __builtin_mve_vld1q_uv16qi ((__builtin_neon_qi *) __base);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_u32 (uint32_t const * __base)
{
  return __builtin_mve_vld1q_uv4si ((__builtin_neon_si *) __base);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_u16 (uint16_t const * __base)
{
  return __builtin_mve_vld1q_uv8hi ((__builtin_neon_hi *) __base);
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
__arm_vst1q_s8 (int8_t * __addr, int8x16_t __value)
{
  __builtin_mve_vst1q_sv16qi ((__builtin_neon_qi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_s32 (int32_t * __addr, int32x4_t __value)
{
  __builtin_mve_vst1q_sv4si ((__builtin_neon_si *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_s16 (int16_t * __addr, int16x8_t __value)
{
  __builtin_mve_vst1q_sv8hi ((__builtin_neon_hi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_u8 (uint8_t * __addr, uint8x16_t __value)
{
  __builtin_mve_vst1q_uv16qi ((__builtin_neon_qi *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_u32 (uint32_t * __addr, uint32x4_t __value)
{
  __builtin_mve_vst1q_uv4si ((__builtin_neon_si *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_u16 (uint16_t * __addr, uint16x8_t __value)
{
  __builtin_mve_vst1q_uv8hi ((__builtin_neon_hi *) __addr, __value);
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

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_x_p8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_poly_m_pv16qi (__arm_vuninitializedq_u16 (), __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_x_p16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_poly_m_pv8hi (__arm_vuninitializedq_u32 (), __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_sv16qi (__arm_vuninitializedq_s16 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_sv8hi (__arm_vuninitializedq_s32 (), __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_sv4si (__arm_vuninitializedq_s64 (), __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x_u8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_uv16qi (__arm_vuninitializedq_u16 (), __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x_u16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_uv8hi (__arm_vuninitializedq_u32 (), __a, __b, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmullbq_int_m_uv4si (__arm_vuninitializedq_u64 (), __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_x_p8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_poly_m_pv16qi (__arm_vuninitializedq_u16 (), __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_x_p16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_poly_m_pv8hi (__arm_vuninitializedq_u32 (), __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_sv16qi (__arm_vuninitializedq_s16 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_sv8hi (__arm_vuninitializedq_s32 (), __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_sv4si (__arm_vuninitializedq_s64 (), __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x_u8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_uv16qi (__arm_vuninitializedq_u16 (), __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x_u16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_uv8hi (__arm_vuninitializedq_u32 (), __a, __b, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vmulltq_int_m_uv4si (__arm_vuninitializedq_u64 (), __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_sv16qi (__arm_vuninitializedq_s8 (), __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_sv4si (__arm_vuninitializedq_s32 (), __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x_u8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_uv16qi (__arm_vuninitializedq_u8 (), __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x_u16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_uv8hi (__arm_vuninitializedq_u16 (), __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_uv4si (__arm_vuninitializedq_u32 (), __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_sv16qi (__arm_vuninitializedq_s8 (), __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_sv4si (__arm_vuninitializedq_s32 (), __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x_u8 (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_uv16qi (__arm_vuninitializedq_u8 (), __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x_u16 (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_uv8hi (__arm_vuninitializedq_u16 (), __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x_u32 (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_uv4si (__arm_vuninitializedq_u32 (), __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_x_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot90_m_sv16qi (__arm_vuninitializedq_s8 (), __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_x_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot90_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_x_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot90_m_sv4si (__arm_vuninitializedq_s32 (), __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_x_s8 (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot270_m_sv16qi (__arm_vuninitializedq_s8 (), __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_x_s16 (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot270_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_x_s32 (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vhcaddq_rot270_m_sv4si (__arm_vuninitializedq_s32 (), __a, __b, __p);
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
__arm_vbrsrq_x_n_s8 (int8x16_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_sv16qi (__arm_vuninitializedq_s8 (), __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x_n_s16 (int16x8_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_sv8hi (__arm_vuninitializedq_s16 (), __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x_n_s32 (int32x4_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_sv4si (__arm_vuninitializedq_s32 (), __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x_n_u8 (uint8x16_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_uv16qi (__arm_vuninitializedq_u8 (), __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x_n_u16 (uint16x8_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_uv8hi (__arm_vuninitializedq_u16 (), __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x_n_u32 (uint32x4_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_uv4si (__arm_vuninitializedq_u32 (), __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_s8 (int8x16_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_sv16qi (__arm_vuninitializedq_s8 (), __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_s16 (int16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_sv8hi (__arm_vuninitializedq_s16 (), __a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_s32 (int32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_sv4si (__arm_vuninitializedq_s32 (), __a, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_u8 (uint8x16_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_uv16qi (__arm_vuninitializedq_u8 (), __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_u16 (uint16x8_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_uv8hi (__arm_vuninitializedq_u16 (), __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_u32 (uint32x4_t __a, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_uv4si (__arm_vuninitializedq_u32 (), __a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_n_s16 (const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_n_sv8hi (__arm_vuninitializedq_s16 (), __imm, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_n_s32 (const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_n_sv4si (__arm_vuninitializedq_s32 (), __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_n_u16 (const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_n_uv8hi (__arm_vuninitializedq_u16 (), __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x_n_u32 (const int __imm, mve_pred16_t __p)
{
  return __builtin_mve_vmvnq_m_n_uv4si (__arm_vuninitializedq_u32 (), __imm, __p);
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
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | (*__carry << 29));
  int32x4_t __res = __builtin_mve_vadcq_sv4si (__a, __b);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq_u32 (uint32x4_t __a, uint32x4_t __b, unsigned * __carry)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | (*__carry << 29));
  uint32x4_t __res = __builtin_mve_vadcq_uv4si (__a, __b);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | (*__carry << 29));
  int32x4_t __res = __builtin_mve_vadcq_m_sv4si (__inactive, __a, __b, __p);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vadcq_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | (*__carry << 29));
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
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | (*__carry << 29));
  int32x4_t __res = __builtin_mve_vsbcq_sv4si (__a, __b);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq_u32 (uint32x4_t __a, uint32x4_t __b, unsigned * __carry)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | (*__carry << 29));
  uint32x4_t __res =  __builtin_mve_vsbcq_uv4si (__a, __b);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq_m_s32 (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | (*__carry << 29));
  int32x4_t __res = __builtin_mve_vsbcq_m_sv4si (__inactive, __a, __b, __p);
  *__carry = (__builtin_arm_get_fpscr_nzcvqc () >> 29) & 0x1u;
  return __res;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsbcq_m_u32 (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, unsigned * __carry, mve_pred16_t __p)
{
  __builtin_arm_set_fpscr_nzcvqc((__builtin_arm_get_fpscr_nzcvqc () & ~0x20000000u) | (*__carry << 29));
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
__arm_vcmulq_rot90_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmulq_rot90v8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmulq_rot270v8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmulq_rot180v8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcmulqv8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcaddq_rot90v8hf (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_f16 (float16x8_t __a, float16x8_t __b)
{
  return __builtin_mve_vcaddq_rot270v8hf (__a, __b);
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
__arm_vcmulq_rot90_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmulq_rot90v4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmulq_rot270v4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmulq_rot180v4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcmulqv4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcaddq_rot90v4sf (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_f32 (float32x4_t __a, float32x4_t __b)
{
  return __builtin_mve_vcaddq_rot270v4sf (__a, __b);
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

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
  return __builtin_mve_vcmlaqv8hf (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot180_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
  return __builtin_mve_vcmlaq_rot180v8hf (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot270_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
  return __builtin_mve_vcmlaq_rot270v8hf (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot90_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
  return __builtin_mve_vcmlaq_rot90v8hf (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
  return __builtin_mve_vfmaq_fv8hf (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_n_f16 (float16x8_t __a, float16x8_t __b, float16_t __c)
{
  return __builtin_mve_vfmaq_n_fv8hf (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmasq_n_f16 (float16x8_t __a, float16x8_t __b, float16_t __c)
{
  return __builtin_mve_vfmasq_n_fv8hf (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmsq_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
  return __builtin_mve_vfmsq_fv8hf (__a, __b, __c);
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

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_f16 (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_fv8hf (__a, __b, __p);
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

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return __builtin_mve_vcmlaqv4sf (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot180_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return __builtin_mve_vcmlaq_rot180v4sf (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot270_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return __builtin_mve_vcmlaq_rot270v4sf (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot90_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return __builtin_mve_vcmlaq_rot90v4sf (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return __builtin_mve_vfmaq_fv4sf (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_n_f32 (float32x4_t __a, float32x4_t __b, float32_t __c)
{
  return __builtin_mve_vfmaq_n_fv4sf (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmasq_n_f32 (float32x4_t __a, float32x4_t __b, float32_t __c)
{
  return __builtin_mve_vfmasq_n_fv4sf (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmsq_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
  return __builtin_mve_vfmsq_fv4sf (__a, __b, __c);
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

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq_f32 (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vpselq_fv4sf (__a, __b, __p);
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

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m_n_f32 (float32x4_t __inactive, float32x4_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_fv4sf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m_n_f16 (float16x8_t __inactive, float16x8_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_fv8hf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m_f32 (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_fv4sf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m_f16 (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_fv8hf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m_f32 (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_fv4sf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m_f16 (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_fv8hf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_m_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vcmlaq_m_fv4sf (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_m_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vcmlaq_m_fv8hf (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot180_m_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vcmlaq_rot180_m_fv4sf (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot180_m_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vcmlaq_rot180_m_fv8hf (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot270_m_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vcmlaq_rot270_m_fv4sf (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot270_m_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vcmlaq_rot270_m_fv8hf (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot90_m_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vcmlaq_rot90_m_fv4sf (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot90_m_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vcmlaq_rot90_m_fv8hf (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_m_f32 (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_m_fv4sf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_m_f16 (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_m_fv8hf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_m_f32 (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot180_m_fv4sf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_m_f16 (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot180_m_fv8hf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_m_f32 (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot270_m_fv4sf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_m_f16 (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot270_m_fv8hf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_m_f32 (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot90_m_fv4sf (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_m_f16 (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot90_m_fv8hf (__inactive, __a, __b, __p);
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
__arm_vfmaq_m_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vfmaq_m_fv4sf (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_m_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vfmaq_m_fv8hf (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_m_n_f32 (float32x4_t __a, float32x4_t __b, float32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vfmaq_m_n_fv4sf (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_m_n_f16 (float16x8_t __a, float16x8_t __b, float16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vfmaq_m_n_fv8hf (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmasq_m_n_f32 (float32x4_t __a, float32x4_t __b, float32_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vfmasq_m_n_fv4sf (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmasq_m_n_f16 (float16x8_t __a, float16x8_t __b, float16_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vfmasq_m_n_fv8hf (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmsq_m_f32 (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vfmsq_m_fv4sf (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmsq_m_f16 (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
  return __builtin_mve_vfmsq_m_fv8hf (__a, __b, __c, __p);
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
__arm_vld1q_f32 (float32_t const * __base)
{
  return __builtin_mve_vld1q_fv4sf((__builtin_neon_si *) __base);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q_f16 (float16_t const * __base)
{
  return __builtin_mve_vld1q_fv8hf((__builtin_neon_hi *) __base);
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
__arm_vst1q_f32 (float32_t * __addr, float32x4_t __value)
{
  __builtin_mve_vst1q_fv4sf ((__builtin_neon_si *) __addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q_f16 (float16_t * __addr, float16x8_t __value)
{
  __builtin_mve_vst1q_fv8hf ((__builtin_neon_hi *) __addr, __value);
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

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x_f16 (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_fv8hf (__arm_vuninitializedq_f16 (), __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x_f32 (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot90_m_fv4sf (__arm_vuninitializedq_f32 (), __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x_f16 (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_fv8hf (__arm_vuninitializedq_f16 (), __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x_f32 (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcaddq_rot270_m_fv4sf (__arm_vuninitializedq_f32 (), __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_x_f16 (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_m_fv8hf (__arm_vuninitializedq_f16 (), __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_x_f32 (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_m_fv4sf (__arm_vuninitializedq_f32 (), __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_x_f16 (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot90_m_fv8hf (__arm_vuninitializedq_f16 (), __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_x_f32 (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot90_m_fv4sf (__arm_vuninitializedq_f32 (), __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_x_f16 (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot180_m_fv8hf (__arm_vuninitializedq_f16 (), __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_x_f32 (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot180_m_fv4sf (__arm_vuninitializedq_f32 (), __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_x_f16 (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot270_m_fv8hf (__arm_vuninitializedq_f16 (), __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_x_f32 (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vcmulq_rot270_m_fv4sf (__arm_vuninitializedq_f32 (), __a, __b, __p);
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
__arm_vbrsrq_x_n_f16 (float16x8_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_fv8hf (__arm_vuninitializedq_f16 (), __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x_n_f32 (float32x4_t __a, int32_t __b, mve_pred16_t __p)
{
  return __builtin_mve_vbrsrq_m_n_fv4sf (__arm_vuninitializedq_f32 (), __a, __b, __p);
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

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq (int8x16_t __a)
{
 return __arm_vmvnq_s8 (__a);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq (int16x8_t __a)
{
 return __arm_vmvnq_s16 (__a);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq (int32x4_t __a)
{
 return __arm_vmvnq_s32 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq (uint8x16_t __a)
{
 return __arm_vmvnq_u8 (__a);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq (uint16x8_t __a)
{
 return __arm_vmvnq_u16 (__a);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq (uint32x4_t __a)
{
 return __arm_vmvnq_u32 (__a);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vornq_u8 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vmulltq_int_u8 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vmullbq_int_u8 (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vmladavq_u8 (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90 (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vcaddq_rot90_u8 (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270 (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vcaddq_rot270_u8 (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vbicq_u8 (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq (uint8x16_t __a, int32_t __b)
{
 return __arm_vbrsrq_n_u8 (__a, __b);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq (int8x16_t __a, const int __imm)
{
 return __arm_vqshluq_n_s8 (__a, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (int8x16_t __a, int8x16_t __b)
{
 return __arm_vornq_s8 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int (int8x16_t __a, int8x16_t __b)
{
 return __arm_vmulltq_int_s8 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int (int8x16_t __a, int8x16_t __b)
{
 return __arm_vmullbq_int_s8 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq (int8x16_t __a, int8x16_t __b)
{
 return __arm_vmlsdavxq_s8 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq (int8x16_t __a, int8x16_t __b)
{
 return __arm_vmlsdavq_s8 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq (int8x16_t __a, int8x16_t __b)
{
 return __arm_vmladavxq_s8 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq (int8x16_t __a, int8x16_t __b)
{
 return __arm_vmladavq_s8 (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90 (int8x16_t __a, int8x16_t __b)
{
 return __arm_vhcaddq_rot90_s8 (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270 (int8x16_t __a, int8x16_t __b)
{
 return __arm_vhcaddq_rot270_s8 (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90 (int8x16_t __a, int8x16_t __b)
{
 return __arm_vcaddq_rot90_s8 (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270 (int8x16_t __a, int8x16_t __b)
{
 return __arm_vcaddq_rot270_s8 (__a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq (int8x16_t __a, int32_t __b)
{
 return __arm_vbrsrq_n_s8 (__a, __b);
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

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vmulltq_int_u16 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vmullbq_int_u16 (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vmladavq_u16 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90 (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vcaddq_rot90_u16 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270 (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vcaddq_rot270_u16 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vbicq_u16 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq (uint16x8_t __a, int32_t __b)
{
 return __arm_vbrsrq_n_u16 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq (int16x8_t __a, const int __imm)
{
 return __arm_vqshluq_n_s16 (__a, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vornq_s16 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmulltq_int_s16 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmullbq_int_s16 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmlsdavxq_s16 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmlsdavq_s16 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmladavxq_s16 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmladavq_s16 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90 (int16x8_t __a, int16x8_t __b)
{
 return __arm_vhcaddq_rot90_s16 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270 (int16x8_t __a, int16x8_t __b)
{
 return __arm_vhcaddq_rot270_s16 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90 (int16x8_t __a, int16x8_t __b)
{
 return __arm_vcaddq_rot90_s16 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270 (int16x8_t __a, int16x8_t __b)
{
 return __arm_vcaddq_rot270_s16 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq (int16x8_t __a, int32_t __b)
{
 return __arm_vbrsrq_n_s16 (__a, __b);
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

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vmulltq_int_u32 (__a, __b);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vmullbq_int_u32 (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vmladavq_u32 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90 (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vcaddq_rot90_u32 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270 (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vcaddq_rot270_u32 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vbicq_u32 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq (uint32x4_t __a, int32_t __b)
{
 return __arm_vbrsrq_n_u32 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq (int32x4_t __a, const int __imm)
{
 return __arm_vqshluq_n_s32 (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vornq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vornq_s32 (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmulltq_int_s32 (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmullbq_int_s32 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmlsdavxq_s32 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmlsdavq_s32 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmladavxq_s32 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmladavq_s32 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90 (int32x4_t __a, int32x4_t __b)
{
 return __arm_vhcaddq_rot90_s32 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270 (int32x4_t __a, int32x4_t __b)
{
 return __arm_vhcaddq_rot270_s32 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90 (int32x4_t __a, int32x4_t __b)
{
 return __arm_vcaddq_rot90_s32 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270 (int32x4_t __a, int32x4_t __b)
{
 return __arm_vcaddq_rot270_s32 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq (int32x4_t __a, int32_t __b)
{
 return __arm_vbrsrq_n_s32 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vbicq_s32 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vmulltq_poly_p8 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly (uint8x16_t __a, uint8x16_t __b)
{
 return __arm_vmullbq_poly_p8 (__a, __b);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vmlaldavq_u16 (__a, __b);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint16x8_t __a, const int __imm)
{
 return __arm_vbicq_n_u16 (__a, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vqdmulltq_s16 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq (int16x8_t __a, int16_t __b)
{
 return __arm_vqdmulltq_n_s16 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vqdmullbq_s16 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq (int16x8_t __a, int16_t __b)
{
 return __arm_vqdmullbq_n_s16 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavxq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmlsldavxq_s16 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmlsldavq_s16 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavxq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmlaldavxq_s16 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq (int16x8_t __a, int16x8_t __b)
{
 return __arm_vmlaldavq_s16 (__a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (int16x8_t __a, const int __imm)
{
 return __arm_vbicq_n_s16 (__a, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vmulltq_poly_p16 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly (uint16x8_t __a, uint16x8_t __b)
{
 return __arm_vmullbq_poly_p16 (__a, __b);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vmlaldavq_u32 (__a, __b);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (uint32x4_t __a, const int __imm)
{
 return __arm_vbicq_n_u32 (__a, __imm);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vqdmulltq_s32 (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq (int32x4_t __a, int32_t __b)
{
 return __arm_vqdmulltq_n_s32 (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vqdmullbq_s32 (__a, __b);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq (int32x4_t __a, int32_t __b)
{
 return __arm_vqdmullbq_n_s32 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavxq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmlsldavxq_s32 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmlsldavq_s32 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavxq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmlaldavxq_s32 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vmlaldavq_s32 (__a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbicq (int32x4_t __a, const int __imm)
{
 return __arm_vbicq_n_s32 (__a, __imm);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhq (uint32x4_t __a, uint32x4_t __b)
{
 return __arm_vrmlaldavhq_u32 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhxq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vrmlsldavhxq_s32 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vrmlsldavhq_s32 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhxq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vrmlaldavhxq_s32 (__a, __b);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhq (int32x4_t __a, int32x4_t __b)
{
 return __arm_vrmlaldavhq_s32 (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq (uint32_t __a, int8x16_t __b, int8x16_t __c)
{
 return __arm_vabavq_s8 (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq (uint32_t __a, int16x8_t __b, int16x8_t __c)
{
 return __arm_vabavq_s16 (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq (uint32_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vabavq_s32 (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq (uint32_t __a, uint8x16_t __b, uint8x16_t __c)
{
 return __arm_vabavq_u8 (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq (uint32_t __a, uint16x8_t __b, uint16x8_t __c)
{
 return __arm_vabavq_u16 (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq (uint32_t __a, uint32x4_t __b, uint32x4_t __c)
{
 return __arm_vabavq_u32 (__a, __b, __c);
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

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaq (int64_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vrmlaldavhaq_s32 (__a, __b, __c);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaq (uint64_t __a, uint32x4_t __b, uint32x4_t __c)
{
 return __arm_vrmlaldavhaq_u32 (__a, __b, __c);
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

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_u8 (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_s8 (__a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (uint8x16_t __inactive, uint8x16_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_m_u8 (__inactive, __a, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq (uint8x16_t __a, uint8x16_t __b, uint8_t __c)
{
 return __arm_vmlasq_n_u8 (__a, __b, __c);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq (uint8x16_t __a, uint8x16_t __b, uint8_t __c)
{
 return __arm_vmlaq_n_u8 (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmladavq_p_u8 (__a, __b, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq (uint32_t __a, uint8x16_t __b, uint8x16_t __c)
{
 return __arm_vmladavaq_u8 (__a, __b, __c);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq (uint8x16_t __a, uint8x16_t __b, const int __imm)
{
 return __arm_vsriq_n_u8 (__a, __b, __imm);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq (uint8x16_t __a, uint8x16_t __b, const int __imm)
{
 return __arm_vsliq_n_u8 (__a, __b, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (int8x16_t __inactive, int8x16_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_m_s8 (__inactive, __a, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq_p (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmlsdavxq_p_s8 (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq_p (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmlsdavq_p_s8 (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq_p (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmladavxq_p_s8 (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmladavq_p_s8 (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
 return __arm_vqrdmlsdhxq_s8 (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
 return __arm_vqrdmlsdhq_s8 (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq (int8x16_t __a, int8x16_t __b, int8_t __c)
{
 return __arm_vqrdmlashq_n_s8 (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq (int8x16_t __a, int8x16_t __b, int8_t __c)
{
 return __arm_vqdmlashq_n_s8 (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq (int8x16_t __a, int8x16_t __b, int8_t __c)
{
 return __arm_vqrdmlahq_n_s8 (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
 return __arm_vqrdmladhxq_s8 (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
 return __arm_vqrdmladhq_s8 (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
 return __arm_vqdmlsdhxq_s8 (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
 return __arm_vqdmlsdhq_s8 (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq (int8x16_t __a, int8x16_t __b, int8_t __c)
{
 return __arm_vqdmlahq_n_s8 (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
 return __arm_vqdmladhxq_s8 (__inactive, __a, __b);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq (int8x16_t __inactive, int8x16_t __a, int8x16_t __b)
{
 return __arm_vqdmladhq_s8 (__inactive, __a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq (int32_t __a, int8x16_t __b, int8x16_t __c)
{
 return __arm_vmlsdavaxq_s8 (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq (int32_t __a, int8x16_t __b, int8x16_t __c)
{
 return __arm_vmlsdavaq_s8 (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq (int8x16_t __a, int8x16_t __b, int8_t __c)
{
 return __arm_vmlasq_n_s8 (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq (int8x16_t __a, int8x16_t __b, int8_t __c)
{
 return __arm_vmlaq_n_s8 (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq (int32_t __a, int8x16_t __b, int8x16_t __c)
{
 return __arm_vmladavaxq_s8 (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq (int32_t __a, int8x16_t __b, int8x16_t __c)
{
 return __arm_vmladavaq_s8 (__a, __b, __c);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq (int8x16_t __a, int8x16_t __b, const int __imm)
{
 return __arm_vsriq_n_s8 (__a, __b, __imm);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq (int8x16_t __a, int8x16_t __b, const int __imm)
{
 return __arm_vsliq_n_s8 (__a, __b, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_u16 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_s16 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (uint16x8_t __inactive, uint16x8_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_m_u16 (__inactive, __a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq (uint16x8_t __a, uint16x8_t __b, uint16_t __c)
{
 return __arm_vmlasq_n_u16 (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq (uint16x8_t __a, uint16x8_t __b, uint16_t __c)
{
 return __arm_vmlaq_n_u16 (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmladavq_p_u16 (__a, __b, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq (uint32_t __a, uint16x8_t __b, uint16x8_t __c)
{
 return __arm_vmladavaq_u16 (__a, __b, __c);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq (uint16x8_t __a, uint16x8_t __b, const int __imm)
{
 return __arm_vsriq_n_u16 (__a, __b, __imm);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq (uint16x8_t __a, uint16x8_t __b, const int __imm)
{
 return __arm_vsliq_n_u16 (__a, __b, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (int16x8_t __inactive, int16x8_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_m_s16 (__inactive, __a, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq_p (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmlsdavxq_p_s16 (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq_p (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmlsdavq_p_s16 (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq_p (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmladavxq_p_s16 (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmladavq_p_s16 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
 return __arm_vqrdmlsdhxq_s16 (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
 return __arm_vqrdmlsdhq_s16 (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq (int16x8_t __a, int16x8_t __b, int16_t __c)
{
 return __arm_vqrdmlashq_n_s16 (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq (int16x8_t __a, int16x8_t __b, int16_t __c)
{
 return __arm_vqdmlashq_n_s16 (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq (int16x8_t __a, int16x8_t __b, int16_t __c)
{
 return __arm_vqrdmlahq_n_s16 (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
 return __arm_vqrdmladhxq_s16 (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
 return __arm_vqrdmladhq_s16 (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
 return __arm_vqdmlsdhxq_s16 (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
 return __arm_vqdmlsdhq_s16 (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq (int16x8_t __a, int16x8_t __b, int16_t __c)
{
 return __arm_vqdmlahq_n_s16 (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
 return __arm_vqdmladhxq_s16 (__inactive, __a, __b);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq (int16x8_t __inactive, int16x8_t __a, int16x8_t __b)
{
 return __arm_vqdmladhq_s16 (__inactive, __a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq (int32_t __a, int16x8_t __b, int16x8_t __c)
{
 return __arm_vmlsdavaxq_s16 (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq (int32_t __a, int16x8_t __b, int16x8_t __c)
{
 return __arm_vmlsdavaq_s16 (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq (int16x8_t __a, int16x8_t __b, int16_t __c)
{
 return __arm_vmlasq_n_s16 (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq (int16x8_t __a, int16x8_t __b, int16_t __c)
{
 return __arm_vmlaq_n_s16 (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq (int32_t __a, int16x8_t __b, int16x8_t __c)
{
 return __arm_vmladavaxq_s16 (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq (int32_t __a, int16x8_t __b, int16x8_t __c)
{
 return __arm_vmladavaq_s16 (__a, __b, __c);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq (int16x8_t __a, int16x8_t __b, const int __imm)
{
 return __arm_vsriq_n_s16 (__a, __b, __imm);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq (int16x8_t __a, int16x8_t __b, const int __imm)
{
 return __arm_vsliq_n_s16 (__a, __b, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_u32 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_s32 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (uint32x4_t __inactive, uint32x4_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_m_u32 (__inactive, __a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq (uint32x4_t __a, uint32x4_t __b, uint32_t __c)
{
 return __arm_vmlasq_n_u32 (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq (uint32x4_t __a, uint32x4_t __b, uint32_t __c)
{
 return __arm_vmlaq_n_u32 (__a, __b, __c);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmladavq_p_u32 (__a, __b, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq (uint32_t __a, uint32x4_t __b, uint32x4_t __c)
{
 return __arm_vmladavaq_u32 (__a, __b, __c);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq (uint32x4_t __a, uint32x4_t __b, const int __imm)
{
 return __arm_vsriq_n_u32 (__a, __b, __imm);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq (uint32x4_t __a, uint32x4_t __b, const int __imm)
{
 return __arm_vsliq_n_u32 (__a, __b, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (int32x4_t __inactive, int32x4_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_m_s32 (__inactive, __a, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavxq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmlsdavxq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmlsdavq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavxq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmladavxq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmladavq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
 return __arm_vqrdmlsdhxq_s32 (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
 return __arm_vqrdmlsdhq_s32 (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq (int32x4_t __a, int32x4_t __b, int32_t __c)
{
 return __arm_vqrdmlashq_n_s32 (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq (int32x4_t __a, int32x4_t __b, int32_t __c)
{
 return __arm_vqdmlashq_n_s32 (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq (int32x4_t __a, int32x4_t __b, int32_t __c)
{
 return __arm_vqrdmlahq_n_s32 (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
 return __arm_vqrdmladhxq_s32 (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
 return __arm_vqrdmladhq_s32 (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
 return __arm_vqdmlsdhxq_s32 (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
 return __arm_vqdmlsdhq_s32 (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq (int32x4_t __a, int32x4_t __b, int32_t __c)
{
 return __arm_vqdmlahq_n_s32 (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
 return __arm_vqdmladhxq_s32 (__inactive, __a, __b);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq (int32x4_t __inactive, int32x4_t __a, int32x4_t __b)
{
 return __arm_vqdmladhq_s32 (__inactive, __a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq (int32_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vmlsdavaxq_s32 (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq (int32_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vmlsdavaq_s32 (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq (int32x4_t __a, int32x4_t __b, int32_t __c)
{
 return __arm_vmlasq_n_s32 (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq (int32x4_t __a, int32x4_t __b, int32_t __c)
{
 return __arm_vmlaq_n_s32 (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq (int32_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vmladavaxq_s32 (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq (int32_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vmladavaq_s32 (__a, __b, __c);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq (int32x4_t __a, int32x4_t __b, const int __imm)
{
 return __arm_vsriq_n_s32 (__a, __b, __imm);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq (int32x4_t __a, int32x4_t __b, const int __imm)
{
 return __arm_vsliq_n_s32 (__a, __b, __imm);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (uint64x2_t __a, uint64x2_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_u64 (__a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (int64x2_t __a, int64x2_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_s64 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaxq (int64_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vrmlaldavhaxq_s32 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhaq (int64_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vrmlsldavhaq_s32 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhaxq (int64_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vrmlsldavhaxq_s32 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vrmlaldavhq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhxq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vrmlaldavhxq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vrmlsldavhq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhxq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vrmlsldavhxq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhq_p (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vrmlaldavhq_p_u32 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (int16x8_t __inactive, const int __imm, mve_pred16_t __p)
{
 return __arm_vmvnq_m_n_s16 (__inactive, __imm, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq (int64_t __a, int16x8_t __b, int16x8_t __c)
{
 return __arm_vmlaldavaq_s16 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaxq (int64_t __a, int16x8_t __b, int16x8_t __c)
{
 return __arm_vmlaldavaxq_s16 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaq (int64_t __a, int16x8_t __b, int16x8_t __c)
{
 return __arm_vmlsldavaq_s16 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaxq (int64_t __a, int16x8_t __b, int16x8_t __c)
{
 return __arm_vmlsldavaxq_s16 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_p (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmlaldavq_p_s16 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavxq_p (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmlaldavxq_p_s16 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavq_p (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmlsldavq_p_s16 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavxq_p (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmlsldavxq_p_s16 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (uint16x8_t __inactive, const int __imm, mve_pred16_t __p)
{
 return __arm_vmvnq_m_n_u16 (__inactive, __imm, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq (uint64_t __a, uint16x8_t __b, uint16x8_t __c)
{
 return __arm_vmlaldavaq_u16 (__a, __b, __c);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_p (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmlaldavq_p_u16 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (int32x4_t __inactive, const int __imm, mve_pred16_t __p)
{
 return __arm_vmvnq_m_n_s32 (__inactive, __imm, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq (int64_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vmlaldavaq_s32 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaxq (int64_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vmlaldavaxq_s32 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaq (int64_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vmlsldavaq_s32 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaxq (int64_t __a, int32x4_t __b, int32x4_t __c)
{
 return __arm_vmlsldavaxq_s32 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmlaldavq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavxq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmlaldavxq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmlsldavq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavxq_p (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmlsldavxq_p_s32 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_m (uint32x4_t __inactive, const int __imm, mve_pred16_t __p)
{
 return __arm_vmvnq_m_n_u32 (__inactive, __imm, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq (uint64_t __a, uint32x4_t __b, uint32x4_t __c)
{
 return __arm_vmlaldavaq_u32 (__a, __b, __c);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavq_p (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmlaldavq_p_u32 (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m (int8x16_t __a, int8x16_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsriq_m_n_s8 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_m (uint8x16_t __inactive, int8x16_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vqshluq_m_n_s8 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p (uint32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
 return __arm_vabavq_p_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m (uint8x16_t __a, uint8x16_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsriq_m_n_u8 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p (uint32_t __a, uint8x16_t __b, uint8x16_t __c, mve_pred16_t __p)
{
 return __arm_vabavq_p_u8 (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m (int16x8_t __a, int16x8_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsriq_m_n_s16 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_m (uint16x8_t __inactive, int16x8_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vqshluq_m_n_s16 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p (uint32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
 return __arm_vabavq_p_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m (uint16x8_t __a, uint16x8_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsriq_m_n_u16 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p (uint32_t __a, uint16x8_t __b, uint16x8_t __c, mve_pred16_t __p)
{
 return __arm_vabavq_p_u16 (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m (int32x4_t __a, int32x4_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsriq_m_n_s32 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqshluq_m (uint32x4_t __inactive, int32x4_t __a, const int __imm, mve_pred16_t __p)
{
 return __arm_vqshluq_m_n_s32 (__inactive, __a, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p (uint32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vabavq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsriq_m (uint32x4_t __a, uint32x4_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsriq_m_n_u32 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vabavq_p (uint32_t __a, uint32x4_t __b, uint32x4_t __c, mve_pred16_t __p)
{
 return __arm_vabavq_p_u32 (__a, __b, __c, __p);
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
__arm_vbrsrq_m (int8x16_t __inactive, int8x16_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_m_n_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m (int32x4_t __inactive, int32x4_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_m_n_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m (int16x8_t __inactive, int16x8_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_m_n_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m (uint8x16_t __inactive, uint8x16_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_m_n_u8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m (uint32x4_t __inactive, uint32x4_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_m_n_u32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m (uint16x8_t __inactive, uint16x8_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_m_n_u16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m (uint8x16_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_m_u8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_m_u32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m (uint16x8_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_m_u16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m (uint8x16_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_m_u8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m (uint32x4_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_m_u32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m (uint16x8_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_m_u16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot270_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot270_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot270_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot90_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot90_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot90_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p (int32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
 return __arm_vmladavaq_p_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p (int32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmladavaq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p (int32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmladavaq_p_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p (uint32_t __a, uint8x16_t __b, uint8x16_t __c, mve_pred16_t __p)
{
 return __arm_vmladavaq_p_u8 (__a, __b, __c, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p (uint32_t __a, uint32x4_t __b, uint32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmladavaq_p_u32 (__a, __b, __c, __p);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaq_p (uint32_t __a, uint16x8_t __b, uint16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmladavaq_p_u16 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq_p (int32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
 return __arm_vmladavaxq_p_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq_p (int32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmladavaxq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmladavaxq_p (int32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmladavaxq_p_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
 return __arm_vmlaq_m_n_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
 return __arm_vmlaq_m_n_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
 return __arm_vmlaq_m_n_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m (uint8x16_t __a, uint8x16_t __b, uint8_t __c, mve_pred16_t __p)
{
 return __arm_vmlaq_m_n_u8 (__a, __b, __c, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m (uint32x4_t __a, uint32x4_t __b, uint32_t __c, mve_pred16_t __p)
{
 return __arm_vmlaq_m_n_u32 (__a, __b, __c, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaq_m (uint16x8_t __a, uint16x8_t __b, uint16_t __c, mve_pred16_t __p)
{
 return __arm_vmlaq_m_n_u16 (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
 return __arm_vmlasq_m_n_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
 return __arm_vmlasq_m_n_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
 return __arm_vmlasq_m_n_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m (uint8x16_t __a, uint8x16_t __b, uint8_t __c, mve_pred16_t __p)
{
 return __arm_vmlasq_m_n_u8 (__a, __b, __c, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m (uint32x4_t __a, uint32x4_t __b, uint32_t __c, mve_pred16_t __p)
{
 return __arm_vmlasq_m_n_u32 (__a, __b, __c, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlasq_m (uint16x8_t __a, uint16x8_t __b, uint16_t __c, mve_pred16_t __p)
{
 return __arm_vmlasq_m_n_u16 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq_p (int32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
 return __arm_vmlsdavaq_p_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq_p (int32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmlsdavaq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaq_p (int32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmlsdavaq_p_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq_p (int32_t __a, int8x16_t __b, int8x16_t __c, mve_pred16_t __p)
{
 return __arm_vmlsdavaxq_p_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq_p (int32_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmlsdavaxq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsdavaxq_p (int32_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmlsdavaxq_p_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m (int16x8_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m (int64x2_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m (int32x4_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m (uint16x8_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_m_u8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m (uint64x2_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_m_u32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_m (uint32x4_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_m_u16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m (int16x8_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m (int64x2_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m (int32x4_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m (uint16x8_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_m_u8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m (uint64x2_t __inactive, uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_m_u32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_m (uint32x4_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_m_u16 (__inactive, __a, __b, __p);
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

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vqdmladhq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqdmladhq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqdmladhq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vqdmladhxq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqdmladhxq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmladhxq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqdmladhxq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq_m (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
 return __arm_vqdmlahq_m_n_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq_m (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
 return __arm_vqdmlahq_m_n_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlahq_m (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
 return __arm_vqdmlahq_m_n_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vqdmlsdhq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqdmlsdhq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqdmlsdhq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vqdmlsdhxq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqdmlsdhxq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlsdhxq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqdmlsdhxq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmladhq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmladhq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmladhq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmladhxq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmladhxq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmladhxq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmladhxq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq_m (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
 return __arm_vqrdmlahq_m_n_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq_m (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
 return __arm_vqrdmlahq_m_n_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlahq_m (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
 return __arm_vqrdmlahq_m_n_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq_m (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
 return __arm_vqrdmlashq_m_n_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq_m (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
 return __arm_vqrdmlashq_m_n_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlashq_m (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
 return __arm_vqrdmlashq_m_n_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq_m (int8x16_t __a, int8x16_t __b, int8_t __c, mve_pred16_t __p)
{
 return __arm_vqdmlashq_m_n_s8 (__a, __b, __c, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq_m (int16x8_t __a, int16x8_t __b, int16_t __c, mve_pred16_t __p)
{
 return __arm_vqdmlashq_m_n_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmlashq_m (int32x4_t __a, int32x4_t __b, int32_t __c, mve_pred16_t __p)
{
 return __arm_vqdmlashq_m_n_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmlsdhq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmlsdhq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmlsdhq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq_m (int8x16_t __inactive, int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmlsdhxq_m_s8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq_m (int32x4_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmlsdhxq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqrdmlsdhxq_m (int16x8_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqrdmlsdhxq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m (int8x16_t __a, int8x16_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsliq_m_n_s8 (__a, __b, __imm, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m (int32x4_t __a, int32x4_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsliq_m_n_s32 (__a, __b, __imm, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m (int16x8_t __a, int16x8_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsliq_m_n_s16 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m (uint8x16_t __a, uint8x16_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsliq_m_n_u8 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m (uint32x4_t __a, uint32x4_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsliq_m_n_u32 (__a, __b, __imm, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vsliq_m (uint16x8_t __a, uint16x8_t __b, const int __imm, mve_pred16_t __p)
{
 return __arm_vsliq_m_n_u16 (__a, __b, __imm, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_p (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmlaldavaq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_p (int64_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmlaldavaq_p_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_p (uint64_t __a, uint32x4_t __b, uint32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmlaldavaq_p_u32 (__a, __b, __c, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaq_p (uint64_t __a, uint16x8_t __b, uint16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmlaldavaq_p_u16 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaxq_p (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmlaldavaxq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlaldavaxq_p (int64_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmlaldavaxq_p_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaq_p (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmlsldavaq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaq_p (int64_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmlsldavaq_p_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaxq_p (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vmlsldavaxq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmlsldavaxq_p (int64_t __a, int16x8_t __b, int16x8_t __c, mve_pred16_t __p)
{
 return __arm_vmlsldavaxq_p_s16 (__a, __b, __c, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_m (uint16x8_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_poly_m_p8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_m (uint32x4_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_poly_m_p16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_m (uint16x8_t __inactive, uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_poly_m_p8 (__inactive, __a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_m (uint32x4_t __inactive, uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_poly_m_p16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_m (int64x2_t __inactive, int32x4_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vqdmullbq_m_n_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_m (int32x4_t __inactive, int16x8_t __a, int16_t __b, mve_pred16_t __p)
{
 return __arm_vqdmullbq_m_n_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_m (int64x2_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqdmullbq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmullbq_m (int32x4_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqdmullbq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_m (int64x2_t __inactive, int32x4_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vqdmulltq_m_n_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_m (int32x4_t __inactive, int16x8_t __a, int16_t __b, mve_pred16_t __p)
{
 return __arm_vqdmulltq_m_n_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_m (int64x2_t __inactive, int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vqdmulltq_m_s32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vqdmulltq_m (int32x4_t __inactive, int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vqdmulltq_m_s16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaq_p (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vrmlaldavhaq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaq_p (uint64_t __a, uint32x4_t __b, uint32x4_t __c, mve_pred16_t __p)
{
 return __arm_vrmlaldavhaq_p_u32 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlaldavhaxq_p (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vrmlaldavhaxq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhaq_p (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vrmlsldavhaq_p_s32 (__a, __b, __c, __p);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vrmlsldavhaxq_p (int64_t __a, int32x4_t __b, int32x4_t __c, mve_pred16_t __p)
{
 return __arm_vrmlsldavhaxq_p_s32 (__a, __b, __c, __p);
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

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q (int8_t const * __base)
{
 return __arm_vld1q_s8 (__base);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q (int32_t const * __base)
{
 return __arm_vld1q_s32 (__base);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q (int16_t const * __base)
{
 return __arm_vld1q_s16 (__base);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q (uint8_t const * __base)
{
 return __arm_vld1q_u8 (__base);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q (uint32_t const * __base)
{
 return __arm_vld1q_u32 (__base);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q (uint16_t const * __base)
{
 return __arm_vld1q_u16 (__base);
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
__arm_vst1q (int8_t * __addr, int8x16_t __value)
{
 __arm_vst1q_s8 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q (int32_t * __addr, int32x4_t __value)
{
 __arm_vst1q_s32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q (int16_t * __addr, int16x8_t __value)
{
 __arm_vst1q_s16 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q (uint8_t * __addr, uint8x16_t __value)
{
 __arm_vst1q_u8 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q (uint32_t * __addr, uint32x4_t __value)
{
 __arm_vst1q_u32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q (uint16_t * __addr, uint16x8_t __value)
{
 __arm_vst1q_u16 (__addr, __value);
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

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_x (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_poly_x_p8 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_poly_x (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_poly_x_p16 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_x_s8 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_x_s16 (__a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_x_s32 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_x_u8 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_x_u16 (__a, __b, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmullbq_int_x (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmullbq_int_x_u32 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_x (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_poly_x_p8 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_poly_x (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_poly_x_p16 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_x_s8 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_x_s16 (__a, __b, __p);
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_x_s32 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_x_u8 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_x_u16 (__a, __b, __p);
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmulltq_int_x (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vmulltq_int_x_u32 (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_x_s8 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_x_s16 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_x_s32 (__a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_x_u8 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_x_u16 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_x_u32 (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_x_s8 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_x_s16 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_x_s32 (__a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x (uint8x16_t __a, uint8x16_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_x_u8 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x (uint16x8_t __a, uint16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_x_u16 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x (uint32x4_t __a, uint32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_x_u32 (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_x (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot90_x_s8 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_x (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot90_x_s16 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot90_x (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot90_x_s32 (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_x (int8x16_t __a, int8x16_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot270_x_s8 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_x (int16x8_t __a, int16x8_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot270_x_s16 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vhcaddq_rot270_x (int32x4_t __a, int32x4_t __b, mve_pred16_t __p)
{
 return __arm_vhcaddq_rot270_x_s32 (__a, __b, __p);
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
__arm_vbrsrq_x (int8x16_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_x_n_s8 (__a, __b, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x (int16x8_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_x_n_s16 (__a, __b, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x (int32x4_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_x_n_s32 (__a, __b, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x (uint8x16_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_x_n_u8 (__a, __b, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x (uint16x8_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_x_n_u16 (__a, __b, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x (uint32x4_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_x_n_u32 (__a, __b, __p);
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x (int8x16_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_x_s8 (__a, __p);
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x (int16x8_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_x_s16 (__a, __p);
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x (int32x4_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_x_s32 (__a, __p);
}

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x (uint8x16_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_x_u8 (__a, __p);
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x (uint16x8_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_x_u16 (__a, __p);
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vmvnq_x (uint32x4_t __a, mve_pred16_t __p)
{
 return __arm_vmvnq_x_u32 (__a, __p);
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
__arm_vbrsrq (float16x8_t __a, int32_t __b)
{
 return __arm_vbrsrq_n_f16 (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq (float32x4_t __a, int32_t __b)
{
 return __arm_vbrsrq_n_f32 (__a, __b);
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
__arm_vcmulq_rot90 (float16x8_t __a, float16x8_t __b)
{
 return __arm_vcmulq_rot90_f16 (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270 (float16x8_t __a, float16x8_t __b)
{
 return __arm_vcmulq_rot270_f16 (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180 (float16x8_t __a, float16x8_t __b)
{
 return __arm_vcmulq_rot180_f16 (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq (float16x8_t __a, float16x8_t __b)
{
 return __arm_vcmulq_f16 (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90 (float16x8_t __a, float16x8_t __b)
{
 return __arm_vcaddq_rot90_f16 (__a, __b);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270 (float16x8_t __a, float16x8_t __b)
{
 return __arm_vcaddq_rot270_f16 (__a, __b);
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
__arm_vcmulq_rot90 (float32x4_t __a, float32x4_t __b)
{
 return __arm_vcmulq_rot90_f32 (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270 (float32x4_t __a, float32x4_t __b)
{
 return __arm_vcmulq_rot270_f32 (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180 (float32x4_t __a, float32x4_t __b)
{
 return __arm_vcmulq_rot180_f32 (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq (float32x4_t __a, float32x4_t __b)
{
 return __arm_vcmulq_f32 (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90 (float32x4_t __a, float32x4_t __b)
{
 return __arm_vcaddq_rot90_f32 (__a, __b);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270 (float32x4_t __a, float32x4_t __b)
{
 return __arm_vcaddq_rot270_f32 (__a, __b);
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

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
 return __arm_vcmlaq_f16 (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot180 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
 return __arm_vcmlaq_rot180_f16 (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot270 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
 return __arm_vcmlaq_rot270_f16 (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot90 (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
 return __arm_vcmlaq_rot90_f16 (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
 return __arm_vfmaq_f16 (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq (float16x8_t __a, float16x8_t __b, float16_t __c)
{
 return __arm_vfmaq_n_f16 (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmasq (float16x8_t __a, float16x8_t __b, float16_t __c)
{
 return __arm_vfmasq_n_f16 (__a, __b, __c);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmsq (float16x8_t __a, float16x8_t __b, float16x8_t __c)
{
 return __arm_vfmsq_f16 (__a, __b, __c);
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

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_f16 (__a, __b, __p);
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

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
 return __arm_vcmlaq_f32 (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot180 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
 return __arm_vcmlaq_rot180_f32 (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot270 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
 return __arm_vcmlaq_rot270_f32 (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot90 (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
 return __arm_vcmlaq_rot90_f32 (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
 return __arm_vfmaq_f32 (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq (float32x4_t __a, float32x4_t __b, float32_t __c)
{
 return __arm_vfmaq_n_f32 (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmasq (float32x4_t __a, float32x4_t __b, float32_t __c)
{
 return __arm_vfmasq_n_f32 (__a, __b, __c);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmsq (float32x4_t __a, float32x4_t __b, float32x4_t __c)
{
 return __arm_vfmsq_f32 (__a, __b, __c);
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

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vpselq (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vpselq_f32 (__a, __b, __p);
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

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m (float32x4_t __inactive, float32x4_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_m_n_f32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_m (float16x8_t __inactive, float16x8_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_m_n_f16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_m_f32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_m (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_m_f16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_m_f32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_m (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_m_f16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_m (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
 return __arm_vcmlaq_m_f32 (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_m (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
 return __arm_vcmlaq_m_f16 (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot180_m (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
 return __arm_vcmlaq_rot180_m_f32 (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot180_m (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
 return __arm_vcmlaq_rot180_m_f16 (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot270_m (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
 return __arm_vcmlaq_rot270_m_f32 (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot270_m (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
 return __arm_vcmlaq_rot270_m_f16 (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot90_m (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
 return __arm_vcmlaq_rot90_m_f32 (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmlaq_rot90_m (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
 return __arm_vcmlaq_rot90_m_f16 (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_m (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_m_f32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_m (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_m_f16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_m (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot180_m_f32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_m (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot180_m_f16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_m (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot270_m_f32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_m (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot270_m_f16 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_m (float32x4_t __inactive, float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot90_m_f32 (__inactive, __a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_m (float16x8_t __inactive, float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot90_m_f16 (__inactive, __a, __b, __p);
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
__arm_vfmaq_m (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
 return __arm_vfmaq_m_f32 (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_m (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
 return __arm_vfmaq_m_f16 (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_m (float32x4_t __a, float32x4_t __b, float32_t __c, mve_pred16_t __p)
{
 return __arm_vfmaq_m_n_f32 (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmaq_m (float16x8_t __a, float16x8_t __b, float16_t __c, mve_pred16_t __p)
{
 return __arm_vfmaq_m_n_f16 (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmasq_m (float32x4_t __a, float32x4_t __b, float32_t __c, mve_pred16_t __p)
{
 return __arm_vfmasq_m_n_f32 (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmasq_m (float16x8_t __a, float16x8_t __b, float16_t __c, mve_pred16_t __p)
{
 return __arm_vfmasq_m_n_f16 (__a, __b, __c, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmsq_m (float32x4_t __a, float32x4_t __b, float32x4_t __c, mve_pred16_t __p)
{
 return __arm_vfmsq_m_f32 (__a, __b, __c, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vfmsq_m (float16x8_t __a, float16x8_t __b, float16x8_t __c, mve_pred16_t __p)
{
 return __arm_vfmsq_m_f16 (__a, __b, __c, __p);
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

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q (float32_t const * __base)
{
 return __arm_vld1q_f32 (__base);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vld1q (float16_t const * __base)
{
 return __arm_vld1q_f16 (__base);
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
__arm_vst1q (float32_t * __addr, float32x4_t __value)
{
 __arm_vst1q_f32 (__addr, __value);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vst1q (float16_t * __addr, float16x8_t __value)
{
 __arm_vst1q_f16 (__addr, __value);
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
__arm_vcaddq_rot90_x (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_x_f16 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot90_x (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot90_x_f32 (__a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_x_f16 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcaddq_rot270_x (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcaddq_rot270_x_f32 (__a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_x (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_x_f16 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_x (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_x_f32 (__a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_x (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot90_x_f16 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot90_x (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot90_x_f32 (__a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_x (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot180_x_f16 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot180_x (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot180_x_f32 (__a, __b, __p);
}

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_x (float16x8_t __a, float16x8_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot270_x_f16 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vcmulq_rot270_x (float32x4_t __a, float32x4_t __b, mve_pred16_t __p)
{
 return __arm_vcmulq_rot270_x_f32 (__a, __b, __p);
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
__arm_vbrsrq_x (float16x8_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_x_n_f16 (__a, __b, __p);
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vbrsrq_x (float32x4_t __a, int32_t __b, mve_pred16_t __p)
{
 return __arm_vbrsrq_x_n_f32 (__a, __b, __p);
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
	_Float16: __ARM_mve_type_fp_n, \
	__fp16: __ARM_mve_type_fp_n, \
	float: __ARM_mve_type_fp_n, \
	double: __ARM_mve_type_fp_n, \
	long long: __ARM_mve_type_int_n, \
	unsigned char: __ARM_mve_type_int_n, \
	unsigned short: __ARM_mve_type_int_n, \
	unsigned int: __ARM_mve_type_int_n, \
	unsigned long: __ARM_mve_type_int_n, \
	unsigned long long: __ARM_mve_type_int_n, \
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
	default: __ARM_mve_unsupported_type))
#endif /* MVE Floating point.  */

extern void *__ARM_undef;
#define __ARM_mve_coerce(param, type) \
    _Generic(param, type: param, default: *(type *)__ARM_undef)
#define __ARM_mve_coerce1(param, type) \
    _Generic(param, type: param, const type: param, default: *(type *)__ARM_undef)
#define __ARM_mve_coerce2(param, type) \
    _Generic(param, type: param, __fp16: param, default: _Generic (param, _Float16: param, float16_t: param, float32_t: param, default: *(type *)__ARM_undef))
#define __ARM_mve_coerce3(param, type) \
    _Generic(param, type: param, default: _Generic (param, int8_t: param, int16_t: param, int32_t: param, int64_t: param, uint8_t: param, uint16_t: param, uint32_t: param, uint64_t: param, default: *(type *)__ARM_undef))

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */

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

#define __arm_vcvtbq_f32(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vcvtbq_f32_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define __arm_vcvttq_f32(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vcvttq_f32_f16 (__ARM_mve_coerce(__p0, float16x8_t)));})

#define __arm_vmvnq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vmvnq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vmvnq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vmvnq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vmvnq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vmvnq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vmvnq_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

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
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vbicq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce1 (__p1, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vbicq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce1 (__p1, int)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vbicq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce1 (__p1, int)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vbicq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce1 (__p1, int)), \
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

#define __arm_vcmulq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define __arm_vcmulq_rot180(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot180_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot180_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define __arm_vcmulq_rot270(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot270_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot270_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

#define __arm_vcmulq_rot90(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot90_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot90_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t)));})

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

#define __arm_vqshluq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqshluq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqshluq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqshluq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1));})

#define __arm_vmlaldavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vqdmulltq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmulltq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce3(p1, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmulltq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce3(p1, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmulltq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmulltq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vqdmullbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmullbq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce3(p1, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmullbq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce3(p1, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmullbq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmullbq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vmulltq_poly(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_poly_p8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_poly_p16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define __arm_vmullbq_poly(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_poly_p8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_poly_p16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define __arm_vmulltq_int(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulltq_int_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulltq_int_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulltq_int_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_int_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_int_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulltq_int_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vhcaddq_rot270(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot270_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot270_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot270_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vhcaddq_rot90(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot90_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot90_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot90_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vmullbq_int(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmullbq_int_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmullbq_int_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmullbq_int_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_int_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_int_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmullbq_int_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

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

#define __arm_vmlaq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vsriq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vsriq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vsriq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsriq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vsriq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vsriq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsriq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vsliq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vsliq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vsliq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsliq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vsliq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vsliq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsliq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vqrdmlsdhxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmlsdhxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmlsdhxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmlsdhxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqrdmlsdhq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmlsdhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmlsdhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmlsdhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqrdmlashq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqrdmlashq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqrdmlashq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
	    int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqrdmlashq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vqdmlashq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqdmlashq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmlashq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
	    int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmlashq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vqrdmlahq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqrdmlahq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqrdmlahq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqrdmlahq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vmlasq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vqdmlahq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqdmlahq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmlahq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmlahq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vqrdmladhxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmladhxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmladhxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmladhxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqrdmladhq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmladhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmladhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmladhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqdmlsdhxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmlsdhxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmlsdhxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmlsdhxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqdmlsdhq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmlsdhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmlsdhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmlsdhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqdmladhxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmladhxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmladhxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmladhxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqdmladhq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmladhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmladhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmladhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

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

#define __arm_vcmlaq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmlaq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmlaq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vcmlaq_rot180(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmlaq_rot180_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmlaq_rot180_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vcmlaq_rot270(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmlaq_rot270_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmlaq_rot270_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vcmlaq_rot90(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmlaq_rot90_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmlaq_rot90_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t)));})

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

#define __arm_vfmaq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_fp_n]: __arm_vfmaq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce2(p2, double)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_fp_n]: __arm_vfmaq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce2(p2, double)), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vfmaq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vfmaq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vfmsq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vfmsq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vfmsq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vfmasq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_fp_n]: __arm_vfmasq_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce2(p2, double)), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_fp_n]: __arm_vfmasq_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce2(p2, double)));})

#define __arm_vpselq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vpselq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vpselq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vpselq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_int64x2_t][__ARM_mve_type_int64x2_t]: __arm_vpselq_s64 (__ARM_mve_coerce(__p0, int64x2_t), __ARM_mve_coerce(__p1, int64x2_t), p2), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vpselq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vpselq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vpselq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint64x2_t][__ARM_mve_type_uint64x2_t]: __arm_vpselq_u64 (__ARM_mve_coerce(__p0, uint64x2_t), __ARM_mve_coerce(__p1, uint64x2_t), p2), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vpselq_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vpselq_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2));})

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

#define __arm_vbrsrq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbrsrq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2, p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbrsrq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbrsrq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbrsrq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbrsrq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbrsrq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vbrsrq_m_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vbrsrq_m_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), p2, p3));})

#define __arm_vcaddq_rot270_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot270_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot270_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot270_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot270_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot270_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot270_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcaddq_rot270_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcaddq_rot270_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcaddq_rot90_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot90_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot90_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot90_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot90_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot90_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot90_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcaddq_rot90_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcaddq_rot90_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmlaq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmlaq_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmlaq_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmlaq_rot180_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmlaq_rot180_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmlaq_rot180_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmlaq_rot270_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmlaq_rot270_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmlaq_rot270_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmlaq_rot90_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmlaq_rot90_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmlaq_rot90_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmulq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmulq_rot180_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot180_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot180_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmulq_rot270_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot270_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot270_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmulq_rot90_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)] [__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot90_m_f16(__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot90_m_f32(__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vfmaq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vfmaq_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vfmaq_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_fp_n]: __arm_vfmaq_m_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce2(p2, double), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_fp_n]: __arm_vfmaq_m_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce2(p2, double), p3));})

#define __arm_vfmasq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_fp_n]: __arm_vfmasq_m_n_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce2(p2, double), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_fp_n]: __arm_vfmasq_m_n_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce2(p2, double), p3));})

#define __arm_vfmsq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vfmsq_m_f16 (__ARM_mve_coerce(__p0, float16x8_t), __ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vfmsq_m_f32 (__ARM_mve_coerce(__p0, float32x4_t), __ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

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

#define __arm_vld1q(p0) (\
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld1q_s8 (__ARM_mve_coerce1(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld1q_s16 (__ARM_mve_coerce1(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld1q_s32 (__ARM_mve_coerce1(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld1q_u8 (__ARM_mve_coerce1(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld1q_u16 (__ARM_mve_coerce1(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld1q_u32 (__ARM_mve_coerce1(p0, uint32_t *)), \
  int (*)[__ARM_mve_type_float16_t_ptr]: __arm_vld1q_f16 (__ARM_mve_coerce1(p0, float16_t *)), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vld1q_f32 (__ARM_mve_coerce1(p0, float32_t *))))

#define __arm_vld1q_z(p0,p1) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld1q_z_s8 (__ARM_mve_coerce1(p0, int8_t *), p1), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld1q_z_s16 (__ARM_mve_coerce1(p0, int16_t *), p1), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld1q_z_s32 (__ARM_mve_coerce1(p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld1q_z_u8 (__ARM_mve_coerce1(p0, uint8_t *), p1), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld1q_z_u16 (__ARM_mve_coerce1(p0, uint16_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld1q_z_u32 (__ARM_mve_coerce1(p0, uint32_t *), p1), \
  int (*)[__ARM_mve_type_float16_t_ptr]: __arm_vld1q_z_f16 (__ARM_mve_coerce1(p0, float16_t *), p1), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vld1q_z_f32 (__ARM_mve_coerce1(p0, float32_t *), p1)))

#define __arm_vld2q(p0) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld2q_s8 (__ARM_mve_coerce1(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld2q_s16 (__ARM_mve_coerce1(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld2q_s32 (__ARM_mve_coerce1(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld2q_u8 (__ARM_mve_coerce1(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld2q_u16 (__ARM_mve_coerce1(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld2q_u32 (__ARM_mve_coerce1(p0, uint32_t *)), \
  int (*)[__ARM_mve_type_float16_t_ptr]: __arm_vld2q_f16 (__ARM_mve_coerce1(p0, float16_t *)), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vld2q_f32 (__ARM_mve_coerce1(p0, float32_t *))))

#define __arm_vld4q(p0) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld4q_s8 (__ARM_mve_coerce1(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld4q_s16 (__ARM_mve_coerce1(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld4q_s32 (__ARM_mve_coerce1(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld4q_u8 (__ARM_mve_coerce1(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld4q_u16 (__ARM_mve_coerce1(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld4q_u32 (__ARM_mve_coerce1(p0, uint32_t *)), \
  int (*)[__ARM_mve_type_float16_t_ptr]: __arm_vld4q_f16 (__ARM_mve_coerce1(p0, float16_t *)), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vld4q_f32 (__ARM_mve_coerce1(p0, float32_t *))))

#define __arm_vldrhq_gather_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_s16 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_s32 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_u16 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_u32 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_f16 (__ARM_mve_coerce1(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define __arm_vldrhq_gather_offset_z(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_s16 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_z_s32 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_u16 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_z_u32 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_f16 (__ARM_mve_coerce1(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2));})

#define __arm_vldrhq_gather_shifted_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_s16 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_s32 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_u16 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_u32 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_f16 (__ARM_mve_coerce1(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define __arm_vldrhq_gather_shifted_offset_z(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_s16 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_z_s32 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_u16 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_z_u32 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_f16 (__ARM_mve_coerce1(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2));})

#define __arm_vldrwq_gather_offset(p0,p1) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_offset_s32 (__ARM_mve_coerce1(p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_offset_u32 (__ARM_mve_coerce1(p0, uint32_t *), p1), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vldrwq_gather_offset_f32 (__ARM_mve_coerce1(p0, float32_t *), p1)))

#define __arm_vldrwq_gather_offset_z(p0,p1,p2) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_offset_z_s32 (__ARM_mve_coerce1(p0, int32_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_offset_z_u32 (__ARM_mve_coerce1(p0, uint32_t *), p1, p2), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vldrwq_gather_offset_z_f32 (__ARM_mve_coerce1(p0, float32_t *), p1, p2)))

#define __arm_vldrwq_gather_shifted_offset(p0,p1) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_shifted_offset_s32 (__ARM_mve_coerce1(p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_shifted_offset_u32 (__ARM_mve_coerce1(p0, uint32_t *), p1), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vldrwq_gather_shifted_offset_f32 (__ARM_mve_coerce1(p0, float32_t *), p1)))

#define __arm_vldrwq_gather_shifted_offset_z(p0,p1,p2) ( \
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_s32 (__ARM_mve_coerce1(p0, int32_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_u32 (__ARM_mve_coerce1(p0, uint32_t *), p1, p2), \
  int (*)[__ARM_mve_type_float32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_f32 (__ARM_mve_coerce1(p0, float32_t *), p1, p2)))

#define __arm_vst1q_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vst1q_p_s8 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vst1q_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vst1q_p_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vst1q_p_u8 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vst1q_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vst1q_p_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8_t]: __arm_vst1q_p_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vst1q_p_f32 (__ARM_mve_coerce(p0, float32_t *), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vst2q(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x2_t]: __arm_vst2q_s8 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x2_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x2_t]: __arm_vst2q_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x2_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x2_t]: __arm_vst2q_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x2_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x2_t]: __arm_vst2q_u8 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x2_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x2_t]: __arm_vst2q_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x2_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x2_t]: __arm_vst2q_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x2_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8x2_t]: __arm_vst2q_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, float16x8x2_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4x2_t]: __arm_vst2q_f32 (__ARM_mve_coerce(p0, float32_t *), __ARM_mve_coerce(__p1, float32x4x2_t)));})

#define __arm_vst1q(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vst1q_s8 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vst1q_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vst1q_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vst1q_u8 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vst1q_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vst1q_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8_t]: __arm_vst1q_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, float16x8_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vst1q_f32 (__ARM_mve_coerce(p0, float32_t *), __ARM_mve_coerce(__p1, float32x4_t)));})

#define __arm_vstrhq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrhq_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrhq_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8_t]: __arm_vstrhq_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, float16x8_t)));})

#define __arm_vstrhq_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrhq_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrhq_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_float16x8_t]: __arm_vstrhq_p_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, float16x8_t), p2));})

#define __arm_vstrhq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_offset_p_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3));})

#define __arm_vstrhq_scatter_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_offset_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t)));})

#define __arm_vstrhq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3));})

#define __arm_vstrhq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_shifted_offset_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t)));})

#define __arm_vstrwq_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_p_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_p_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_p_f32 (__ARM_mve_coerce(p0, float32_t *), __ARM_mve_coerce(__p1, float32x4_t), p2));})

#define __arm_vstrwq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_f32 (__ARM_mve_coerce(p0, float32_t *), __ARM_mve_coerce(__p1, float32x4_t)));})

#define __arm_vstrhq_scatter_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_offset_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t)));})

#define __arm_vstrhq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_offset_p_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3));})

#define __arm_vstrhq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_shifted_offset_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t)));})

#define __arm_vstrhq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_float16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_f16 (__ARM_mve_coerce(p0, float16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3));})

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
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_offset_s32 (__ARM_mve_coerce(__p0, int32_t *), p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_offset_u32 (__ARM_mve_coerce(__p0, uint32_t *), p1, __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_offset_f32 (__ARM_mve_coerce(__p0, float32_t *), p1, __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vstrwq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_offset_p_s32 (__ARM_mve_coerce(__p0, int32_t *), p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_offset_p_u32 (__ARM_mve_coerce(__p0, uint32_t *), p1, __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_offset_p_f32 (__ARM_mve_coerce(__p0, float32_t *), p1, __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vstrwq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_s32 (__ARM_mve_coerce(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_u32 (__ARM_mve_coerce(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_shifted_offset_f32 (__ARM_mve_coerce(p0, float32_t *), __p1, __ARM_mve_coerce(__p2, float32x4_t)));})

#define __arm_vstrwq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_f32 (__ARM_mve_coerce(p0, float32_t *), __p1, __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vstrwq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_f32 (__ARM_mve_coerce(p0, float32_t *), __p1, __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vstrwq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_s32 (__ARM_mve_coerce(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_u32 (__ARM_mve_coerce(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t)), \
  int (*)[__ARM_mve_type_float32_t_ptr][__ARM_mve_type_float32x4_t]: __arm_vstrwq_scatter_shifted_offset_f32 (__ARM_mve_coerce(p0, float32_t *), __p1, __ARM_mve_coerce(__p2, float32x4_t)));})

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

#define __arm_vbrsrq_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vbrsrq_x_n_s8 (__ARM_mve_coerce(__p1, int8x16_t), p2, p3), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vbrsrq_x_n_s16 (__ARM_mve_coerce(__p1, int16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vbrsrq_x_n_s32 (__ARM_mve_coerce(__p1, int32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vbrsrq_x_n_u8 (__ARM_mve_coerce(__p1, uint8x16_t), p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vbrsrq_x_n_u16 (__ARM_mve_coerce(__p1, uint16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vbrsrq_x_n_u32 (__ARM_mve_coerce(__p1, uint32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_float16x8_t]: __arm_vbrsrq_x_n_f16 (__ARM_mve_coerce(__p1, float16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_float32x4_t]: __arm_vbrsrq_x_n_f32 (__ARM_mve_coerce(__p1, float32x4_t), p2, p3));})

#define __arm_vcaddq_rot270_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot270_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot270_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot270_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot270_x_u8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot270_x_u16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot270_x_u32 (__ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcaddq_rot270_x_f16 (__ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcaddq_rot270_x_f32 (__ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcaddq_rot90_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot90_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot90_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot90_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot90_x_u8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot90_x_u16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot90_x_u32 (__ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3), \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcaddq_rot90_x_f16 (__ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcaddq_rot90_x_f32 (__ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmulq_rot180_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot180_x_f16 (__ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot180_x_f32 (__ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmulq_rot270_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot270_x_f16 (__ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot270_x_f32 (__ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

#define __arm_vcmulq_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_x_f16 (__ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_x_f32 (__ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

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

#define __arm_vcmulq_rot90_x(p1,p2,p3)  ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_float16x8_t][__ARM_mve_type_float16x8_t]: __arm_vcmulq_rot90_x_f16 (__ARM_mve_coerce(__p1, float16x8_t), __ARM_mve_coerce(__p2, float16x8_t), p3), \
  int (*)[__ARM_mve_type_float32x4_t][__ARM_mve_type_float32x4_t]: __arm_vcmulq_rot90_x_f32 (__ARM_mve_coerce(__p1, float32x4_t), __ARM_mve_coerce(__p2, float32x4_t), p3));})

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
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int8x16_t]: __arm_vsetq_lane_s8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t]: __arm_vsetq_lane_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t]: __arm_vsetq_lane_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int64x2_t]: __arm_vsetq_lane_s64 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int64x2_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint8x16_t]: __arm_vsetq_lane_u8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t]: __arm_vsetq_lane_u16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t]: __arm_vsetq_lane_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint64x2_t]: __arm_vsetq_lane_u64 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint64x2_t), p2), \
  int (*)[__ARM_mve_type_fp_n][__ARM_mve_type_float16x8_t]: __arm_vsetq_lane_f16 (__ARM_mve_coerce2(p0, double), __ARM_mve_coerce(__p1, float16x8_t), p2), \
  int (*)[__ARM_mve_type_fp_n][__ARM_mve_type_float32x4_t]: __arm_vsetq_lane_f32 (__ARM_mve_coerce2(p0, double), __ARM_mve_coerce(__p1, float32x4_t), p2));})

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
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x4_t]: __arm_vst4q_s8 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x4_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x4_t]: __arm_vst4q_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x4_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x4_t]: __arm_vst4q_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x4_t]: __arm_vst4q_u8 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x4_t]: __arm_vst4q_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x4_t]: __arm_vst4q_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x4_t)));})

#define __arm_vmvnq(p0) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vmvnq_s8 (__ARM_mve_coerce(__p0, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vmvnq_s16 (__ARM_mve_coerce(__p0, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vmvnq_s32 (__ARM_mve_coerce(__p0, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vmvnq_u8 (__ARM_mve_coerce(__p0, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vmvnq_u16 (__ARM_mve_coerce(__p0, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vmvnq_u32 (__ARM_mve_coerce(__p0, uint32x4_t)));})

#define __arm_vqshluq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vqshluq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vqshluq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vqshluq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1));})

#define __arm_vornq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vornq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vornq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vornq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vornq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vornq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vornq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vmulltq_int(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulltq_int_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulltq_int_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulltq_int_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_int_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_int_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulltq_int_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vmullbq_int(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmullbq_int_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmullbq_int_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmullbq_int_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_int_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_int_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmullbq_int_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vhcaddq_rot90(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot90_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot90_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot90_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vhcaddq_rot270(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot270_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot270_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot270_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vcaddq_rot90(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot90_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot90_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot90_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot90_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot90_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot90_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vcaddq_rot270(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot270_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot270_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot270_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot270_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot270_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot270_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vbrsrq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vbrsrq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vbrsrq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vbrsrq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vbrsrq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vbrsrq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vbrsrq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1));})

#define __arm_vbicq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vbicq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce1 (__p1, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vbicq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce1 (__p1, int)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vbicq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce1 (__p1, int)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vbicq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce1 (__p1, int)), \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbicq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbicq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbicq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbicq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbicq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbicq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vmulltq_poly(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_poly_p8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_poly_p16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define __arm_vmullbq_poly(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_poly_p8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_poly_p16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)));})

#define __arm_vmlaldavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vqdmulltq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmulltq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce3(p1, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmulltq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce3(p1, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmulltq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmulltq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vqdmullbq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmullbq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce3(p1, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmullbq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce3(p1, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmullbq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmullbq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

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

#define __arm_vqrdmlsdhq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmlsdhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmlsdhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmlsdhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqrdmlsdhxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmlsdhxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmlsdhxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmlsdhxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vsliq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vsliq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vsliq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsliq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vsliq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vsliq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsliq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vsriq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vsriq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vsriq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsriq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vsriq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vsriq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsriq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vqrdmlashq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqrdmlashq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqrdmlashq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqrdmlashq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vqdmlashq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqdmlashq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmlashq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmlashq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vqrdmlahq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqrdmlahq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqrdmlahq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqrdmlahq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vqrdmladhxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmladhxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmladhxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmladhxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqrdmladhq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmladhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmladhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmladhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqdmlsdhxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmlsdhxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmlsdhxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmlsdhxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vmlaq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vmlaq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vmlasq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vmlasq_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vpselq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vpselq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vpselq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vpselq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_int64x2_t][__ARM_mve_type_int64x2_t]: __arm_vpselq_s64 (__ARM_mve_coerce(__p0, int64x2_t), __ARM_mve_coerce(__p1, int64x2_t), p2), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vpselq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vpselq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vpselq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint64x2_t][__ARM_mve_type_uint64x2_t]: __arm_vpselq_u64 (__ARM_mve_coerce(__p0, uint64x2_t), __ARM_mve_coerce(__p1, uint64x2_t), p2));})

#define __arm_vqdmlahq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqdmlahq_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmlahq_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmlahq_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int)));})

#define __arm_vqdmlsdhq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmlsdhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmlsdhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmlsdhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqdmladhxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmladhxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmladhxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmladhxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vqdmladhq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmladhq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmladhq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmladhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vabavq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vabavq_p_s8(__p0, __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vabavq_p_s16(__p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vabavq_p_s32(__p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vabavq_p_u8(__p0, __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vabavq_p_u16(__p0, __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vabavq_p_u32(__p0, __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

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

#define __arm_vbrsrq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vbrsrq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __p2, p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vbrsrq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vbrsrq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __p2, p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vbrsrq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vbrsrq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vbrsrq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __p2, p3));})

#define __arm_vcaddq_rot270_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot270_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot270_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot270_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot270_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot270_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot270_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vcaddq_rot90_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot90_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot90_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot90_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot90_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot90_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot90_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmladavaq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavaq_p_s8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavaq_p_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavaq_p_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmladavaq_p_u8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmladavaq_p_u16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmladavaq_p_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

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
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_s8 (__ARM_mve_coerce1(p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_s16 (__ARM_mve_coerce1(p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_s32 (__ARM_mve_coerce1(p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_u8 (__ARM_mve_coerce1(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_u16 (__ARM_mve_coerce1(p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_u32 (__ARM_mve_coerce1(p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vstrwq_scatter_base_p(p0,p1,p2,p3) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_base_p_s32 (p0, p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_base_p_u32 (p0, p1, __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vld1q(p0) (\
  _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld1q_s8 (__ARM_mve_coerce1(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld1q_s16 (__ARM_mve_coerce1(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld1q_s32 (__ARM_mve_coerce1(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld1q_u8 (__ARM_mve_coerce1(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld1q_u16 (__ARM_mve_coerce1(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld1q_u32 (__ARM_mve_coerce1(p0, uint32_t *))))

#define __arm_vldrhq_gather_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_s16 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_s32 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_u16 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_u32 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vldrhq_gather_offset_z(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_s16 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_z_s32 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_offset_z_u16 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_offset_z_u32 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vldrhq_gather_shifted_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_s16 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_s32 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_u16 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_u32 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vldrhq_gather_shifted_offset_z(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_s16 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_z_s32 (__ARM_mve_coerce1(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrhq_gather_shifted_offset_z_u16 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrhq_gather_shifted_offset_z_u32 (__ARM_mve_coerce1(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vldrwq_gather_offset(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_offset_s32 (__ARM_mve_coerce1(__p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_offset_u32 (__ARM_mve_coerce1(__p0, uint32_t *), p1));})

#define __arm_vldrwq_gather_offset_z(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_offset_z_s32 (__ARM_mve_coerce1(__p0, int32_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_offset_z_u32 (__ARM_mve_coerce1(__p0, uint32_t *), p1, p2));})

#define __arm_vldrwq_gather_shifted_offset(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_shifted_offset_s32 (__ARM_mve_coerce1(__p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_shifted_offset_u32 (__ARM_mve_coerce1(__p0, uint32_t *), p1));})

#define __arm_vldrwq_gather_shifted_offset_z(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_s32 (__ARM_mve_coerce1(__p0, int32_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vldrwq_gather_shifted_offset_z_u32 (__ARM_mve_coerce1(__p0, uint32_t *), p1, p2));})

#define __arm_vst1q(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vst1q_s8 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vst1q_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vst1q_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vst1q_u8 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vst1q_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vst1q_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vst1q_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vst1q_p_s8 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vst1q_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vst1q_p_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vst1q_p_u8 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vst1q_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vst1q_p_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vst2q(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16x2_t]: __arm_vst2q_s8 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16x2_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8x2_t]: __arm_vst2q_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8x2_t)), \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4x2_t]: __arm_vst2q_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4x2_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16x2_t]: __arm_vst2q_u8 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16x2_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8x2_t]: __arm_vst2q_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8x2_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4x2_t]: __arm_vst2q_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4x2_t)));})

#define __arm_vstrhq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrhq_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrhq_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vstrhq_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrhq_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrhq_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vstrhq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrhq_scatter_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrhq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrhq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})


#define __arm_vstrwq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vstrwq_p(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_p_s32 (__ARM_mve_coerce(p0, int32_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_p_u32 (__ARM_mve_coerce(p0, uint32_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

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
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrhq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_offset_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_offset_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_offset_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_offset_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrhq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrhq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_s16 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce(p0, int16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrhq_scatter_shifted_offset_p_u16 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint16_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrhq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce(p0, uint16_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrwq_scatter_offset(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_offset_s32 (__ARM_mve_coerce(__p0, int32_t *), p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_offset_u32 (__ARM_mve_coerce(__p0, uint32_t *), p1, __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrwq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_offset_p_s32 (__ARM_mve_coerce(__p0, int32_t *), p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_offset_p_u32 (__ARM_mve_coerce(__p0, uint32_t *), p1, __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrwq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_s32 (__ARM_mve_coerce(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_u32 (__ARM_mve_coerce(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrwq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_s32 (__ARM_mve_coerce(p0, int32_t *), __p1, __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrwq_scatter_shifted_offset_p_u32 (__ARM_mve_coerce(p0, uint32_t *), __p1, __ARM_mve_coerce(__p2, uint32x4_t), p3));})

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

#define __arm_vcaddq_rot270_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot270_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot270_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot270_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot270_x_u8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot270_x_u16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot270_x_u32 (__ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vcaddq_rot90_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vcaddq_rot90_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vcaddq_rot90_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vcaddq_rot90_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vcaddq_rot90_x_u8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vcaddq_rot90_x_u16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vcaddq_rot90_x_u32 (__ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmullbq_int_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmullbq_int_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmullbq_int_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmullbq_int_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_int_x_u8( __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_int_x_u16( __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmullbq_int_x_u32( __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmullbq_poly_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_poly_x_p8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_poly_x_p16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3));})

#define __arm_vmulltq_int_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulltq_int_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulltq_int_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulltq_int_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_int_x_u8( __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_int_x_u16( __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulltq_int_x_u32( __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmulltq_poly_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_poly_x_p8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_poly_x_p16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3));})

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

#define __arm_vbrsrq_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vbrsrq_x_n_s8 (__ARM_mve_coerce(__p1, int8x16_t), p2, p3), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vbrsrq_x_n_s16 (__ARM_mve_coerce(__p1, int16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vbrsrq_x_n_s32 (__ARM_mve_coerce(__p1, int32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vbrsrq_x_n_u8 (__ARM_mve_coerce(__p1, uint8x16_t), p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vbrsrq_x_n_u16 (__ARM_mve_coerce(__p1, uint16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vbrsrq_x_n_u32 (__ARM_mve_coerce(__p1, uint32x4_t), p2, p3));})

#define __arm_vld1q_z(p0,p1) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld1q_z_s8 (__ARM_mve_coerce1(p0, int8_t *), p1), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld1q_z_s16 (__ARM_mve_coerce1(p0, int16_t *), p1), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld1q_z_s32 (__ARM_mve_coerce1(p0, int32_t *), p1), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld1q_z_u8 (__ARM_mve_coerce1(p0, uint8_t *), p1), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld1q_z_u16 (__ARM_mve_coerce1(p0, uint16_t *), p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld1q_z_u32 (__ARM_mve_coerce1(p0, uint32_t *), p1)))

#define __arm_vld2q(p0) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld2q_s8 (__ARM_mve_coerce1(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld2q_s16 (__ARM_mve_coerce1(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld2q_s32 (__ARM_mve_coerce1(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld2q_u8 (__ARM_mve_coerce1(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld2q_u16 (__ARM_mve_coerce1(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld2q_u32 (__ARM_mve_coerce1(p0, uint32_t *))))


#define __arm_vld4q(p0) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr]: __arm_vld4q_s8 (__ARM_mve_coerce1(p0, int8_t *)), \
  int (*)[__ARM_mve_type_int16_t_ptr]: __arm_vld4q_s16 (__ARM_mve_coerce1(p0, int16_t *)), \
  int (*)[__ARM_mve_type_int32_t_ptr]: __arm_vld4q_s32 (__ARM_mve_coerce1(p0, int32_t *)), \
  int (*)[__ARM_mve_type_uint8_t_ptr]: __arm_vld4q_u8 (__ARM_mve_coerce1(p0, uint8_t *)), \
  int (*)[__ARM_mve_type_uint16_t_ptr]: __arm_vld4q_u16 (__ARM_mve_coerce1(p0, uint16_t *)), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vld4q_u32 (__ARM_mve_coerce1(p0, uint32_t *))))

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
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int8x16_t]: __arm_vsetq_lane_s8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t]: __arm_vsetq_lane_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t]: __arm_vsetq_lane_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int64x2_t]: __arm_vsetq_lane_s64 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int64x2_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint8x16_t]: __arm_vsetq_lane_u8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t]: __arm_vsetq_lane_u16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t]: __arm_vsetq_lane_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint64x2_t]: __arm_vsetq_lane_u64 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint64x2_t), p2));})

#endif /* MVE Integer.  */



#define __arm_vmvnq_x(p1,p2) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vmvnq_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vmvnq_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vmvnq_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vmvnq_x_u8 (__ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vmvnq_x_u16 (__ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vmvnq_x_u32 (__ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vdwdupq_x_u8(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_x_n_u8 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_x_wb_u8 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4));})

#define __arm_vdwdupq_x_u16(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_x_n_u16 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_x_wb_u16 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4));})

#define __arm_vdwdupq_x_u32(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_x_n_u32 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_x_wb_u32 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4));})

#define __arm_viwdupq_x_u8(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_x_n_u8 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_x_wb_u8 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4));})

#define __arm_viwdupq_x_u16(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_x_n_u16 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_x_wb_u16 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4));})

#define __arm_viwdupq_x_u32(p1,p2,p3,p4) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_x_n_u32 ((uint32_t) __p1, p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_x_wb_u32 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4));})

#define __arm_vidupq_x_u8(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_x_n_u8 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_x_wb_u8 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3));})

#define __arm_vddupq_x_u8(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_x_n_u8 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_x_wb_u8 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3));})

#define __arm_vidupq_x_u16(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_x_n_u16 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_x_wb_u16 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3));})

#define __arm_vddupq_x_u16(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_x_n_u16 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_x_wb_u16 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3));})

#define __arm_vidupq_x_u32(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_x_n_u32 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_x_wb_u32 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3));})

#define __arm_vddupq_x_u32(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_x_n_u32 ((uint32_t) __p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_x_wb_u32 (__ARM_mve_coerce(__p1, uint32_t *), p2, p3));})

#define __arm_vhcaddq_rot270_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot270_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot270_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot270_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vhcaddq_rot90_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot90_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot90_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot90_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

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
  int (*)[__ARM_mve_type_int64_t_ptr]: __arm_vldrdq_gather_offset_s64 (__ARM_mve_coerce1(p0, int64_t *), p1), \
  int (*)[__ARM_mve_type_uint64_t_ptr]: __arm_vldrdq_gather_offset_u64 (__ARM_mve_coerce1(p0, uint64_t *), p1)))

#define __arm_vldrdq_gather_offset_z(p0,p1,p2) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr]: __arm_vldrdq_gather_offset_z_s64 (__ARM_mve_coerce1(p0, int64_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint64_t_ptr]: __arm_vldrdq_gather_offset_z_u64 (__ARM_mve_coerce1(p0, uint64_t *), p1, p2)))

#define __arm_vldrdq_gather_shifted_offset(p0,p1) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr]: __arm_vldrdq_gather_shifted_offset_s64 (__ARM_mve_coerce1(p0, int64_t *), p1), \
  int (*)[__ARM_mve_type_uint64_t_ptr]: __arm_vldrdq_gather_shifted_offset_u64 (__ARM_mve_coerce1(p0, uint64_t *), p1)))

#define __arm_vldrdq_gather_shifted_offset_z(p0,p1,p2) ( _Generic( (int (*)[__ARM_mve_typeid(p0)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr]: __arm_vldrdq_gather_shifted_offset_z_s64 (__ARM_mve_coerce1(p0, int64_t *), p1, p2), \
  int (*)[__ARM_mve_type_uint64_t_ptr]: __arm_vldrdq_gather_shifted_offset_z_u64 (__ARM_mve_coerce1(p0, uint64_t *), p1, p2)))

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
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_z_s8 (__ARM_mve_coerce1(p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_z_s16 (__ARM_mve_coerce1(p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_z_s32 (__ARM_mve_coerce1(p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_z_u8 (__ARM_mve_coerce1(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_z_u16 (__ARM_mve_coerce1(p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_z_u32 (__ARM_mve_coerce1(p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vqrdmlahq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqrdmlahq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqrdmlahq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqrdmlahq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int), p3));})

#define __arm_vqrdmlashq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqrdmlashq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqrdmlashq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqrdmlashq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int), p3));})

#define __arm_vqdmlashq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqdmlashq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmlashq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmlashq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int), p3));})

#define __arm_vsliq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vsliq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t),  p2, p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vsliq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t),  p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsliq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t),  p2, p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vsliq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t),  p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vsliq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t),  p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsliq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t),  p2, p3));})

#define __arm_vqrdmlsdhxq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmlsdhxq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmlsdhxq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmlsdhxq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vqrdmlsdhq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmlsdhq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmlsdhq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmlsdhq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vmlaldavaq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavaq_p_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavaq_p_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmlaldavaq_p_u16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmlaldavaq_p_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmlaldavaxq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavaxq_p_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavaxq_p_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vmlsldavaq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavaq_p_s16(__p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavaq_p_s32(__p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vmlsldavaxq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavaxq_p_s16(__p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavaxq_p_s32(__p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vrmlaldavhaxq_p(p0,p1,p2,p3) __arm_vrmlaldavhaxq_p_s32(p0,p1,p2,p3)

#define __arm_vrmlsldavhaq_p(p0,p1,p2,p3) __arm_vrmlsldavhaq_p_s32(p0,p1,p2,p3)

#define __arm_vrmlsldavhaxq_p(p0,p1,p2,p3) __arm_vrmlsldavhaxq_p_s32(p0,p1,p2,p3)

#define __arm_vqdmladhq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmladhq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmladhq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmladhq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vqdmladhxq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmladhxq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmladhxq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmladhxq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vqdmlsdhq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmlsdhq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmlsdhq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmlsdhq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vqdmlsdhxq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqdmlsdhxq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmlsdhxq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmlsdhxq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vmvnq_m(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmvnq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmvnq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmvnq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmvnq_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmvnq_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmvnq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vmvnq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce1(__p1, int) , p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vmvnq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce1(__p1, int) , p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vmvnq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce1(__p1, int) , p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vmvnq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce1(__p1, int) , p2));})

#define __arm_vqshluq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqshluq_m_n_s8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqshluq_m_n_s16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqshluq_m_n_s32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2, p3));})

#define __arm_vsriq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vsriq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2, p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vsriq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vsriq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2, p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vsriq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vsriq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vsriq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2, p3));})

#define __arm_vhcaddq_rot270_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot270_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot270_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot270_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vhcaddq_rot90_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vhcaddq_rot90_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vhcaddq_rot90_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vhcaddq_rot90_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vmlaq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vmlaq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vmlaq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vmlaq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vmlaq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vmlaq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vmlaq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce3(p2, int), p3));})

#define __arm_vmlasq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vmlasq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vmlasq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vmlasq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vmlasq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vmlasq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vmlasq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce3(p2, int), p3));})

#define __arm_vmullbq_int_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmullbq_int_m_s8 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmullbq_int_m_s16 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int64x2_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmullbq_int_m_s32 (__ARM_mve_coerce(__p0, int64x2_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_int_m_u8 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_int_m_u16 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint64x2_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmullbq_int_m_u32 (__ARM_mve_coerce(__p0, uint64x2_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmulltq_int_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulltq_int_m_s8 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulltq_int_m_s16 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int64x2_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulltq_int_m_s32 (__ARM_mve_coerce(__p0, int64x2_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_int_m_u8 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_int_m_u16 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint64x2_t][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulltq_int_m_u32 (__ARM_mve_coerce(__p0, uint64x2_t), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmulltq_poly_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_poly_m_p8 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_poly_m_p16 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3));})

#define __arm_vqdmlahq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int_n]: __arm_vqdmlahq_m_n_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmlahq_m_n_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmlahq_m_n_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int), p3));})

#define __arm_vqdmullbq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmullbq_m_s16 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int64x2_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmullbq_m_s32 (__ARM_mve_coerce(__p0, int64x2_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmullbq_m_n_s16 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int64x2_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmullbq_m_n_s32 (__ARM_mve_coerce(__p0, int64x2_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int), p3));})

#define __arm_vqdmulltq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int_n]: __arm_vqdmulltq_m_n_s16 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int64x2_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int_n]: __arm_vqdmulltq_m_n_s32 (__ARM_mve_coerce(__p0, int64x2_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce3(p2, int), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqdmulltq_m_s16 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int64x2_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqdmulltq_m_s32 (__ARM_mve_coerce(__p0, int64x2_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vqrdmladhq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmladhq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmladhq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmladhq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vqrdmladhxq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vqrdmladhxq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vqrdmladhxq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vqrdmladhxq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vmlsdavaxq_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavaxq_p_s8 (p0, __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavaxq_p_s16 (p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavaxq_p_s32 (p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vmlsdavaq(p0,p1,p2) ({  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavaq_s8(p0, __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavaq_s16(p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavaq_s32(p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vmlsdavaxq(p0,p1,p2) ({ __typeof(p2) __p2 = (p2); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavaxq_s8(p0, __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavaxq_s16(p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavaxq_s32(p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vmlsdavq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavq_p_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavq_p_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2));})

#define __arm_vmlsdavxq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavxq_p_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavxq_p_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavxq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2));})

#define __arm_vmlsdavaq_p(p0,p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavaq_p_s8(p0, __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavaq_p_s16(p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavaq_p_s32(p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vmladavaxq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavaxq_p_s8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavaxq_p_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavaxq_p_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3));})

#define __arm_vmullbq_poly_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_poly_m_p8 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_poly_m_p16 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3));})

#define __arm_vldrbq_gather_offset(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_s8(__ARM_mve_coerce1(p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_s16(__ARM_mve_coerce1(p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_s32(__ARM_mve_coerce1(p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vldrbq_gather_offset_u8(__ARM_mve_coerce1(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vldrbq_gather_offset_u16(__ARM_mve_coerce1(p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vldrbq_gather_offset_u32(__ARM_mve_coerce1(p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vidupq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
 __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
 int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vidupq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vidupq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vidupq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_m_wb_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3), \
 int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_m_wb_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3), \
 int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_m_wb_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3));})

#define __arm_vddupq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
 __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
 int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vddupq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vddupq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vddupq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), (uint32_t) __p1, p2, p3), \
 int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_m_wb_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3), \
 int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_m_wb_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3), \
 int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_m_wb_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3));})

#define __arm_vidupq_u16(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_n_u16 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_wb_u16 (__ARM_mve_coerce(__p0, uint32_t *), p1));})

#define __arm_vidupq_u32(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_n_u32 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_wb_u32 (__ARM_mve_coerce(__p0, uint32_t *), p1));})

#define __arm_vidupq_u8(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vidupq_n_u8 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vidupq_wb_u8 (__ARM_mve_coerce(__p0, uint32_t *), p1));})

#define __arm_vddupq_u16(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_n_u16 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_wb_u16 (__ARM_mve_coerce(__p0, uint32_t *), p1));})

#define __arm_vddupq_u32(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_n_u32 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_wb_u32 (__ARM_mve_coerce(__p0, uint32_t *), p1));})

#define __arm_vddupq_u8(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vddupq_n_u8 ((uint32_t) __p0, p1), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vddupq_wb_u8 (__ARM_mve_coerce(__p0, uint32_t *), p1));})

#define __arm_viwdupq_m(p0,p1,p2,p3,p4) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_viwdupq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce3(p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_viwdupq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce3(p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_viwdupq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce3(p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_m_wb_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_m_wb_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_m_wb_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4));})

#define __arm_viwdupq_u16(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_n_u16 (__ARM_mve_coerce3(p0, int), p1, (const int) p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_wb_u16 (__ARM_mve_coerce(__p0, uint32_t *), p1, (const int) p2));})

#define __arm_viwdupq_u32(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_n_u32 (__ARM_mve_coerce3(p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_wb_u32 (__ARM_mve_coerce(__p0, uint32_t *), p1, p2));})

#define __arm_viwdupq_u8(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_viwdupq_n_u8 (__ARM_mve_coerce3(p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_viwdupq_wb_u8 (__ARM_mve_coerce(__p0, uint32_t *), p1, p2));})

#define __arm_vdwdupq_m(p0,p1,p2,p3,p4) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_int_n]: __arm_vdwdupq_m_n_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce3(p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_int_n]: __arm_vdwdupq_m_n_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce3(p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_int_n]: __arm_vdwdupq_m_n_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce3(p1, int), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_m_wb_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_m_wb_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_m_wb_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32_t *), p2, p3, p4));})

#define __arm_vdwdupq_u16(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_n_u16 (__ARM_mve_coerce3(p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_wb_u16 (__ARM_mve_coerce(__p0, uint32_t *), p1, p2));})

#define __arm_vdwdupq_u32(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_n_u32 (__ARM_mve_coerce3(p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_wb_u32 (__ARM_mve_coerce(__p0, uint32_t *), p1, p2));})

#define __arm_vdwdupq_u8(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int_n]: __arm_vdwdupq_n_u8 (__ARM_mve_coerce3(p0, int), p1, p2), \
  int (*)[__ARM_mve_type_uint32_t_ptr]: __arm_vdwdupq_wb_u8 (__ARM_mve_coerce(__p0, uint32_t *), p1, p2));})

#define __arm_vshlcq_m(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)])0, \
  int (*)[__ARM_mve_type_int8x16_t]: __arm_vshlcq_m_s8 (__ARM_mve_coerce(__p0, int8x16_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_int16x8_t]: __arm_vshlcq_m_s16 (__ARM_mve_coerce(__p0, int16x8_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_int32x4_t]: __arm_vshlcq_m_s32 (__ARM_mve_coerce(__p0, int32x4_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_uint8x16_t]: __arm_vshlcq_m_u8 (__ARM_mve_coerce(__p0, uint8x16_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_uint16x8_t]: __arm_vshlcq_m_u16 (__ARM_mve_coerce(__p0, uint16x8_t), p1, p2, p3), \
  int (*)[__ARM_mve_type_uint32x4_t]: __arm_vshlcq_m_u32 (__ARM_mve_coerce(__p0, uint32x4_t), p1, p2, p3));})

#define __arm_vabavq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vabavq_s8 (__p0, __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vabavq_s16 (__p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vabavq_s32 (__p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vabavq_u8 (__p0, __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vabavq_u16 (__p0, __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vabavq_u32 (__p0, __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vabavq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vabavq_p_s8(__p0, __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vabavq_p_s16(__p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vabavq_p_s32(__p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vabavq_p_u8(__p0, __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vabavq_p_u16(__p0, __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vabavq_p_u32(__p0, __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmladavaq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavaq_s8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavaq_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavaq_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmladavaq_u8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmladavaq_u16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmladavaq_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vmladavaq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavaq_p_s8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavaq_p_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavaq_p_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmladavaq_p_u8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmladavaq_p_u16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmladavaq_p_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmladavaxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavaxq_s8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavaxq_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavaxq_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmladavaxq_u8 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmladavaxq_u16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmladavaxq_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vmladavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmladavq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmladavq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmladavq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vmladavq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavq_p_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavq_p_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmladavq_p_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmladavq_p_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmladavq_p_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vmladavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmladavxq_u8 (__ARM_mve_coerce(__p0, uint8x16_t), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmladavxq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmladavxq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vmladavxq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmladavxq_p_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmladavxq_p_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmladavxq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2));})

#define __arm_vmlaldavaq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavaq_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavaq_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmlaldavaq_u16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmlaldavaq_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vmlaldavaxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavaxq_s16 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavaxq_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vmlaldavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmlaldavq_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmlaldavq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vmlaldavq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavq_p_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmlaldavq_p_u16 (__ARM_mve_coerce(__p0, uint16x8_t), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmlaldavq_p_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vmlaldavxq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlaldavxq_p_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlaldavxq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2));})

#define __arm_vmlsdavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vmlsdavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmlsdavxq_s8 (__ARM_mve_coerce(__p0, int8x16_t), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsdavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsdavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vmlsldavaq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavaq_s16(__p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavaq_s32(__p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vmlsldavaxq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavaxq_s16(__p0, __ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavaxq_s32(__p0, __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)));})

#define __arm_vmlsldavq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vmlsldavq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavq_p_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2));})

#define __arm_vmlsldavxq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavxq_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavxq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)));})

#define __arm_vmlsldavxq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmlsldavxq_p_s16 (__ARM_mve_coerce(__p0, int16x8_t), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmlsldavxq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2));})

#define __arm_vmullbq_int_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmullbq_int_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmullbq_int_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmullbq_int_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_int_x_u8( __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_int_x_u16( __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmullbq_int_x_u32( __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmullbq_poly_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmullbq_poly_x_p8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmullbq_poly_x_p16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3));})

#define __arm_vmulltq_int_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8x16_t][__ARM_mve_type_int8x16_t]: __arm_vmulltq_int_x_s8 (__ARM_mve_coerce(__p1, int8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int16x8_t][__ARM_mve_type_int16x8_t]: __arm_vmulltq_int_x_s16 (__ARM_mve_coerce(__p1, int16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vmulltq_int_x_s32 (__ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_int_x_u8( __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_int_x_u16( __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vmulltq_int_x_u32( __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vmulltq_poly_x(p1,p2,p3) ({ __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vmulltq_poly_x_p8 (__ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vmulltq_poly_x_p16 (__ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3));})

#define __arm_vrmlaldavhaxq(p0,p1,p2) __arm_vrmlaldavhaxq_s32(p0,p1,p2)

#define __arm_vrmlaldavhq(p0,p1) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrmlaldavhq_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vrmlaldavhq_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vrmlaldavhq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrmlaldavhq_p_s32 (__ARM_mve_coerce(__p0, int32x4_t), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vrmlaldavhq_p_u32 (__ARM_mve_coerce(__p0, uint32x4_t), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vrmlaldavhxq(p0,p1) __arm_vrmlaldavhxq_s32(p0,p1)

#define __arm_vrmlaldavhxq_p(p0,p1,p2) __arm_vrmlaldavhxq_p_s32(p0,p1,p2)

#define __arm_vrmlsldavhaq(p0,p1,p2) __arm_vrmlsldavhaq_s32(p0,p1,p2)

#define __arm_vrmlsldavhaxq(p0,p1,p2) __arm_vrmlsldavhaxq_s32(p0,p1,p2)

#define __arm_vrmlsldavhq(p0,p1) __arm_vrmlsldavhq_s32(p0,p1)

#define __arm_vrmlsldavhq_p(p0,p1,p2) __arm_vrmlsldavhq_p_s32(p0,p1,p2)

#define __arm_vrmlsldavhxq(p0,p1) __arm_vrmlsldavhxq_s32(p0,p1)

#define __arm_vrmlsldavhxq_p(p0,p1,p2) __arm_vrmlsldavhxq_p_s32(p0,p1,p2)

#define __arm_vstrbq(p0,p1) ({ __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vstrbq_s8 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrbq_s16 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int16x8_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrbq_s32 (__ARM_mve_coerce(p0, int8_t *), __ARM_mve_coerce(__p1, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vstrbq_u8 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrbq_u16 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrbq_u32 (__ARM_mve_coerce(p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t)));})

#define __arm_vstrbq_p(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int8x16_t]: __arm_vstrbq_p_s8 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, int8x16_t), p2), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int16x8_t]: __arm_vstrbq_p_s16 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, int16x8_t), p2), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_int32x4_t]: __arm_vstrbq_p_s32 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, int32x4_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t]: __arm_vstrbq_p_u8 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t]: __arm_vstrbq_p_u16 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t), p2), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t]: __arm_vstrbq_p_u32 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t), p2));})

#define __arm_vstrdq_scatter_base(p0,p1,p2) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_base_s64 (p0, p1, __ARM_mve_coerce(__p2, int64x2_t)), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_base_u64 (p0, p1, __ARM_mve_coerce(__p2, uint64x2_t)));})

#define __arm_vstrdq_scatter_base_p(p0,p1,p2,p3) ({ __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_base_p_s64 (p0, p1, __ARM_mve_coerce(__p2, int64x2_t), p3), \
  int (*)[__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_base_p_u64 (p0, p1, __ARM_mve_coerce(__p2, uint64x2_t), p3));})

#define __arm_vrmlaldavhaq(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrmlaldavhaq_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vrmlaldavhaq_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vrmlaldavhaq_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_int32x4_t][__ARM_mve_type_int32x4_t]: __arm_vrmlaldavhaq_p_s32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, int32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_int_n][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vrmlaldavhaq_p_u32 (__ARM_mve_coerce3(p0, int), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrbq_scatter_offset(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vstrbq_scatter_offset_s8 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, int8x16_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrbq_scatter_offset_s16 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t)), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrbq_scatter_offset_s32 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vstrbq_scatter_offset_u8 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrbq_scatter_offset_u16 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t)), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrbq_scatter_offset_u32 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t)));})

#define __arm_vstrbq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p1) __p1 = (p1); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p1)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint8x16_t][__ARM_mve_type_int8x16_t]: __arm_vstrbq_scatter_offset_p_s8 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, int8x16_t), p3), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_int16x8_t]: __arm_vstrbq_scatter_offset_p_s16 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, int16x8_t), p3), \
  int (*)[__ARM_mve_type_int8_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_int32x4_t]: __arm_vstrbq_scatter_offset_p_s32 (__ARM_mve_coerce(__p0, int8_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, int32x4_t), p3), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint8x16_t][__ARM_mve_type_uint8x16_t]: __arm_vstrbq_scatter_offset_p_u8 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint8x16_t), __ARM_mve_coerce(__p2, uint8x16_t), p3), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint16x8_t][__ARM_mve_type_uint16x8_t]: __arm_vstrbq_scatter_offset_p_u16 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint16x8_t), __ARM_mve_coerce(__p2, uint16x8_t), p3), \
  int (*)[__ARM_mve_type_uint8_t_ptr][__ARM_mve_type_uint32x4_t][__ARM_mve_type_uint32x4_t]: __arm_vstrbq_scatter_offset_p_u32 (__ARM_mve_coerce(__p0, uint8_t *), __ARM_mve_coerce(__p1, uint32x4_t), __ARM_mve_coerce(__p2, uint32x4_t), p3));})

#define __arm_vstrdq_scatter_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr][__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_offset_p_s64 (__ARM_mve_coerce(__p0, int64_t *), p1, __ARM_mve_coerce(__p2, int64x2_t), p3), \
  int (*)[__ARM_mve_type_uint64_t_ptr][__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_offset_p_u64 (__ARM_mve_coerce(__p0, uint64_t *), p1, __ARM_mve_coerce(__p2, uint64x2_t), p3));})

#define __arm_vstrdq_scatter_offset(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr][__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_offset_s64 (__ARM_mve_coerce(__p0, int64_t *), p1, __ARM_mve_coerce(__p2, int64x2_t)), \
  int (*)[__ARM_mve_type_uint64_t_ptr][__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_offset_u64 (__ARM_mve_coerce(__p0, uint64_t *), p1, __ARM_mve_coerce(__p2, uint64x2_t)));})

#define __arm_vstrdq_scatter_shifted_offset_p(p0,p1,p2,p3) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr][__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_shifted_offset_p_s64 (__ARM_mve_coerce(__p0, int64_t *), p1, __ARM_mve_coerce(__p2, int64x2_t), p3), \
  int (*)[__ARM_mve_type_uint64_t_ptr][__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_shifted_offset_p_u64 (__ARM_mve_coerce(__p0, uint64_t *), p1, __ARM_mve_coerce(__p2, uint64x2_t), p3));})

#define __arm_vstrdq_scatter_shifted_offset(p0,p1,p2) ({ __typeof(p0) __p0 = (p0); \
  __typeof(p2) __p2 = (p2); \
  _Generic( (int (*)[__ARM_mve_typeid(__p0)][__ARM_mve_typeid(__p2)])0, \
  int (*)[__ARM_mve_type_int64_t_ptr][__ARM_mve_type_int64x2_t]: __arm_vstrdq_scatter_shifted_offset_s64 (__ARM_mve_coerce(__p0, int64_t *), p1, __ARM_mve_coerce(__p2, int64x2_t)), \
  int (*)[__ARM_mve_type_uint64_t_ptr][__ARM_mve_type_uint64x2_t]: __arm_vstrdq_scatter_shifted_offset_u64 (__ARM_mve_coerce(__p0, uint64_t *), p1, __ARM_mve_coerce(__p2, uint64x2_t)));})

#endif /* __cplusplus  */
#endif /* __ARM_FEATURE_MVE  */
#endif /* _GCC_ARM_MVE_H.  */
