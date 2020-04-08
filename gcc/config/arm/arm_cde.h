/* Arm Custom Datapath Extension (CDE) intrinsics include file.

   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Arm Ltd.

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

#ifndef _GCC_ARM_CDE_H
#define _GCC_ARM_CDE_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#if defined (__ARM_FEATURE_CDE)

#if defined (__ARM_FP) || defined (__ARM_FEATURE_MVE)

/* CDE builtins using FPU/MVE registers.  */

/* uint32_t
   __arm_vcx1_u32(int coproc, uint32_t imm);  */
#define __arm_vcx1_u32(coproc, imm) \
	__builtin_arm_vcx1si(coproc, imm)

/* uint32_t
   __arm_vcx1a_u32(int coproc, uint32_t acc, uint32_t imm);  */
#define __arm_vcx1a_u32(coproc, acc, imm) \
	__builtin_arm_vcx1asi(coproc, acc, imm)

/* uint32_t
   __arm_vcx2_u32(int coproc, uint32_t n, uint32_t imm);  */
#define __arm_vcx2_u32(coproc, n, imm) \
	__builtin_arm_vcx2si(coproc, n, imm)

/* uint32_t
   __arm_vcx2a_u32(int coproc, uint32_t acc, uint32_t n, uint32_t imm);  */
#define __arm_vcx2a_u32(coproc, acc, n, imm) \
	__builtin_arm_vcx2asi(coproc, acc, n, imm)

/* uint32_t
   __arm_vcx3_u32(int coproc, uint32_t n, uint32_t m, uint32_t imm);  */
#define __arm_vcx3_u32(coproc, n, m, imm) \
	__builtin_arm_vcx3si(coproc, n, m, imm)

/* uint32_t
   __arm_vcx3a_u32(int coproc, uint32_t acc, uint32_t n, uint32_t m,
		   uint32_t imm);  */
#define __arm_vcx3a_u32(coproc, acc, n, m, imm) \
	__builtin_arm_vcx3asi(coproc, acc, n, m, imm)

/* uint64_t
   __arm_vcx1d_u64(int coproc, uint32_t imm);  */
#define __arm_vcx1d_u64(coproc, imm) \
	__builtin_arm_vcx1di(coproc, imm)

/* uint64_t
   __arm_vcx1da_u64(int coproc, uint64_t acc, uint32_t imm);  */
#define __arm_vcx1da_u64(coproc, acc, imm) \
	__builtin_arm_vcx1adi(coproc, acc, imm)

/* uint64_t
   __arm_vcx2d_u64(int coproc, uint64_t m, uint32_t imm);  */
#define __arm_vcx2d_u64(coproc, m, imm) \
	__builtin_arm_vcx2di(coproc, m, imm)

/* uint64_t
   __arm_vcx2da_u64(int coproc, uint64_t acc, uint64_t m, uint32_t imm);  */
#define __arm_vcx2da_u64(coproc, acc, m, imm) \
	__builtin_arm_vcx2adi(coproc, acc, m, imm)

/* uint64_t
   __arm_vcx3d_u64(int coproc, uint64_t n, uint64_t m, uint32_t imm);  */
#define __arm_vcx3d_u64(coproc, n, m, imm) \
	__builtin_arm_vcx3di(coproc, n, m, imm)

/* uint64_t
   __arm_vcx3da_u64(int coproc, uint64_t acc, uint64_t n, uint64_t m,
		    uint32_t imm);  */
#define __arm_vcx3da_u64(coproc, acc, n, m, imm) \
	__builtin_arm_vcx3adi(coproc, acc, n, m, imm)

#endif /* __ARM_FP || __ARM_FEATURE_MVE.  */
#endif /* __ARM_FEATURE_CDE.  */

#ifdef __cplusplus
}
#endif

#endif
