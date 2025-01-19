/* AArch64 FP8 helper functions.
   Do not include this file directly. Use one of arm_neon.h
   arm_sme.h arm_sve.h instead.

   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

#ifndef _GCC_ARM_PRIVATE_FP8_H
#define _GCC_ARM_PRIVATE_FP8_H

#if !defined(_AARCH64_NEON_H_) && !defined(_ARM_SVE_H_)
#error "This file should not be used standalone. Please include one of arm_neon.h arm_sve.h arm_sme.h instead."
#endif

#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

  typedef __mfp8 mfloat8_t;

  typedef uint64_t fpm_t;

  enum __ARM_FPM_FORMAT
  {
    __ARM_FPM_E5M2,
    __ARM_FPM_E4M3,
  };

  enum __ARM_FPM_OVERFLOW
  {
    __ARM_FPM_INFNAN,
    __ARM_FPM_SATURATE,
  };

#define __arm_fpm_init() (0)

#define __arm_set_fpm_src1_format(__fpm, __format)                             \
  ((__fpm & ~(uint64_t)0x7) | (__format & (uint64_t)0x7))
#define __arm_set_fpm_src2_format(__fpm, __format)                             \
  ((__fpm & ~((uint64_t)0x7 << 3)) | ((__format & (uint64_t)0x7) << 3))
#define __arm_set_fpm_dst_format(__fpm, __format)                              \
  ((__fpm & ~((uint64_t)0x7 << 6)) | ((__format & (uint64_t)0x7) << 6))
#define __arm_set_fpm_overflow_cvt(__fpm, __behaviour)                         \
  ((__fpm & ~((uint64_t)0x1 << 15)) | ((__behaviour & (uint64_t)0x1) << 15))
#define __arm_set_fpm_overflow_mul(__fpm, __behaviour)                         \
  ((__fpm & ~((uint64_t)0x1 << 14)) | ((__behaviour & (uint64_t)0x1) << 14))
#define __arm_set_fpm_lscale(__fpm, __scale)                                   \
  ((__fpm & ~((uint64_t)0x7f << 16)) | ((__scale & (uint64_t)0x7f) << 16))
#define __arm_set_fpm_lscale2(__fpm, __scale)                                  \
  ((__fpm & ~((uint64_t)0x3f << 32)) | ((__scale & (uint64_t)0x3f) << 32))
#define __arm_set_fpm_nscale(__fpm, __scale)                                   \
  ((__fpm & ~((uint64_t)0xff << 24)) | ((__scale & (uint64_t)0xff) << 24))

#ifdef __cplusplus
}
#endif

#endif
