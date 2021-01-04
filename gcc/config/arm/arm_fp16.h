/* ARM FP16 intrinsics include file.

   Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

#ifndef _GCC_ARM_FP16_H
#define _GCC_ARM_FP16_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* Intrinsics for FP16 instructions.  */
#pragma GCC push_options
#pragma GCC target ("fpu=fp-armv8")

#if defined (__ARM_FEATURE_FP16_SCALAR_ARITHMETIC)

typedef __fp16 float16_t;

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vabsh_f16 (float16_t __a)
{
  return __builtin_neon_vabshf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vaddh_f16 (float16_t __a, float16_t __b)
{
  return __a + __b;
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvtah_s32_f16 (float16_t __a)
{
  return __builtin_neon_vcvtahssi (__a);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvtah_u32_f16 (float16_t __a)
{
  return __builtin_neon_vcvtahusi (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vcvth_f16_s32 (int32_t __a)
{
  return __builtin_neon_vcvthshf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vcvth_f16_u32 (uint32_t __a)
{
  return __builtin_neon_vcvthuhf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vcvth_n_f16_s32 (int32_t __a, const int __b)
{
  return __builtin_neon_vcvths_nhf (__a, __b);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vcvth_n_f16_u32 (uint32_t __a, const int __b)
{
  return __builtin_neon_vcvthu_nhf ((int32_t)__a, __b);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvth_n_s32_f16 (float16_t __a, const int __b)
{
  return __builtin_neon_vcvths_nsi (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvth_n_u32_f16 (float16_t __a, const int __b)
{
  return (uint32_t)__builtin_neon_vcvthu_nsi (__a, __b);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvth_s32_f16 (float16_t __a)
{
  return __builtin_neon_vcvthssi (__a);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvth_u32_f16 (float16_t __a)
{
  return __builtin_neon_vcvthusi (__a);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvtmh_s32_f16 (float16_t __a)
{
  return __builtin_neon_vcvtmhssi (__a);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvtmh_u32_f16 (float16_t __a)
{
  return __builtin_neon_vcvtmhusi (__a);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvtnh_s32_f16 (float16_t __a)
{
  return __builtin_neon_vcvtnhssi (__a);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvtnh_u32_f16 (float16_t __a)
{
  return __builtin_neon_vcvtnhusi (__a);
}

__extension__ static __inline int32_t __attribute__ ((__always_inline__))
vcvtph_s32_f16 (float16_t __a)
{
  return __builtin_neon_vcvtphssi (__a);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
vcvtph_u32_f16 (float16_t __a)
{
  return __builtin_neon_vcvtphusi (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vdivh_f16 (float16_t __a, float16_t __b)
{
  return __a / __b;
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vfmah_f16 (float16_t __a, float16_t __b, float16_t __c)
{
  return __builtin_neon_vfmahf (__a, __b, __c);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vfmsh_f16 (float16_t __a, float16_t __b, float16_t __c)
{
  return __builtin_neon_vfmshf (__a, __b, __c);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vmaxnmh_f16 (float16_t __a, float16_t __b)
{
  return __builtin_neon_vmaxnmhf (__a, __b);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vminnmh_f16 (float16_t __a, float16_t __b)
{
  return __builtin_neon_vminnmhf (__a, __b);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vmulh_f16 (float16_t __a, float16_t __b)
{
  return __a * __b;
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vnegh_f16 (float16_t __a)
{
  return  - __a;
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vrndah_f16 (float16_t __a)
{
  return __builtin_neon_vrndahf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vrndh_f16 (float16_t __a)
{
  return __builtin_neon_vrndhf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vrndih_f16 (float16_t __a)
{
  return __builtin_neon_vrndihf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vrndmh_f16 (float16_t __a)
{
  return __builtin_neon_vrndmhf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vrndnh_f16 (float16_t __a)
{
  return __builtin_neon_vrndnhf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vrndph_f16 (float16_t __a)
{
  return __builtin_neon_vrndphf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vrndxh_f16 (float16_t __a)
{
  return __builtin_neon_vrndxhf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vsqrth_f16 (float16_t __a)
{
  return __builtin_neon_vsqrthf (__a);
}

__extension__ static __inline float16_t __attribute__ ((__always_inline__))
vsubh_f16 (float16_t __a, float16_t __b)
{
  return __a - __b;
}

#endif /* __ARM_FEATURE_FP16_SCALAR_ARITHMETIC  */
#pragma GCC pop_options

#ifdef __cplusplus
}
#endif

#endif
