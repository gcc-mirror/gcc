/* Arm MVE intrinsics include file.

   Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#ifndef _GCC_ARM_MVE_TYPES_H
#define _GCC_ARM_MVE_TYPES_H

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */
typedef __fp16 float16_t;
typedef float float32_t;
#endif

#pragma GCC arm "arm_mve_types.h"

__extension__ extern __inline uint8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_u8 (void)
{
  uint8x16_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline uint16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_u16 (void)
{
  uint16x8_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline uint32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_u32 (void)
{
  uint32x4_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline uint64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_u64 (void)
{
  uint64x2_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline int8x16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_s8 (void)
{
  int8x16_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline int16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_s16 (void)
{
  int16x8_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline int32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_s32 (void)
{
  int32x4_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

__extension__ extern __inline int64x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_s64 (void)
{
  int64x2_t __uninit;
  __asm__ ("": "=w"(__uninit));
  return __uninit;
}

#if (__ARM_FEATURE_MVE & 2) /* MVE Floating point.  */

__extension__ extern __inline float16x8_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_f16 (void)
{
  float16x8_t __uninit;
  __asm__ ("": "=w" (__uninit));
  return __uninit;
}

__extension__ extern __inline float32x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_vuninitializedq_f32 (void)
{
  float32x4_t __uninit;
  __asm__ ("": "=w" (__uninit));
  return __uninit;
}

#endif

#ifdef __cplusplus

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
#endif /* __cplusplus */

#endif /* _GCC_ARM_MVE_H.  */
