/* Arm BF16 intrinsics include file.

   Copyright (C) 2019-2025 Free Software Foundation, Inc.

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


#ifndef _GCC_ARM_BF16_H
#define _GCC_ARM_BF16_H 1

#ifdef __cplusplus
extern "C" {
#endif

typedef __bf16 bfloat16_t;
typedef float float32_t;

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtah_f32_bf16 (bfloat16_t __a)
{
  return __builtin_neon_vbfcvtbf (__a);
}

__extension__ extern __inline bfloat16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvth_bf16_f32 (float32_t __a)
{
  return __builtin_neon_vbfcvtsf (__a);
}

#ifdef __cplusplus
}
#endif

#endif
