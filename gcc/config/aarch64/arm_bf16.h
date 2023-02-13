/* Arm BF16 instrinsics include file.

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

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _AARCH64_BF16_H_
#define _AARCH64_BF16_H_

typedef __bf16 bfloat16_t;
typedef float float32_t;

#pragma GCC push_options
#pragma GCC target ("+nothing+bf16+nosimd")

__extension__ extern __inline bfloat16_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvth_bf16_f32 (float32_t __a)
{
  return __builtin_aarch64_bfcvtbf (__a);
}

__extension__ extern __inline float32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
vcvtah_f32_bf16 (bfloat16_t __a)
{
  return __builtin_aarch64_bfcvtsf (__a);
}

#pragma GCC pop_options

#endif
