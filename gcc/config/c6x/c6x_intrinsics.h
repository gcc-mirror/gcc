/* Intrinsics for TI C6X.

   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

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

#ifndef _GCC_C6X_INTRINSICS_H
#define _GCC_C6X_INTRINSICS_H

#if !defined(__TMS320C6X__)
# error "c6x_intrinsics.h is only supported for C6X targets"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* Define vector types.  */
typedef uint8_t __uv4qi __attribute__((vector_size (4)));
typedef int16_t __v2hi __attribute__((vector_size (4)));
typedef int32_t __v2si __attribute__((vector_size (8)));

__extension__ static __inline int __attribute__ ((__always_inline__))
_abs (int src)
{
  return __builtin_c6x_abs (src);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_abs2 (int src)
{
  return (int)__builtin_c6x_abs2 ((__v2hi)src);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_sadd (int src1, int src2)
{
  return __builtin_c6x_sadd (src1, src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_ssub (int src1, int src2)
{
  return __builtin_c6x_ssub (src1, src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_add2 (int src1, int src2)
{
  return (int)__builtin_c6x_add2 ((__v2hi)src1, (__v2hi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_sub2 (int src1, int src2)
{
  return (int)__builtin_c6x_sub2 ((__v2hi)src1, (__v2hi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_add4 (int src1, int src2)
{
  return (int)__builtin_c6x_add4 ((__uv4qi)src1, (__uv4qi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_sub4 (int src1, int src2)
{
  return (int)__builtin_c6x_sub4 ((__uv4qi)src1, (__uv4qi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_sadd2 (int src1, int src2)
{
  return (int)__builtin_c6x_sadd2 ((__v2hi)src1, (__v2hi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_ssub2 (int src1, int src2)
{
  return (int)__builtin_c6x_ssub2 ((__v2hi)src1, (__v2hi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_saddu4 (int src1, int src2)
{
  return (int)__builtin_c6x_saddu4 ((__uv4qi)src1, (__uv4qi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_smpy (int src1, int src2)
{
  return __builtin_c6x_smpy (src1, src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_smpylh (int src1, int src2)
{
  return __builtin_c6x_smpylh (src1, src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_smpyhl (int src1, int src2)
{
  return __builtin_c6x_smpyhl (src1, src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_smpyh (int src1, int src2)
{
  return __builtin_c6x_smpyh (src1, src2);
}

__extension__ static __inline long long __attribute__ ((__always_inline__))
_smpy2ll (int src1, int src2)
{
  return (long long)__builtin_c6x_smpy2 ((__v2hi)src1, (__v2hi)src2);
}

__extension__ static __inline long long __attribute__ ((__always_inline__))
_mpy2ll (int src1, int src2)
{
  return (long long)__builtin_c6x_mpy2 ((__v2hi)src1, (__v2hi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_extr (int src1, int src2)
{
  return __builtin_c6x_extr (src1, src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_extru (int src1, int src2)
{
  return __builtin_c6x_extru (src1, src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_clrr (int src1, int src2)
{
  return __builtin_c6x_clrr (src1, src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_avg2 (int src1, int src2)
{
  return (int)__builtin_c6x_avg2 ((__v2hi)src1, (__v2hi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_avgu4 (int src1, int src2)
{
  return (int)__builtin_c6x_avgu4 ((__uv4qi)src1, (__uv4qi)src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_sshl (int src1, int src2)
{
  return __builtin_c6x_sshl (src1, src2);
}

__extension__ static __inline int __attribute__ ((__always_inline__))
_subc (int src1, int src2)
{
  return __builtin_c6x_subc (src1, src2);
}


#ifdef __cplusplus
}
#endif

#endif
