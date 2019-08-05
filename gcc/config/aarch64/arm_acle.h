/* AArch64 Non-NEON ACLE intrinsics include file.

   Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

#ifndef _GCC_ARM_ACLE_H
#define _GCC_ARM_ACLE_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#pragma GCC push_options

#pragma GCC target ("+nothing+crc")

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32b (uint32_t __a, uint8_t __b)
{
  return __builtin_aarch64_crc32b (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32cb (uint32_t __a, uint8_t __b)
{
  return __builtin_aarch64_crc32cb (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32ch (uint32_t __a, uint16_t __b)
{
  return __builtin_aarch64_crc32ch (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32cw (uint32_t __a, uint32_t __b)
{
  return __builtin_aarch64_crc32cw (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32cd (uint32_t __a, uint64_t __b)
{
  return __builtin_aarch64_crc32cx (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32h (uint32_t __a, uint16_t __b)
{
  return __builtin_aarch64_crc32h (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32w (uint32_t __a, uint32_t __b)
{
  return __builtin_aarch64_crc32w (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32d (uint32_t __a, uint64_t __b)
{
  return __builtin_aarch64_crc32x (__a, __b);
}

#pragma GCC pop_options

#ifdef __ARM_FEATURE_TME
#pragma GCC push_options
#pragma GCC target ("+nothing+tme")

#define _TMFAILURE_REASON     0x00007fffu
#define _TMFAILURE_RTRY       0x00008000u
#define _TMFAILURE_CNCL       0x00010000u
#define _TMFAILURE_MEM        0x00020000u
#define _TMFAILURE_IMP        0x00040000u
#define _TMFAILURE_ERR        0x00080000u
#define _TMFAILURE_SIZE       0x00100000u
#define _TMFAILURE_NEST       0x00200000u
#define _TMFAILURE_DBG        0x00400000u
#define _TMFAILURE_INT        0x00800000u
#define _TMFAILURE_TRIVIAL    0x01000000u

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
__tstart (void)
{
  return __builtin_aarch64_tstart ();
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__tcommit (void)
{
  __builtin_aarch64_tcommit ();
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__tcancel (const uint64_t __reason)
{
  __builtin_aarch64_tcancel (__reason);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
__ttest (void)
{
  return __builtin_aarch64_ttest ();
}

#pragma GCC pop_options
#endif

#ifdef __cplusplus
}
#endif

#endif
