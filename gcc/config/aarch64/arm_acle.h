/* AArch64 Non-NEON ACLE intrinsics include file.

   Copyright (C) 2014-2022 Free Software Foundation, Inc.
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
#include <stddef.h>

#pragma GCC aarch64 "arm_acle.h"

#ifdef __cplusplus
extern "C" {
#endif

#define _GCC_ARM_ACLE_ROR_FN(NAME, TYPE)				  \
__extension__ extern __inline TYPE					  \
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))	  \
NAME (TYPE __value, uint32_t __rotate)					  \
{									  \
  size_t __size = sizeof (TYPE) * __CHAR_BIT__;				  \
  __rotate = __rotate % __size;						  \
  return __value >> __rotate | __value << ((__size - __rotate) % __size); \
}

_GCC_ARM_ACLE_ROR_FN (__ror, uint32_t)
_GCC_ARM_ACLE_ROR_FN (__rorl, unsigned long)
_GCC_ARM_ACLE_ROR_FN (__rorll, uint64_t)

#undef _GCC_ARM_ACLE_ROR_FN

#define _GCC_ARM_ACLE_DATA_FN(NAME, BUILTIN, ITYPE, RTYPE)	    \
__extension__ extern __inline RTYPE				    \
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__)) \
__##NAME (ITYPE __value)					    \
{								    \
  return __builtin_##BUILTIN (__value);				    \
}

_GCC_ARM_ACLE_DATA_FN (clz, clz, uint32_t, unsigned int)
_GCC_ARM_ACLE_DATA_FN (clzl, clzl, unsigned long, unsigned int)
_GCC_ARM_ACLE_DATA_FN (clzll, clzll, uint64_t, unsigned int)
_GCC_ARM_ACLE_DATA_FN (cls, clrsb, uint32_t, unsigned int)
_GCC_ARM_ACLE_DATA_FN (clsl, clrsbl, unsigned long, unsigned int)
_GCC_ARM_ACLE_DATA_FN (clsll, clrsbll, uint64_t, unsigned int)
_GCC_ARM_ACLE_DATA_FN (rev16, aarch64_rev16, uint32_t, uint32_t)
_GCC_ARM_ACLE_DATA_FN (rev16l, aarch64_rev16l, unsigned long, unsigned long)
_GCC_ARM_ACLE_DATA_FN (rev16ll, aarch64_rev16ll, uint64_t, uint64_t)
_GCC_ARM_ACLE_DATA_FN (rbit, aarch64_rbit, uint32_t, uint32_t)
_GCC_ARM_ACLE_DATA_FN (rbitl, aarch64_rbitl, unsigned long, unsigned long)
_GCC_ARM_ACLE_DATA_FN (rbitll, aarch64_rbitll, uint64_t, uint64_t)
_GCC_ARM_ACLE_DATA_FN (revsh, bswap16, int16_t, int16_t)
_GCC_ARM_ACLE_DATA_FN (rev, bswap32, uint32_t, uint32_t)
_GCC_ARM_ACLE_DATA_FN (revll, bswap64, uint64_t, uint64_t)

#undef _GCC_ARM_ACLE_DATA_FN

__extension__ extern __inline unsigned long
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__revl (unsigned long __value)
{
  if (sizeof (unsigned long) == 8)
    return __revll (__value);
  else
    return __rev (__value);
}

#pragma GCC push_options
#pragma GCC target ("arch=armv8.3-a")
__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__jcvt (double __a)
{
  return __builtin_aarch64_jcvtzs (__a);
}

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.5-a")
__extension__ extern __inline float
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rint32zf (float __a)
{
  return __builtin_aarch64_frint32zsf (__a);
}

__extension__ extern __inline double
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rint32z (double __a)
{
  return __builtin_aarch64_frint32zdf (__a);
}

__extension__ extern __inline float
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rint64zf (float __a)
{
  return __builtin_aarch64_frint64zsf (__a);
}

__extension__ extern __inline double
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rint64z (double __a)
{
  return __builtin_aarch64_frint64zdf (__a);
}

__extension__ extern __inline float
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rint32xf (float __a)
{
  return __builtin_aarch64_frint32xsf (__a);
}

__extension__ extern __inline double
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rint32x (double __a)
{
  return __builtin_aarch64_frint32xdf (__a);
}

__extension__ extern __inline float
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rint64xf (float __a)
{
  return __builtin_aarch64_frint64xsf (__a);
}

__extension__ extern __inline double
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rint64x (double __a)
{
  return __builtin_aarch64_frint64xdf (__a);
}


#pragma GCC pop_options

#pragma GCC push_options

#pragma GCC target ("+nothing+crc")

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__crc32b (uint32_t __a, uint8_t __b)
{
  return __builtin_aarch64_crc32b (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__crc32cb (uint32_t __a, uint8_t __b)
{
  return __builtin_aarch64_crc32cb (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__crc32ch (uint32_t __a, uint16_t __b)
{
  return __builtin_aarch64_crc32ch (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__crc32cw (uint32_t __a, uint32_t __b)
{
  return __builtin_aarch64_crc32cw (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__crc32cd (uint32_t __a, uint64_t __b)
{
  return __builtin_aarch64_crc32cx (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__crc32h (uint32_t __a, uint16_t __b)
{
  return __builtin_aarch64_crc32h (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__crc32w (uint32_t __a, uint32_t __b)
{
  return __builtin_aarch64_crc32w (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
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

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__tstart (void)
{
  return __builtin_aarch64_tstart ();
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__tcommit (void)
{
  __builtin_aarch64_tcommit ();
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__tcancel (const uint64_t __reason)
{
  __builtin_aarch64_tcancel (__reason);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__ttest (void)
{
  return __builtin_aarch64_ttest ();
}

#pragma GCC pop_options
#endif

#ifdef __ARM_FEATURE_LS64
#pragma GCC push_options
#pragma GCC target ("+nothing+ls64")

typedef __arm_data512_t data512_t;

__extension__ extern __inline data512_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_ld64b (const void *__addr)
{
  return __builtin_aarch64_ld64b (__addr);
}

__extension__ extern __inline void
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_st64b (void *__addr, data512_t __value)
{
  __builtin_aarch64_st64b (__addr, __value);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_st64bv (void *__addr, data512_t __value)
{
  return __builtin_aarch64_st64bv (__addr, __value);
}

__extension__ extern __inline uint64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__arm_st64bv0 (void *__addr, data512_t __value)
{
  return __builtin_aarch64_st64bv0 (__addr, __value);
}

#pragma GCC pop_options
#endif

#pragma GCC push_options
#pragma GCC target ("+nothing+rng")
__extension__ extern __inline int
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rndr (uint64_t *__res)
{
  return __builtin_aarch64_rndr (__res);
}

__extension__ extern __inline int
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__rndrrs (uint64_t *__res)
{
  return __builtin_aarch64_rndrrs (__res);
}

#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8.5-a+memtag")

#define __arm_mte_create_random_tag(__ptr, __u64_mask) \
  __builtin_aarch64_memtag_irg(__ptr, __u64_mask)

#define __arm_mte_exclude_tag(__ptr, __u64_excluded) \
  __builtin_aarch64_memtag_gmi(__ptr, __u64_excluded)

#define __arm_mte_ptrdiff(__ptr_a, __ptr_b) \
  __builtin_aarch64_memtag_subp(__ptr_a, __ptr_b)

#define __arm_mte_increment_tag(__ptr, __u_offset) \
  __builtin_aarch64_memtag_inc_tag(__ptr, __u_offset)

#define __arm_mte_set_tag(__tagged_address) \
  __builtin_aarch64_memtag_set_tag(__tagged_address)

#define __arm_mte_get_tag(__address) \
  __builtin_aarch64_memtag_get_tag(__address)

#pragma GCC pop_options

#ifdef __cplusplus
}
#endif

#endif
