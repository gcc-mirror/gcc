/* ARM Non-NEON ACLE intrinsics include file.

   Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#if (!__thumb__ || __thumb2__) &&  __ARM_ARCH >= 4
__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_cdp (const unsigned int __coproc, const unsigned int __opc1,
	   const unsigned int __CRd, const unsigned int __CRn,
	   const unsigned int __CRm, const unsigned int __opc2)
{
  return __builtin_arm_cdp (__coproc, __opc1, __CRd, __CRn, __CRm, __opc2);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_ldc (const unsigned int __coproc, const unsigned int __CRd,
	   const void * __p)
{
  return __builtin_arm_ldc (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_ldcl (const unsigned int __coproc, const unsigned int __CRd,
	    const void * __p)
{
  return __builtin_arm_ldcl (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_stc (const unsigned int __coproc, const unsigned int __CRd,
	   void * __p)
{
  return __builtin_arm_stc (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_stcl (const unsigned int __coproc, const unsigned int __CRd,
	    void * __p)
{
  return __builtin_arm_stcl (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_mcr (const unsigned int __coproc, const unsigned int __opc1,
	   uint32_t __value, const unsigned int __CRn, const unsigned int __CRm,
	   const unsigned int __opc2)
{
  return __builtin_arm_mcr (__coproc, __opc1, __value, __CRn, __CRm, __opc2);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__arm_mrc (const unsigned int __coproc, const unsigned int __opc1,
	   const unsigned int __CRn, const unsigned int __CRm,
	   const unsigned int __opc2)
{
  return __builtin_arm_mrc (__coproc, __opc1, __CRn, __CRm, __opc2);
}
#if __ARM_ARCH >= 5
__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_cdp2 (const unsigned int __coproc, const unsigned int __opc1,
	    const unsigned int __CRd, const unsigned int __CRn,
	    const unsigned int __CRm, const unsigned int __opc2)
{
  return __builtin_arm_cdp2 (__coproc, __opc1, __CRd, __CRn, __CRm, __opc2);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_ldc2 (const unsigned int __coproc, const unsigned int __CRd,
	    const void * __p)
{
  return __builtin_arm_ldc2 (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_ldc2l (const unsigned int __coproc, const unsigned int __CRd,
	     const void * __p)
{
  return __builtin_arm_ldc2l (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_stc2 (const unsigned int __coproc, const unsigned int __CRd,
	    void * __p)
{
  return __builtin_arm_stc2 (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_stc2l (const unsigned int __coproc, const unsigned int __CRd,
	     void * __p)
{
  return __builtin_arm_stc2l (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_mcr2 (const unsigned int __coproc, const unsigned int __opc1,
	    uint32_t __value, const unsigned int __CRn,
	    const unsigned int __CRm, const unsigned int __opc2)
{
  return __builtin_arm_mcr2 (__coproc, __opc1, __value, __CRn, __CRm, __opc2);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__arm_mrc2 (const unsigned int __coproc, const unsigned int __opc1,
	    const unsigned int __CRn, const unsigned int __CRm,
	    const unsigned int __opc2)
{
  return __builtin_arm_mrc2 (__coproc, __opc1, __CRn, __CRm, __opc2);
}

#if __ARM_ARCH >= 6 ||  defined (__ARM_ARCH_5TE__)

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_mcrr (const unsigned int __coproc, const unsigned int __opc1,
	    uint64_t __value, const unsigned int __CRm)
{
  return __builtin_arm_mcrr (__coproc, __opc1, __value, __CRm);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
__arm_mrrc (const unsigned int __coproc, const unsigned int __opc1,
	    const unsigned int __CRm)
{
  return __builtin_arm_mrrc (__coproc, __opc1, __CRm);
}

#if __ARM_ARCH >= 6

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_mcrr2 (const unsigned int __coproc, const unsigned int __opc1,
	    uint64_t __value, const unsigned int __CRm)
{
  return __builtin_arm_mcrr2 (__coproc, __opc1, __value, __CRm);
}

__extension__ static __inline uint64_t __attribute__ ((__always_inline__))
__arm_mrrc2 (const unsigned int __coproc, const unsigned int __opc1,
	     const unsigned int __CRm)
{
  return __builtin_arm_mrrc2 (__coproc, __opc1,  __CRm);
}
#endif /* __ARM_ARCH >= 6.  */
#endif /* __ARM_ARCH >= 6 ||  defined (__ARM_ARCH_5TE__).  */
#endif /*  __ARM_ARCH >= 5.  */
#endif /* (!__thumb__ || __thumb2__) &&  __ARM_ARCH >= 4.  */

#pragma GCC push_options
#if __ARM_ARCH >= 8
#pragma GCC target ("arch=armv8-a+crc")

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32b (uint32_t __a, uint8_t __b)
{
  return __builtin_arm_crc32b (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32h (uint32_t __a, uint16_t __b)
{
  return __builtin_arm_crc32h (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32w (uint32_t __a, uint32_t __b)
{
  return __builtin_arm_crc32w (__a, __b);
}

#ifdef __ARM_32BIT_STATE
__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32d (uint32_t __a, uint64_t __b)
{
  uint32_t __d;

  __d = __crc32w (__crc32w (__a, __b & 0xffffffffULL), __b >> 32);
  return __d;
}
#endif

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32cb (uint32_t __a, uint8_t __b)
{
  return __builtin_arm_crc32cb (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32ch (uint32_t __a, uint16_t __b)
{
  return __builtin_arm_crc32ch (__a, __b);
}

__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32cw (uint32_t __a, uint32_t __b)
{
  return __builtin_arm_crc32cw (__a, __b);
}

#ifdef __ARM_32BIT_STATE
__extension__ static __inline uint32_t __attribute__ ((__always_inline__))
__crc32cd (uint32_t __a, uint64_t __b)
{
  uint32_t __d;

  __d = __crc32cw (__crc32cw (__a, __b & 0xffffffffULL), __b >> 32);
  return __d;
}
#endif

#endif /* __ARM_ARCH >= 8.  */
#pragma GCC pop_options

#ifdef __cplusplus
}
#endif

#endif
