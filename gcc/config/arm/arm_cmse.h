/* ARMv8-M Secure Extensions intrinsics include file.

   Copyright (C) 2015-2018 Free Software Foundation, Inc.
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


#ifndef _GCC_ARM_CMSE_H
#define _GCC_ARM_CMSE_H

#ifdef __cplusplus
extern "C" {
#endif

#if __ARM_FEATURE_CMSE & 1

#include <stddef.h>

#ifdef __ARM_BIG_ENDIAN

typedef union {
  struct cmse_address_info {
#if __ARM_FEATURE_CMSE & 2
    unsigned idau_region:8;
    unsigned idau_region_valid:1;
    unsigned secure:1;
    unsigned nonsecure_readwrite_ok:1;
    unsigned nonsecure_read_ok:1;
#else
    unsigned :12;
#endif
    unsigned readwrite_ok:1;
    unsigned read_ok:1;
#if __ARM_FEATURE_CMSE & 2
    unsigned sau_region_valid:1;
#else
    unsigned :1;
#endif
    unsigned mpu_region_valid:1;
#if __ARM_FEATURE_CMSE & 2
    unsigned sau_region:8;
#else
    unsigned :8;
#endif
    unsigned mpu_region:8;
  } flags;
  unsigned value;
} cmse_address_info_t;

#else

typedef union {
  struct cmse_address_info {
    unsigned mpu_region:8;
#if __ARM_FEATURE_CMSE & 2
    unsigned sau_region:8;
#else
    unsigned :8;
#endif
    unsigned mpu_region_valid:1;
#if __ARM_FEATURE_CMSE & 2
    unsigned sau_region_valid:1;
#else
    unsigned :1;
#endif
    unsigned read_ok:1;
    unsigned readwrite_ok:1;
#if __ARM_FEATURE_CMSE & 2
    unsigned nonsecure_read_ok:1;
    unsigned nonsecure_readwrite_ok:1;
    unsigned secure:1;
    unsigned idau_region_valid:1;
    unsigned idau_region:8;
#else
    unsigned :12;
#endif
  } flags;
  unsigned value;
} cmse_address_info_t;

#endif /* __ARM_BIG_ENDIAN */

#define cmse_TT_fptr(p) (__cmse_TT_fptr ((__cmse_fptr)(p)))

typedef void (*__cmse_fptr)(void);

#define __CMSE_TT_ASM(flags) \
{ \
  cmse_address_info_t __result; \
   __asm__ ("tt" # flags " %0,%1" \
	   : "=r"(__result) \
	   : "r"(__p) \
	   : "memory"); \
  return __result; \
}

__extension__ static __inline __attribute__ ((__always_inline__))
cmse_address_info_t
__cmse_TT_fptr (__cmse_fptr __p)
__CMSE_TT_ASM ()

__extension__ static __inline __attribute__ ((__always_inline__))
cmse_address_info_t
cmse_TT (void *__p)
__CMSE_TT_ASM ()

#define cmse_TTT_fptr(p) (__cmse_TTT_fptr ((__cmse_fptr)(p)))

__extension__ static __inline __attribute__ ((__always_inline__))
cmse_address_info_t
__cmse_TTT_fptr (__cmse_fptr __p)
__CMSE_TT_ASM (t)

__extension__ static __inline __attribute__ ((__always_inline__))
cmse_address_info_t
cmse_TTT (void *__p)
__CMSE_TT_ASM (t)

#if __ARM_FEATURE_CMSE & 2

#define cmse_TTA_fptr(p) (__cmse_TTA_fptr ((__cmse_fptr)(p)))

__extension__ static __inline __attribute__ ((__always_inline__))
cmse_address_info_t
__cmse_TTA_fptr (__cmse_fptr __p)
__CMSE_TT_ASM (a)

__extension__ static __inline __attribute__ ((__always_inline__))
cmse_address_info_t
cmse_TTA (void *__p)
__CMSE_TT_ASM (a)

#define cmse_TTAT_fptr(p) (__cmse_TTAT_fptr ((__cmse_fptr)(p)))

__extension__ static __inline cmse_address_info_t
__attribute__ ((__always_inline__))
__cmse_TTAT_fptr (__cmse_fptr __p)
__CMSE_TT_ASM (at)

__extension__ static __inline cmse_address_info_t
__attribute__ ((__always_inline__))
cmse_TTAT (void *__p)
__CMSE_TT_ASM (at)

/* FIXME: diagnose use outside cmse_nonsecure_entry functions.  */
__extension__ static __inline int __attribute__ ((__always_inline__))
cmse_nonsecure_caller (void)
{
  return __builtin_arm_cmse_nonsecure_caller ();
}

#define CMSE_AU_NONSECURE	2
#define CMSE_MPU_NONSECURE	16
#define CMSE_NONSECURE		18

#define cmse_nsfptr_create(p) ((__typeof__ ((p))) ((__INTPTR_TYPE__) (p) & ~1))

#define cmse_is_nsfptr(p) (!((__INTPTR_TYPE__) (p) & 1))

#endif /* __ARM_FEATURE_CMSE & 2 */

#define CMSE_MPU_UNPRIV		4
#define CMSE_MPU_READWRITE	1
#define CMSE_MPU_READ		8

__extension__ void *
cmse_check_address_range (void *, size_t, int);

#define cmse_check_pointed_object(p, f) \
  ((__typeof__ ((p))) cmse_check_address_range ((p), sizeof (*(p)), (f)))

#endif /* __ARM_FEATURE_CMSE & 1 */

#ifdef __cplusplus
}
#endif

#endif /* _GCC_ARM_CMSE_H */
