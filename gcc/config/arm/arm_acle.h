/* ARM Non-NEON ACLE intrinsics include file.

   Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

#define _GCC_ARM_ACLE_ROR_FN(NAME, TYPE)				  \
__extension__ extern __inline TYPE					  \
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))	  \
NAME (TYPE __value, uint32_t __rotate)					  \
{									  \
  int __size = (int) sizeof (TYPE) * __CHAR_BIT__;                        \
  __rotate = __rotate % __size;                                           \
  return __value >> __rotate | __value << ((__size - __rotate) % __size); \
}

_GCC_ARM_ACLE_ROR_FN (__ror, uint32_t)
_GCC_ARM_ACLE_ROR_FN (__rorl, unsigned long)
_GCC_ARM_ACLE_ROR_FN (__rorll, uint64_t)

#undef _GCC_ARM_ACLE_ROR_FN

#define _GCC_ARM_ACLE_DATA_FN(NAME, ITYPE, RTYPE) \
__extension__ extern __inline RTYPE				    \
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__)) \
__##NAME (ITYPE __value)

#define _GCC_ARM_ACLE_DATA_ALIAS(NAME, BUILTIN, ITYPE, RTYPE)	    \
_GCC_ARM_ACLE_DATA_FN(NAME, ITYPE, RTYPE) \
{                                         \
  return __builtin_##BUILTIN (__value);   \
}

_GCC_ARM_ACLE_DATA_ALIAS (clz, clz, uint32_t, unsigned int)
_GCC_ARM_ACLE_DATA_ALIAS (clzl, clzl, unsigned long, unsigned int)
_GCC_ARM_ACLE_DATA_ALIAS (clzll, clzll, uint64_t, unsigned int)
_GCC_ARM_ACLE_DATA_ALIAS (cls, clrsb, uint32_t, unsigned int)
_GCC_ARM_ACLE_DATA_ALIAS (clsl, clrsbl, unsigned long, unsigned int)
_GCC_ARM_ACLE_DATA_ALIAS (clsll, clrsbll, uint64_t, unsigned int)
_GCC_ARM_ACLE_DATA_ALIAS (revsh, bswap16, int16_t, int16_t)
_GCC_ARM_ACLE_DATA_ALIAS (rev, bswap32, uint32_t, uint32_t)
_GCC_ARM_ACLE_DATA_ALIAS (revl, bswap32, unsigned long, unsigned long)
_GCC_ARM_ACLE_DATA_ALIAS (revll, bswap64, uint64_t, uint64_t)
#if __ARM_ARCH >= 6
_GCC_ARM_ACLE_DATA_ALIAS (rev16, arm_rev16si2, uint32_t, uint32_t)
_GCC_ARM_ACLE_DATA_ALIAS (rev16l, arm_rev16si2, unsigned long, unsigned long)
#else
_GCC_ARM_ACLE_DATA_FN(rev16, uint32_t, uint32_t) {
  return ((__value & 0xff00ff) << 8 | (__value & 0xff00ff00) >> 8);
}
_GCC_ARM_ACLE_DATA_FN(rev16l, unsigned long, unsigned long) {
  return ((__value & 0xff00ff) << 8 | (__value & 0xff00ff00) >> 8);
}
#endif
_GCC_ARM_ACLE_DATA_FN(rev16ll, uint64_t, uint64_t) {
  return __rev16l(__value) | (uint64_t)__rev16l(__value >> 32) << 32;
}

#if __ARM_ARCH_6T2__ ||  __ARM_ARCH >= 7
_GCC_ARM_ACLE_DATA_ALIAS (rbit, arm_rbit, uint32_t, uint32_t)
_GCC_ARM_ACLE_DATA_ALIAS (rbitl, arm_rbit, unsigned long, unsigned int)
_GCC_ARM_ACLE_DATA_FN(rbitll, uint64_t, uint64_t) {
  return ((uint64_t)__rbit(__value) << 32) | __rbit(__value >> 32);
}
#endif

#undef _GCC_ARM_ACLE_DATA_ALIAS
#undef _GCC_ARM_ACLE_DATA_FN

#if (!__thumb__ || __thumb2__) &&  __ARM_ARCH >= 4
__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_cdp (const unsigned int __coproc, const unsigned int __opc1,
	   const unsigned int __CRd, const unsigned int __CRn,
	   const unsigned int __CRm, const unsigned int __opc2)
{
  __builtin_arm_cdp (__coproc, __opc1, __CRd, __CRn, __CRm, __opc2);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_ldc (const unsigned int __coproc, const unsigned int __CRd,
	   const void * __p)
{
  __builtin_arm_ldc (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_ldcl (const unsigned int __coproc, const unsigned int __CRd,
	    const void * __p)
{
  __builtin_arm_ldcl (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_stc (const unsigned int __coproc, const unsigned int __CRd,
	   void * __p)
{
  __builtin_arm_stc (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_stcl (const unsigned int __coproc, const unsigned int __CRd,
	    void * __p)
{
  __builtin_arm_stcl (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_mcr (const unsigned int __coproc, const unsigned int __opc1,
	   uint32_t __value, const unsigned int __CRn, const unsigned int __CRm,
	   const unsigned int __opc2)
{
  __builtin_arm_mcr (__coproc, __opc1, __value, __CRn, __CRm, __opc2);
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
  __builtin_arm_cdp2 (__coproc, __opc1, __CRd, __CRn, __CRm, __opc2);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_ldc2 (const unsigned int __coproc, const unsigned int __CRd,
	    const void * __p)
{
  __builtin_arm_ldc2 (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_ldc2l (const unsigned int __coproc, const unsigned int __CRd,
	     const void * __p)
{
  __builtin_arm_ldc2l (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_stc2 (const unsigned int __coproc, const unsigned int __CRd,
	    void * __p)
{
  __builtin_arm_stc2 (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_stc2l (const unsigned int __coproc, const unsigned int __CRd,
	     void * __p)
{
  __builtin_arm_stc2l (__coproc, __CRd, __p);
}

__extension__ static __inline void __attribute__ ((__always_inline__))
__arm_mcr2 (const unsigned int __coproc, const unsigned int __opc1,
	    uint32_t __value, const unsigned int __CRn,
	    const unsigned int __CRm, const unsigned int __opc2)
{
  __builtin_arm_mcr2 (__coproc, __opc1, __value, __CRn, __CRm, __opc2);
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
  __builtin_arm_mcrr (__coproc, __opc1, __value, __CRm);
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
  __builtin_arm_mcrr2 (__coproc, __opc1, __value, __CRm);
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

#ifdef __ARM_FEATURE_SIMD32
typedef int32_t int16x2_t;
typedef uint32_t uint16x2_t;
typedef int32_t int8x4_t;
typedef uint32_t uint8x4_t;

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__sxtab16 (int16x2_t __a, int8x4_t __b)
{
  return __builtin_arm_sxtab16 (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__sxtb16 (int8x4_t __a)
{
  return __builtin_arm_sxtb16 (__a);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uxtab16 (uint16x2_t __a, uint8x4_t __b)
{
  return __builtin_arm_uxtab16 (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uxtb16 (uint8x4_t __a)
{
  return __builtin_arm_uxtb16 (__a);
}

__extension__ extern __inline int8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__qadd8 (int8x4_t __a, int8x4_t __b)
{
  return __builtin_arm_qadd8 (__a, __b);
}

__extension__ extern __inline int8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__qsub8 (int8x4_t __a, int8x4_t __b)
{
  return __builtin_arm_qsub8 (__a, __b);
}

__extension__ extern __inline int8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__shadd8 (int8x4_t __a, int8x4_t __b)
{
  return __builtin_arm_shadd8 (__a, __b);
}

__extension__ extern __inline int8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__shsub8 (int8x4_t __a, int8x4_t __b)
{
  return __builtin_arm_shsub8 (__a, __b);
}

__extension__ extern __inline uint8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uhadd8 (uint8x4_t __a, uint8x4_t __b)
{
  return __builtin_arm_uhadd8 (__a, __b);
}

__extension__ extern __inline uint8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uhsub8 (uint8x4_t __a, uint8x4_t __b)
{
  return __builtin_arm_uhsub8 (__a, __b);
}

__extension__ extern __inline uint8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uqadd8 (uint8x4_t __a, uint8x4_t __b)
{
  return __builtin_arm_uqadd8 (__a, __b);
}

__extension__ extern __inline uint8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uqsub8 (uint8x4_t __a, uint8x4_t __b)
{
  return __builtin_arm_uqsub8 (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__qadd16 (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_qadd16 (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__qasx (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_qasx (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__qsax (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_qsax (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__qsub16 (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_qsub16 (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__shadd16 (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_shadd16 (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__shasx (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_shasx (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__shsax (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_shsax (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__shsub16 (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_shsub16 (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uhadd16 (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uhadd16 (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uhasx (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uhasx (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uhsax (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uhsax (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uhsub16 (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uhsub16 (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uqadd16 (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uqadd16 (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uqasx (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uqasx (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uqsax (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uqsax (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uqsub16 (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uqsub16 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smusd (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_smusd (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smusdx (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_smusdx (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__usad8 (uint8x4_t __a, uint8x4_t __b)
{
  return __builtin_arm_usad8 (__a, __b);
}

__extension__ extern __inline uint32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__usada8 (uint8x4_t __a, uint8x4_t __b, uint32_t __c)
{
  return __builtin_arm_usada8 (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smlald (int16x2_t __a, int16x2_t __b, int64_t __c)
{
  return __builtin_arm_smlald (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smlaldx (int16x2_t __a, int16x2_t __b, int64_t __c)
{
  return __builtin_arm_smlaldx (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smlsld (int16x2_t __a, int16x2_t __b, int64_t __c)
{
  return __builtin_arm_smlsld (__a, __b, __c);
}

__extension__ extern __inline int64_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smlsldx (int16x2_t __a, int16x2_t __b, int64_t __c)
{
  return __builtin_arm_smlsldx (__a, __b, __c);
}

__extension__ extern __inline uint8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__sel (uint8x4_t __a, uint8x4_t __b)
{
  return __builtin_arm_sel (__a, __b);
}

__extension__ extern __inline int8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__sadd8 (int8x4_t __a, int8x4_t __b)
{
  return __builtin_arm_sadd8 (__a, __b);
}

__extension__ extern __inline int8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__ssub8 (int8x4_t __a, int8x4_t __b)
{
  return __builtin_arm_ssub8 (__a, __b);
}

__extension__ extern __inline uint8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uadd8 (uint8x4_t __a, uint8x4_t __b)
{
  return __builtin_arm_uadd8 (__a, __b);
}

__extension__ extern __inline uint8x4_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__usub8 (uint8x4_t __a, uint8x4_t __b)
{
  return __builtin_arm_usub8 (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__sadd16 (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_sadd16 (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__sasx (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_sasx (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__ssax (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_ssax (__a, __b);
}

__extension__ extern __inline int16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__ssub16 (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_ssub16 (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uadd16 (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uadd16 (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__uasx (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_uasx (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__usax (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_usax (__a, __b);
}

__extension__ extern __inline uint16x2_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__usub16 (uint16x2_t __a, uint16x2_t __b)
{
  return __builtin_arm_usub16 (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smlad (int16x2_t __a, int16x2_t __b, int32_t __c)
{
  return __builtin_arm_smlad (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smladx (int16x2_t __a, int16x2_t __b, int32_t __c)
{
  return __builtin_arm_smladx (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smlsd (int16x2_t __a, int16x2_t __b, int32_t __c)
{
  return __builtin_arm_smlsd (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smlsdx (int16x2_t __a, int16x2_t __b, int32_t __c)
{
  return __builtin_arm_smlsdx (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smuad (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_smuad (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smuadx (int16x2_t __a, int16x2_t __b)
{
  return __builtin_arm_smuadx (__a, __b);
}

#define __ssat16(__a, __sat)					\
  __extension__							\
  ({								\
    int16x2_t __arg = (__a);					\
    __builtin_sat_imm_check (__sat, 1, 16);			\
    int16x2_t __res = __builtin_arm_ssat16 (__arg, __sat);	\
    __res;							\
  })

#define __usat16(__a, __sat)					\
  __extension__							\
  ({								\
    int16x2_t __arg = (__a);					\
    __builtin_sat_imm_check (__sat, 0, 15);			\
    int16x2_t __res = __builtin_arm_usat16 (__arg, __sat);	\
    __res;							\
  })

#endif

#ifdef __ARM_FEATURE_SAT

#define __ssat(__a, __sat)				\
  __extension__						\
  ({							\
    int32_t __arg = (__a);				\
    __builtin_sat_imm_check (__sat, 1, 32);		\
    int32_t __res = __builtin_arm_ssat (__arg, __sat);	\
    __res;						\
  })

#define __usat(__a, __sat)				\
  __extension__						\
  ({							\
    int32_t __arg = (__a);				\
    __builtin_sat_imm_check (__sat, 0, 31);		\
    uint32_t __res = __builtin_arm_usat (__arg, __sat);	\
    __res;						\
  })

#endif

#ifdef __ARM_FEATURE_QBIT
__extension__ extern __inline void
__attribute__  ((__always_inline__, __gnu_inline__, __artificial__))
__ignore_saturation (void)
{
  /* ACLE designates this intrinsic as a hint.
     Implement as a nop for now.  */
}

/* These are defined as macros because the implementation of the builtins
   requires easy access to the current function so wrapping it in an
   always_inline function complicates things.  */

#define __saturation_occurred __builtin_arm_saturation_occurred

#define __set_saturation_occurred(__a)			\
  __extension__						\
  ({							\
    int __arg = (__a);					\
    __builtin_arm_set_saturation (__arg);		\
  })
#endif

#ifdef __ARM_FEATURE_DSP
__extension__ extern __inline int32_t
__attribute__  ((__always_inline__, __gnu_inline__, __artificial__))
__qadd (int32_t __a, int32_t __b)
{
  return __builtin_arm_qadd (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__  ((__always_inline__, __gnu_inline__, __artificial__))
__qsub (int32_t __a, int32_t __b)
{
  return __builtin_arm_qsub (__a, __b);
}

__extension__ extern __inline int32_t
__attribute__  ((__always_inline__, __gnu_inline__, __artificial__))
__qdbl (int32_t __x)
{
  return __qadd (__x, __x);
}

__extension__ extern __inline int32_t
__attribute__  ((__always_inline__, __gnu_inline__, __artificial__))
__smlabb (int32_t __a, int32_t __b, int32_t __c)
{
  return __builtin_arm_smlabb (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__  ((__always_inline__, __gnu_inline__, __artificial__))
__smlatb (int32_t __a, int32_t __b, int32_t __c)
{
  return __builtin_arm_smlatb (__a, __b, __c);
}

/* smlatb is equivalent to smlabt with the two multiplication operands
   swapped around.  */
__extension__ extern __inline int32_t
__attribute__  ((__always_inline__, __gnu_inline__, __artificial__))
__smlabt (int32_t __a, int32_t __b, int32_t __c)
{
  return __smlatb (__b, __a, __c);
}

__extension__ extern __inline int32_t
__attribute__  ((__always_inline__, __gnu_inline__, __artificial__))
__smlatt (int32_t __a, int32_t __b, int32_t __c)
{
  return __builtin_arm_smlatt (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smlawb (int32_t __a, int32_t __b, int32_t __c)
{
  return __builtin_arm_smlawb (__a, __b, __c);
}

__extension__ extern __inline int32_t
__attribute__ ((__always_inline__, __gnu_inline__, __artificial__))
__smlawt (int32_t __a, int32_t __b, int32_t __c)
{
  return __builtin_arm_smlawt (__a, __b, __c);
}
#endif

#pragma GCC push_options
#ifdef __ARM_FEATURE_CRC32
#ifdef __ARM_FP
#pragma GCC target ("arch=armv8-a+crc+simd")
#else
#pragma GCC target ("arch=armv8-a+crc")
#endif

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

#endif /* __ARM_FEATURE_CRC32  */
#pragma GCC pop_options

#ifdef __cplusplus
}
#endif

#endif
