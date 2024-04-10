/* Intrinsics for LoongArch BASE operations.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.

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

#ifndef _GCC_LOONGARCH_BASE_INTRIN_H
#define _GCC_LOONGARCH_BASE_INTRIN_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct drdtime
{
  unsigned long dvalue;
  unsigned long dtimeid;
} __drdtime_t;

typedef struct rdtime
{
  unsigned int value;
  unsigned int timeid;
} __rdtime_t;

#ifdef __loongarch64
extern __inline __drdtime_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__rdtime_d (void)
{
  __drdtime_t __drdtime;
  __asm__ volatile (
    "rdtime.d\t%[val],%[tid]\n\t"
    : [val]"=&r"(__drdtime.dvalue),[tid]"=&r"(__drdtime.dtimeid)
    :);
  return __drdtime;
}
#endif

extern __inline __rdtime_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__rdtimeh_w (void)
{
  __rdtime_t __rdtime;
  __asm__ volatile (
    "rdtimeh.w\t%[val],%[tid]\n\t"
    : [val]"=&r"(__rdtime.value),[tid]"=&r"(__rdtime.timeid)
    :);
  return __rdtime;
}

extern __inline __rdtime_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__rdtimel_w (void)
{
  __rdtime_t __rdtime;
  __asm__ volatile (
    "rdtimel.w\t%[val],%[tid]\n\t"
    : [val]"=&r"(__rdtime.value),[tid]"=&r"(__rdtime.timeid)
    :);
  return __rdtime;
}

/* Assembly instruction format:	rj, fcsr.  */
/* Data types in instruction templates:  USI, UQI.  */
#define __movfcsr2gr(/*ui5*/ _1) __builtin_loongarch_movfcsr2gr ((_1));

/* Assembly instruction format:	fcsr, rj.  */
/* Data types in instruction templates:  VOID, UQI, USI.  */
#define __movgr2fcsr(/*ui5*/ _1, _2) \
  __builtin_loongarch_movgr2fcsr ((_1), _2);

#if defined __loongarch64
/* Assembly instruction format:	ui5, rj, si12.  */
/* Data types in instruction templates:  VOID, USI, UDI, SI.  */
#define __cacop_d(/*ui5*/ _1, /*unsigned long int*/ _2, /*si12*/ _3) \
  __builtin_loongarch_cacop_d ((_1), (_2), (_3))
#else
#error "Unsupported ABI."
#endif

/* Assembly instruction format:	rd, rj.  */
/* Data types in instruction templates:  USI, USI.  */
extern __inline unsigned int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__cpucfg (unsigned int _1)
{
  return __builtin_loongarch_cpucfg (_1);
}

#ifdef __loongarch64
/* Assembly instruction format:	rj, rk.  */
/* Data types in instruction templates:  DI, DI.  */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__asrtle_d (long int _1, long int _2)
{
  __builtin_loongarch_asrtle_d (_1, _2);
}

/* Assembly instruction format:	rj, rk.  */
/* Data types in instruction templates:  DI, DI.  */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__asrtgt_d (long int _1, long int _2)
{
  __builtin_loongarch_asrtgt_d (_1, _2);
}
#endif

#if defined __loongarch64
/* Assembly instruction format:	rd, rj, ui5.  */
/* Data types in instruction templates:  DI, DI, UQI.  */
#define __lddir_d(/*long int*/ _1, /*ui5*/ _2) \
  __builtin_loongarch_lddir_d ((_1), (_2))
#else
#error "Unsupported ABI."
#endif

#if defined __loongarch64
/* Assembly instruction format:	rj, ui5.  */
/* Data types in instruction templates:  VOID, DI, UQI.  */
#define __ldpte_d(/*long int*/ _1, /*ui5*/ _2) \
  __builtin_loongarch_ldpte_d ((_1), (_2))
#else
#error "Unsupported ABI."
#endif

/* Assembly instruction format:	rd, rj, rk.  */
/* Data types in instruction templates:  SI, QI, SI.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crc_w_b_w (char _1, int _2)
{
  return __builtin_loongarch_crc_w_b_w (_1, _2);
}

/* Assembly instruction format:	rd, rj, rk.  */
/* Data types in instruction templates:  SI, HI, SI.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crc_w_h_w (short _1, int _2)
{
  return __builtin_loongarch_crc_w_h_w (_1, _2);
}

/* Assembly instruction format:	rd, rj, rk.  */
/* Data types in instruction templates:  SI, SI, SI.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crc_w_w_w (int _1, int _2)
{
  return __builtin_loongarch_crc_w_w_w (_1, _2);
}

#ifdef __loongarch64
/* Assembly instruction format:	rd, rj, rk.  */
/* Data types in instruction templates:  SI, DI, SI.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crc_w_d_w (long int _1, int _2)
{
  return __builtin_loongarch_crc_w_d_w (_1, _2);
}
#endif

/* Assembly instruction format:	rd, rj, rk.  */
/* Data types in instruction templates:  SI, QI, SI.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crcc_w_b_w (char _1, int _2)
{
  return __builtin_loongarch_crcc_w_b_w (_1, _2);
}

/* Assembly instruction format:	rd, rj, rk.  */
/* Data types in instruction templates:  SI, HI, SI.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crcc_w_h_w (short _1, int _2)
{
  return __builtin_loongarch_crcc_w_h_w (_1, _2);
}

/* Assembly instruction format:	rd, rj, rk.  */
/* Data types in instruction templates:  SI, SI, SI.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crcc_w_w_w (int _1, int _2)
{
  return __builtin_loongarch_crcc_w_w_w (_1, _2);
}

#ifdef __loongarch64
/* Assembly instruction format:	rd, rj, rk.  */
/* Data types in instruction templates:  SI, DI, SI.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crcc_w_d_w (long int _1, int _2)
{
  return __builtin_loongarch_crcc_w_d_w (_1, _2);
}
#endif

/* Assembly instruction format:	rd, ui14.  */
/* Data types in instruction templates:  USI, USI.  */
#define __csrrd_w(/*ui14*/ _1) \
  __builtin_loongarch_csrrd_w ((_1))

/* Assembly instruction format:	rd, ui14.  */
/* Data types in instruction templates:  USI, USI, USI.  */
#define __csrwr_w(/*unsigned int*/ _1, /*ui14*/ _2) \
  __builtin_loongarch_csrwr_w ((_1), (_2))

/* Assembly instruction format:	rd, rj, ui14.  */
/* Data types in instruction templates:  USI, USI, USI, USI.  */
#define __csrxchg_w(/*unsigned int*/ _1, /*unsigned int*/ _2, /*ui14*/ _3) \
  __builtin_loongarch_csrxchg_w ((_1), (_2), (_3))

#ifdef __loongarch64
/* Assembly instruction format:	rd, ui14.  */
/* Data types in instruction templates:  UDI, USI.  */
#define __csrrd_d(/*ui14*/ _1) \
  __builtin_loongarch_csrrd_d ((_1))

/* Assembly instruction format:	rd, ui14.  */
/* Data types in instruction templates:  UDI, UDI, USI.  */
#define __csrwr_d(/*unsigned long int*/ _1, /*ui14*/ _2) \
  __builtin_loongarch_csrwr_d ((_1), (_2))

/* Assembly instruction format:	rd, rj, ui14.  */
/* Data types in instruction templates:  UDI, UDI, UDI, USI.  */
#define __csrxchg_d(/*unsigned long int*/ _1, /*unsigned long int*/ _2, \
		   /*ui14*/ _3) \
  __builtin_loongarch_csrxchg_d ((_1), (_2), (_3))
#endif

/* Assembly instruction format:	rd, rj.  */
/* Data types in instruction templates:  UQI, USI.  */
extern __inline unsigned char
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrrd_b (unsigned int _1)
{
  return __builtin_loongarch_iocsrrd_b (_1);
}

/* Assembly instruction format:	rd, rj.  */
/* Data types in instruction templates:  UHI, USI.  */
extern __inline unsigned short
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrrd_h (unsigned int _1)
{
  return __builtin_loongarch_iocsrrd_h (_1);
}

/* Assembly instruction format:	rd, rj.  */
/* Data types in instruction templates:  USI, USI.  */
extern __inline unsigned int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrrd_w (unsigned int _1)
{
  return __builtin_loongarch_iocsrrd_w (_1);
}

#ifdef __loongarch64
/* Assembly instruction format:	rd, rj.  */
/* Data types in instruction templates:  UDI, USI.  */
extern __inline unsigned long int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrrd_d (unsigned int _1)
{
  return __builtin_loongarch_iocsrrd_d (_1);
}
#endif

/* Assembly instruction format:	rd, rj.  */
/* Data types in instruction templates:  VOID, UQI, USI.  */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrwr_b (unsigned char _1, unsigned int _2)
{
  __builtin_loongarch_iocsrwr_b (_1, _2);
}

/* Assembly instruction format:	rd, rj.  */
/* Data types in instruction templates:  VOID, UHI, USI.  */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrwr_h (unsigned short _1, unsigned int _2)
{
  __builtin_loongarch_iocsrwr_h (_1, _2);
}

/* Assembly instruction format:	rd, rj.  */
/* Data types in instruction templates:  VOID, USI, USI.  */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrwr_w (unsigned int _1, unsigned int _2)
{
  __builtin_loongarch_iocsrwr_w (_1, _2);
}

#ifdef __loongarch64
/* Assembly instruction format:	rd, rj.  */
/* Data types in instruction templates:  VOID, UDI, USI.  */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrwr_d (unsigned long int _1, unsigned int _2)
{
  __builtin_loongarch_iocsrwr_d (_1, _2);
}
#endif

#ifdef __loongarch_frecipe
/* Assembly instruction format: fd, fj.  */
/* Data types in instruction templates:  SF, SF.  */
extern __inline float
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__frecipe_s (float _1)
{
  return __builtin_loongarch_frecipe_s (_1);
}

/* Assembly instruction format: fd, fj.  */
/* Data types in instruction templates:  DF, DF.  */
extern __inline double
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__frecipe_d (double _1)
{
  return __builtin_loongarch_frecipe_d (_1);
}

/* Assembly instruction format: fd, fj.  */
/* Data types in instruction templates:  SF, SF.  */
extern __inline float
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__frsqrte_s (float _1)
{
  return __builtin_loongarch_frsqrte_s (_1);
}

/* Assembly instruction format: fd, fj.  */
/* Data types in instruction templates:  DF, DF.  */
extern __inline double
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__frsqrte_d (double _1)
{
  return __builtin_loongarch_frsqrte_d (_1);
}
#endif

/* Assembly instruction format:	ui15.  */
/* Data types in instruction templates:  USI.  */
#define __dbar(/*ui15*/ _1) __builtin_loongarch_dbar ((_1))

/* Assembly instruction format:	ui15.  */
/* Data types in instruction templates:  USI.  */
#define __ibar(/*ui15*/ _1) __builtin_loongarch_ibar ((_1))

/* Assembly instruction format:	ui15.  */
/* Data types in instruction templates:  USI.  */
#define __syscall(/*ui15*/ _1) __builtin_loongarch_syscall ((_1))

/* Assembly instruction format:	ui15.  */
/* Data types in instruction templates:  USI.  */
#define __break(/*ui15*/ _1) __builtin_loongarch_break ((_1))

#ifdef __cplusplus
}
#endif
#endif /* _GCC_LOONGARCH_BASE_INTRIN_H */
