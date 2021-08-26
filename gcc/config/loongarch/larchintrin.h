/* Intrinsics for LoongArch BASE operations.

   Copyright (C) 2021 Free Software Foundation, Inc.
   Contributed by xuchenghua@loongson.cn.

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
__builtin_loongarch_rdtime_d (void)
{
  __drdtime_t drdtime;
  __asm__ volatile (
    "rdtime.d\t%[val],%[tid]\n\t"
    : [val]"=&r"(drdtime.dvalue),[tid]"=&r"(drdtime.dtimeid)
    :
  );
  return drdtime;
}
#define __rdtime_d __builtin_loongarch_rdtime_d
#endif

extern __inline __rdtime_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__builtin_loongarch_rdtimeh_w (void)
{
  __rdtime_t rdtime;
  __asm__ volatile (
    "rdtimeh.w\t%[val],%[tid]\n\t"
    : [val]"=&r"(rdtime.value),[tid]"=&r"(rdtime.timeid)
    :
  );
  return rdtime;
}
#define __rdtimel_w __builtin_loongarch_rdtimel_w

extern __inline __rdtime_t
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__builtin_loongarch_rdtimel_w (void)
{
  __rdtime_t rdtime;
  __asm__ volatile (
    "rdtimel.w\t%[val],%[tid]\n\t"
    : [val]"=&r"(rdtime.value),[tid]"=&r"(rdtime.timeid)
    :
  );
  return rdtime;
}
#define __rdtimeh_w __builtin_loongarch_rdtimeh_w

/* Assembly instruction format:          rj, fcsr */
/* Data types in instruction templates:  USI, UQI */
#define __movfcsr2gr(/*ui5*/ _1) __builtin_loongarch_movfcsr2gr ((_1));

/* Assembly instruction format:          0, fcsr, rj  */
/* Data types in instruction templates:  VOID, UQI, USI */
#define __movgr2fcsr(/*ui5*/ _1, _2) \
  __builtin_loongarch_movgr2fcsr ((unsigned short) _1, (unsigned int) _2);

#if defined __loongarch64
/* Assembly instruction format:          ui5, rj, si12 */
/* Data types in instruction templates:  VOID, USI, UDI, SI */
#define __dcacop(/*ui5*/ _1, /*unsigned long int*/ _2, /*si12*/ _3) \
  ((void) __builtin_loongarch_dcacop ((_1), (unsigned long int) (_2), (_3)))
#else
#error "Don't support this ABI."
#endif

/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  USI, USI */
extern __inline unsigned int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__cpucfg (unsigned int _1)
{
  return (unsigned int) __builtin_loongarch_cpucfg ((unsigned int) _1);
}

#ifdef __loongarch64
/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  DI, DI */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__asrtle_d (long int _1, long int _2)
{
  __builtin_loongarch_asrtle_d ((long int) _1, (long int) _2);
}

/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  DI, DI */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__asrtgt_d (long int _1, long int _2)
{
  __builtin_loongarch_asrtgt_d ((long int) _1, (long int) _2);
}
#endif

#if defined __loongarch64
/* Assembly instruction format:          rd, rj, ui5 */
/* Data types in instruction templates:  DI, DI, UQI */
#define __dlddir(/*long int*/ _1, /*ui5*/ _2) \
  ((long int) __builtin_loongarch_dlddir ((long int) (_1), (_2)))
#else
#error "Don't support this ABI."
#endif

#if defined __loongarch64
/* Assembly instruction format:          rj, ui5 */
/* Data types in instruction templates:  VOID, DI, UQI */
#define __dldpte(/*long int*/ _1, /*ui5*/ _2) \
  ((void) __builtin_loongarch_dldpte ((long int) (_1), (_2)))
#else
#error "Don't support this ABI."
#endif

/* Assembly instruction format:          rd, rj, rk */
/* Data types in instruction templates:  SI, QI, SI */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crc_w_b_w (char _1, int _2)
{
  return (int) __builtin_loongarch_crc_w_b_w ((char) _1, (int) _2);
}

/* Assembly instruction format:          rd, rj, rk */
/* Data types in instruction templates:  SI, HI, SI */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crc_w_h_w (short _1, int _2)
{
  return (int) __builtin_loongarch_crc_w_h_w ((short) _1, (int) _2);
}

/* Assembly instruction format:          rd, rj, rk */
/* Data types in instruction templates:  SI, SI, SI */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crc_w_w_w (int _1, int _2)
{
  return (int) __builtin_loongarch_crc_w_w_w ((int) _1, (int) _2);
}

#ifdef __loongarch64
/* Assembly instruction format:          rd, rj, rk */
/* Data types in instruction templates:  SI, DI, SI */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crc_w_d_w (long int _1, int _2)
{
  return (int) __builtin_loongarch_crc_w_d_w ((long int) _1, (int) _2);
}
#endif

/* Assembly instruction format:          rd, rj, rk */
/* Data types in instruction templates:  SI, QI, SI */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crcc_w_b_w (char _1, int _2)
{
  return (int) __builtin_loongarch_crcc_w_b_w ((char) _1, (int) _2);
}

/* Assembly instruction format:          rd, rj, rk */
/* Data types in instruction templates:  SI, HI, SI */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crcc_w_h_w (short _1, int _2)
{
  return (int) __builtin_loongarch_crcc_w_h_w ((short) _1, (int) _2);
}

/* Assembly instruction format:          rd, rj, rk */
/* Data types in instruction templates:  SI, SI, SI */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crcc_w_w_w (int _1, int _2)
{
  return (int) __builtin_loongarch_crcc_w_w_w ((int) _1, (int) _2);
}

#ifdef __loongarch64
/* Assembly instruction format:          rd, rj, rk */
/* Data types in instruction templates:  SI, DI, SI */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__crcc_w_d_w (long int _1, int _2)
{
  return (int) __builtin_loongarch_crcc_w_d_w ((long int) _1, (int) _2);
}
#endif

/* Assembly instruction format:          rd, ui14 */
/* Data types in instruction templates:  USI, USI */
#define __csrrd(/*ui14*/ _1) ((unsigned int) __builtin_loongarch_csrrd ((_1)))

/* Assembly instruction format:          rd, ui14 */
/* Data types in instruction templates:  USI, USI, USI */
#define __csrwr(/*unsigned int*/ _1, /*ui14*/ _2) \
  ((unsigned int) __builtin_loongarch_csrwr ((unsigned int) (_1), (_2)))

/* Assembly instruction format:          rd, rj, ui14 */
/* Data types in instruction templates:  USI, USI, USI, USI */
#define __csrxchg(/*unsigned int*/ _1, /*unsigned int*/ _2, /*ui14*/ _3) \
  ((unsigned int) __builtin_loongarch_csrxchg ((unsigned int) (_1), \
					       (unsigned int) (_2), (_3)))

#ifdef __loongarch64
/* Assembly instruction format:          rd, ui14 */
/* Data types in instruction templates:  UDI, USI */
#define __dcsrrd(/*ui14*/ _1) \
  ((unsigned long int) __builtin_loongarch_dcsrrd ((_1)))

/* Assembly instruction format:          rd, ui14 */
/* Data types in instruction templates:  UDI, UDI, USI */
#define __dcsrwr(/*unsigned long int*/ _1, /*ui14*/ _2) \
  ((unsigned long int) __builtin_loongarch_dcsrwr ((unsigned long int) (_1), \
						   (_2)))

/* Assembly instruction format:          rd, rj, ui14 */
/* Data types in instruction templates:  UDI, UDI, UDI, USI */
#define __dcsrxchg(/*unsigned long int*/ _1, /*unsigned long int*/ _2, \
		   /*ui14*/ _3) \
  ((unsigned long int) __builtin_loongarch_dcsrxchg ( \
    (unsigned long int) (_1), (unsigned long int) (_2), (_3)))
#endif

/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  UQI, USI */
extern __inline unsigned char
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrrd_b (unsigned int _1)
{
  return (unsigned char) __builtin_loongarch_iocsrrd_b ((unsigned int) _1);
}

/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  UHI, USI */
extern __inline unsigned char
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrrd_h (unsigned int _1)
{
  return (unsigned short) __builtin_loongarch_iocsrrd_h ((unsigned int) _1);
}

/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  USI, USI */
extern __inline unsigned int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrrd_w (unsigned int _1)
{
  return (unsigned int) __builtin_loongarch_iocsrrd_w ((unsigned int) _1);
}

#ifdef __loongarch64
/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  UDI, USI */
extern __inline unsigned long int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrrd_d (unsigned int _1)
{
  return (unsigned long int) __builtin_loongarch_iocsrrd_d ((unsigned int) _1);
}
#endif

/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  VOID, UQI, USI */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrwr_b (unsigned char _1, unsigned int _2)
{
  return (void) __builtin_loongarch_iocsrwr_b ((unsigned char) _1,
					       (unsigned int) _2);
}

/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  VOID, UHI, USI */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrwr_h (unsigned short _1, unsigned int _2)
{
  return (void) __builtin_loongarch_iocsrwr_h ((unsigned short) _1,
					       (unsigned int) _2);
}

/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  VOID, USI, USI */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrwr_w (unsigned int _1, unsigned int _2)
{
  return (void) __builtin_loongarch_iocsrwr_w ((unsigned int) _1,
					       (unsigned int) _2);
}

#ifdef __loongarch64
/* Assembly instruction format:          rd, rj */
/* Data types in instruction templates:  VOID, UDI, USI */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__iocsrwr_d (unsigned long int _1, unsigned int _2)
{
  return (void) __builtin_loongarch_iocsrwr_d ((unsigned long int) _1,
					       (unsigned int) _2);
}
#endif

/* Assembly instruction format:          ui15 */
/* Data types in instruction templates:  UQI */
#define __dbar(/*ui15*/ _1) __builtin_loongarch_dbar ((_1))

/* Assembly instruction format:          ui15 */
/* Data types in instruction templates:  UQI */
#define __ibar(/*ui15*/ _1) __builtin_loongarch_ibar ((_1))

#define __builtin_loongarch_syscall(a) \
  { \
    __asm__ volatile ("syscall %0\n\t" ::"I"(a)); \
  }
#define __syscall __builtin_loongarch_syscall

#define __builtin_loongarch_break(a) \
  { \
    __asm__ volatile ("break %0\n\t" ::"I"(a)); \
  }
#define __break __builtin_loongarch_break

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__builtin_loongarch_tlbsrch (void)
{
  __asm__ volatile ("tlbsrch\n\t");
}
#define __tlbsrch __builtin_loongarch_tlbsrch

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__builtin_loongarch_tlbrd (void)
{
  __asm__ volatile ("tlbrd\n\t");
}
#define __tlbrd __builtin_loongarch_tlbrd

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__builtin_loongarch_tlbwr (void)
{
  __asm__ volatile ("tlbwr\n\t");
}
#define __tlbwr __builtin_loongarch_tlbwr

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__builtin_loongarch_tlbfill (void)
{
  __asm__ volatile ("tlbfill\n\t");
}
#define __tlbfill __builtin_loongarch_tlbfill

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__builtin_loongarch_tlbclr (void)
{
  __asm__ volatile ("tlbclr\n\t");
}
#define __tlbclr __builtin_loongarch_tlbclr

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__builtin_loongarch_tlbflush (void)
{
  __asm__ volatile ("tlbflush\n\t");
}
#define __tlbflush __builtin_loongarch_tlbflush

#ifdef __cplusplus
}
#endif
#endif /* _GCC_LOONGARCH_BASE_INTRIN_H */
