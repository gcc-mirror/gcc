/* Copyright (C) 2007-2013 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* ISO/IEC JTC1 SC22 WG14 N1169
 * Date: 2006-04-04
 * ISO/IEC TR 18037
 * Programming languages - C - Extensions to support embedded processors
 */

#ifndef _AVRGCC_STDFIX_H
#define _AVRGCC_STDFIX_H

/* 7.18a.1 Introduction.  */
/* 7.18a.3 Precision macros.  */

#include <stdfix-gcc.h>

#define _GCC_TYPEPUN(A, B)                      \
  __builtin_memcpy (&A, &B, sizeof (A))

/* 7.18a.6  The fixed-point intrinsic functions.  */

#if __SIZEOF_INT__ == 2

typedef signed char int_hr_t;
typedef unsigned char uint_uhr_t;

typedef short int int_r_t;
typedef short unsigned int uint_ur_t;

typedef short int int_hk_t;
typedef short unsigned int uint_uhk_t;

typedef long int int_lr_t;
typedef long unsigned int uint_ulr_t;

typedef long int int_k_t;
typedef long unsigned int uint_uk_t;

typedef long long int int_llr_t;
typedef long long unsigned int uint_ullr_t;

typedef long long int int_lk_t;
typedef long long unsigned int uint_ulk_t;

typedef long long int int_llk_t;
typedef long long unsigned int uint_ullk_t;

#else /* __SIZEOF_INT__ = 1 (for -mint8) */


typedef signed char int_hr_t;
typedef unsigned char uint_uhr_t;

typedef long int int_r_t;
typedef long unsigned int uint_ur_t;

typedef long int int_hk_t;
typedef long unsigned int uint_uhk_t;

typedef long long int int_lr_t;
typedef long long unsigned int uint_ulr_t;

typedef long long int int_k_t;
typedef long long unsigned int uint_uk_t;

#endif /* __SIZEOF_INT__ == 2 */


/* 7.18a.6.2 The fixed-point absolute value functions. */

/* short fract (hr): abshr */

static __inline__ __attribute__((__always_inline__))
short fract abshr (const short fract __q)
{
  return __builtin_avr_abshr (__q);
}

/* fract (r): absr */

static __inline__ __attribute__((__always_inline__))
fract absr (const fract __q)
{
  return __builtin_avr_absr (__q);
}

/* long fract (lr): abslr */

static __inline__ __attribute__((__always_inline__))
long fract abslr (const long fract __q)
{
  return __builtin_avr_abslr (__q);
}

/* short accum (hk): abshk */

static __inline__ __attribute__((__always_inline__))
short accum abshk (const short accum __q)
{
  return __builtin_avr_abshk (__q);
}

/* accum (k): absk */

static __inline__ __attribute__((__always_inline__))
accum absk (const accum __q)
{
  return __builtin_avr_absk (__q);
}

#if __SIZEOF_INT__ == 2

/* long long fract (llr): absllr */

static __inline__ __attribute__((__always_inline__))
long long fract absllr (const long long fract __q) /* GCC extension */
{
  return __builtin_avr_absllr (__q);
}

/* long accum (lk): abslk */

static __inline__ __attribute__((__always_inline__))
long accum abslk (const long accum __q)
{
  return __builtin_avr_abslk (__q);
}

/* long long accum (llk): absllk */

static __inline__ __attribute__((__always_inline__))
long long accum absllk (const long long accum __q) /* GCC extension */
{
  return __builtin_avr_absllk (__q);
}

#endif /* __SIZEOF_INT__ == 2 */


/* 7.18a.6.5 The bitwise fixed-point to integer conversion functions. */
/* 7.18a.6.6 The bitwise integer to fixed-point conversion functions. */

/* short fract (hr): bitshr, bitsuhr, hrbits, uhrbits */

static __inline__ __attribute__((__always_inline__))
int_hr_t bitshr (const short fract __q)
{
  int_hr_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
uint_uhr_t bitsuhr (const unsigned short fract __q)
{
  uint_uhr_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
short fract hrbits (const int_hr_t __i)
{
  short fract __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
unsigned short fract uhrbits (const uint_uhr_t __i)
{
  unsigned short fract __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

/* fract (r): bitsr, bitsur, rbits, urbits */

static __inline__ __attribute__((__always_inline__))
int_r_t bitsr (const fract __q)
{
  int_r_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
uint_ur_t bitsur (const unsigned fract __q)
{
  uint_ur_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
fract rbits (const int_r_t __i)
{
  fract __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
unsigned fract urbits (const uint_ur_t __i)
{
  unsigned fract __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

/* long fract (lr): bitslr, bitsulr, lrbits, ulrbits */

static __inline__ __attribute__((__always_inline__))
int_lr_t bitslr (const long fract __q)
{
  int_lr_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
uint_ulr_t bitsulr (const unsigned long fract __q)
{
  uint_ulr_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
long fract lrbits (const int_lr_t __i)
{
  long fract __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
unsigned long fract ulrbits (const uint_ulr_t __i)
{
  unsigned long fract __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

/* short accum (hk): bitshk, bitsuhk, hkbits, uhkbits */

static __inline__ __attribute__((__always_inline__))
int_hk_t bitshk (const short accum __q)
{
  int_hk_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
uint_uhk_t bitsuhk (const unsigned short accum __q)
{
  uint_uhk_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
short accum hkbits (const int_hk_t __i)
{
  short accum __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
unsigned short accum uhkbits (const uint_uhk_t __i)
{
  unsigned short accum __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

/* accum (k): bitsk, bitsuk, kbits, ukbits */

static __inline__ __attribute__((__always_inline__))
int_k_t bitsk (const accum __q)
{
  int_k_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
uint_uk_t bitsuk (const unsigned accum __q)
{
  uint_uk_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
accum kbits (const int_k_t __i)
{
  accum __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
unsigned accum ukbits (const uint_uk_t __i)
{
  unsigned accum __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

#if __SIZEOF_INT__ == 2

/* long long fract (llr): bitsllr, bitsullr, llrbits, ullrbits */

static __inline__ __attribute__((__always_inline__))
int_llr_t bitsllr (const long long fract __q)
{
  int_llr_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
uint_ullr_t bitsullr (const unsigned long long fract __q)
{
  uint_ullr_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
long long fract llrbits (const int_llr_t __i)
{
  long long fract __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
unsigned long long fract ullrbits (const uint_ullr_t __i)
{
  unsigned long long fract __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

/* long accum (lk): bitslk, bitsulk, lkbits, ulkbits */

static __inline__ __attribute__((__always_inline__))
int_lk_t bitslk (const long accum __q)
{
  int_lk_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
uint_ulk_t bitsulk (const unsigned long accum __q)
{
  uint_ulk_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
long accum lkbits (const int_lk_t __i)
{
  long accum __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
unsigned long accum ulkbits (const uint_ulk_t __i)
{
  unsigned long accum __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

/* long long accum (llk): bitsllk, bitsullk, llkbits, ullkbits */

static __inline__ __attribute__((__always_inline__))
int_llk_t bitsllk (const long long accum __q)
{
  int_llk_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
uint_ullk_t bitsullk (const unsigned long long accum __q)
{
  uint_ullk_t __result;
  _GCC_TYPEPUN (__result, __q);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
long long accum llkbits (const int_llk_t __i)
{
  long long accum __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

static __inline__ __attribute__((__always_inline__))
unsigned long long accum ullkbits (const uint_ullk_t __i)
{
  unsigned long long accum __result;
  _GCC_TYPEPUN (__result, __i);
  return __result;
}

#endif /* __SIZEOF_INT__ == 2 */
#endif /* _AVRGCC_STDFIX_H */
