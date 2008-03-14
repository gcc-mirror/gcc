/* Copyright (C) 2007 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */

/* Common definition of the ROUND and PTEST intrinsics that are shared
   between SSE4.1 and SSE5.  */

#ifndef _MMINTRIN_COMMON_H_INCLUDED
#define _MMINTRIN_COMMON_H_INCLUDED

#if !defined(__SSE5__) && !defined(__SSE4_1__)
# error "SSE5 or SSE4.1 instruction set not enabled"
#else

/* Rounding mode macros. */
#define _MM_FROUND_TO_NEAREST_INT	0x00
#define _MM_FROUND_TO_NEG_INF		0x01
#define _MM_FROUND_TO_POS_INF		0x02
#define _MM_FROUND_TO_ZERO		0x03
#define _MM_FROUND_CUR_DIRECTION	0x04

#define _MM_FROUND_RAISE_EXC		0x00
#define _MM_FROUND_NO_EXC		0x08

#define _MM_FROUND_NINT		\
  (_MM_FROUND_TO_NEAREST_INT | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_FLOOR	\
  (_MM_FROUND_TO_NEG_INF | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_CEIL		\
  (_MM_FROUND_TO_POS_INF | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_TRUNC	\
  (_MM_FROUND_TO_ZERO | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_RINT		\
  (_MM_FROUND_CUR_DIRECTION | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_NEARBYINT	\
  (_MM_FROUND_CUR_DIRECTION | _MM_FROUND_NO_EXC)

/* Test Instruction */
/* Packed integer 128-bit bitwise comparison. Return 1 if
   (__V & __M) == 0.  */
extern __inline int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_testz_si128 (__m128i __M, __m128i __V)
{
  return __builtin_ia32_ptestz128 ((__v2di)__M, (__v2di)__V);
}

/* Packed integer 128-bit bitwise comparison. Return 1 if
   (__V & ~__M) == 0.  */
extern __inline int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_testc_si128 (__m128i __M, __m128i __V)
{
  return __builtin_ia32_ptestc128 ((__v2di)__M, (__v2di)__V);
}

/* Packed integer 128-bit bitwise comparison. Return 1 if
   (__V & __M) != 0 && (__V & ~__M) != 0.  */
extern __inline int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_testnzc_si128 (__m128i __M, __m128i __V)
{
  return __builtin_ia32_ptestnzc128 ((__v2di)__M, (__v2di)__V);
}

/* Macros for packed integer 128-bit comparison intrinsics.  */
#define _mm_test_all_zeros(M, V) _mm_testz_si128 ((M), (V))

#define _mm_test_all_ones(V) \
  _mm_testc_si128 ((V), _mm_cmpeq_epi32 ((V), (V)))

#define _mm_test_mix_ones_zeros(M, V) _mm_testnzc_si128 ((M), (V))

/* Packed/scalar double precision floating point rounding.  */

#ifdef __OPTIMIZE__
extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_round_pd (__m128d __V, const int __M)
{
  return (__m128d) __builtin_ia32_roundpd ((__v2df)__V, __M);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_round_sd(__m128d __D, __m128d __V, const int __M)
{
  return (__m128d) __builtin_ia32_roundsd ((__v2df)__D,
					   (__v2df)__V,
					   __M);
}
#else
#define _mm_round_pd(V, M) \
  ((__m128d) __builtin_ia32_roundpd ((__v2df)(__m128d)(V), (int)(M)))

#define _mm_round_sd(D, V, M)						\
  ((__m128d) __builtin_ia32_roundsd ((__v2df)(__m128d)(D),		\
				     (__v2df)(__m128d)(V), (int)(M)))
#endif

/* Packed/scalar single precision floating point rounding.  */

#ifdef __OPTIMIZE__
extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_round_ps (__m128 __V, const int __M)
{
  return (__m128) __builtin_ia32_roundps ((__v4sf)__V, __M);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_round_ss (__m128 __D, __m128 __V, const int __M)
{
  return (__m128) __builtin_ia32_roundss ((__v4sf)__D,
					  (__v4sf)__V,
					  __M);
}
#else
#define _mm_round_ps(V, M) \
  ((__m128) __builtin_ia32_roundps ((__v4sf)(__m128)(V), (int)(M)))

#define _mm_round_ss(D, V, M)						\
  ((__m128) __builtin_ia32_roundss ((__v4sf)(__m128)(D),		\
				    (__v4sf)(__m128)(V), (int)(M)))
#endif

/* Macros for ceil/floor intrinsics.  */
#define _mm_ceil_pd(V)	   _mm_round_pd ((V), _MM_FROUND_CEIL)
#define _mm_ceil_sd(D, V)  _mm_round_sd ((D), (V), _MM_FROUND_CEIL)

#define _mm_floor_pd(V)	   _mm_round_pd((V), _MM_FROUND_FLOOR)
#define _mm_floor_sd(D, V) _mm_round_sd ((D), (V), _MM_FROUND_FLOOR)

#define _mm_ceil_ps(V)	   _mm_round_ps ((V), _MM_FROUND_CEIL)
#define _mm_ceil_ss(D, V)  _mm_round_ss ((D), (V), _MM_FROUND_CEIL)

#define _mm_floor_ps(V)	   _mm_round_ps ((V), _MM_FROUND_FLOOR)
#define _mm_floor_ss(D, V) _mm_round_ss ((D), (V), _MM_FROUND_FLOOR)

#endif /* __SSE5__/__SSE4_1__ */

#endif /* _MMINTRIN_COMMON_H_INCLUDED */
