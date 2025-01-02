/* Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

#ifndef _IMMINTRIN_H_INCLUDED
#error "Never use <sm3intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _SM3INTRIN_H_INCLUDED
#define _SM3INTRIN_H_INCLUDED

#ifndef __SM3__
#pragma GCC push_options
#pragma GCC target("sm3")
#define __DISABLE_SM3__
#endif /* __SM3__ */

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_sm3msg1_epi32 (__m128i __A, __m128i __B, __m128i __C)
{
  return (__m128i) __builtin_ia32_vsm3msg1 ((__v4si) __A, (__v4si) __B,
					    (__v4si) __C);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_sm3msg2_epi32 (__m128i __A, __m128i __B, __m128i __C)
{
  return (__m128i) __builtin_ia32_vsm3msg2 ((__v4si) __A, (__v4si) __B,
					    (__v4si) __C);
}

#ifdef __OPTIMIZE__
extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_sm3rnds2_epi32 (__m128i __A, __m128i __B, __m128i __C, const int __D)
{
  return (__m128i) __builtin_ia32_vsm3rnds2 ((__v4si) __A, (__v4si) __B,
					     (__v4si) __C, __D);
}
#else
#define _mm_sm3rnds2_epi32(A, B, C, D)					\
  ((__m128i) __builtin_ia32_vsm3rnds2 ((__v4si) (A), (__v4si) (B),	\
				       (__v4si) (C), (int) (D)))
#endif

#ifdef __DISABLE_SM3__
#undef __DISABLE_SM3__
#pragma GCC pop_options
#endif /* __DISABLE_SM3__ */

#endif /* _SM3INTRIN_H_INCLUDED */
