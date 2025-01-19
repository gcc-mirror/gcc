/* Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

#if !defined _IMMINTRIN_H_INCLUDED
#error "Never use <avx10_2copyintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2COPYINTRIN_H_INCLUDED
#define _AVX10_2COPYINTRIN_H_INCLUDED

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_move_epi32 (__m128i __A)
{
  return _mm_set_epi32 (0, 0, 0, ((__v4si) __A)[0]);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_move_epi16 (__m128i __A)
{
  return _mm_set_epi16 (0, 0, 0, 0, 0, 0, 0, ((__v8hi) __A)[0]);
}

#endif /* _AVX10_2COPYINTRIN_H_INCLUDED */
