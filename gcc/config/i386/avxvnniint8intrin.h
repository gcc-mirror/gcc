/* Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
#error "Never use <avxvnniint8vlintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVXVNNIINT8INTRIN_H_INCLUDED
#define _AVXVNNIINT8INTRIN_H_INCLUDED

#if !defined(__AVXVNNIINT8__)
#pragma GCC push_options
#pragma GCC target("avxvnniint8")
#define __DISABLE_AVXVNNIINT8__
#endif /* __AVXVNNIINT8__ */

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_dpbssd_epi32 (__m128i __W, __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssd128 ((__v4si) __W, (__v4si) __A, (__v4si) __B);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_dpbssds_epi32 (__m128i __W, __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssds128 ((__v4si) __W, (__v4si) __A, (__v4si) __B);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_dpbsud_epi32 (__m128i __W, __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsud128 ((__v4si) __W, (__v4si) __A, (__v4si) __B);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_dpbsuds_epi32 (__m128i __W, __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsuds128 ((__v4si) __W, (__v4si) __A, (__v4si) __B);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_dpbuud_epi32 (__m128i __W, __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuud128 ((__v4si) __W, (__v4si) __A, (__v4si) __B);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_dpbuuds_epi32 (__m128i __W, __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuuds128 ((__v4si) __W, (__v4si) __A, (__v4si) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_dpbssd_epi32 (__m256i __W, __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssd256 ((__v8si) __W, (__v8si) __A, (__v8si) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_dpbssds_epi32 (__m256i __W, __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssds256 ((__v8si) __W, (__v8si) __A, (__v8si) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_dpbsud_epi32 (__m256i __W, __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsud256 ((__v8si) __W, (__v8si) __A, (__v8si) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_dpbsuds_epi32 (__m256i __W, __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsuds256 ((__v8si) __W, (__v8si) __A, (__v8si) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_dpbuud_epi32 (__m256i __W, __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuud256 ((__v8si) __W, (__v8si) __A, (__v8si) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_dpbuuds_epi32 (__m256i __W, __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuuds256 ((__v8si) __W, (__v8si) __A, (__v8si) __B);
}

#ifdef __DISABLE_AVXVNNIINT8__
#undef __DISABLE_AVXVNNIINT8__
#pragma GCC pop_options
#endif /* __DISABLE_AVXVNNIINT8__ */

#endif /* __AVXVNNIINT8INTRIN_H_INCLUDED */
