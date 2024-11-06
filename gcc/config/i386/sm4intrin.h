/* Copyright (C) 2023-2024 Free Software Foundation, Inc.

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
#error "Never use <sm4intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _SM4INTRIN_H_INCLUDED
#define _SM4INTRIN_H_INCLUDED

#ifndef __SM4__
#pragma GCC push_options
#pragma GCC target("sm4")
#define __DISABLE_SM4__
#endif /* __SM4__ */

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_sm4key4_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_vsm4key4128 ((__v4si) __A, (__v4si) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sm4key4_epi32 (__m256i __A, __m256i __B)
{
  return (__m256i) __builtin_ia32_vsm4key4256 ((__v8si) __A, (__v8si) __B);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_sm4rnds4_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_vsm4rnds4128 ((__v4si) __A, (__v4si) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sm4rnds4_epi32 (__m256i __A, __m256i __B)
{
  return (__m256i) __builtin_ia32_vsm4rnds4256 ((__v8si) __A, (__v8si) __B);
}

#ifdef __DISABLE_SM4__
#undef __DISABLE_SM4__
#pragma GCC pop_options
#endif /* __DISABLE_SM4__ */

#if !defined (__SM4__) || !defined (__AVX10_2_512__)
#pragma GCC push_options
#pragma GCC target("sm4,avx10.2-512")
#define __DISABLE_SM4_512__
#endif /* __SM4_512__ */

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sm4key4_epi32 (__m512i __A, __m512i __B)
{
  return (__m512i) __builtin_ia32_vsm4key4512 ((__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sm4rnds4_epi32 (__m512i __A, __m512i __B)
{
  return (__m512i) __builtin_ia32_vsm4rnds4512 ((__v16si) __A, (__v16si) __B);
}

#ifdef __DISABLE_SM4_512__
#undef __DISABLE_SM4_512__
#pragma GCC pop_options
#endif /* __DISABLE_SM4_512__ */

#endif /* _SM4INTRIN_H_INCLUDED */
