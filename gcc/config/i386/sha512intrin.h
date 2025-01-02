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
#error "Never use <sha512intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _SHA512INTRIN_H_INCLUDED
#define _SHA512INTRIN_H_INCLUDED

#ifndef __SHA512__
#pragma GCC push_options
#pragma GCC target("sha512")
#define __DISABLE_SHA512__
#endif /* __SHA512__ */

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sha512msg1_epi64 (__m256i __A, __m128i __B)
{
  return (__m256i) __builtin_ia32_vsha512msg1 ((__v4di) __A, (__v2di) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sha512msg2_epi64 (__m256i __A, __m256i __B)
{
  return (__m256i) __builtin_ia32_vsha512msg2 ((__v4di) __A, (__v4di) __B);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sha512rnds2_epi64 (__m256i __A, __m256i __B, __m128i __C)
{
  return (__m256i) __builtin_ia32_vsha512rnds2 ((__v4di) __A, (__v4di) __B,
						(__v2di) __C);
}

#ifdef __DISABLE_SHA512__
#undef __DISABLE_SHA512__
#pragma GCC pop_options
#endif /* __DISABLE_SHA512__ */

#endif /* _SHA512INTRIN_H_INCLUDED */
