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

#ifndef _IMMINTRIN_H_INCLUDED
#error "Never use <avxifmaintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVXIFMAINTRIN_H_INCLUDED
#define _AVXIFMAINTRIN_H_INCLUDED

#ifndef __AVXIFMA__
#pragma GCC push_options
#pragma GCC target("avxifma")
#define __DISABLE_AVXIFMA__
#endif /* __AVXIFMA__ */

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_madd52lo_avx_epu64 (__m128i __X, __m128i __Y, __m128i __Z)
{
  return (__m128i) __builtin_ia32_vpmadd52luq128 ((__v2di) __X,
						  (__v2di) __Y,
						  (__v2di) __Z);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_madd52hi_avx_epu64 (__m128i __X, __m128i __Y, __m128i __Z)
{
  return (__m128i) __builtin_ia32_vpmadd52huq128 ((__v2di) __X,
						  (__v2di) __Y,
						  (__v2di) __Z);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_madd52lo_avx_epu64 (__m256i __X, __m256i __Y, __m256i __Z)
{
  return (__m256i) __builtin_ia32_vpmadd52luq256 ((__v4di) __X,
						  (__v4di) __Y,
						  (__v4di) __Z);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_madd52hi_avx_epu64 (__m256i __X, __m256i __Y, __m256i __Z)
{
  return (__m256i) __builtin_ia32_vpmadd52huq256 ((__v4di) __X,
						  (__v4di) __Y,
						  (__v4di) __Z);
}

#ifdef __DISABLE_AVXIFMA__
#undef __DISABLE_AVXIFMA__
#pragma GCC pop_options
#endif /* __DISABLE_AVXIFMA__ */

#endif /* _AVXIFMAINTRIN_H_INCLUDED */
