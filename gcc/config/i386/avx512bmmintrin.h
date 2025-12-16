/* Copyright (C) 2025 Free Software Foundation, Inc.

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
# error "Never use <avx512bmmintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX512BMMINTRIN_H_INCLUDED
#define _AVX512BMMINTRIN_H_INCLUDED

#ifndef __AVX512BMM__
#pragma GCC push_options
#pragma GCC target("avx512bmm")
#define __DISABLE_AVX512BMM__
#endif /* __AVX512BMM__ */

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_bmacor16x16x16 (__m512i __A, __m512i __B, __m512i __C)
{
  return (__m512i) __builtin_ia32_vbmacor16x16x16_v32hi ((__v32hi) __A,
							 (__v32hi) __B,
							 (__v32hi) __C);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_bmacxor16x16x16 (__m512i __A, __m512i __B, __m512i __C)
{
  return (__m512i) __builtin_ia32_vbmacxor16x16x16_v32hi ((__v32hi) __A,
							  (__v32hi) __B,
							  (__v32hi) __C);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_bitrev_epi8 (__mmask64 __U, __m512i __A, __m512i __B)
{
  return (__m512i) __builtin_ia32_vbitrevb512_mask ((__v64qi) __A,
						    (__v64qi) __B,
						    (__mmask64) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_bitrev_epi8 (__mmask64 __U, __m512i __A)
{
  return (__m512i) __builtin_ia32_vbitrevb512_mask ((__v64qi) __A,
						    (__v64qi)(__m512i)
						    _mm512_setzero_epi32 (),
						    (__mmask64) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_bitrev_epi8 (__m512i __A)
{
  return (__m512i) __builtin_ia32_vbitrevb512_mask ((__v64qi) __A,
						    (__v64qi)(__m512i)
						    _mm512_undefined_epi32 (),
						    (__mmask64) -1);
}

#ifdef __DISABLE_AVX512BMM__
#undef __DISABLE_AVX512BMM__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512BMM__ */

#endif /* _AVX512BMMINTRIN_H_INCLUDED */
