/* Copyright (C) 2024-2026 Free Software Foundation, Inc.

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
#error "Never use <avx10_2convertintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2CONVERTINTRIN_H_INCLUDED
#define _AVX10_2CONVERTINTRIN_H_INCLUDED

#if !defined(__AVX10_2__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2__
#endif /* __AVX10_2__ */

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbf8_ph (__m128i __A)
{
  return (__m128h) _mm_castsi128_ph ((__m128i) _mm_slli_epi16 (
	 (__m128i) _mm_cvtepi8_epi16 (__A), 8));
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbf8_ph (__m128h __S, __mmask8 __U, __m128i __A)
{
  return (__m128h) _mm_castsi128_ph ((__m128i) _mm_mask_slli_epi16 (
	 (__m128i) __S, __U, (__m128i) _mm_cvtepi8_epi16 (__A), 8));
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbf8_ph (__mmask8 __U, __m128i __A)
{
  return (__m128h) _mm_castsi128_ph ((__m128i) _mm_slli_epi16 (
	 (__m128i) _mm_maskz_cvtepi8_epi16 (__U, __A), 8));
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbf8_ph (__m128i __A)
{
  return (__m256h) _mm256_castsi256_ph ((__m256i) _mm256_slli_epi16 (
	 (__m256i) _mm256_cvtepi8_epi16 (__A), 8));
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbf8_ph (__m256h __S, __mmask16 __U, __m128i __A)
{
  return (__m256h) _mm256_castsi256_ph ((__m256i) _mm256_mask_slli_epi16 (
	 (__m256i) __S, __U, (__m256i) _mm256_cvtepi8_epi16 (__A), 8));
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbf8_ph (__mmask16 __U, __m128i __A)
{
  return (__m256h) _mm256_castsi256_ph ((__m256i) _mm256_slli_epi16 (
	 (__m256i) _mm256_maskz_cvtepi8_epi16 (__U, __A), 8));
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbf8_ph (__m256i __A)
{
  return (__m512h) _mm512_castsi512_ph ((__m512i) _mm512_slli_epi16 (
	 (__m512i) _mm512_cvtepi8_epi16 (__A), 8));
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbf8_ph (__m512h __S, __mmask32 __U, __m256i __A)
{
  return (__m512h) _mm512_castsi512_ph ((__m512i) _mm512_mask_slli_epi16 (
	 (__m512i) __S, __U, (__m512i) _mm512_cvtepi8_epi16 (__A), 8));
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbf8_ph (__mmask32 __U, __m256i __A)
{
  return (__m512h) _mm512_castsi512_ph ((__m512i) _mm512_slli_epi16 (
	 (__m512i) _mm512_maskz_cvtepi8_epi16 (__U, __A), 8));
}

#ifdef __DISABLE_AVX10_2__
#undef __DISABLE_AVX10_2__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2__ */

#endif /* __AVX10_2CONVERTINTRIN_H_INCLUDED */
