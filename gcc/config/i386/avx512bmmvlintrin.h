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
# error "Never use <avx512bmmvlintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX512BMMVLINTRIN_H_INCLUDED
#define _AVX512BMMVLINTRIN_H_INCLUDED

#if !defined(__AVX512VL__) || !defined(__AVX512BMM__)
#pragma GCC push_options
#pragma GCC target("avx512bmm,avx512vl")
#define __DISABLE_AVX512BMMVL__
#endif /* __AVX512BMM__ */

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_bmacor16x16x16 (__m256i __A, __m256i __B, __m256i __C)
{
   return (__m256i) __builtin_ia32_vbmacor16x16x16_v16hi ((__v16hi) __A,
							  (__v16hi) __B,
							  (__v16hi) __C);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_bmacxor16x16x16 (__m256i __A, __m256i __B, __m256i __C)
{
  return (__m256i) __builtin_ia32_vbmacxor16x16x16_v16hi ((__v16hi) __A,
							  (__v16hi) __B,
							  (__v16hi) __C);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm128_mask_bitrev_epi8 (__mmask16 __U, __m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_vbitrevb128_mask ((__v16qi) __A,
						    (__v16qi) __B,
						    (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm128_maskz_bitrev_epi8 (__mmask16 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vbitrevb128_mask ((__v16qi) __A,
						    (__v16qi)(__m128i)
						    _mm_setzero_si128 (),
						    (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm128_bitrev_epi8 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vbitrevb128_mask ((__v16qi) __A,
						    (__v16qi)(__m128i)
						    _mm_undefined_si128 (),
						    (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_bitrev_epi8 (__mmask32 __U, __m256i __A, __m256i __B)
{
  return (__m256i) __builtin_ia32_vbitrevb256_mask ((__v32qi) __A,
						    (__v32qi) __B,
						    (__mmask32) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_bitrev_epi8 (__mmask32 __U, __m256i __A)
{
  return (__m256i) __builtin_ia32_vbitrevb256_mask ((__v32qi) __A,
						    (__v32qi)(__m256i)
						    _mm256_setzero_si256 (),
						    (__mmask32) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_bitrev_epi8 (__m256i __A)
{
  return (__m256i) __builtin_ia32_vbitrevb256_mask ((__v32qi) __A,
						    (__v32qi)(__m256i)
						    _mm256_undefined_si256 (),
						    (__mmask32) -1);
}

#ifdef __DISABLE_AVX512BMMVL__
#undef __DISABLE_AVX512BMMVL__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512BMMVL__ */

#endif /* _AVX512BMMVLINTRIN_H_INCLUDED */
