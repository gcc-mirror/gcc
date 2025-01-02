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

#if !defined _IMMINTRIN_H_INCLUDED
# error "Never use <avx512bitalgvlintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX512BITALGVLINTRIN_H_INCLUDED
#define _AVX512BITALGVLINTRIN_H_INCLUDED

#if !defined(__AVX512BITALG__) || !defined(__AVX512VL__) || defined (__EVEX512__)
#pragma GCC push_options
#pragma GCC target("avx512bitalg,avx512vl,no-evex512")
#define __DISABLE_AVX512BITALGVL__
#endif /* __AVX512BITALGVL__ */

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_popcnt_epi8 (__m256i __W, __mmask32 __U, __m256i __A)
{
  return (__m256i) __builtin_ia32_vpopcountb_v32qi_mask ((__v32qi) __A,
							 (__v32qi) __W,
							 (__mmask32) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_popcnt_epi8 (__mmask32 __U, __m256i __A)
{
  return (__m256i) __builtin_ia32_vpopcountb_v32qi_mask ((__v32qi) __A,
						(__v32qi)
						 _mm256_avx512_setzero_si256 (),
						(__mmask32) __U);
}

extern __inline __mmask32
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_bitshuffle_epi64_mask (__m256i __A, __m256i __B)
{
  return (__mmask32) __builtin_ia32_vpshufbitqmb256_mask ((__v32qi) __A,
						 (__v32qi) __B,
						 (__mmask32) -1);
}

extern __inline __mmask32
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_bitshuffle_epi64_mask (__mmask32 __M, __m256i __A, __m256i __B)
{
  return (__mmask32) __builtin_ia32_vpshufbitqmb256_mask ((__v32qi) __A,
						 (__v32qi) __B,
						 (__mmask32) __M);
}

extern __inline __mmask16
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_bitshuffle_epi64_mask (__m128i __A, __m128i __B)
{
  return (__mmask16) __builtin_ia32_vpshufbitqmb128_mask ((__v16qi) __A,
						 (__v16qi) __B,
						 (__mmask16) -1);
}

extern __inline __mmask16
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_bitshuffle_epi64_mask (__mmask16 __M, __m128i __A, __m128i __B)
{
  return (__mmask16) __builtin_ia32_vpshufbitqmb128_mask ((__v16qi) __A,
						 (__v16qi) __B,
						 (__mmask16) __M);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_popcnt_epi8 (__m256i __A)
{
  return (__m256i) __builtin_ia32_vpopcountb_v32qi ((__v32qi) __A);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_popcnt_epi16 (__m256i __A)
{
  return (__m256i) __builtin_ia32_vpopcountw_v16hi ((__v16hi) __A);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_popcnt_epi8 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vpopcountb_v16qi ((__v16qi) __A);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_popcnt_epi16 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vpopcountw_v8hi ((__v8hi) __A);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_popcnt_epi16 (__m256i __W, __mmask16 __U, __m256i __A)
{
  return (__m256i) __builtin_ia32_vpopcountw_v16hi_mask ((__v16hi) __A,
							(__v16hi) __W,
							(__mmask16) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_popcnt_epi16 (__mmask16 __U, __m256i __A)
{
  return (__m256i) __builtin_ia32_vpopcountw_v16hi_mask ((__v16hi) __A,
						(__v16hi)
						_mm256_avx512_setzero_si256 (),
						(__mmask16) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_popcnt_epi8 (__m128i __W, __mmask16 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vpopcountb_v16qi_mask ((__v16qi) __A,
							 (__v16qi) __W,
							 (__mmask16) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_popcnt_epi8 (__mmask16 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vpopcountb_v16qi_mask ((__v16qi) __A,
							 (__v16qi)
							 _mm_avx512_setzero_si128 (),
							 (__mmask16) __U);
}
extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_popcnt_epi16 (__m128i __W, __mmask8 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vpopcountw_v8hi_mask ((__v8hi) __A,
							(__v8hi) __W,
							(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_popcnt_epi16 (__mmask8 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vpopcountw_v8hi_mask ((__v8hi) __A,
							(__v8hi)
							_mm_avx512_setzero_si128 (),
							(__mmask8) __U);
}
#ifdef __DISABLE_AVX512BITALGVL__
#undef __DISABLE_AVX512BITALGVL__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512BITALGVL__ */

#endif /* _AVX512BITALGVLINTRIN_H_INCLUDED */
