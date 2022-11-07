/* Copyright (C) 2019-2022 Free Software Foundation, Inc.

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
#error "Never use <avx512bf16vlintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX512BF16VLINTRIN_H_INCLUDED
#define _AVX512BF16VLINTRIN_H_INCLUDED

#if !defined(__AVX512VL__) || !defined(__AVX512BF16__)
#pragma GCC push_options
#pragma GCC target("avx512bf16,avx512vl")
#define __DISABLE_AVX512BF16VL__
#endif /* __AVX512BF16__ */

/* Internal data types for implementing the intrinsics.  */
typedef __bf16 __v16bf __attribute__ ((__vector_size__ (32)));
typedef __bf16 __v8bf __attribute__ ((__vector_size__ (16)));

/* The Intel API is flexible enough that we must allow aliasing with other
   vector types, and their scalar components.  */
typedef __bf16 __m256bh __attribute__ ((__vector_size__ (32), __may_alias__));
typedef __bf16 __m128bh __attribute__ ((__vector_size__ (16), __may_alias__));

typedef __bf16 __bfloat16;

#define _mm256_cvtneps_pbh(A) \
  (__m128bh) __builtin_ia32_cvtneps2bf16_v8sf (A)
#define _mm_cvtneps_pbh(A) \
  (__m128bh) __builtin_ia32_cvtneps2bf16_v4sf (A)

/* vcvtne2ps2bf16 */

extern __inline __m256bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtne2ps_pbh (__m256 __A, __m256 __B)
{
  return (__m256bh)__builtin_ia32_cvtne2ps2bf16_v16bf(__A, __B);
}

extern __inline __m256bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtne2ps_pbh (__m256bh __A, __mmask16 __B, __m256 __C, __m256 __D)
{
  return (__m256bh)__builtin_ia32_cvtne2ps2bf16_v16bf_mask(__C, __D, __A, __B);
}

extern __inline __m256bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtne2ps_pbh (__mmask16 __A, __m256 __B, __m256 __C)
{
  return (__m256bh)__builtin_ia32_cvtne2ps2bf16_v16bf_maskz(__B, __C, __A);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtne2ps_pbh (__m128 __A, __m128 __B)
{
  return (__m128bh)__builtin_ia32_cvtne2ps2bf16_v8bf(__A, __B);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtne2ps_pbh (__m128bh __A, __mmask8 __B, __m128 __C, __m128 __D)
{
  return (__m128bh)__builtin_ia32_cvtne2ps2bf16_v8bf_mask(__C, __D, __A, __B);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtne2ps_pbh (__mmask8 __A, __m128 __B, __m128 __C)
{
  return (__m128bh)__builtin_ia32_cvtne2ps2bf16_v8bf_maskz(__B, __C, __A);
}

/* vcvtneps2bf16 */

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtneps_pbh (__m128bh __A, __mmask8 __B, __m256 __C)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v8sf_mask(__C, __A, __B);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtneps_pbh (__mmask8 __A, __m256 __B)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v8sf_maskz(__B, __A);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtneps_pbh (__m128bh __A, __mmask8 __B, __m128 __C)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v4sf_mask(__C, __A, __B);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtneps_pbh (__mmask8 __A, __m128 __B)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v4sf_maskz(__B, __A);
}

/* vdpbf16ps */

extern __inline __m256
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_dpbf16_ps (__m256 __A, __m256bh __B, __m256bh __C)
{
  return (__m256)__builtin_ia32_dpbf16ps_v8sf(__A, __B, __C);
}

extern __inline __m256
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbf16_ps (__m256 __A, __mmask8 __B, __m256bh __C, __m256bh __D)
{
  return (__m256)__builtin_ia32_dpbf16ps_v8sf_mask(__A, __C, __D, __B);
}

extern __inline __m256
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbf16_ps (__mmask8 __A, __m256 __B, __m256bh __C, __m256bh __D)
{
  return (__m256)__builtin_ia32_dpbf16ps_v8sf_maskz(__B, __C, __D, __A);
}

extern __inline __m128
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_dpbf16_ps (__m128 __A, __m128bh __B, __m128bh __C)
{
  return (__m128)__builtin_ia32_dpbf16ps_v4sf(__A, __B, __C);
}

extern __inline __m128
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbf16_ps (__m128 __A, __mmask8 __B, __m128bh __C, __m128bh __D)
{
  return (__m128)__builtin_ia32_dpbf16ps_v4sf_mask(__A, __C, __D, __B);
}

extern __inline __m128
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbf16_ps (__mmask8 __A, __m128 __B, __m128bh __C, __m128bh __D)
{
  return (__m128)__builtin_ia32_dpbf16ps_v4sf_maskz(__B, __C, __D, __A);
}

extern __inline __bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtness_sbh (float __A)
{
  __v4sf __V = {__A, 0, 0, 0};
  __v8bf __R = __builtin_ia32_cvtneps2bf16_v4sf_mask ((__v4sf)__V,
	       (__v8bf)_mm_undefined_si128 (), (__mmask8)-1);
  return __R[0];
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtpbh_ps (__m128bh __A)
{
  return (__m128)_mm_castsi128_ps ((__m128i)_mm_slli_epi32 (
	 (__m128i)_mm_cvtepi16_epi32 ((__m128i)__A), 16));
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtpbh_ps (__m128bh __A)
{
  return (__m256)_mm256_castsi256_ps ((__m256i)_mm256_slli_epi32 (
	 (__m256i)_mm256_cvtepi16_epi32 ((__m128i)__A), 16));
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtpbh_ps (__mmask8 __U, __m128bh __A)
{
  return (__m128)_mm_castsi128_ps ((__m128i)_mm_slli_epi32 (
	 (__m128i)_mm_maskz_cvtepi16_epi32 (
	 (__mmask8)__U, (__m128i)__A), 16));
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtpbh_ps (__mmask8 __U, __m128bh __A)
{
  return (__m256)_mm256_castsi256_ps ((__m256i)_mm256_slli_epi32 (
	 (__m256i)_mm256_maskz_cvtepi16_epi32 (
	 (__mmask8)__U, (__m128i)__A), 16));
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtpbh_ps (__m128 __S, __mmask8 __U, __m128bh __A)
{
  return (__m128)_mm_castsi128_ps ((__m128i)_mm_mask_slli_epi32 (
	 (__m128i)__S, (__mmask8)__U, (__m128i)_mm_cvtepi16_epi32 (
	 (__m128i)__A), 16));
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtpbh_ps (__m256 __S, __mmask8 __U, __m128bh __A)
{
  return (__m256)_mm256_castsi256_ps ((__m256i)_mm256_mask_slli_epi32 (
	 (__m256i)__S, (__mmask8)__U, (__m256i)_mm256_cvtepi16_epi32 (
	 (__m128i)__A), 16));
}

#ifdef __DISABLE_AVX512BF16VL__
#undef __DISABLE_AVX512BF16VL__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512BF16VL__ */

#endif /* _AVX512BF16VLINTRIN_H_INCLUDED */
