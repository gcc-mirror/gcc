/* Copyright (C) 2026 Free Software Foundation, Inc.
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
#error "Never use <avx10v2auxintrin.h> directly; include <immintrin.h> instead."

#endif

#ifndef _AVX10V2AUXINTRIN_H_INCLUDED
#define _AVX10V2AUXINTRIN_H_INCLUDED

#if !defined(__AVX10V2AUX__)
#pragma GCC push_options
#pragma GCC target("avx10v2aux")
#define __DISABLE_AVX10V2AUX__
#endif /* __AVX10V2AUX__ */

// VCVTPS2BF8 - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtps_bf8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8128_mask ((__v4sf) __A,
						      (__v16qi)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtps_bf8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8128_mask ((__v4sf) __A,
						      (__v16qi) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtps_bf8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8128_mask ((__v4sf) __A,
						      (__v16qi)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

// VCVTPS2BF8 - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtps_bf8 (__m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8256_mask ((__v8sf) __A,
						      (__v16qi)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtps_bf8 (__m128i __W, __mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8256_mask ((__v8sf) __A,
						      (__v16qi) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtps_bf8 (__mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8256_mask ((__v8sf) __A,
						      (__v16qi)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

// VCVTPS2BF8 - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtps_bf8 (__m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8512_mask ((__v16sf) __A,
						      (__v16qi)
						      _mm_undefined_si128 (),
						      (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtps_bf8 (__m128i __W, __mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8512_mask ((__v16sf) __A,
						      (__v16qi) __W,
						      (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtps_bf8 (__mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8512_mask ((__v16sf) __A,
						      (__v16qi)
						      _mm_setzero_si128 (),
						      (__mmask16) __U);
}

// VCVTPS2BF8S - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_ps_bf8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8s128_mask ((__v4sf) __A,
						       (__v16qi)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_ps_bf8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8s128_mask ((__v4sf) __A,
						       (__v16qi) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_ps_bf8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8s128_mask ((__v4sf) __A,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

// VCVTPS2BF8S - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_ps_bf8 (__m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8s256_mask ((__v8sf) __A,
						       (__v16qi)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_ps_bf8 (__m128i __W, __mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8s256_mask ((__v8sf) __A,
						       (__v16qi) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_ps_bf8 (__mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8s256_mask ((__v8sf) __A,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

// VCVTPS2BF8S - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_ps_bf8 (__m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8s512_mask ((__v16sf) __A,
						       (__v16qi)
						       _mm_undefined_si128 (),
						       (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_ps_bf8 (__m128i __W, __mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8s512_mask ((__v16sf) __A,
						       (__v16qi) __W,
						       (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_ps_bf8 (__mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2bf8s512_mask ((__v16sf) __A,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask16) __U);
}

// VCVTPS2HF8 - 128-bit
extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtps_hf8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8128_mask ((__v4sf) __A,
						      (__v16qi)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtps_hf8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8128_mask ((__v4sf) __A,
						      (__v16qi) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtps_hf8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8128_mask ((__v4sf) __A,
						      (__v16qi)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

// VCVTPS2HF8 - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtps_hf8 (__m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8256_mask ((__v8sf) __A,
						      (__v16qi)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtps_hf8 (__m128i __W, __mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8256_mask ((__v8sf) __A,
						      (__v16qi) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtps_hf8 (__mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8256_mask ((__v8sf) __A,
						      (__v16qi)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

// VCVTPS2HF8 - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtps_hf8 (__m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8512_mask ((__v16sf) __A,
						      (__v16qi)
						      _mm_undefined_si128 (),
						      (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtps_hf8 (__m128i __W, __mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8512_mask ((__v16sf) __A,
						      (__v16qi) __W,
						      (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtps_hf8 (__mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8512_mask ((__v16sf) __A,
						      (__v16qi)
						      _mm_setzero_si128 (),
						      (__mmask16) __U);
}

// VCVTPS2HF8S - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_ps_hf8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8s128_mask ((__v4sf) __A,
						       (__v16qi)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_ps_hf8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8s128_mask ((__v4sf) __A,
						       (__v16qi) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_ps_hf8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8s128_mask ((__v4sf) __A,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

// VCVTPS2HF8S - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_ps_hf8 (__m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8s256_mask ((__v8sf) __A,
						       (__v16qi)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_ps_hf8 (__m128i __W, __mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8s256_mask ((__v8sf) __A,
						       (__v16qi) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_ps_hf8 (__mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8s256_mask ((__v8sf) __A,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

// VCVTPS2HF8S - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_ps_hf8 (__m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8s512_mask ((__v16sf) __A,
						       (__v16qi)
						       _mm_undefined_si128 (),
						       (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_ps_hf8 (__m128i __W, __mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8s512_mask ((__v16sf) __A,
						       (__v16qi) __W,
						       (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_ps_hf8 (__mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtps2hf8s512_mask ((__v16sf) __A,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask16) __U);
}

#ifdef __DISABLE_AVX10V2AUX__
#undef __DISABLE_AVX10V2AUX__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10V2AUX__ */

#endif /* _AVX10V2AUXINTRIN_H_INCLUDED */
