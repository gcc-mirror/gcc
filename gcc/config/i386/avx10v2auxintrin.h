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

// VCVTROPS2HF8 - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtrops_hf8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8128_mask ((__v4sf) __A,
							(__v16qi)
							_mm_undefined_si128 (),
							(__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtrops_hf8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8128_mask ((__v4sf) __A,
							(__v16qi) __W,
							(__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtrops_hf8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8128_mask ((__v4sf) __A,
							(__v16qi)
							_mm_setzero_si128 (),
							(__mmask8) __U);
}

// VCVTROPS2HF8 - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtrops_hf8 (__m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8256_mask ((__v8sf) __A,
							(__v16qi)
							_mm_undefined_si128 (),
							(__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtrops_hf8 (__m128i __W, __mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8256_mask ((__v8sf) __A,
							(__v16qi) __W,
							(__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtrops_hf8 (__mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8256_mask ((__v8sf) __A,
							(__v16qi)
							_mm_setzero_si128 (),
							(__mmask8) __U);
}

// VCVTROPS2HF8 - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtrops_hf8 (__m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8512_mask ((__v16sf) __A,
							(__v16qi)
							_mm_undefined_si128 (),
							(__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtrops_hf8 (__m128i __W, __mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8512_mask ((__v16sf) __A,
							(__v16qi) __W,
							(__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtrops_hf8 (__mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8512_mask ((__v16sf) __A,
							(__v16qi)
							_mm_setzero_si128 (),
							(__mmask16) __U);
}

// VCVTROPS2HF8S - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_rops_hf8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8s128_mask ((__v4sf) __A,
							 (__v16qi)
							 _mm_undefined_si128 (),
							 (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_rops_hf8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8s128_mask ((__v4sf) __A,
							 (__v16qi) __W,
							 (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_rops_hf8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8s128_mask ((__v4sf) __A,
							 (__v16qi)
							 _mm_setzero_si128 (),
							 (__mmask8) __U);
}

// VCVTROPS2HF8S - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_rops_hf8 (__m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8s256_mask ((__v8sf) __A,
							 (__v16qi)
							 _mm_undefined_si128 (),
							 (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_rops_hf8 (__m128i __W, __mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8s256_mask ((__v8sf) __A,
							 (__v16qi) __W,
							 (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_rops_hf8 (__mmask8 __U, __m256 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8s256_mask ((__v8sf) __A,
							 (__v16qi)
							 _mm_setzero_si128 (),
							 (__mmask8) __U);
}

// VCVTROPS2HF8S - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_rops_hf8 (__m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8s512_mask ((__v16sf) __A,
							 (__v16qi)
							 _mm_undefined_si128 (),
							 (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_rops_hf8 (__m128i __W, __mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8s512_mask ((__v16sf) __A,
							 (__v16qi) __W,
							 (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_rops_hf8 (__mmask16 __U, __m512 __A)
{
  return (__m128i) __builtin_ia32_vcvtrops2hf8s512_mask ((__v16sf) __A,
							 (__v16qi)
							 _mm_setzero_si128 (),
							 (__mmask16) __U);
}

// VCVTBIASPS2BF8 - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbiasps_bf8 (__m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8128_mask ((__v4si) __A,
							  (__v4sf) __B,
							  (__v16qi)(__m128i)
							  _mm_undefined_si128 (),
							  (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbiasps_bf8 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8128_mask ((__v4si) __A,
							  (__v4sf) __B,
							  (__v16qi) __W,
							  (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbiasps_bf8 (__mmask8 __U, __m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8128_mask ((__v4si) __A,
							  (__v4sf) __B,
							  (__v16qi)(__m128i)
							  _mm_setzero_si128 (),
							  (__mmask8) __U);
}

// VCVTBIASPS2BF8 - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbiasps_bf8 (__m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8256_mask ((__v8si) __A,
							  (__v8sf) __B,
							  (__v16qi)
							  _mm_undefined_si128 (),
							  (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbiasps_bf8 (__m128i __W, __mmask8 __U,
			   __m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8256_mask ((__v8si) __A,
							  (__v8sf) __B,
							  (__v16qi) __W,
							  (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbiasps_bf8 (__mmask8 __U, __m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8256_mask ((__v8si) __A,
							  (__v8sf) __B,
							  (__v16qi)
							  _mm_setzero_si128 (),
							  (__mmask8) __U);
}

// VCVTBIASPS2BF8 - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbiasps_bf8 (__m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8512_mask ((__v16si) __A,
							  (__v16sf) __B,
							  (__v16qi)
							  _mm_undefined_si128 (),
							  (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbiasps_bf8 (__m128i __W, __mmask16 __U,
			   __m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8512_mask ((__v16si) __A,
							  (__v16sf) __B,
							  (__v16qi) __W,
							  (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbiasps_bf8 (__mmask16 __U, __m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8512_mask ((__v16si) __A,
							  (__v16sf) __B,
							  (__v16qi)
							  _mm_setzero_si128 (),
							  (__mmask16) __U);
}

// VCVTBIASPS2BF8S - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_biasps_bf8 (__m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8s128_mask ((__v4si) __A,
							   (__v4sf) __B,
							   (__v16qi)
							   _mm_undefined_si128 (),
							   (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_biasps_bf8 (__m128i __W, __mmask8 __U,
			  __m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8s128_mask ((__v4si) __A,
							   (__v4sf) __B,
							   (__v16qi) __W,
							   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_biasps_bf8 (__mmask8 __U, __m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8s128_mask ((__v4si) __A,
							   (__v4sf) __B,
							   (__v16qi)
							   _mm_setzero_si128 (),
							   (__mmask8) __U);
}

// VCVTBIASPS2BF8S - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_biasps_bf8 (__m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8s256_mask ((__v8si) __A,
							   (__v8sf) __B,
							   (__v16qi)
							   _mm_undefined_si128 (),
							   (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_biasps_bf8 (__m128i __W, __mmask8 __U,
			     __m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8s256_mask ((__v8si) __A,
							   (__v8sf) __B,
							   (__v16qi) __W,
							   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_biasps_bf8 (__mmask8 __U, __m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8s256_mask ((__v8si) __A,
							   (__v8sf) __B,
							   (__v16qi)
							   _mm_setzero_si128 (),
							   (__mmask8) __U);
}

// VCVTBIASPS2BF8S - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_biasps_bf8 (__m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8s512_mask ((__v16si) __A,
							   (__v16sf) __B,
							   (__v16qi)
							   _mm_undefined_si128 (),
							   (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_biasps_bf8 (__m128i __W, __mmask16 __U,
			     __m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8s512_mask ((__v16si) __A,
							   (__v16sf) __B,
							   (__v16qi) __W,
							   (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_biasps_bf8 (__mmask16 __U, __m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2bf8s512_mask ((__v16si) __A,
							   (__v16sf) __B,
							   (__v16qi)
							   _mm_setzero_si128 (),
							   (__mmask16) __U);
}

// VCVTBIASPS2HF8 - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbiasps_hf8 (__m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8128_mask ((__v4si) __A,
							  (__v4sf) __B,
							  (__v16qi)
							  _mm_undefined_si128 (),
							  (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbiasps_hf8 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8128_mask ((__v4si) __A,
							  (__v4sf) __B,
							  (__v16qi) __W,
							  (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbiasps_hf8 (__mmask8 __U, __m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8128_mask ((__v4si) __A,
							  (__v4sf) __B,
							  (__v16qi)
							  _mm_setzero_si128 (),
							  (__mmask8) __U);
}

// VCVTBIASPS2HF8 - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbiasps_hf8 (__m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8256_mask ((__v8si) __A,
							  (__v8sf) __B,
							  (__v16qi)
							  _mm_undefined_si128 (),
							  (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbiasps_hf8 (__m128i __W, __mmask8 __U,
			   __m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8256_mask ((__v8si) __A,
							  (__v8sf) __B,
							  (__v16qi) __W,
							  (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbiasps_hf8 (__mmask8 __U, __m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8256_mask ((__v8si) __A,
							  (__v8sf) __B,
							  (__v16qi)
							  _mm_setzero_si128 (),
							  (__mmask8) __U);
}

// VCVTBIASPS2HF8 - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbiasps_hf8 (__m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8512_mask ((__v16si) __A,
							  (__v16sf) __B,
							  (__v16qi)
							  _mm_undefined_si128 (),
							  (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbiasps_hf8 (__m128i __W, __mmask16 __U,
			   __m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8512_mask ((__v16si) __A,
							  (__v16sf) __B,
							  (__v16qi) __W,
							  (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbiasps_hf8 (__mmask16 __U, __m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8512_mask ((__v16si) __A,
							  (__v16sf) __B,
							  (__v16qi)
							  _mm_setzero_si128 (),
							  (__mmask16) __U);
}

// VCVTBIASPS2HF8S - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_biasps_hf8 (__m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8s128_mask ((__v4si) __A,
							   (__v4sf) __B,
							   (__v16qi)
							   _mm_undefined_si128 (),
							   (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_biasps_hf8 (__m128i __W, __mmask8 __U,
			  __m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8s128_mask ((__v4si) __A,
							   (__v4sf) __B,
							   (__v16qi) __W,
							   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_biasps_hf8 (__mmask8 __U, __m128i __A, __m128 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8s128_mask ((__v4si) __A,
							   (__v4sf) __B,
							   (__v16qi)
							   _mm_setzero_si128 (),
							   (__mmask8) __U);
}

// VCVTBIASPS2HF8S - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_biasps_hf8 (__m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8s256_mask ((__v8si) __A,
							   (__v8sf) __B,
							   (__v16qi)
							   _mm_undefined_si128 (),
							   (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_biasps_hf8 (__m128i __W, __mmask8 __U,
			     __m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8s256_mask ((__v8si) __A,
							   (__v8sf) __B,
							   (__v16qi) __W,
							   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_biasps_hf8 (__mmask8 __U, __m256i __A, __m256 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8s256_mask ((__v8si) __A,
							   (__v8sf) __B,
							   (__v16qi)
							   _mm_setzero_si128 (),
							   (__mmask8) __U);
}

// VCVTBIASPS2HF8S - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_biasps_hf8 (__m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8s512_mask ((__v16si) __A,
							   (__v16sf) __B,
							   (__v16qi)
							   _mm_undefined_si128 (),
							   (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_biasps_hf8 (__m128i __W, __mmask16 __U,
			     __m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8s512_mask ((__v16si) __A,
							   (__v16sf) __B,
							   (__v16qi) __W,
							   (__mmask16) __U);
}


extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_biasps_hf8 (__mmask16 __U, __m512i __A, __m512 __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasps2hf8s512_mask ((__v16si) __A,
							   (__v16sf) __B,
							   (__v16qi)
							   _mm_setzero_si128 (),
							   (__mmask16) __U);
}

// VCVTBF82PS - 128-bit

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbf8_ps (__m128i __A)
{
  return (__m128) __builtin_ia32_vcvtbf82ps128_mask ((__v16qi) __A,
						     (__v4sf)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}


extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbf8_ps (__m128 __W, __mmask8 __U, __m128i __A)
{
  return (__m128) __builtin_ia32_vcvtbf82ps128_mask ((__v16qi) __A,
						     (__v4sf) __W,
						     (__mmask8) __U);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbf8_ps (__mmask8 __U, __m128i __A)
{
  return (__m128) __builtin_ia32_vcvtbf82ps128_mask ((__v16qi) __A,
						     (__v4sf)
						     _mm_setzero_si128 (),
						     (__mmask8) __U);
}

// VCVTBF82PS - 256-bit

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbf8_ps (__m128i __A)
{
  return (__m256) __builtin_ia32_vcvtbf82ps256_mask ((__v16qi) __A,
						     (__v8sf)
						     _mm256_undefined_si256 (),
						     (__mmask8) -1);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbf8_ps (__m256 __W, __mmask8 __U, __m128i __A)
{
  return (__m256) __builtin_ia32_vcvtbf82ps256_mask ((__v16qi) __A,
						     (__v8sf) __W,
						     (__mmask8) __U);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbf8_ps (__mmask8 __U, __m128i __A)
{
  return (__m256) __builtin_ia32_vcvtbf82ps256_mask ((__v16qi) __A,
						     (__v8sf)
						     _mm256_setzero_si256 (),
						     (__mmask8) __U);
}

// VCVTBF82PS - 512-bit

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbf8_ps (__m128i __A)
{
  return (__m512) __builtin_ia32_vcvtbf82ps512_mask ((__v16qi) __A,
						     (__v16sf)
						     _mm512_undefined_si512 (),
						     (__mmask16) -1);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbf8_ps (__m512 __W, __mmask16 __U, __m128i __A)
{
  return (__m512) __builtin_ia32_vcvtbf82ps512_mask ((__v16qi) __A,
						     (__v16sf) __W,
						     (__mmask16) __U);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbf8_ps (__mmask16 __U, __m128i __A)
{
  return (__m512) __builtin_ia32_vcvtbf82ps512_mask ((__v16qi) __A,
						     (__v16sf)
						     _mm512_setzero_si512 (),
						     (__mmask16) __U);
}

// // VCVTHF82PS - 128-bit

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvthf8_ps (__m128i __A)
{
  return (__m128) __builtin_ia32_vcvthf82ps128_mask ((__v16qi) __A,
						     (__v4sf)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvthf8_ps (__m128 __W, __mmask8 __U, __m128i __A)
{
  return (__m128) __builtin_ia32_vcvthf82ps128_mask ((__v16qi) __A,
						     (__v4sf) __W,
						     (__mmask8) __U);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvthf8_ps (__mmask8 __U, __m128i __A)
{
  return (__m128) __builtin_ia32_vcvthf82ps128_mask ((__v16qi) __A,
						     (__v4sf)
						     _mm_setzero_si128 (),
						     (__mmask8) __U);
}

// VCVTHF82PS - 256-bit

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvthf8_ps (__m128i __A)
{
  return (__m256) __builtin_ia32_vcvthf82ps256_mask ((__v16qi) __A,
						     (__v8sf)
						     _mm256_undefined_si256 (),
						     (__mmask8) -1);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvthf8_ps (__m256 __W, __mmask8 __U, __m128i __A)
{
  return (__m256) __builtin_ia32_vcvthf82ps256_mask ((__v16qi) __A,
						     (__v8sf) __W,
						     (__mmask8) __U);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvthf8_ps (__mmask8 __U, __m128i __A)
{
  return (__m256) __builtin_ia32_vcvthf82ps256_mask ((__v16qi) __A,
						     (__v8sf)
						     _mm256_setzero_si256 (),
						     (__mmask8) __U);
}

// VCVTHF82PS - 512-bit

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvthf8_ps (__m128i __A)
{
  return (__m512) __builtin_ia32_vcvthf82ps512_mask ((__v16qi) __A,
						     (__v16sf)
						     _mm512_undefined_si512 (),
						     (__mmask16) -1);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvthf8_ps (__m512 __W, __mmask16 __U, __m128i __A)
{
  return (__m512) __builtin_ia32_vcvthf82ps512_mask ((__v16qi) __A,
						     (__v16sf) __W,
						     (__mmask16) __U);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvthf8_ps (__mmask16 __U, __m128i __A)
{
  return (__m512) __builtin_ia32_vcvthf82ps512_mask ((__v16qi) __A,
						     (__v16sf)
						     _mm512_setzero_si512 (),
						     (__mmask16) __U);
}

// VCVTBF82BF4S

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_bf8_bf4 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vcvtbf82bf4s128 ((__v16qi) __A);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_bf8_bf4 (__m256i __A)
{
  return (__m128i) __builtin_ia32_vcvtbf82bf4s256 ((__v32qi) __A);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_bf8_bf4 (__m512i __A)
{
  return (__m256i) __builtin_ia32_vcvtbf82bf4s512 ((__v64qi) __A);
}

// VCVTHF82BF4S

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_hf8_bf4 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vcvthf82bf4s128 ((__v16qi) __A);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_hf8_bf4 (__m256i __A)
{
  return (__m128i) __builtin_ia32_vcvthf82bf4s256 ((__v32qi) __A);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_hf8_bf4 (__m512i __A)
{
  return (__m256i) __builtin_ia32_vcvthf82bf4s512 ((__v64qi) __A);
}

// VCVTBF42HF8 - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbf4_hf8 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vcvtbf42hf8128_mask ((__v16qi) __A,
						       (__v16qi)
						       _mm_undefined_si128 (),
						       (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbf4_hf8 (__m128i __W, __mmask16 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vcvtbf42hf8128_mask ((__v16qi) __A,
						       (__v16qi) __W,
						       (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbf4_hf8 (__mmask16 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vcvtbf42hf8128_mask ((__v16qi) __A,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask16) __U);
}

// VCVTBF42HF8 - 256-bit

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbf4_hf8 (__m128i __A)
{
  return (__m256i) __builtin_ia32_vcvtbf42hf8256_mask ((__v16qi) __A,
						       (__v32qi)
						       _mm256_undefined_si256 (),
						       (__mmask32) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbf4_hf8 (__m256i __W, __mmask32 __U, __m128i __A)
{
  return (__m256i) __builtin_ia32_vcvtbf42hf8256_mask ((__v16qi) __A,
						       (__v32qi) __W,
						       (__mmask32) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbf4_hf8 (__mmask32 __U, __m128i __A)
{
  return (__m256i) __builtin_ia32_vcvtbf42hf8256_mask ((__v16qi) __A,
						       (__v32qi)
						       _mm256_setzero_si256 (),
						       (__mmask32) __U);
}

// VCVTBF42HF8 - 512-bit

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbf4_hf8 (__m256i __A)
{
  return (__m512i) __builtin_ia32_vcvtbf42hf8512_mask ((__v32qi) __A,
						       (__v64qi)
						       _mm512_undefined_si512 (),
						       (__mmask64) -1);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbf4_hf8 (__m512i __W, __mmask64 __U, __m256i __A)
{
  return (__m512i) __builtin_ia32_vcvtbf42hf8512_mask ((__v32qi) __A,
						       (__v64qi) __W,
						       (__mmask64) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbf4_hf8 (__mmask64 __U, __m256i __A)
{
  return (__m512i) __builtin_ia32_vcvtbf42hf8512_mask ((__v32qi) __A,
						       (__v64qi)
						       _mm512_setzero_si512 (),
						       (__mmask64) __U);
}

// VCVTBF82BF6S

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_bf8_bf6 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vcvtbf82bf6s128 ((__v16qi)__A);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_bf8_bf6 (__m256i __A)
{
  return (__m256i) __builtin_ia32_vcvtbf82bf6s256 ((__v32qi)__A);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_bf8_bf6 (__m512i __A)
{
  return (__m512i) __builtin_ia32_vcvtbf82bf6s512 ((__v64qi)__A);
}

// VCVTHF82HF6S

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_hf8_hf6 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vcvthf82hf6s128 ((__v16qi)__A);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_hf8_hf6 (__m256i __A)
{
  return (__m256i) __builtin_ia32_vcvthf82hf6s256 ((__v32qi)__A);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_hf8_hf6 (__m512i __A)
{
  return (__m512i) __builtin_ia32_vcvthf82hf6s512 ((__v64qi)__A);
}

// VCVTBF62HF8 - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbf6_hf8 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vcvtbf62hf8128_mask ((__v16qi)__A,
						       (__v16qi)
						       _mm_undefined_si128 (),
						       (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbf6_hf8 (__m128i __W, __mmask16 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vcvtbf62hf8128_mask ((__v16qi)__A,
						       (__v16qi) __W,
						       (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbf6_hf8 (__mmask16 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vcvtbf62hf8128_mask ((__v16qi)__A,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask16) __U);
}

// VCVTBF62HF8 - 256-bit

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbf6_hf8 (__m256i __A)
{
  return (__m256i) __builtin_ia32_vcvtbf62hf8256_mask ((__v32qi)__A,
						       (__v32qi)
						       _mm256_undefined_si256 (),
						       (__mmask32) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbf6_hf8 (__m256i __W, __mmask32 __U, __m256i __A)
{
  return (__m256i) __builtin_ia32_vcvtbf62hf8256_mask ((__v32qi)__A,
						       (__v32qi) __W,
						       (__mmask32) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbf6_hf8 (__mmask32 __U, __m256i __A)
{
  return (__m256i) __builtin_ia32_vcvtbf62hf8256_mask ((__v32qi)__A,
						       (__v32qi)
						       _mm256_setzero_si256 (),
						       (__mmask32) __U);
}

// VCVTBF62HF8 - 512-bit

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbf6_hf8 (__m512i __A)
{
  return (__m512i) __builtin_ia32_vcvtbf62hf8512_mask ((__v64qi)__A,
						       (__v64qi)
						       _mm512_undefined_si512 (),
						       (__mmask64) -1);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbf6_hf8 (__m512i __W, __mmask64 __U, __m512i __A)
{
  return (__m512i) __builtin_ia32_vcvtbf62hf8512_mask ((__v64qi)__A,
						       (__v64qi) __W,
						       (__mmask64) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbf6_hf8 (__mmask64 __U, __m512i __A)
{
  return (__m512i) __builtin_ia32_vcvtbf62hf8512_mask ((__v64qi)__A,
						       (__v64qi)
						       _mm512_setzero_si512 (),
						       (__mmask64) __U);
}

// VCVTHF62HF8 - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvthf6_hf8 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vcvthf62hf8128_mask ((__v16qi)__A,
						       (__v16qi)
						       _mm_undefined_si128 (),
						       (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvthf6_hf8 (__m128i __W, __mmask16 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vcvthf62hf8128_mask ((__v16qi)__A,
						       (__v16qi) __W,
						       (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvthf6_hf8 (__mmask16 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vcvthf62hf8128_mask ((__v16qi)__A,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask16) __U);
}

// VCVTHF62HF8 - 256-bit

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvthf6_hf8 (__m256i __A)
{
  return (__m256i) __builtin_ia32_vcvthf62hf8256_mask ((__v32qi)__A,
						       (__v32qi)
						       _mm256_undefined_si256 (),
						       (__mmask32) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvthf6_hf8 (__m256i __W, __mmask32 __U, __m256i __A)
{
  return (__m256i) __builtin_ia32_vcvthf62hf8256_mask ((__v32qi)__A,
						       (__v32qi) __W,
						       (__mmask32) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvthf6_hf8 (__mmask32 __U, __m256i __A)
{
  return (__m256i) __builtin_ia32_vcvthf62hf8256_mask ((__v32qi)__A,
						       (__v32qi)
						       _mm256_setzero_si256 (),
						       (__mmask32) __U);
}

// VCVTHF62HF8 - 512-bit

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvthf6_hf8 (__m512i __A)
{
  return (__m512i) __builtin_ia32_vcvthf62hf8512_mask ((__v64qi)__A,
						       (__v64qi)
						       _mm512_undefined_si512 (),
						       (__mmask64) -1);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvthf6_hf8 (__m512i __W, __mmask64 __U, __m512i __A)
{
  return (__m512i) __builtin_ia32_vcvthf62hf8512_mask ((__v64qi)__A,
						       (__v64qi) __W,
						       (__mmask64) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvthf6_hf8 (__mmask64 __U, __m512i __A)
{
  return (__m512i) __builtin_ia32_vcvthf62hf8512_mask ((__v64qi)__A,
						       (__v64qi)
						       _mm512_setzero_si512 (),
						       (__mmask64) __U);
}

// VPMOVSSDB - 128-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtss_epi32_epi8 (__m128i __A)
{
  return (__m128i) __builtin_ia32_vpmovssdb128_mask ((__v4si) __A,
						     (__v16qi)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtss_epi32_epi8 (__m128i __W, __mmask8 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vpmovssdb128_mask ((__v4si) __A,
						     (__v16qi) __W,
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtss_epi32_epi8 (__mmask8 __U, __m128i __A)
{
  return (__m128i) __builtin_ia32_vpmovssdb128_mask ((__v4si) __A,
						     (__v16qi)
						     _mm_setzero_si128 (),
						     (__mmask8) __U);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtss_epi32_storeu_epi8 (void * __P, __mmask8 __U, __m128i __A)
{
  __builtin_ia32_vpmovssdb128mem_mask ((unsigned int *) __P,
				       (__v4si) __A,
				       (__mmask8) __U);
}

// VPMOVSSDB - 256-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtss_epi32_epi8 (__m256i __A)
{
  return (__m128i) __builtin_ia32_vpmovssdb256_mask ((__v8si) __A,
						     (__v16qi)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtss_epi32_epi8 (__m128i __W, __mmask8 __U, __m256i __A)
{
  return (__m128i) __builtin_ia32_vpmovssdb256_mask ((__v8si) __A,
						     (__v16qi) __W,
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtss_epi32_epi8 (__mmask8 __U, __m256i __A)
{
  return (__m128i) __builtin_ia32_vpmovssdb256_mask ((__v8si) __A,
						     (__v16qi)
						     _mm_setzero_si128 (),
						     (__mmask8) __U);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtss_epi32_storeu_epi8 (void * __P, __mmask8 __U, __m256i __A)
{
  __builtin_ia32_vpmovssdb256mem_mask ((unsigned long long *) __P,
				       (__v8si) __A,
				       (__mmask8) __U);
}

// VPMOVSSDB - 512-bit

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtss_epi32_epi8 (__m512i __A)
{
  return (__m128i) __builtin_ia32_vpmovssdb512_mask ((__v16si) __A,
						     (__v16qi)
						     _mm_undefined_si128 (),
						     (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtss_epi32_epi8 (__m128i __W, __mmask16 __U, __m512i __A)
{
  return (__m128i) __builtin_ia32_vpmovssdb512_mask ((__v16si) __A,
						     (__v16qi) __W,
						     (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtss_epi32_epi8 (__mmask16 __U, __m512i __A)
{
  return (__m128i) __builtin_ia32_vpmovssdb512_mask ((__v16si) __A,
						     (__v16qi)
						     _mm_setzero_si128 (),
						     (__mmask16) __U);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtss_epi32_storeu_epi8 (void * __P, __mmask16 __U, __m512i __A)
{
  __builtin_ia32_vpmovssdb512mem_mask ((__v16qi *) __P,
				       (__v16si) __A,
				       (__mmask16) __U);
}

// VUNPACKB - 128-bit
#ifdef __OPTIMIZE__
extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_unpack_epi8 (__m128i __A, const int __B)
{
  return (__m128i) __builtin_ia32_vunpackb128_mask ((__v16qi) __A,
						    (const int) __B,
						    (__v16qi)
						    _mm_undefined_si128 (),
						    (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_unpack_epi8 (__m128i __W, __mmask16 __U,
		      __m128i __A, const int __B)
{
  return (__m128i) __builtin_ia32_vunpackb128_mask ((__v16qi) __A,
						    (const int) __B,
						    (__v16qi) __W,
						    (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_unpack_epi8 (__mmask16 __U, __m128i __A, const int __B)
{
  return (__m128i) __builtin_ia32_vunpackb128_mask ((__v16qi) __A,
						    (const int) __B,
						    (__v16qi)
						    _mm_setzero_si128 (),
						    (__mmask16) __U);
}

// VUNPACKB - 256-bit

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_unpack_epi8 (__m256i __A, const int __B)
{
  return (__m256i) __builtin_ia32_vunpackb256_mask ((__v32qi) __A,
						    (const int) __B,
						    (__v32qi)
						    _mm256_undefined_si256 (),
						    (__mmask32) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_unpack_epi8 (__m256i __W, __mmask32 __U,
			 __m256i __A, const int __B)
{
  return (__m256i) __builtin_ia32_vunpackb256_mask ((__v32qi) __A,
						    (const int) __B,
						    (__v32qi) __W,
						    (__mmask32) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_unpack_epi8 (__mmask32 __U, __m256i __A, const int __B)
{
  return (__m256i) __builtin_ia32_vunpackb256_mask ((__v32qi) __A,
						    (const int) __B,
						    (__v32qi)
						    _mm256_setzero_si256 (),
						    (__mmask32) __U);
}

// VUNPACKB - 512-bit

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_unpack_epi8 (__m512i __A, const int __B)
{
  return (__m512i) __builtin_ia32_vunpackb512_mask ((__v64qi) __A,
						    (const int) __B,
						    (__v64qi)
						    _mm512_undefined_si512 (),
						    (__mmask64) -1);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_unpack_epi8 (__m512i __W, __mmask64 __U, __m512i __A,
			 const int __B)
{
  return (__m512i) __builtin_ia32_vunpackb512_mask ((__v64qi) __A,
						    (const int) __B,
						    (__v64qi) __W,
						    (__mmask64) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_unpack_epi8 (__mmask64 __U, __m512i __A, const int __B)
{
  return (__m512i) __builtin_ia32_vunpackb512_mask ((__v64qi) __A,
						    (const int) __B,
						    (__v64qi)
						    _mm512_setzero_si512 (),
						    (__mmask64) __U);
}

#else
#define _mm_unpack_epi8(A, imm)					\
  ((__m128i) __builtin_ia32_vunpackb128_mask ((__v16qi)(__m128i)(A),	\
					      (int)(imm),		\
					      (__v16qi)(__m128i)	\
					      (_mm_undefined_si128 ()),	\
					      (__mmask16)(-1)))

#define _mm_mask_unpack_epi8(W, U, A, imm)				\
  ((__m128i) __builtin_ia32_vunpackb128_mask ((__v16qi)(__m128i)(A),	\
					      (int)(imm),		\
					      (__v16qi)(__m128i)(W),	\
					      (__mmask16)(U)))

#define _mm_maskz_unpack_epi8(U, A, imm)				\
  ((__m128i) __builtin_ia32_vunpackb128_mask ((__v16qi)(__m128i)(A),	\
					      (int)(imm),		\
					      (__v16qi)(__m128i)	\
					      (_mm_undefined_si128 ()),	\
					      (__mmask16)(U)))

#define _mm256_unpack_epi8(A, imm)					\
  ((__m256i) __builtin_ia32_vunpackb256_mask ((__v32qi)(__m256i)(A),	\
					      (int)(imm),		\
					      (__v32qi)(__m256i)	\
					      (_mm256_undefined_si256 ()),  \
					      (__mmask32)(-1)))

#define _mm256_mask_unpack_epi8(W, U, A, imm)				\
  ((__m256i) __builtin_ia32_vunpackb256_mask ((__v32qi)(__m256i)(A),	\
					      (int)(imm),		\
					      (__v32qi)(__m256i)(W),	\
					      (__mmask32)(U)))

#define _mm256_maskz_unpack_epi8(U, A, imm)				\
  ((__m256i) __builtin_ia32_vunpackb256_mask ((__v32qi)(__m256i)(A),	\
					      (int)(imm),		\
					      (__v32qi)(__m256i)	\
					      (_mm256_undefined_si256 ()),  \
					      (__mmask32)(U)))

#define _mm512_unpack_epi8(A, imm)					\
  ((__m512i) __builtin_ia32_vunpackb512_mask ((__v64qi)(__m512i)(A),	\
					      (int)(imm),		\
					      (__v64qi)(__m512i)	\
					      (_mm512_undefined_si512 ()),  \
					      (__mmask64)(-1)))

#define _mm512_mask_unpack_epi8(W, U, A, imm)				\
  ((__m512i) __builtin_ia32_vunpackb512_mask ((__v64qi)(__m512i)(A),	\
					      (int)(imm),		\
					      (__v64qi)(__m512i)(W),	\
					      (__mmask64)(U)))

#define _mm512_maskz_unpack_epi8(U, A, imm)				\
  ((__m512i) __builtin_ia32_vunpackb512_mask ((__v64qi)(__m512i)(A),	\
					      (int)(imm),		\
					      (__v64qi)(__m512i)	\
					      (_mm512_undefined_si512 ()),  \
					      (__mmask64)(U)))
#endif

#ifdef __DISABLE_AVX10V2AUX__
#undef __DISABLE_AVX10V2AUX__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10V2AUX__ */

#endif /* _AVX10V2AUXINTRIN_H_INCLUDED */
