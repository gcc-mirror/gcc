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
#error "Never use <avx10v1auxintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10V1AUXINTRIN_H_INCLUDED
#define _AVX10V1AUXINTRIN_H_INCLUDED

#if !defined(__AVX10V1AUX__)
#pragma GCC push_options
#pragma GCC target("avx10v1aux")
#define __DISABLE_AVX10V1AUX__
#endif /* __AVX10V1AUX__ */

/* FP16 to FP8 converts(Single-source). */

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtph_bf8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8128_mask ((__v8hf) __A,
						      (__v16qi)(__m128i)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtph_bf8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8128_mask ((__v8hf) __A,
						      (__v16qi)(__m128i) __W,
						      (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtph_bf8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8128_mask ((__v8hf) __A,
						      (__v16qi)(__m128i)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtph_bf8 (__m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8256_mask ((__v16hf) __A,
						      (__v16qi)(__m128i)
						      _mm_undefined_si128 (),
						      (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtph_bf8 (__m128i __W, __mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8256_mask ((__v16hf) __A,
						      (__v16qi)(__m128i) __W,
						      (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtph_bf8 (__mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8256_mask ((__v16hf) __A,
						      (__v16qi)(__m128i)
						      _mm_setzero_si128 (),
						      (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtph_bf8 (__m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2bf8512_mask ((__v32hf) __A,
						      (__v32qi) (__m256i)
						      _mm256_undefined_si256 (),
						      (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtph_bf8 (__m256i __W, __mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2bf8512_mask ((__v32hf) __A,
						      (__v32qi) (__m256i) __W,
						      (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtph_bf8 (__mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2bf8512_mask ((__v32hf) __A,
						      (__v32qi) (__m256i)
						      _mm256_setzero_si256 (),
						      (__mmask32) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_ph_bf8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8s128_mask ((__v8hf) __A,
						       (__v16qi)(__m128i)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_ph_bf8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8s128_mask ((__v8hf) __A,
						       (__v16qi)(__m128i) __W,
						       (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_ph_bf8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8s128_mask ((__v8hf) __A,
						       (__v16qi)(__m128i)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_ph_bf8 (__m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8s256_mask ((__v16hf) __A,
						       (__v16qi)(__m128i)
						       _mm_undefined_si128 (),
						       (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_ph_bf8 (__m128i __W, __mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8s256_mask ((__v16hf) __A,
						       (__v16qi)(__m128i) __W,
						       (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_ph_bf8 (__mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2bf8s256_mask ((__v16hf) __A,
						       (__v16qi)(__m128i)
						       _mm_setzero_si128 (),
						       (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_ph_bf8 (__m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2bf8s512_mask ((__v32hf) __A,
						       (__v32qi) (__m256i)
						       _mm256_undefined_si256 (),
						       (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_ph_bf8 (__m256i __W, __mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2bf8s512_mask ((__v32hf) __A,
						       (__v32qi) (__m256i) __W,
						       (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_ph_bf8 (__mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2bf8s512_mask ((__v32hf) __A,
						       (__v32qi) (__m256i)
						       _mm256_setzero_si256 (),
						       (__mmask32) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtph_hf8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8128_mask ((__v8hf) __A,
						      (__v16qi)(__m128i)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtph_hf8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8128_mask ((__v8hf) __A,
						      (__v16qi)(__m128i) __W,
						      (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtph_hf8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8128_mask ((__v8hf) __A,
						      (__v16qi)(__m128i)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtph_hf8 (__m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8256_mask ((__v16hf) __A,
						      (__v16qi)(__m128i)
						      _mm_undefined_si128 (),
						      (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtph_hf8 (__m128i __W, __mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8256_mask ((__v16hf) __A,
						      (__v16qi)(__m128i) __W,
						      (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtph_hf8 (__mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8256_mask ((__v16hf) __A,
						      (__v16qi)(__m128i)
						      _mm_setzero_si128 (),
						      (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtph_hf8 (__m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2hf8512_mask ((__v32hf) __A,
						      (__v32qi) (__m256i)
						      _mm256_undefined_si256 (),
						      (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtph_hf8 (__m256i __W, __mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2hf8512_mask ((__v32hf) __A,
						      (__v32qi)(__m256i) __W,
						      (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtph_hf8 (__mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2hf8512_mask ((__v32hf) __A,
						      (__v32qi) (__m256i)
						      _mm256_setzero_si256 (),
						      (__mmask32) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_ph_hf8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8s128_mask ((__v8hf) __A,
						       (__v16qi)(__m128i)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_ph_hf8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8s128_mask ((__v8hf) __A,
						       (__v16qi)(__m128i) __W,
						       (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_ph_hf8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8s128_mask ((__v8hf) __A,
						       (__v16qi)(__m128i)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_ph_hf8 (__m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8s256_mask ((__v16hf) __A,
						       (__v16qi)(__m128i)
						       _mm_undefined_si128 (),
						       (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_ph_hf8 (__m128i __W, __mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8s256_mask ((__v16hf) __A,
						       (__v16qi)(__m128i) __W,
						       (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_ph_hf8 (__mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtph2hf8s256_mask ((__v16hf) __A,
						       (__v16qi)(__m128i)
						       _mm_setzero_si128 (),
						       (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_ph_hf8 (__m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2hf8s512_mask ((__v32hf) __A,
						       (__v32qi) (__m256i)
						       _mm256_undefined_si256 (),
						       (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_ph_hf8 (__m256i __W, __mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2hf8s512_mask ((__v32hf) __A,
						       (__v32qi) (__m256i) __W,
						       (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_ph_hf8 (__mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtph2hf8s512_mask ((__v32hf) __A,
						       (__v32qi) (__m256i)
						       _mm256_setzero_si256 (),
						       (__mmask32) __U);
}

/* FP16 to FP8 converts(Two-source). */

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt2ph_bf8 (__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2bf8128_mask ((__v8hf) __A,
						       (__v8hf) __B,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvt2ph_bf8 (__m128i __W, __mmask16 __U,
		     __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2bf8128_mask ((__v8hf) __A,
						       (__v8hf) __B,
						       (__v16qi) __W,
						       (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvt2ph_bf8 (__mmask16 __U, __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2bf8128_mask ((__v8hf) __A,
						       (__v8hf) __B,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt2ph_bf8 (__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2bf8256_mask ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v32qi)
						       _mm256_setzero_si256 (),
						       (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt2ph_bf8 (__m256i __W, __mmask32 __U,
			__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2bf8256_mask ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v32qi) __W,
						       (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt2ph_bf8 (__mmask32 __U, __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2bf8256_mask ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v32qi)
						       _mm256_setzero_si256 (),
						       (__mmask32) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt2ph_bf8 (__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2bf8512_mask ((__v32hf) __A,
						       (__v32hf) __B,
						       (__v64qi)
						       _mm512_setzero_si512 (),
						       (__mmask64) -1);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt2ph_bf8 (__m512i __W, __mmask64 __U,
			__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2bf8512_mask ((__v32hf) __A,
						       (__v32hf) __B,
						       (__v64qi) __W,
						       (__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt2ph_bf8 (__mmask64 __U,  __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2bf8512_mask ((__v32hf) __A,
						       (__v32hf) __B,
						       (__v64qi)
						       _mm512_setzero_si512 (),
						       (__mmask64) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_2ph_bf8 (__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2bf8s128_mask ((__v8hf) __A,
							(__v8hf) __B,
							(__v16qi)
							_mm_setzero_si128 (),
							(__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_2ph_bf8 (__m128i __W, __mmask16 __U,
		       __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2bf8s128_mask ((__v8hf) __A,
							(__v8hf) __B,
							(__v16qi) __W,
							(__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_2ph_bf8 (__mmask16 __U, __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2bf8s128_mask ((__v8hf) __A,
							(__v8hf) __B,
							(__v16qi)
							_mm_setzero_si128 (),
							(__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_2ph_bf8 (__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2bf8s256_mask ((__v16hf) __A,
							(__v16hf) __B,
							(__v32qi)
							_mm256_setzero_si256 (),
							(__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_2ph_bf8 (__m256i __W, __mmask32 __U,
			  __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2bf8s256_mask ((__v16hf) __A,
							(__v16hf) __B,
							(__v32qi) __W,
							(__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_2ph_bf8 (__mmask32 __U, __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2bf8s256_mask ((__v16hf) __A,
							(__v16hf) __B,
							(__v32qi)
							_mm256_setzero_si256 (),
							(__mmask32) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_2ph_bf8 (__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2bf8s512_mask ((__v32hf) __A,
							(__v32hf) __B,
							(__v64qi)
							_mm512_setzero_si512 (),
							(__mmask64) -1);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_2ph_bf8 (__m512i __W, __mmask64 __U,
			  __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2bf8s512_mask ((__v32hf) __A,
							(__v32hf) __B,
							(__v64qi) __W,
							(__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_2ph_bf8 (__mmask64 __U, __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2bf8s512_mask ((__v32hf) __A,
							(__v32hf) __B,
							(__v64qi)
							_mm512_setzero_si512 (),
							(__mmask64) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt2ph_hf8 (__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2hf8128_mask ((__v8hf) __A,
						       (__v8hf) __B,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvt2ph_hf8 (__m128i __W, __mmask16 __U,
		     __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2hf8128_mask ((__v8hf) __A,
						       (__v8hf) __B,
						       (__v16qi) __W,
						       (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvt2ph_hf8 (__mmask16 __U, __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2hf8128_mask ((__v8hf) __A,
						       (__v8hf) __B,
						       (__v16qi)
						       _mm_setzero_si128 (),
						       (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt2ph_hf8 (__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2hf8256_mask ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v32qi)
						       _mm256_setzero_si256 (),
						       (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt2ph_hf8 (__m256i __W, __mmask32 __U,
			__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2hf8256_mask ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v32qi) __W,
						       (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt2ph_hf8 (__mmask32 __U, __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2hf8256_mask ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v32qi)
						       _mm256_setzero_si256 (),
						       (__mmask32) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt2ph_hf8 (__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2hf8512_mask ((__v32hf) __A,
						       (__v32hf) __B,
						       (__v64qi)
						       _mm512_setzero_si512 (),
						       (__mmask64) -1);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt2ph_hf8 (__m512i __W, __mmask64 __U,
			__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2hf8512_mask ((__v32hf) __A,
						       (__v32hf) __B,
						       (__v64qi) __W,
						       (__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt2ph_hf8 (__mmask64 __U, __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2hf8512_mask ((__v32hf) __A,
						       (__v32hf) __B,
						       (__v64qi)
						       _mm512_setzero_si512 (),
						       (__mmask64) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_2ph_hf8 (__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2hf8s128_mask ((__v8hf) __A,
							(__v8hf) __B,
							(__v16qi)
							_mm_setzero_si128 (),
							(__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_2ph_hf8 (__m128i __W, __mmask16 __U,
		       __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2hf8s128_mask ((__v8hf) __A,
							(__v8hf) __B,
							(__v16qi) __W,
							(__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_2ph_hf8 (__mmask16 __U, __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvt2ph2hf8s128_mask ((__v8hf) __A,
							(__v8hf) __B,
							(__v16qi)
							_mm_setzero_si128 (),
							(__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_2ph_hf8 (__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2hf8s256_mask ((__v16hf) __A,
							(__v16hf) __B,
							(__v32qi)
							_mm256_setzero_si256 (),
							(__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_2ph_hf8 (__m256i __W, __mmask32 __U,
			  __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2hf8s256_mask ((__v16hf) __A,
							(__v16hf) __B,
							(__v32qi) __W,
							(__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_2ph_hf8 (__mmask32 __U, __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvt2ph2hf8s256_mask ((__v16hf) __A,
							(__v16hf) __B,
							(__v32qi)
							_mm256_setzero_si256 (),
							(__mmask32) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_2ph_hf8 (__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2hf8s512_mask ((__v32hf) __A,
							(__v32hf) __B,
							(__v64qi)
							_mm512_setzero_si512 (),
							(__mmask64) -1);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_2ph_hf8 (__m512i __W, __mmask64 __U,
			  __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2hf8s512_mask ((__v32hf) __A,
							(__v32hf) __B,
							(__v64qi) __W,
							(__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_2ph_hf8 (__mmask64 __U, __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvt2ph2hf8s512_mask ((__v32hf) __A,
							(__v32hf) __B,
							(__v64qi)
							_mm512_setzero_si512 (),
							(__mmask64) __U);
}

/* FP16 to FP8 Converts (Biased) */

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbiasph_bf8 (__m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8128 ((__v16qi) __A,
						     (__v8hf) __B);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbiasph_bf8 (__m128i __W, __mmask8 __U, __m128i __A,
			__m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8128_mask ((__v16qi) __A,
							  (__v8hf) __B,
							  (__v16qi)(__m128i) __W,
							  (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbiasph_bf8 (__mmask8 __U, __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8128_mask ((__v16qi) __A,
							  (__v8hf) __B,
							  (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							  (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbiasph_bf8 (__m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i)
							  _mm_undefined_si128 (),
							  (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbiasph_bf8 (__m128i __W, __mmask16 __U, __m256i __A,
			   __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i) __W,
							  (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbiasph_bf8 (__mmask16 __U, __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							  (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbiasph_bf8 (__m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i)
							  _mm256_undefined_si256 (),
							  (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbiasph_bf8 (__m256i __W, __mmask32 __U,
			   __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i) __W,
							  (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbiasph_bf8 (__mmask32 __U, __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i)
							  _mm256_setzero_si256 (),
							  (__mmask32) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_biasph_bf8 (__m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s128 ((__v16qi) __A,
						      (__v8hf) __B);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_biasph_bf8 (__m128i __W, __mmask8 __U,
			  __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s128_mask ((__v16qi) __A,
							   (__v8hf) __B,
							   (__v16qi)(__m128i) __W,
							   (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_biasph_bf8 (__mmask8 __U, __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s128_mask ((__v16qi) __A,
							   (__v8hf) __B,
							   (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							   (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_biasph_bf8 (__m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i)
							   _mm_undefined_si128 (),
							   (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_biasph_bf8 (__m128i __W, __mmask16 __U,
			     __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i) __W,
							   (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_biasph_bf8 (__mmask16 __U, __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							   (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_biasph_bf8 (__m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i)
							   _mm256_undefined_si256 (),
							   (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_biasph_bf8 (__m256i __W, __mmask32 __U,
			     __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i) __W,
							   (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_biasph_bf8 (__mmask32 __U, __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i)
							   _mm256_setzero_si256 (),
							   (__mmask32) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbiasph_hf8 (__m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8128 ((__v16qi) __A,
						     (__v8hf) __B);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbiasph_hf8 (__m128i __W, __mmask8 __U, __m128i __A,
			__m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8128_mask ((__v16qi) __A,
							  (__v8hf) __B,
							  (__v16qi)(__m128i) __W,
							  (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbiasph_hf8 (__mmask8 __U, __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8128_mask ((__v16qi) __A,
							  (__v8hf) __B,
							  (__v16qi)(__m128i)
							  _mm_setzero_si128 (),
							  (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbiasph_hf8 (__m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i)
							  _mm_undefined_si128 (),
							  (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbiasph_hf8 (__m128i __W, __mmask16 __U,
			   __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i) __W,
							  (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbiasph_hf8 (__mmask16 __U, __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i)
							  _mm_setzero_si128 (),
							  (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbiasph_hf8 (__m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i)
					 		  _mm256_undefined_si256 (),
							  (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbiasph_hf8 (__m256i __W, __mmask32 __U, __m512i __A,
			   __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i) __W,
							  (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbiasph_hf8 (__mmask32 __U, __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i)
							  _mm256_setzero_si256 (),
							  (__mmask32) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvts_biasph_hf8 (__m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s128 ((__v16qi) __A,
						      (__v8hf) __B);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvts_biasph_hf8 (__m128i __W, __mmask8 __U,
			  __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s128_mask ((__v16qi) __A,
							   (__v8hf) __B,
							   (__v16qi)(__m128i) __W,
							   (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvts_biasph_hf8 (__mmask8 __U, __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s128_mask ((__v16qi) __A,
							   (__v8hf) __B,
							   (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							   (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvts_biasph_hf8 (__m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i)
							   _mm_undefined_si128 (),
							   (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvts_biasph_hf8 (__m128i __W, __mmask16 __U,
			     __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i) __W,
							   (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvts_biasph_hf8 (__mmask16 __U, __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							   (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvts_biasph_hf8 (__m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i)
							   _mm256_undefined_si256 (),
							   (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvts_biasph_hf8 (__m256i __W, __mmask32 __U,
			     __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i) __W,
							   (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvts_biasph_hf8 (__mmask32 __U, __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i)
							   _mm256_setzero_si256 (),
							   (__mmask32) __U);
}

/* FP8 to FP16 Converts */

extern __inline__ __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvthf8_ph (__m128i __A)
{
  return (__m128h) __builtin_ia32_vcvthf82ph128_mask ((__v16qi) __A,
						      (__v8hf)(__m128h)
						      _mm_undefined_ph (),
						      (__mmask8) -1);
}

extern __inline__ __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvthf8_ph (__m128h __W, __mmask8 __U, __m128i __A)
{
  return (__m128h) __builtin_ia32_vcvthf82ph128_mask ((__v16qi) __A,
						      (__v8hf)(__m128h) __W,
						      (__mmask8) __U);
}

extern __inline__ __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvthf8_ph (__mmask8 __U, __m128i __A)
{
  return (__m128h) __builtin_ia32_vcvthf82ph128_mask ((__v16qi) __A,
						      (__v8hf)(__m128h)
						      _mm_setzero_ph (),
						      (__mmask8) __U);
}

extern __inline__ __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvthf8_ph (__m128i __A)
{
  return (__m256h) __builtin_ia32_vcvthf82ph256_mask ((__v16qi) __A,
						      (__v16hf)(__m256h)
						      _mm256_undefined_ph (),
						      (__mmask16) -1);
}

extern __inline__ __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvthf8_ph (__m256h __W, __mmask16 __U, __m128i __A)
{
  return (__m256h) __builtin_ia32_vcvthf82ph256_mask ((__v16qi) __A,
						      (__v16hf)(__m256h) __W,
						      (__mmask16) __U);
}

extern __inline__ __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvthf8_ph (__mmask16 __U, __m128i __A)
{
  return (__m256h) __builtin_ia32_vcvthf82ph256_mask ((__v16qi) __A,
						      (__v16hf)(__m256h)
						      _mm256_setzero_ph (),
						      (__mmask16) __U);
}

extern __inline__ __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvthf8_ph (__m256i __A)
{
  return (__m512h) __builtin_ia32_vcvthf82ph512_mask ((__v32qi) __A,
						      (__v32hf) (__m512h)
						      _mm512_undefined_ph (),
						      (__mmask32) -1);
}

extern __inline__ __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvthf8_ph (__m512h __W, __mmask32 __U, __m256i __A)
{
  return (__m512h) __builtin_ia32_vcvthf82ph512_mask ((__v32qi) __A,
						      (__v32hf) (__m512h) __W,
						      (__mmask32) __U);
}

extern __inline__ __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvthf8_ph (__mmask32 __U, __m256i __A)
{
  return (__m512h) __builtin_ia32_vcvthf82ph512_mask ((__v32qi) __A,
						      (__v32hf) (__m512h)
						      _mm512_setzero_ph (),
						      (__mmask32) __U);
}

/* FP32-Pair to FP16 Converts */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtx2ps_ph (__m128 __A, __m128 __B)
{
  return (__m128h) __builtin_ia32_vcvt2ps2phx128_mask ((__v4sf) __A,
						       (__v4sf) __B,
						       (__v8hf)
						       _mm_setzero_ph (),
						       (__mmask8) -1);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtx2ps_ph (__m128h __W, __mmask8 __U, __m128 __A, __m128 __B)
{
  return (__m128h) __builtin_ia32_vcvt2ps2phx128_mask ((__v4sf) __A,
						       (__v4sf) __B,
						       (__v8hf) __W,
						       (__mmask8) __U);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtx2ps_ph (__mmask8 __U, __m128 __A, __m128 __B)
{
  return (__m128h) __builtin_ia32_vcvt2ps2phx128_mask ((__v4sf) __A,
						       (__v4sf) __B,
						       (__v8hf)
						       _mm_setzero_ph (),
						       (__mmask8) __U);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtx2ps_ph (__m256 __A, __m256 __B)
{
  return (__m256h) __builtin_ia32_vcvt2ps2phx256_mask ((__v8sf) __A,
						       (__v8sf) __B,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) -1);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtx2ps_ph (__m256h __W, __mmask16 __U, __m256 __A, __m256 __B)
{
  return (__m256h) __builtin_ia32_vcvt2ps2phx256_mask ((__v8sf) __A,
						       (__v8sf) __B,
						       (__v16hf) __W,
						       (__mmask16) __U);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtx2ps_ph ( __mmask16 __U, __m256 __A, __m256 __B)
{
  return (__m256h) __builtin_ia32_vcvt2ps2phx256_mask ((__v8sf) __A,
						       (__v8sf) __B,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) __U);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtx2ps_ph (__m512 __A, __m512 __B)
{
  return (__m512h) __builtin_ia32_vcvt2ps2phx512_mask_round ((__v16sf) __A,
							     (__v16sf) __B,
							     (__v32hf)
							     _mm512_setzero_ph (),
							     (__mmask32) -1,
							     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtx2ps_ph (__m512h __W, __mmask32 __U, __m512 __A,
			__m512 __B)
{
  return (__m512h) __builtin_ia32_vcvt2ps2phx512_mask_round ((__v16sf) __A,
							     (__v16sf) __B,
							     (__v32hf) __W,
							     (__mmask32) __U,
							     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtx2ps_ph (__mmask32 __U, __m512 __A, __m512 __B)
{
  return (__m512h) __builtin_ia32_vcvt2ps2phx512_mask_round ((__v16sf) __A,
							     (__v16sf) __B,
							     (__v32hf)
							     _mm512_setzero_ph (),
							     (__mmask32) __U,
							     _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtx_round2ps_ph (__m512 __A, __m512 __B, const int __R)
{
  return (__m512h) __builtin_ia32_vcvt2ps2phx512_mask_round ((__v16sf) __A,
							    (__v16sf) __B,
							    (__v32hf)
							    _mm512_setzero_ph (),
							    (__mmask32) -1,
							    __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtx_round2ps_ph (__m512h __W, __mmask32 __U, __m512 __A,
			      __m512 __B, const int __R)
{
  return (__m512h) __builtin_ia32_vcvt2ps2phx512_mask_round ((__v16sf) __A,
							    (__v16sf) __B,
							    (__v32hf) __W,
							    (__mmask32) __U,
							    __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtx_round2ps_ph (__mmask32 __U, __m512 __A,
			      __m512 __B, const int __R)
{
  return (__m512h) __builtin_ia32_vcvt2ps2phx512_mask_round ((__v16sf) __A,
							    (__v16sf) __B,
							    (__v32hf)
							    _mm512_setzero_ph (),
							    (__mmask32) __U,
							    __R);
}

#else
#define _mm512_cvtx_round2ps_ph(A, B, R) \
  ((__m512h) __builtin_ia32_vcvt2ps2phx512_mask_round ((__v16sf) (A), \
						       (__v16sf) (B), \
						       (__v32hf) \
						       (_mm512_setzero_ph ()), \
						       (__mmask32) (-1), \
						       (R)))
#define _mm512_mask_cvtx_round2ps_ph(W, U, A, B, R) \
  ((__m512h) __builtin_ia32_vcvt2ps2phx512_mask_round ((__v16sf) (A), \
						       (__v16sf) (B), \
						       (__v32hf) (W), \
						       (__mmask32) (U), \
						       (R)))
#define _mm512_maskz_cvtx_round2ps_ph(U, A, B, R) \
  ((__m512h) __builtin_ia32_vcvt2ps2phx512_mask_round ((__v16sf) (A), \
						       (__v16sf) (B), \
						       (__v32hf) \
						       (_mm512_setzero_ph ()), \
						       (__mmask32) (U), \
						       (R)))
#endif  /* __OPTIMIZE__  */

#define _mm_dpbssd_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbssd128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbssds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbssds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbsud_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbsud128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbsuds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbsuds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbuud_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbuud128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbuuds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbuuds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm256_dpbssd_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbssd256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbssds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbssds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbsud_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbsud256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbsuds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbsuds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbuud_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbuud256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbuuds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbuuds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm_dpwsud_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwsud128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwsuds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwsuds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwusd_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwusd128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwusds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwusds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwuud_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwuud128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwuuds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwuuds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm256_dpwsud_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwsud256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwsuds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwsuds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwusd_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwusd256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwusds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwusds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwuud_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwuud256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwuuds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwuuds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbssd_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssd_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbssd_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssd_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbssds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbssds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbsud_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsud_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbsud_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsud_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbsuds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsuds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbsuds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsuds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbuud_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuud_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbuud_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuud_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbuuds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuuds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbuuds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuuds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbssd_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssd_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbssd_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssd_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbssds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbssds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbsud_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsud_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbsud_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsud_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbsuds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsuds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbsuds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsuds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbuud_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuud_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbuud_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuud_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbuuds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuuds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbuuds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuuds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbssd_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssd512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbssd_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssd_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbssd_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssd_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbssds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbssds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbssds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbsud_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsud512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbsud_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsud_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbsud_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsud_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbsuds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsuds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbsuds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsuds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbsuds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsuds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbuud_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuud512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbuud_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuud_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbuud_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuud_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbuuds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuuds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbuuds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuuds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbuuds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuuds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwsud_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwsud_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwsud_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwsud_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwsuds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwsuds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwsuds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwsuds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwusd_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwusd_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwusd_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwusd_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwusds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwusds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwusds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwusds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwuud_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwuud_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwuud_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwuud_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwuuds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwuuds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwuuds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwuuds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwsud_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwsud_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwsud_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwsud_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwsuds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwsuds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwsuds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwsuds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwusd_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwusd_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwusd_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwusd_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwusds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwusds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwusds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwusds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwuud_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwuud_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwuud_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwuud_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwuuds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwuuds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwuuds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwuuds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwsud_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsud512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwsud_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsud_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwsud_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsud_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwsuds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsuds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwsuds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsuds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwsuds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsuds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwusd_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusd512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwusd_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusd_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwusd_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusd_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwusds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwusds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwusds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwuud_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuud512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwuud_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuud_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwuud_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuud_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwuuds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuuds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwuuds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuuds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwuuds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuuds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

#ifdef __DISABLE_AVX10V1AUX__
#undef __DISABLE_AVX10V1AUX__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10V1AUX__ */

#endif /* __AVX10V1AUXINTRIN_H_INCLUDED */
