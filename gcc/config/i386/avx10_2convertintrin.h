/* Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#if !defined(__AVX10_2_256__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2_256__
#endif /* __AVX10_2__ */

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
  return (__m256h) __builtin_ia32_vcvt2ps2phx256_mask_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v16hf)
							     _mm256_setzero_ph (),
							     (__mmask16) -1,
							     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtx2ps_ph (__m256h __W, __mmask16 __U, __m256 __A, __m256 __B)
{
  return (__m256h) __builtin_ia32_vcvt2ps2phx256_mask_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v16hf) __W,
							     (__mmask16) __U,
							     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtx2ps_ph ( __mmask16 __U, __m256 __A, __m256 __B)
{
  return (__m256h) __builtin_ia32_vcvt2ps2phx256_mask_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v16hf)
							     _mm256_setzero_ph (),
							     (__mmask16) __U,
							     _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtx_round2ps_ph (__m256 __A, __m256 __B, const int __R)
{
  return (__m256h) __builtin_ia32_vcvt2ps2phx256_mask_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v16hf)
							     _mm256_setzero_ph (),
							     (__mmask16) -1,
							     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtx_round2ps_ph (__m256h __W, __mmask16 __U, __m256 __A,
			      __m256 __B, const int __R)
{
  return (__m256h) __builtin_ia32_vcvt2ps2phx256_mask_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v16hf) __W,
							     (__mmask16) __U,
							     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtx_round2ps_ph (__mmask16 __U, __m256 __A,
			       __m256 __B, const int __R)
{
  return (__m256h) __builtin_ia32_vcvt2ps2phx256_mask_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v16hf)
							     _mm256_setzero_ph (),
							     (__mmask16) __U,
							     __R);
}

#else
#define _mm256_cvtx_round2ps_ph(A, B, R) \
  ((__m256h) __builtin_ia32_vcvt2ps2phx256_mask_round ((__v8sf) (A), \
						       (__v8sf) (B), \
						       (__v16hf) \
						       (_mm256_setzero_ph ()), \
						       (__mmask16) (-1), \
						       (R)))

#define _mm256_mask_cvtx_round2ps_ph(W, U, A, B, R) \
  ((__m256h) __builtin_ia32_vcvt2ps2phx256_mask_round ((__v8sf) (A), \
						       (__v8sf) (B),  \
						       (__v16hf) (W), \
						       (__mmask16) (U), \
						       (R)))

#define _mm256_maskz_cvtx_round2ps_ph(U, A, B, R) \
  ((__m256h) __builtin_ia32_vcvt2ps2phx256_mask_round ((__v8sf) (A), \
						       (__v8sf) (B),  \
						       (__v16hf) \
						       (_mm256_setzero_ph ()),  \
						       (__mmask16) (U), \
						       (R)))
#endif  /* __OPTIMIZE__  */

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbiasph_pbf8 (__m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8128 ((__v16qi) __A,
						     (__v8hf) __B);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbiasph_pbf8 (__m128i __W, __mmask8 __U, __m128i __A,
			     __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8128_mask ((__v16qi) __A,
							  (__v8hf) __B,
							  (__v16qi)(__m128i) __W,
							  (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbiasph_pbf8 (__mmask8 __U, __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8128_mask ((__v16qi) __A,
							  (__v8hf) __B,
							  (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							  (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbiasph_pbf8 (__m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i)
							  _mm_undefined_si128 (),
							  (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbiasph_pbf8 (__m128i __W, __mmask16 __U, __m256i __A,
				__m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i) __W,
							  (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbiasph_pbf8 (__mmask16 __U, __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							  (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbiassph_pbf8 (__m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s128 ((__v16qi) __A,
						      (__v8hf) __B);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbiassph_pbf8 (__m128i __W, __mmask8 __U,
			  __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s128_mask ((__v16qi) __A,
							   (__v8hf) __B,
							   (__v16qi)(__m128i) __W,
							   (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbiassph_pbf8 (__mmask8 __U, __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s128_mask ((__v16qi) __A,
							   (__v8hf) __B,
							   (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							   (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbiassph_pbf8 (__m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i)
							   _mm_undefined_si128 (),
							   (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbiassph_pbf8 (__m128i __W, __mmask16 __U,
			     __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i) __W,
							   (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbiassph_pbf8 (__mmask16 __U, __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2bf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							   (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbiasph_phf8 (__m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8128 ((__v16qi) __A,
						     (__v8hf) __B);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbiasph_phf8 (__m128i __W, __mmask8 __U, __m128i __A,
			     __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8128_mask ((__v16qi) __A,
							  (__v8hf) __B,
							  (__v16qi)(__m128i) __W,
							  (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbiasph_phf8 (__mmask8 __U, __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8128_mask ((__v16qi) __A,
							  (__v8hf) __B,
							  (__v16qi)(__m128i)
							  _mm_setzero_si128 (),
							  (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbiasph_phf8 (__m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i)
							  _mm_undefined_si128 (),
							  (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbiasph_phf8 (__m128i __W, __mmask16 __U,
			    __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i) __W,
							  (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbiasph_phf8 (__mmask16 __U, __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8256_mask ((__v32qi) __A,
							  (__v16hf) __B,
							  (__v16qi)(__m128i)
							  _mm_setzero_si128 (),
							  (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtbiassph_phf8 (__m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s128 ((__v16qi) __A,
						      (__v8hf) __B);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtbiassph_phf8 (__m128i __W, __mmask8 __U,
			  __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s128_mask ((__v16qi) __A,
							   (__v8hf) __B,
							   (__v16qi)(__m128i) __W,
							   (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtbiassph_phf8 (__mmask8 __U, __m128i __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s128_mask ((__v16qi) __A,
							   (__v8hf) __B,
							   (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							   (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtbiassph_phf8 (__m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i)
							   _mm_undefined_si128 (),
							   (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtbiassph_phf8 (__m128i __W, __mmask16 __U,
			     __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i) __W,
							   (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtbiassph_phf8 (__mmask16 __U, __m256i __A, __m256h __B)
{
  return (__m128i) __builtin_ia32_vcvtbiasph2hf8s256_mask ((__v32qi) __A,
							   (__v16hf) __B,
							   (__v16qi)(__m128i)
							   _mm_setzero_si128 (),
							   (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtne2ph_pbf8 (__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2bf8128_mask ((__v8hf) __A,
						 	 (__v8hf) __B,
							 (__v16qi)
							 _mm_setzero_si128 (),
							 (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtne2ph_pbf8 (__m128i __W, __mmask16 __U,
			__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2bf8128_mask ((__v8hf) __A,
							 (__v8hf) __B,
							 (__v16qi) __W,
							 (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtne2ph_pbf8 (__mmask16 __U, __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2bf8128_mask ((__v8hf) __A,
						 	 (__v8hf) __B,
							 (__v16qi)
							 _mm_setzero_si128 (),
							 (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtne2ph_pbf8 (__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2bf8256_mask ((__v16hf) __A,
							 (__v16hf) __B,
							 (__v32qi)
							 _mm256_setzero_si256 (),
							 (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtne2ph_pbf8 (__m256i __W, __mmask32 __U,
			   __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2bf8256_mask ((__v16hf) __A,
							 (__v16hf) __B,
							 (__v32qi) __W,
							 (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtne2ph_pbf8 (__mmask32 __U, __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2bf8256_mask ((__v16hf) __A,
							 (__v16hf) __B,
							 (__v32qi)
							 _mm256_setzero_si256 (),
							 (__mmask32) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtnes2ph_pbf8 (__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2bf8s128_mask ((__v8hf) __A,
							  (__v8hf) __B,
							  (__v16qi)
							  _mm_setzero_si128 (),
							  (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtnes2ph_pbf8 (__m128i __W, __mmask16 __U,
			 __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2bf8s128_mask ((__v8hf) __A,
							  (__v8hf) __B,
							  (__v16qi) __W,
							  (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtnes2ph_pbf8 (__mmask16 __U, __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2bf8s128_mask ((__v8hf) __A,
							  (__v8hf) __B,
							  (__v16qi)
							  _mm_setzero_si128 (),
							  (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtnes2ph_pbf8 (__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2bf8s256_mask ((__v16hf) __A,
							  (__v16hf) __B,
							  (__v32qi)
							  _mm256_setzero_si256 (),
							  (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtnes2ph_pbf8 (__m256i __W, __mmask32 __U,
			    __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2bf8s256_mask ((__v16hf) __A,
							  (__v16hf) __B,
							  (__v32qi) __W,
							  (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtnes2ph_pbf8 (__mmask32 __U, __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2bf8s256_mask ((__v16hf) __A,
							  (__v16hf) __B,
							  (__v32qi)
							  _mm256_setzero_si256 (),
							  (__mmask32) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtne2ph_phf8 (__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2hf8128_mask ((__v8hf) __A,
						    	 (__v8hf) __B,
							 (__v16qi)
							 _mm_setzero_si128 (),
							 (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtne2ph_phf8 (__m128i __W, __mmask16 __U,
			__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2hf8128_mask ((__v8hf) __A,
							 (__v8hf) __B,
							 (__v16qi) __W,
							 (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtne2ph_phf8 (__mmask16 __U, __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2hf8128_mask ((__v8hf) __A,
						    	 (__v8hf) __B,
							 (__v16qi)
							 _mm_setzero_si128 (),
							 (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtne2ph_phf8 (__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2hf8256_mask ((__v16hf) __A,
						    	 (__v16hf) __B,
							 (__v32qi)
							 _mm256_setzero_si256 (),
							 (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtne2ph_phf8 (__m256i __W, __mmask32 __U,
			   __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2hf8256_mask ((__v16hf) __A,
							 (__v16hf) __B,
							 (__v32qi) __W,
							 (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtne2ph_phf8 (__mmask32 __U, __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2hf8256_mask ((__v16hf) __A,
						    	 (__v16hf) __B,
							 (__v32qi)
							 _mm256_setzero_si256 (),
							 (__mmask32) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtnes2ph_phf8 (__m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2hf8s128_mask ((__v8hf) __A,
						     	  (__v8hf) __B,
							  (__v16qi)
							  _mm_setzero_si128 (),
							  (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtnes2ph_phf8 (__m128i __W, __mmask16 __U,
			 __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2hf8s128_mask ((__v8hf) __A,
						     	  (__v8hf) __B,
							  (__v16qi) __W,
							  (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtnes2ph_phf8 (__mmask16 __U, __m128h __A, __m128h __B)
{
  return (__m128i) __builtin_ia32_vcvtne2ph2hf8s128_mask ((__v8hf) __A,
						     	  (__v8hf) __B,
							  (__v16qi)
							  _mm_setzero_si128 (),
							  (__mmask16) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtnes2ph_phf8 (__m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2hf8s256_mask ((__v16hf) __A,
						     	  (__v16hf) __B,
							  (__v32qi)
							  _mm256_setzero_si256 (),
							  (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtnes2ph_phf8 (__m256i __W, __mmask32 __U,
			    __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2hf8s256_mask ((__v16hf) __A,
							  (__v16hf) __B,
							  (__v32qi) __W,
							  (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtnes2ph_phf8 (__mmask32 __U, __m256h __A, __m256h __B)
{
  return (__m256i) __builtin_ia32_vcvtne2ph2hf8s256_mask ((__v16hf) __A,
						     	  (__v16hf) __B,
							  (__v32qi)
							  _mm256_setzero_si256 (),
							  (__mmask32) __U);
}

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

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtneph_pbf8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8128_mask ((__v8hf) __A,
							(__v16qi)(__m128i)
							_mm_undefined_si128 (),
							(__mmask8) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtneph_pbf8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8128_mask ((__v8hf) __A,
							(__v16qi)(__m128i) __W,
							(__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtneph_pbf8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8128_mask ((__v8hf) __A,
							(__v16qi)(__m128i)
							_mm_setzero_si128 (),
							(__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtneph_pbf8 (__m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8256_mask ((__v16hf) __A,
							(__v16qi)(__m128i)
							_mm_undefined_si128 (),
							(__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtneph_pbf8 (__m128i __W, __mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8256_mask ((__v16hf) __A,
							(__v16qi)(__m128i) __W,
							(__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtneph_pbf8 (__mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8256_mask ((__v16hf) __A,
							(__v16qi)(__m128i)
							_mm_setzero_si128 (),
							(__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtnesph_pbf8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8s128_mask ((__v8hf) __A,
							 (__v16qi)(__m128i)
							 _mm_undefined_si128 (),
							 (__mmask8) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtnesph_pbf8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8s128_mask ((__v8hf) __A,
							 (__v16qi)(__m128i) __W,
							 (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtnesph_pbf8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8s128_mask ((__v8hf) __A,
							 (__v16qi)(__m128i)
							 _mm_setzero_si128 (),
							 (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtnesph_pbf8 (__m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8s256_mask ((__v16hf) __A,
							 (__v16qi)(__m128i)
							 _mm_undefined_si128 (),
							 (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtnesph_pbf8 (__m128i __W, __mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8s256_mask ((__v16hf) __A,
							 (__v16qi)(__m128i) __W,
							 (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtnesph_pbf8 (__mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2bf8s256_mask ((__v16hf) __A,
							 (__v16qi)(__m128i)
							 _mm_setzero_si128 (),
							 (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtneph_phf8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8128_mask ((__v8hf) __A,
							(__v16qi)(__m128i)
							_mm_undefined_si128 (),
							(__mmask8) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtneph_phf8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8128_mask ((__v8hf) __A,
							(__v16qi)(__m128i) __W,
							(__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtneph_phf8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8128_mask ((__v8hf) __A,
							(__v16qi)(__m128i)
							_mm_setzero_si128 (),
							(__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtneph_phf8 (__m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8256_mask ((__v16hf) __A,
							(__v16qi)(__m128i)
							_mm_undefined_si128 (),
							(__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtneph_phf8 (__m128i __W, __mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8256_mask ((__v16hf) __A,
							(__v16qi)(__m128i) __W,
							(__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtneph_phf8 (__mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8256_mask ((__v16hf) __A,
							(__v16qi)(__m128i)
							_mm_setzero_si128 (),
							(__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtnesph_phf8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8s128_mask ((__v8hf) __A,
							 (__v16qi)(__m128i)
							 _mm_undefined_si128 (),
							 (__mmask8) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtnesph_phf8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8s128_mask ((__v8hf) __A,
							 (__v16qi)(__m128i) __W,
							 (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtnesph_phf8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8s128_mask ((__v8hf) __A,
							 (__v16qi)(__m128i)
							 _mm_setzero_si128 (),
							 (__mmask8) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtnesph_phf8 (__m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8s256_mask ((__v16hf) __A,
							 (__v16qi)(__m128i)
							 _mm_undefined_si128 (),
							 (__mmask16) -1);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtnesph_phf8 (__m128i __W, __mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8s256_mask ((__v16hf) __A,
							 (__v16qi)(__m128i) __W,
							 (__mmask16) __U);
}

extern __inline__ __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtnesph_phf8 (__mmask16 __U, __m256h __A)
{
  return (__m128i) __builtin_ia32_vcvtneph2hf8s256_mask ((__v16hf) __A,
							 (__v16qi)(__m128i)
							 _mm_setzero_si128 (),
							 (__mmask16) __U);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtpbf8_ph (__m128i __A)
{
  return (__m128h) _mm_castsi128_ph ((__m128i) _mm_slli_epi16 (
	 (__m128i) _mm_cvtepi8_epi16 (__A), 8));
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtpbf8_ph (__m128h __S, __mmask8 __U, __m128i __A)
{
  return (__m128h) _mm_castsi128_ph ((__m128i) _mm_mask_slli_epi16 (
	 (__m128i) __S, __U, (__m128i) _mm_cvtepi8_epi16 (__A), 8));
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtpbf8_ph (__mmask8 __U, __m128i __A)
{
  return (__m128h) _mm_castsi128_ph ((__m128i) _mm_slli_epi16 (
	 (__m128i) _mm_maskz_cvtepi8_epi16 (__U, __A), 8));
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtpbf8_ph (__m128i __A)
{
  return (__m256h) _mm256_castsi256_ph ((__m256i) _mm256_slli_epi16 (
	 (__m256i) _mm256_cvtepi8_epi16 (__A), 8));
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtpbf8_ph (__m256h __S, __mmask8 __U, __m128i __A)
{
  return (__m256h) _mm256_castsi256_ph ((__m256i) _mm256_mask_slli_epi16 (
	 (__m256i) __S, __U, (__m256i) _mm256_cvtepi8_epi16 (__A), 8));
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtpbf8_ph (__mmask8 __U, __m128i __A)
{
  return (__m256h) _mm256_castsi256_ph ((__m256i) _mm256_slli_epi16 (
	 (__m256i) _mm256_maskz_cvtepi8_epi16 (__U, __A), 8));
}

#ifdef __DISABLE_AVX10_2_256__
#undef __DISABLE_AVX10_2_256__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2_256__ */

#endif /* __AVX10_2CONVERTINTRIN_H_INCLUDED */
