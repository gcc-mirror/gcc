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

#ifndef _IMMINTRIN_H_INCLUDED
#error "Never use <avx10_2-512convertintrin.h> directly; include <immintrin.h> instead."
#endif // _IMMINTRIN_H_INCLUDED

#ifndef __AVX10_2_512CONVERTINTRIN_H_INCLUDED
#define __AVX10_2_512CONVERTINTRIN_H_INCLUDED

#ifndef __AVX10_2_512__
#pragma GCC push_options
#pragma GCC target("avx10.2-512")
#define __DISABLE_AVX10_2_512__
#endif /* __AVX10_2_512__ */

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

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbiasph_pbf8 (__m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i)
							  _mm256_undefined_si256 (),
							  (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbiasph_pbf8 (__m256i __W, __mmask32 __U,
			    __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i) __W,
							  (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbiasph_pbf8 (__mmask32 __U, __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i)
							  _mm256_setzero_si256 (),
							  (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbiassph_pbf8 (__m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i)
							   _mm256_undefined_si256 (),
							   (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbiassph_pbf8 (__m256i __W, __mmask32 __U,
			     __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i) __W,
							   (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbiassph_pbf8 (__mmask32 __U, __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2bf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i)
							   _mm256_setzero_si256 (),
							   (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbiasph_phf8 (__m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i)
					 		  _mm256_undefined_si256 (),
							  (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbiasph_phf8 (__m256i __W, __mmask32 __U, __m512i __A,
				__m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i) __W,
							  (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbiasph_phf8 (__mmask32 __U, __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8512_mask ((__v64qi) __A,
							  (__v32hf) __B,
							  (__v32qi)(__m256i)
							  _mm256_setzero_si256 (),
							  (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtbiassph_phf8 (__m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i)
							   _mm256_undefined_si256 (),
							   (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtbiassph_phf8 (__m256i __W, __mmask32 __U,
			     __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i) __W,
							   (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtbiassph_phf8 (__mmask32 __U, __m512i __A, __m512h __B)
{
  return (__m256i) __builtin_ia32_vcvtbiasph2hf8s512_mask ((__v64qi) __A,
							   (__v32hf) __B,
							   (__v32qi)(__m256i)
							   _mm256_setzero_si256 (),
							   (__mmask32) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtne2ph_pbf8 (__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2bf8512_mask ((__v32hf) __A,
							 (__v32hf) __B,
							 (__v64qi)
							 _mm512_setzero_si512 (),
							 (__mmask64) -1);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtne2ph_pbf8 (__m512i __W, __mmask64 __U,
			   __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2bf8512_mask ((__v32hf) __A,
							 (__v32hf) __B,
							 (__v64qi) __W,
							 (__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtne2ph_pbf8 (__mmask64 __U,  __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2bf8512_mask ((__v32hf) __A,
							 (__v32hf) __B,
							 (__v64qi)
							 _mm512_setzero_si512 (),
							 (__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtnes2ph_pbf8 (__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2bf8s512_mask ((__v32hf) __A,
							  (__v32hf) __B,
							  (__v64qi)
							  _mm512_setzero_si512 (),
							  (__mmask64) -1);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtnes2ph_pbf8 (__m512i __W, __mmask64 __U,
			    __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2bf8s512_mask ((__v32hf) __A,
							  (__v32hf) __B,
							  (__v64qi) __W,
							  (__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtnes2ph_pbf8 (__mmask64 __U, __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2bf8s512_mask ((__v32hf) __A,
							  (__v32hf) __B,
							  (__v64qi)
							  _mm512_setzero_si512 (),
							  (__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtne2ph_phf8 (__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2hf8512_mask ((__v32hf) __A,
							 (__v32hf) __B,
							 (__v64qi)
							 _mm512_setzero_si512 (),
						 	 (__mmask64) -1);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtne2ph_phf8 (__m512i __W, __mmask64 __U,
			   __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2hf8512_mask ((__v32hf) __A,
							 (__v32hf) __B,
							 (__v64qi) __W,
							 (__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtne2ph_phf8 (__mmask64 __U, __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2hf8512_mask ((__v32hf) __A,
							 (__v32hf) __B,
							 (__v64qi)
							 _mm512_setzero_si512 (),
						 	 (__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtnes2ph_phf8 (__m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2hf8s512_mask ((__v32hf) __A,
							  (__v32hf) __B,
							  (__v64qi)
							  _mm512_setzero_si512 (),
							  (__mmask64) -1);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtnes2ph_phf8 (__m512i __W, __mmask64 __U,
			    __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2hf8s512_mask ((__v32hf) __A,
							  (__v32hf) __B,
							  (__v64qi) __W,
							  (__mmask64) __U);
}

extern __inline__ __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtnes2ph_phf8 (__mmask64 __U, __m512h __A, __m512h __B)
{
  return (__m512i) __builtin_ia32_vcvtne2ph2hf8s512_mask ((__v32hf) __A,
							  (__v32hf) __B,
							  (__v64qi)
							  _mm512_setzero_si512 (),
							  (__mmask64) __U);
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

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtneph_pbf8 (__m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2bf8512_mask ((__v32hf) __A,
							(__v32qi) (__m256i)
							_mm256_undefined_si256 (),
							(__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtneph_pbf8 (__m256i __W, __mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2bf8512_mask ((__v32hf) __A,
							(__v32qi) (__m256i) __W,
							(__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtneph_pbf8 (__mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2bf8512_mask ((__v32hf) __A,
							(__v32qi) (__m256i)
							_mm256_setzero_si256 (),
							(__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtnesph_pbf8 (__m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2bf8s512_mask ((__v32hf) __A,
							 (__v32qi) (__m256i)
							 _mm256_undefined_si256 (),
							 (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtnesph_pbf8 (__m256i __W, __mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2bf8s512_mask ((__v32hf) __A,
							 (__v32qi) (__m256i) __W,
							 (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtnesph_pbf8 (__mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2bf8s512_mask ((__v32hf) __A,
							 (__v32qi) (__m256i)
							 _mm256_setzero_si256 (),
							 (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtneph_phf8 (__m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2hf8512_mask ((__v32hf) __A,
							(__v32qi) (__m256i)
							_mm256_undefined_si256 (),
							(__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtneph_phf8 (__m256i __W, __mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2hf8512_mask ((__v32hf) __A,
							(__v32qi)(__m256i) __W,
							(__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtneph_phf8 (__mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2hf8512_mask ((__v32hf) __A,
							(__v32qi) (__m256i)
							_mm256_setzero_si256 (),
							(__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtnesph_phf8 (__m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2hf8s512_mask ((__v32hf) __A,
							 (__v32qi) (__m256i)
							 _mm256_undefined_si256 (),
							 (__mmask32) -1);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtnesph_phf8 (__m256i __W, __mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2hf8s512_mask ((__v32hf) __A,
							 (__v32qi) (__m256i) __W,
							 (__mmask32) __U);
}

extern __inline__ __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtnesph_phf8 (__mmask32 __U, __m512h __A)
{
  return (__m256i) __builtin_ia32_vcvtneph2hf8s512_mask ((__v32hf) __A,
							 (__v32qi) (__m256i)
							 _mm256_setzero_si256 (),
							 (__mmask32) __U);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtpbf8_ph (__m256i __A)
{
  return (__m512h) _mm512_castsi512_ph ((__m512i) _mm512_slli_epi16 (
	 (__m512i) _mm512_cvtepi8_epi16 (__A), 8));
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtpbf8_ph (__m512h __S, __mmask16 __U, __m256i __A)
{
  return (__m512h) _mm512_castsi512_ph ((__m512i) _mm512_mask_slli_epi16 (
	 (__m512i) __S, __U, (__m512i) _mm512_cvtepi8_epi16 (__A), 8));
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtpbf8_ph (__mmask16 __U, __m256i __A)
{
  return (__m512h) _mm512_castsi512_ph ((__m512i) _mm512_slli_epi16 (
	 (__m512i) _mm512_maskz_cvtepi8_epi16 (__U, __A), 8));
}

#ifdef __DISABLE_AVX10_2_512__
#undef __DISABLE_AVX10_2_512__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2_512__ */

#endif /* __AVX10_2_512CONVERTINTRIN_H_INCLUDED */
