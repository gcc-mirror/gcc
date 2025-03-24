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
#error "Never use <avx10_2roundingintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2ROUNDINGINTRIN_H_INCLUDED
#define _AVX10_2ROUNDINGINTRIN_H_INCLUDED

#ifndef __AVX10_2_256__
#pragma GCC push_options
#pragma GCC target("avx10.2-256")
#define __DISABLE_AVX10_2_256__
#endif /* __AVX10_2_256__ */

#ifdef  __OPTIMIZE__
extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_add_round_pd (__m256d __A, __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_addpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_undefined_pd (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_add_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			  __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_addpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df) __W,
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_add_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			   const int __R)
{
  return (__m256d) __builtin_ia32_addpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_setzero_pd (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_add_round_ph (__m256h __A, __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_addph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_undefined_ph (),
						       (__mmask16) -1,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_add_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			  __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_addph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf) __W,
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_add_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			   const int __R)
{
  return (__m256h) __builtin_ia32_addph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_add_round_ps (__m256 __A, __m256 __B, const int __R)
{
  return (__m256) __builtin_ia32_addps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_undefined_ps (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_add_round_ps (__m256 __W, __mmask8 __U, __m256 __A, __m256 __B,
			  const int __R)
{
  return (__m256) __builtin_ia32_addps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf) __W,
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_add_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			   const int __R)
{
  return (__m256) __builtin_ia32_addps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_setzero_ps (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cmp_round_pd_mask (__m256d __A, __m256d __B, const int __C,
			  const int __R)
{
  return (__mmask8) __builtin_ia32_cmppd256_mask_round ((__v4df) __A,
							(__v4df) __B,
							__C,
							(__mmask8) -1,
							__R);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cmp_round_pd_mask (__mmask8 __U, __m256d __A, __m256d __B,
			       const int __C, const int __R)
{
  return (__mmask8) __builtin_ia32_cmppd256_mask_round ((__v4df) __A,
							(__v4df) __B,
							__C,
							(__mmask8) __U,
							__R);
}

extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cmp_round_ph_mask (__m256h __A, __m256h __B, const int __C,
			  const int __R)
{
  return (__mmask16) __builtin_ia32_cmpph256_mask_round ((__v16hf) __A,
							 (__v16hf) __B,
							 __C,
							 (__mmask16) -1,
							 __R);
}

extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cmp_round_ph_mask (__mmask16 __U, __m256h __A, __m256h __B,
			       const int __C, const int __R)
{
  return (__mmask16) __builtin_ia32_cmpph256_mask_round ((__v16hf) __A,
							 (__v16hf) __B,
							 __C,
							 (__mmask16) __U,
							 __R);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cmp_round_ps_mask (__m256 __A, __m256 __B, const int __C, const int __R)
{
  return (__mmask8) __builtin_ia32_cmpps256_mask_round ((__v8sf) __A,
							(__v8sf) __B,
							__C,
							(__mmask8) -1,
							__R);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cmp_round_ps_mask (__mmask8 __U, __m256 __A, __m256 __B,
			       const int __C, const int __R)
{
  return (__mmask8) __builtin_ia32_cmpps256_mask_round ((__v8sf) __A,
							(__v8sf) __B,
							__C,
							(__mmask8) __U,
							__R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepi32_ph (__m256i __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtdq2ph256_mask_round ((__v8si) __A,
							   (__v8hf)
							   _mm_setzero_ph (),
							   (__mmask8) -1,
							   __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepi32_ph (__m128h __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m128h) __builtin_ia32_vcvtdq2ph256_mask_round ((__v8si) __A,
							   (__v8hf) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepi32_ph (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtdq2ph256_mask_round ((__v8si) __A,
							   (__v8hf)
							   _mm_setzero_ph (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepi32_ps (__m256i __A, const int __R)
{
  return (__m256) __builtin_ia32_cvtdq2ps256_mask_round ((__v8si) __A,
							 (__v8sf)
							 _mm256_undefined_ps (),
							 (__mmask8) -1,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepi32_ps (__m256 __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m256) __builtin_ia32_cvtdq2ps256_mask_round ((__v8si) __A,
							 (__v8sf) __W,
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepi32_ps (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m256) __builtin_ia32_cvtdq2ps256_mask_round ((__v8si) __A,
							 (__v8sf)
							 _mm256_setzero_ps (),
							 (__mmask8) __U,
							 __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundpd_ph (__m256d __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtpd2ph256_mask_round ((__v4df) __A,
							   (__v8hf)
							   _mm_setzero_ph (),
							   (__mmask8) -1,
							   __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundpd_ph (__m128h __W, __mmask8 __U, __m256d __A,
			    const int __R)
{
  return (__m128h) __builtin_ia32_vcvtpd2ph256_mask_round ((__v4df) __A,
							   (__v8hf) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundpd_ph (__mmask8 __U, __m256d __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtpd2ph256_mask_round ((__v4df) __A,
							   (__v8hf)
							   _mm_setzero_ph (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundpd_ps (__m256d __A, const int __R)
{
  return (__m128) __builtin_ia32_cvtpd2ps256_mask_round ((__v4df) __A,
							 (__v4sf)
							 _mm_undefined_ps (),
							 (__mmask8) -1,
							 __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundpd_ps (__m128 __W, __mmask8 __U, __m256d __A,
			    const int __R)
{
  return (__m128) __builtin_ia32_cvtpd2ps256_mask_round ((__v4df) __A,
							 (__v4sf) __W,
							 (__mmask8) __U,
							 __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundpd_ps (__mmask8 __U, __m256d __A, const int __R)
{
  return (__m128) __builtin_ia32_cvtpd2ps256_mask_round ((__v4df) __A,
							 (__v4sf)
							 _mm_setzero_ps (),
							 (__mmask8) __U,
							 __R);
}
#else
#define _mm256_add_round_pd(A, B, R) \
  ((__m256d) __builtin_ia32_addpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_undefined_pd ()), \
						 (__mmask8) (-1), \
						 (R)))

#define _mm256_mask_add_round_pd(W, U, A, B, R) \
  ((__m256d) __builtin_ia32_addpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) (W), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_maskz_add_round_pd(U, A, B, R) \
  ((__m256d) __builtin_ia32_addpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_setzero_pd ()), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_add_round_ph(A, B, R) \
  ((__m256h) __builtin_ia32_addph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_undefined_ph ()), \
						 (__mmask16) (-1), \
						 (R)))

#define _mm256_mask_add_round_ph(W, U, A, B, R) \
  ((__m256h) __builtin_ia32_addph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) (W), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_maskz_add_round_ph(U, A, B, R) \
  ((__m256h) __builtin_ia32_addph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_setzero_ph ()), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_add_round_ps(A, B, R) \
  ((__m256) __builtin_ia32_addps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_undefined_ps ()), \
						(__mmask8) (-1), \
						(R)))

#define _mm256_mask_add_round_ps(W, U, A, B, R) \
  ((__m256) __builtin_ia32_addps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) (W), \
						(__mmask8) (U), \
						(R)))

#define _mm256_maskz_add_round_ps(U, A, B, R)\
  ((__m256) __builtin_ia32_addps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_setzero_ps ()), \
						(__mmask8) (U), \
						(R)))

#define _mm256_cmp_round_pd_mask(A, B, C, R) \
  ((__mmask8) __builtin_ia32_cmppd256_mask_round ((__v4df) (A), \
						  (__v4df) (B), \
						  (C), \
						  (__mmask8) (-1), \
						  (R)))

#define _mm256_mask_cmp_round_pd_mask(U, A, B, C, R) \
  ((__mmask8) __builtin_ia32_cmppd256_mask_round ((__v4df) (A), \
						  (__v4df) (B), \
						  (C), \
						  (__mmask8) (U), \
						  (R)))

#define _mm256_cmp_round_ph_mask(A, B, C, R) \
  ((__mmask16) __builtin_ia32_cmpph256_mask_round ((__v16hf) (A), \
						   (__v16hf) (B), \
						   (C), \
						   (__mmask16) (-1), \
						   (R)))

#define _mm256_mask_cmp_round_ph_mask(U, A, B, C, R) \
  ((__mmask16) __builtin_ia32_cmpph256_mask_round ((__v16hf) (A), \
						   (__v16hf) (B), \
						   (C), \
						   (__mmask16) (U), \
						   (R)))

#define _mm256_cmp_round_ps_mask(A, B, C, R) \
  ((__mmask8) __builtin_ia32_cmpps256_mask_round ((__v8sf) (A), \
						  (__v8sf) (B), \
						  (C), \
						  (__mmask8) (-1), \
						  (R)))

#define _mm256_mask_cmp_round_ps_mask(U, A, B, C, R) \
  ((__mmask8) __builtin_ia32_cmpps256_mask_round ((__v8sf) (A), \
						  (__v8sf) (B), \
						  (C), \
						  (__mmask8) (U), \
						  (R)))

#define _mm256_cvt_roundepi32_ph(A, R) \
  ((__m128h) __builtin_ia32_vcvtdq2ph256_mask_round ((__v8si) (A), \
						     (__v8hf) \
						     (_mm_setzero_ph ()), \
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvt_roundepi32_ph(W, U, A, R) \
  ((__m128h) __builtin_ia32_vcvtdq2ph256_mask_round ((__v8si) (A), \
						     (__v8hf) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundepi32_ph(U, A, R) \
  ((__m128h) __builtin_ia32_vcvtdq2ph256_mask_round ((__v8si) (A), \
						     (__v8hf) \
						     (_mm_setzero_ph ()), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvt_roundepi32_ps(A, R) \
  ((__m256) __builtin_ia32_cvtdq2ps256_mask_round ((__v8si) (A), \
						   (__v8sf) \
						   (_mm256_undefined_ps ()), \
						   (__mmask8) (-1), \
						   (R)))

#define _mm256_mask_cvt_roundepi32_ps(W, U, A, R) \
  ((__m256) __builtin_ia32_cvtdq2ps256_mask_round ((__v8si) (A), \
						   (__v8sf) (W), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_maskz_cvt_roundepi32_ps(U, A, R) \
  ((__m256) __builtin_ia32_cvtdq2ps256_mask_round ((__v8si) (A), \
						   (__v8sf) \
						   (_mm256_setzero_ps ()), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_cvt_roundpd_ph(A, R) \
  ((__m128h) __builtin_ia32_vcvtpd2ph256_mask_round ((__v4df) (A), \
						     (_mm_setzero_ph ()), \
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvt_roundpd_ph(W, U, A, R) \
  ((__m128h) __builtin_ia32_vcvtpd2ph256_mask_round ((__v4df) (A), \
						     (__v8hf) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundpd_ph(U, A, R) \
  ((__m128h) __builtin_ia32_vcvtpd2ph256_mask_round ((__v4df) (A), \
						     (_mm_setzero_ph ()), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvt_roundpd_ps(A, R) \
  ((__m128) __builtin_ia32_cvtpd2ps256_mask_round ((__v4df) (A), \
						   (__v4sf) \
						   (_mm_undefined_ps ()), \
						   (__mmask8) (-1), \
						   (R)))

#define _mm256_mask_cvt_roundpd_ps(W, U, A, R) \
  ((__m128) __builtin_ia32_cvtpd2ps256_mask_round ((__v4df) (A), \
						   (__v4sf) (W), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_maskz_cvt_roundpd_ps(U, A, R) \
  ((__m128) __builtin_ia32_cvtpd2ps256_mask_round ((__v4df) (A), \
						   (__v4sf) \
						   (_mm_setzero_ps ()), \
						   (__mmask8) (U), \
						   (R)))
#endif

#ifdef __DISABLE_AVX10_2_256__
#undef __DISABLE_AVX10_2_256__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2_256__ */

#endif /* _AVX10_2ROUNDINGINTRIN_H_INCLUDED */
