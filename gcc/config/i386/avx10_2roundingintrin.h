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

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundpd_epi32 (__m256d __A, const int __R)
{
  return
    (__m128i) __builtin_ia32_cvtpd2dq256_mask_round ((__v4df) __A,
						     (__v4si)
						     _mm_undefined_si128 (),
						     (__mmask8) -1,
						     __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundpd_epi32 (__m128i __W, __mmask8 __U, __m256d __A,
			       const int __R)
{
  return (__m128i) __builtin_ia32_cvtpd2dq256_mask_round ((__v4df) __A,
							  (__v4si) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundpd_epi32 (__mmask8 __U, __m256d __A, const int __R)
{
  return (__m128i) __builtin_ia32_cvtpd2dq256_mask_round ((__v4df) __A,
							  (__v4si)
							  _mm_setzero_si128 (),
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundpd_epi64 (__m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtpd2qq256_mask_round ((__v4df) __A,
						     (__v4di)
						     _mm256_setzero_si256 (),
						     (__mmask8) -1,
						     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundpd_epi64 (__m256i __W, __mmask8 __U, __m256d __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_cvtpd2qq256_mask_round ((__v4df) __A,
							  (__v4di) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundpd_epi64 (__mmask8 __U, __m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtpd2qq256_mask_round ((__v4df) __A,
						     (__v4di)
						     _mm256_setzero_si256 (),
						     (__mmask8) __U,
						     __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundpd_epu32 (__m256d __A, const int __R)
{
  return
    (__m128i) __builtin_ia32_cvtpd2udq256_mask_round ((__v4df) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundpd_epu32 (__m128i __W, __mmask8 __U, __m256d __A,
			       const int __R)
{
  return (__m128i) __builtin_ia32_cvtpd2udq256_mask_round ((__v4df) __A,
							   (__v4si) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundpd_epu32 (__mmask8 __U, __m256d __A, const int __R)
{
  return (__m128i) __builtin_ia32_cvtpd2udq256_mask_round ((__v4df) __A,
							   (__v4si)
							   _mm_setzero_si128 (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundpd_epu64 (__m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtpd2uqq256_mask_round ((__v4df) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundpd_epu64 (__m256i __W, __mmask8 __U, __m256d __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_cvtpd2uqq256_mask_round ((__v4df) __A,
							   (__v4di) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundpd_epu64 (__mmask8 __U, __m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtpd2uqq256_mask_round ((__v4df) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundph_epi32 (__m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2dq256_mask_round ((__v8hf) __A,
						      (__v8si)
						      _mm256_setzero_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundph_epi32 (__m256i __W, __mmask8 __U, __m128h __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_vcvtph2dq256_mask_round ((__v8hf) __A,
							   (__v8si) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundph_epi32 (__mmask8 __U, __m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2dq256_mask_round ((__v8hf) __A,
						      (__v8si)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundph_pd (__m128h __A, const int __R)
{
  return (__m256d) __builtin_ia32_vcvtph2pd256_mask_round ((__v8hf) __A,
							   (__v4df)
							   _mm256_setzero_pd (),
							   (__mmask8) -1,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundph_pd (__m256d __W, __mmask8 __U, __m128h __A,
			    const int __R)
{
  return (__m256d) __builtin_ia32_vcvtph2pd256_mask_round ((__v8hf) __A,
							   (__v4df) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundph_pd (__mmask8 __U, __m128h __A, const int __R)
{
  return (__m256d) __builtin_ia32_vcvtph2pd256_mask_round ((__v8hf) __A,
							   (__v4df)
							   _mm256_setzero_pd (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundph_ps (__m128h __A, const int __R)
{
  return
    (__m256) __builtin_ia32_vcvtph2ps256_mask_round ((__v8hf) __A,
						     (__v8sf)
						     _mm256_undefined_ps (),
						     (__mmask8) -1,
						     __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundph_ps (__m256 __W, __mmask8 __U, __m128h __A,
			    const int __R)
{
  return (__m256) __builtin_ia32_vcvtph2ps256_mask_round ((__v8hf) __A,
							  (__v8sf) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundph_ps (__mmask8 __U, __m128h __A, const int __R)
{
  return (__m256) __builtin_ia32_vcvtph2ps256_mask_round ((__v8hf) __A,
							  (__v8sf)
							  _mm256_setzero_ps (),
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtx_roundph_ps (__m128h __A, const int __R)
{
  return (__m256) __builtin_ia32_vcvtph2psx256_mask_round ((__v8hf) __A,
							   (__v8sf)
							   _mm256_setzero_ps (),
							   (__mmask8) -1,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtx_roundph_ps (__m256 __W, __mmask8 __U, __m128h __A,
			     const int __R)
{
  return (__m256) __builtin_ia32_vcvtph2psx256_mask_round ((__v8hf) __A,
							   (__v8sf) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtx_roundph_ps (__mmask8 __U, __m128h __A, const int __R)
{
  return (__m256) __builtin_ia32_vcvtph2psx256_mask_round ((__v8hf) __A,
							   (__v8sf)
							   _mm256_setzero_ps (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundph_epi64 (__m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2qq256_mask_round ((__v8hf) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundph_epi64 (__m256i __W, __mmask8 __U, __m128h __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_vcvtph2qq256_mask_round ((__v8hf) __A,
							   (__v4di) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundph_epi64 (__mmask8 __U, __m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2qq256_mask_round ((__v8hf) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundph_epu32 (__m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2udq256_mask_round ((__v8hf) __A,
						       (__v8si)
						       _mm256_setzero_si256 (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundph_epu32 (__m256i __W, __mmask8 __U, __m128h __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_vcvtph2udq256_mask_round ((__v8hf) __A,
							    (__v8si) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundph_epu32 (__mmask8 __U, __m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2udq256_mask_round ((__v8hf) __A,
						       (__v8si)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundph_epu64 (__m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2uqq256_mask_round ((__v8hf) __A,
						       (__v4di)
						       _mm256_setzero_si256 (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundph_epu64 (__m256i __W, __mmask8 __U, __m128h __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_vcvtph2uqq256_mask_round ((__v8hf) __A,
							    (__v4di) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundph_epu64 (__mmask8 __U, __m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2uqq256_mask_round ((__v8hf) __A,
						       (__v4di)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundph_epu16 (__m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2uw256_mask_round ((__v16hf) __A,
						      (__v16hi)
						      _mm256_undefined_si256 (),
						      (__mmask16) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundph_epu16 (__m256i __W, __mmask16 __U, __m256h __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_vcvtph2uw256_mask_round ((__v16hf) __A,
							   (__v16hi) __W,
							   (__mmask16) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundph_epu16 (__mmask16 __U, __m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2uw256_mask_round ((__v16hf) __A,
						      (__v16hi)
						      _mm256_setzero_si256 (),
						      (__mmask16) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundph_epi16 (__m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2w256_mask_round ((__v16hf) __A,
						     (__v16hi)
						     _mm256_undefined_si256 (),
						     (__mmask16) -1,
						     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundph_epi16 (__m256i __W, __mmask16 __U, __m256h __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_vcvtph2w256_mask_round ((__v16hf) __A,
							  (__v16hi) __W,
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundph_epi16 (__mmask16 __U, __m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtph2w256_mask_round ((__v16hf) __A,
						     (__v16hi)
						     _mm256_setzero_si256 (),
						     (__mmask16) __U,
						     __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundps_pd (__m128 __A, const int __R)
{
  return
    (__m256d) __builtin_ia32_vcvtps2pd256_mask_round ((__v4sf) __A,
						      (__v4df)
						      _mm256_undefined_pd (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundps_pd (__m256d __W, __mmask8 __U, __m128 __A,
			    const int __R)
{
  return (__m256d) __builtin_ia32_vcvtps2pd256_mask_round ((__v4sf) __A,
							   (__v4df) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundps_pd (__mmask8 __U, __m128 __A, const int __R)
{
  return (__m256d) __builtin_ia32_vcvtps2pd256_mask_round ((__v4sf) __A,
							   (__v4df)
							   _mm256_setzero_pd (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtx_roundps_ph (__m256 __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtps2phx256_mask_round ((__v8sf) __A,
							    (__v8hf)
							    _mm_setzero_ph (),
							    (__mmask8) -1,
							    __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtx_roundps_ph (__m128h __W, __mmask8 __U, __m256 __A,
			     const int __R)
{
  return (__m128h) __builtin_ia32_vcvtps2phx256_mask_round ((__v8sf) __A,
							    (__v8hf) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtx_roundps_ph (__mmask8 __U, __m256 __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtps2phx256_mask_round ((__v8sf) __A,
							    (__v8hf)
							    _mm_setzero_ph (),
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundps_epi32 (__m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtps2dq256_mask_round ((__v8sf) __A,
						      (__v8si)
						      _mm256_undefined_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundps_epi32 (__m256i __W, __mmask8 __U, __m256 __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_vcvtps2dq256_mask_round ((__v8sf) __A,
							   (__v8si) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundps_epi32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvtps2dq256_mask_round ((__v8sf) __A,
						      (__v8si)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundps_epi64 (__m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtps2qq256_mask_round ((__v4sf) __A,
						     (__v4di)
						     _mm256_setzero_si256 (),
						     (__mmask8) -1,
						     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundps_epi64 (__m256i __W, __mmask8 __U, __m128 __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_cvtps2qq256_mask_round ((__v4sf) __A,
							  (__v4di) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundps_epi64 (__mmask8 __U, __m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtps2qq256_mask_round ((__v4sf) __A,
						     (__v4di)
						     _mm256_setzero_si256 (),
						     (__mmask8) __U,
						     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundps_epu32 (__m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtps2udq256_mask_round ((__v8sf) __A,
						      (__v8si)
						      _mm256_undefined_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundps_epu32 (__m256i __W, __mmask8 __U, __m256 __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_cvtps2udq256_mask_round ((__v8sf) __A,
							   (__v8si) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundps_epu32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtps2udq256_mask_round ((__v8sf) __A,
						      (__v8si)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundps_epu64 (__m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtps2uqq256_mask_round ((__v4sf) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundps_epu64 (__m256i __W, __mmask8 __U, __m128 __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_cvtps2uqq256_mask_round ((__v4sf) __A,
							   (__v4di) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundps_epu64 (__mmask8 __U, __m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtps2uqq256_mask_round ((__v4sf) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepi64_pd (__m256i __A, const int __R)
{
  return (__m256d) __builtin_ia32_cvtqq2pd256_mask_round ((__v4di) __A,
							  (__v4df)
							  _mm256_setzero_pd (),
							  (__mmask8) -1,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepi64_pd (__m256d __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m256d) __builtin_ia32_cvtqq2pd256_mask_round ((__v4di) __A,
							  (__v4df) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepi64_pd (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m256d) __builtin_ia32_cvtqq2pd256_mask_round ((__v4di) __A,
							  (__v4df)
							  _mm256_setzero_pd (),
							  (__mmask8) __U,
							  __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepi64_ph (__m256i __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtqq2ph256_mask_round ((__v4di) __A,
							   (__v8hf)
							   _mm_setzero_ph (),
							   (__mmask8) -1,
							   __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepi64_ph (__m128h __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m128h) __builtin_ia32_vcvtqq2ph256_mask_round ((__v4di) __A,
							   (__v8hf) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepi64_ph (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtqq2ph256_mask_round ((__v4di) __A,
							   (__v8hf)
							   _mm_setzero_ph (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepi64_ps (__m256i __A, const int __R)
{
  return (__m128) __builtin_ia32_cvtqq2ps256_mask_round ((__v4di) __A,
							 (__v4sf)
							 _mm_setzero_ps (),
							 (__mmask8) -1,
							 __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepi64_ps (__m128 __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m128) __builtin_ia32_cvtqq2ps256_mask_round ((__v4di) __A,
							 (__v4sf) __W,
							 (__mmask8) __U,
							 __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepi64_ps (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m128) __builtin_ia32_cvtqq2ps256_mask_round ((__v4di) __A,
							 (__v4sf)
							 _mm_setzero_ps (),
							 (__mmask8) __U,
							 __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundpd_epi32 (__m256d __A, const int __R)
{
  return
    (__m128i) __builtin_ia32_cvttpd2dq256_mask_round ((__v4df) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundpd_epi32 (__m128i __W, __mmask8 __U, __m256d __A,
				const int __R)
{
  return (__m128i) __builtin_ia32_cvttpd2dq256_mask_round ((__v4df) __A,
							   (__v4si) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundpd_epi32 (__mmask8 __U, __m256d __A, const int __R)
{
  return (__m128i) __builtin_ia32_cvttpd2dq256_mask_round ((__v4df) __A,
							   (__v4si)
							   _mm_setzero_si128 (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundpd_epi64 (__m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttpd2qq256_mask_round ((__v4df) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundpd_epi64 (__m256i __W, __mmask8 __U, __m256d __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttpd2qq256_mask_round ((__v4df) __A,
							   (__v4di) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundpd_epi64 (__mmask8 __U, __m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttpd2qq256_mask_round ((__v4df) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundpd_epu32 (__m256d __A, const int __R)
{
  return
    (__m128i) __builtin_ia32_cvttpd2udq256_mask_round ((__v4df) __A,
						       (__v4si)
						       _mm_undefined_si128 (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundpd_epu32 (__m128i __W, __mmask8 __U, __m256d __A,
				const int __R)
{
  return (__m128i) __builtin_ia32_cvttpd2udq256_mask_round ((__v4df) __A,
							    (__v4si) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundpd_epu32 (__mmask8 __U, __m256d __A, const int __R)
{
  return
    (__m128i) __builtin_ia32_cvttpd2udq256_mask_round ((__v4df) __A,
						       (__v4si)
						       _mm_setzero_si128 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundpd_epu64 (__m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttpd2uqq256_mask_round ((__v4df) __A,
						       (__v4di) \
						       _mm256_setzero_si256 (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundpd_epu64 (__m256i __W, __mmask8 __U, __m256d __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttpd2uqq256_mask_round ((__v4df) __A,
							    (__v4di) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundpd_epu64 (__mmask8 __U, __m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttpd2uqq256_mask_round ((__v4df) __A,
						       (__v4di)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundph_epi32 (__m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2dq256_mask_round ((__v8hf) __A,
						       (__v8si)
						       _mm256_setzero_si256 (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundph_epi32 (__m256i __W, __mmask8 __U, __m128h __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_vcvttph2dq256_mask_round ((__v8hf) __A,
							    (__v8si) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundph_epi32 (__mmask8 __U, __m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2dq256_mask_round ((__v8hf) __A,
						       (__v8si)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundph_epi64 (__m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2qq256_mask_round ((__v8hf) __A,
						       (__v4di)
						       _mm256_setzero_si256 (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundph_epi64 (__m256i __W, __mmask8 __U, __m128h __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_vcvttph2qq256_mask_round ((__v8hf) __A,
							    (__v4di) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundph_epi64 (__mmask8 __U, __m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2qq256_mask_round ((__v8hf) __A,
						       (__v4di)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundph_epu32 (__m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2udq256_mask_round ((__v8hf) __A,
							(__v8si)
							_mm256_setzero_si256 (),
							(__mmask8) -1,
							__R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundph_epu32 (__m256i __W, __mmask8 __U, __m128h __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_vcvttph2udq256_mask_round ((__v8hf) __A,
							     (__v8si) __W,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundph_epu32 (__mmask8 __U, __m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2udq256_mask_round ((__v8hf) __A,
							(__v8si)
							_mm256_setzero_si256 (),
							(__mmask8) __U,
							__R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundph_epu64 (__m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2uqq256_mask_round ((__v8hf) __A,
							(__v4di)
							_mm256_setzero_si256 (),
							(__mmask8) -1,
							__R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundph_epu64 (__m256i __W, __mmask8 __U, __m128h __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_vcvttph2uqq256_mask_round ((__v8hf) __A,
							     (__v4di) __W,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundph_epu64 (__mmask8 __U, __m128h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2uqq256_mask_round ((__v8hf) __A,
							(__v4di)
							_mm256_setzero_si256 (),
							(__mmask8) __U,
							__R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundph_epu16 (__m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2uw256_mask_round ((__v16hf) __A,
						       (__v16hi)
						       _mm256_setzero_si256 (),
						       (__mmask16) -1,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundph_epu16 (__m256i __W, __mmask16 __U, __m256h __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_vcvttph2uw256_mask_round ((__v16hf) __A,
							    (__v16hi) __W,
							    (__mmask16) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundph_epu16 (__mmask16 __U, __m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2uw256_mask_round ((__v16hf) __A,
						       (__v16hi)
						       _mm256_setzero_si256 (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundph_epi16 (__m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2w256_mask_round ((__v16hf) __A,
						      (__v16hi)
						      _mm256_setzero_si256 (),
						      (__mmask16) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundph_epi16 (__m256i __W, __mmask16 __U, __m256h __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_vcvttph2w256_mask_round ((__v16hf) __A,
							   (__v16hi) __W,
							   (__mmask16) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundph_epi16 (__mmask16 __U, __m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_vcvttph2w256_mask_round ((__v16hf) __A,
						      (__v16hi)
						      _mm256_setzero_si256 (),
						      (__mmask16) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundps_epi32 (__m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2dq256_mask_round ((__v8sf) __A,
						      (__v8si)
						      _mm256_undefined_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundps_epi32 (__m256i __W, __mmask8 __U, __m256 __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2dq256_mask_round ((__v8sf) __A,
							   (__v8si) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundps_epi32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2dq256_mask_round ((__v8sf) __A,
						      (__v8si)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundps_epi64 (__m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2qq256_mask_round ((__v4sf) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundps_epi64 (__m256i __W, __mmask8 __U, __m128 __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2qq256_mask_round ((__v4sf) __A,
							   (__v4di) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundps_epi64 (__mmask8 __U, __m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2qq256_mask_round ((__v4sf) __A,
						      (__v4di)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundps_epu32 (__m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2udq256_mask_round ((__v8sf) __A,
						       (__v8si)
						       _mm256_undefined_si256 (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundps_epu32 (__m256i __W, __mmask8 __U, __m256 __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2udq256_mask_round ((__v8sf) __A,
							    (__v8si) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundps_epu32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2udq256_mask_round ((__v8sf) __A,
						       (__v8si)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtt_roundps_epu64 (__m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2uqq256_mask_round ((__v4sf) __A,
						       (__v4di)
						       _mm256_setzero_si256 (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtt_roundps_epu64 (__m256i __W, __mmask8 __U, __m128 __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2uqq256_mask_round ((__v4sf) __A,
							    (__v4di) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtt_roundps_epu64 (__mmask8 __U, __m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2uqq256_mask_round ((__v4sf) __A,
						       (__v4di)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepu32_ph (__m256i __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtudq2ph256_mask_round ((__v8si) __A,
							    (__v8hf)
							    _mm_setzero_ph (),
							    (__mmask8) -1,
							    __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepu32_ph (__m128h __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m128h) __builtin_ia32_vcvtudq2ph256_mask_round ((__v8si) __A,
							    (__v8hf) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepu32_ph (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtudq2ph256_mask_round ((__v8si) __A,
							    (__v8hf)
							    _mm_setzero_ph (),
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepu32_ps (__m256i __A, const int __R)
{
  return
    (__m256) __builtin_ia32_cvtudq2ps256_mask_round ((__v8si) __A,
						     (__v8sf)
						     _mm256_undefined_ps (),
						     (__mmask8) -1,
						     __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepu32_ps (__m256 __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m256) __builtin_ia32_cvtudq2ps256_mask_round ((__v8si) __A,
							  (__v8sf) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepu32_ps (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m256) __builtin_ia32_cvtudq2ps256_mask_round ((__v8si) __A,
							  (__v8sf)
							  _mm256_setzero_ps (),
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepu64_pd (__m256i __A, const int __R)
{
  return (__m256d) __builtin_ia32_cvtuqq2pd256_mask_round ((__v4di) __A,
							   (__v4df)
							   _mm256_setzero_pd (),
							   (__mmask8) -1,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepu64_pd (__m256d __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m256d) __builtin_ia32_cvtuqq2pd256_mask_round ((__v4di) __A,
							   (__v4df) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepu64_pd (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m256d) __builtin_ia32_cvtuqq2pd256_mask_round ((__v4di) __A,
							   (__v4df)
							   _mm256_setzero_pd (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepu64_ph (__m256i __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtuqq2ph256_mask_round ((__v4di) __A,
							    (__v8hf)
							    _mm_setzero_ph (),
							    (__mmask8) -1,
							    __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepu64_ph (__m128h __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m128h) __builtin_ia32_vcvtuqq2ph256_mask_round ((__v4di) __A,
							    (__v8hf) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepu64_ph (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m128h) __builtin_ia32_vcvtuqq2ph256_mask_round ((__v4di) __A,
							    (__v8hf)
							    _mm_setzero_ph (),
							    (__mmask8) __U,
							    __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepu64_ps (__m256i __A, const int __R)
{
  return (__m128) __builtin_ia32_cvtuqq2ps256_mask_round ((__v4di) __A,
							  (__v4sf)
							  _mm_setzero_ps (),
							  (__mmask8) -1,
							  __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepu64_ps (__m128 __W, __mmask8 __U, __m256i __A,
			       const int __R)
{
  return (__m128) __builtin_ia32_cvtuqq2ps256_mask_round ((__v4di) __A,
							  (__v4sf) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepu64_ps (__mmask8 __U, __m256i __A, const int __R)
{
  return (__m128) __builtin_ia32_cvtuqq2ps256_mask_round ((__v4di) __A,
							  (__v4sf)
							  _mm_setzero_ps (),
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepu16_ph (__m256i __A, const int __R)
{
  return (__m256h) __builtin_ia32_vcvtuw2ph256_mask_round ((__v16hi) __A,
							   (__v16hf)
							   _mm256_setzero_ph (),
							   (__mmask16) -1,
							   __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepu16_ph (__m256h __W, __mmask16 __U, __m256i __A,
			       const int __R)
{
  return (__m256h) __builtin_ia32_vcvtuw2ph256_mask_round ((__v16hi) __A,
							   (__v16hf) __W,
							   (__mmask16) __U,
							   __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepu16_ph (__mmask16 __U, __m256i __A, const int __R)
{
  return (__m256h) __builtin_ia32_vcvtuw2ph256_mask_round ((__v16hi) __A,
							   (__v16hf)
							   _mm256_setzero_ph (),
							   (__mmask16) __U,
							   __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvt_roundepi16_ph (__m256i __A, const int __R)
{
  return (__m256h) __builtin_ia32_vcvtw2ph256_mask_round ((__v16hi) __A,
							  (__v16hf)
							  _mm256_setzero_ph (),
							  (__mmask16) -1,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvt_roundepi16_ph (__m256h __W, __mmask16 __U, __m256i __A,
			       const int __R)
{
  return (__m256h) __builtin_ia32_vcvtw2ph256_mask_round ((__v16hi) __A,
							  (__v16hf) __W,
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvt_roundepi16_ph (__mmask16 __U, __m256i __A, const int __R)
{
  return (__m256h) __builtin_ia32_vcvtw2ph256_mask_round ((__v16hi) __A,
							  (__v16hf)
							  _mm256_setzero_ph (),
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_div_round_pd (__m256d __A, __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_divpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_undefined_pd (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_div_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			  __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_divpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df) __W,
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_div_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			   const int __R)
{
  return (__m256d) __builtin_ia32_divpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_setzero_pd (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_div_round_ph (__m256h __A, __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_divph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) -1,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_div_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			  __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_divph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf) __W,
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_div_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			   const int __R)
{
  return (__m256h) __builtin_ia32_divph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_div_round_ps (__m256 __A, __m256 __B, const int __R)
{
  return (__m256) __builtin_ia32_divps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_undefined_ps (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_div_round_ps (__m256 __W, __mmask8 __U, __m256 __A, __m256 __B,
			  const int __R)
{
  return (__m256) __builtin_ia32_divps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf) __W,
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_div_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			   const int __R)
{
  return (__m256) __builtin_ia32_divps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_setzero_ps (),
						      (__mmask8) __U,
						      __R);
}
extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fcmadd_round_pch (__m256h __A, __m256h __B, __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfcmaddcph256_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf) __D,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fcmadd_round_pch (__m256h __A, __mmask8 __U, __m256h __B,
			      __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfcmaddcph256_mask_round ((__v16hf) __A,
							    (__v16hf) __B,
							    (__v16hf) __D,
							    __U,
							    __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fcmadd_round_pch (__m256h __A, __m256h __B, __m256h __D,
			       __mmask8 __U, const int __R)
{
  return (__m256h) __builtin_ia32_vfcmaddcph256_mask3_round ((__v16hf) __A,
							     (__v16hf) __B,
							     (__v16hf) __D,
							     __U,
							     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fcmadd_round_pch (__mmask8 __U, __m256h __A, __m256h __B,
			       __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfcmaddcph256_maskz_round ((__v16hf) __A,
							     (__v16hf) __B,
							     (__v16hf) __D,
							     __U,
							     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fcmul_round_pch (__m256h __A, __m256h __B, const int __R)
{
  return
    (__m256h) __builtin_ia32_vfcmulcph256_round ((__v16hf) __A,
						 (__v16hf) __B,
						 __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fcmul_round_pch (__m256h __W, __mmask8 __U, __m256h __A,
			     __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_vfcmulcph256_mask_round ((__v16hf) __A,
							   (__v16hf) __B,
							   (__v16hf) __W,
							   (__mmask16) __U,
							   __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fcmul_round_pch (__mmask8 __U, __m256h __A, __m256h __B,
			      const int __R)
{
  return (__m256h) __builtin_ia32_vfcmulcph256_mask_round ((__v16hf) __A,
							   (__v16hf) __B,
							   (__v16hf)
							   _mm256_setzero_ph (),
							   (__mmask16) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fixupimm_round_pd (__m256d __A, __m256d __B, __m256i __D,
			  const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_fixupimmpd256_mask_round ((__v4df) __A,
							    (__v4df) __B,
							    (__v4di) __D,
							    __C,
							    (__mmask8) -1,
							    __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fixupimm_round_pd (__m256d __A, __mmask8 __U, __m256d __B,
			       __m256i __D, const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_fixupimmpd256_mask_round ((__v4df) __A,
							    (__v4df) __B,
							    (__v4di) __D,
							    __C,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fixupimm_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
				__m256i __D, const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_fixupimmpd256_maskz_round ((__v4df) __A,
							     (__v4df) __B,
							     (__v4di) __D,
							     __C,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fixupimm_round_ps (__m256 __A, __m256 __B, __m256i __D, const int __C,
			  const int __R)
{
  return (__m256) __builtin_ia32_fixupimmps256_mask_round ((__v8sf) __A,
							   (__v8sf) __B,
							   (__v8si) __D,
							   __C,
							   (__mmask8) -1,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fixupimm_round_ps (__m256 __A, __mmask8 __U, __m256 __B,
			       __m256i __D, const int __C, const int __R)
{
  return (__m256) __builtin_ia32_fixupimmps256_mask_round ((__v8sf) __A,
							   (__v8sf) __B,
							   (__v8si) __D,
							   __C,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fixupimm_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
				__m256i __D, const int __C, const int __R)
{
  return (__m256) __builtin_ia32_fixupimmps256_maskz_round ((__v8sf) __A,
							    (__v8sf) __B,
							    (__v8si) __D,
							    __C,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmadd_round_pd (__m256d __A, __m256d __B, __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmaddpd256_mask_round ((__v4df) __A,
							  (__v4df) __B,
							  (__v4df) __D,
							  (__mmask8) -1,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmadd_round_pd (__m256d __A, __mmask8 __U, __m256d __B,
			    __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmaddpd256_mask_round ((__v4df) __A,
							  (__v4df) __B,
							  (__v4df) __D,
							  (__mmask8) __U, __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmadd_round_pd (__m256d __A, __m256d __B, __m256d __D,
			     __mmask8 __U, const int __R)
{
  return (__m256d) __builtin_ia32_vfmaddpd256_mask3_round ((__v4df) __A,
							   (__v4df) __B,
							   (__v4df) __D,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmadd_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			     __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmaddpd256_maskz_round ((__v4df) __A,
							   (__v4df) __B,
							   (__v4df) __D,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmadd_round_ph (__m256h __A, __m256h __B, __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddph256_mask_round ((__v16hf) __A,
							  (__v16hf) __B,
							  (__v16hf) __D,
							  (__mmask16) -1,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmadd_round_ph (__m256h __A, __mmask16 __U, __m256h __B,
			    __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddph256_mask_round ((__v16hf) __A,
							  (__v16hf) __B,
							  (__v16hf) __D,
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmadd_round_ph (__m256h __A, __m256h __B, __m256h __D,
			     __mmask16 __U, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddph256_mask3_round ((__v16hf) __A,
							   (__v16hf) __B,
							   (__v16hf) __D,
							   (__mmask16) __U,
							   __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmadd_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			     __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddph256_maskz_round ((__v16hf) __A,
							   (__v16hf) __B,
							   (__v16hf) __D,
							   (__mmask16) __U,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmadd_round_ps (__m256 __A, __m256 __B, __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmaddps256_mask_round ((__v8sf) __A,
							 (__v8sf) __B,
							 (__v8sf) __D,
							 (__mmask8) -1,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmadd_round_ps (__m256 __A, __mmask8 __U, __m256 __B,
			    __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmaddps256_mask_round ((__v8sf) __A,
							 (__v8sf) __B,
							 (__v8sf) __D,
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmadd_round_ps (__m256 __A, __m256 __B, __m256 __D,
			     __mmask8 __U, const int __R)
{
  return (__m256) __builtin_ia32_vfmaddps256_mask3_round ((__v8sf) __A,
							  (__v8sf) __B,
							  (__v8sf) __D,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmadd_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			     __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmaddps256_maskz_round ((__v8sf) __A,
							  (__v8sf) __B,
							  (__v8sf) __D,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmadd_round_pch (__m256h __A, __m256h __B, __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddcph256_round ((__v16hf) __A,
						      (__v16hf) __B,
						      (__v16hf) __D,
						      __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmadd_round_pch (__m256h __A, __mmask16 __U, __m256h __B,
			     __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddcph256_mask_round ((__v16hf) __A,
							   (__v16hf) __B,
							   (__v16hf) __D,
							   __U,
							   __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmadd_round_pch (__m256h __A, __m256h __B, __m256h __D,
			      __mmask16 __U, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddcph256_mask3_round ((__v16hf) __A,
							    (__v16hf) __B,
							    (__v16hf) __D,
							    __U,
							    __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmadd_round_pch (__mmask16 __U, __m256h __A, __m256h __B,
			      __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddcph256_maskz_round ((__v16hf) __A,
							    (__v16hf) __B,
							    (__v16hf) __D,
							    __U,
							    __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmaddsub_round_pd (__m256d __A, __m256d __B, __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmaddsubpd256_mask_round ((__v4df) __A,
							     (__v4df) __B,
							     (__v4df) __D,
							     (__mmask8) -1,
							     __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmaddsub_round_pd (__m256d __A, __mmask8 __U, __m256d __B,
			       __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmaddsubpd256_mask_round ((__v4df) __A,
							     (__v4df) __B,
							     (__v4df) __D,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmaddsub_round_pd (__m256d __A, __m256d __B, __m256d __D,
				__mmask8 __U, const int __R)
{
  return (__m256d) __builtin_ia32_vfmaddsubpd256_mask3_round ((__v4df) __A,
							      (__v4df) __B,
							      (__v4df) __D,
							      (__mmask8) __U,
							      __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmaddsub_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
				__m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmaddsubpd256_maskz_round ((__v4df) __A,
							      (__v4df) __B,
							      (__v4df) __D,
							      (__mmask8) __U,
							      __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmaddsub_round_ph (__m256h __A, __m256h __B, __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddsubph256_mask_round ((__v16hf) __A,
							     (__v16hf) __B,
							     (__v16hf) __D,
							     (__mmask16) -1,
							     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmaddsub_round_ph (__m256h __A, __mmask16 __U, __m256h __B,
			       __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddsubph256_mask_round ((__v16hf) __A,
							     (__v16hf) __B,
							     (__v16hf) __D,
							     (__mmask16) __U,
							     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmaddsub_round_ph (__m256h __A, __m256h __B, __m256h __D,
				__mmask16 __U, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddsubph256_mask3_round ((__v16hf) __A,
							      (__v16hf) __B,
							      (__v16hf) __D,
							      (__mmask16) __U,
							      __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmaddsub_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
				__m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmaddsubph256_maskz_round ((__v16hf) __A,
							      (__v16hf) __B,
							      (__v16hf) __D,
							      (__mmask16) __U,
							      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmaddsub_round_ps (__m256 __A, __m256 __B, __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmaddsubps256_mask_round ((__v8sf) __A,
							    (__v8sf) __B,
							    (__v8sf) __D,
							    (__mmask8) -1,
							    __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmaddsub_round_ps (__m256 __A, __mmask8 __U, __m256 __B,
			       __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmaddsubps256_mask_round ((__v8sf) __A,
							    (__v8sf) __B,
							    (__v8sf) __D,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmaddsub_round_ps (__m256 __A, __m256 __B, __m256 __D,
				__mmask8 __U, const int __R)
{
  return (__m256) __builtin_ia32_vfmaddsubps256_mask3_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v8sf) __D,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmaddsub_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
				__m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmaddsubps256_maskz_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v8sf) __D,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmsub_round_pd (__m256d __A, __m256d __B, __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmsubpd256_mask_round ((__v4df) __A,
							  (__v4df) __B,
							  (__v4df) __D,
							  (__mmask8) -1, __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmsub_round_pd (__m256d __A, __mmask8 __U, __m256d __B,
			    __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmsubpd256_mask_round ((__v4df) __A,
							  (__v4df) __B,
							  (__v4df) __D,
							  (__mmask8) __U, __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmsub_round_pd (__m256d __A, __m256d __B, __m256d __D,
			     __mmask8 __U, const int __R)
{
  return (__m256d) __builtin_ia32_vfmsubpd256_mask3_round ((__v4df) __A,
							   (__v4df) __B,
							   (__v4df) __D,
							   (__mmask8) __U, __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmsub_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			     __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmsubpd256_maskz_round ((__v4df) __A,
							   (__v4df) __B,
							   (__v4df) __D,
							   (__mmask8) __U, __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmsub_round_ph (__m256h __A, __m256h __B, __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfmsubph256_mask_round ((__v16hf) __A,
					   (__v16hf) __B,
					   (__v16hf) __D,
					   (__mmask16) -1, __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmsub_round_ph (__m256h __A, __mmask16 __U, __m256h __B,
			    __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfmsubph256_mask_round ((__v16hf) __A,
					   (__v16hf) __B,
					   (__v16hf) __D,
					   (__mmask16) __U, __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmsub_round_ph (__m256h __A, __m256h __B, __m256h __D,
			     __mmask16 __U, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfmsubph256_mask3_round ((__v16hf) __A,
					    (__v16hf) __B,
					    (__v16hf) __D,
					    (__mmask16) __U, __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmsub_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			     __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfmsubph256_maskz_round ((__v16hf) __A,
					    (__v16hf) __B,
					    (__v16hf) __D,
					    (__mmask16) __U, __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmsub_round_ps (__m256 __A, __m256 __B, __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmsubps256_mask_round ((__v8sf) __A,
							 (__v8sf) __B,
							 (__v8sf) __D,
							 (__mmask8) -1, __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmsub_round_ps (__m256 __A, __mmask8 __U, __m256 __B,
			    __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmsubps256_mask_round ((__v8sf) __A,
							 (__v8sf) __B,
							 (__v8sf) __D,
							 (__mmask8) __U, __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmsub_round_ps (__m256 __A, __m256 __B, __m256 __D,
			     __mmask8 __U, const int __R)
{
  return (__m256) __builtin_ia32_vfmsubps256_mask3_round ((__v8sf) __A,
							  (__v8sf) __B,
							  (__v8sf) __D,
							  (__mmask8) __U, __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmsub_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			     __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmsubps256_maskz_round ((__v8sf) __A,
							  (__v8sf) __B,
							  (__v8sf) __D,
							  (__mmask8) __U, __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmsubadd_round_pd (__m256d __A, __m256d __B, __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmsubaddpd256_mask_round ((__v4df) __A,
							     (__v4df) __B,
							     (__v4df) __D,
							     (__mmask8) -1,
							     __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmsubadd_round_pd (__m256d __A, __mmask8 __U, __m256d __B,
			       __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmsubaddpd256_mask_round ((__v4df) __A,
							     (__v4df) __B,
							     (__v4df) __D,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmsubadd_round_pd (__m256d __A, __m256d __B, __m256d __D,
				__mmask8 __U, const int __R)
{
  return (__m256d) __builtin_ia32_vfmsubaddpd256_mask3_round ((__v4df) __A,
							      (__v4df) __B,
							      (__v4df) __D,
							      (__mmask8) __U,
							      __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmsubadd_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
				__m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfmsubaddpd256_maskz_round ((__v4df) __A,
							      (__v4df) __B,
							      (__v4df) __D,
							      (__mmask8) __U,
							      __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmsubadd_round_ph (__m256h __A, __m256h __B, __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfmsubaddph256_mask_round ((__v16hf) __A,
					      (__v16hf) __B,
					      (__v16hf) __D,
					      (__mmask16) -1,
					      __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmsubadd_round_ph (__m256h __A, __mmask16 __U, __m256h __B,
			       __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfmsubaddph256_mask_round ((__v16hf) __A,
					      (__v16hf) __B,
					      (__v16hf) __D,
					      (__mmask16) __U,
					      __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmsubadd_round_ph (__m256h __A, __m256h __B, __m256h __D,
				__mmask16 __U, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfmsubaddph256_mask3_round ((__v16hf) __A,
					       (__v16hf) __B,
					       (__v16hf) __D,
					       (__mmask16) __U,
					       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmsubadd_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
				__m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfmsubaddph256_maskz_round ((__v16hf) __A,
					       (__v16hf) __B,
					       (__v16hf) __D,
					       (__mmask16) __U,
					       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmsubadd_round_ps (__m256 __A, __m256 __B, __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmsubaddps256_mask_round ((__v8sf) __A,
							    (__v8sf) __B,
							    (__v8sf) __D,
							    (__mmask8) -1,
							    __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmsubadd_round_ps (__m256 __A, __mmask8 __U, __m256 __B,
			       __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmsubaddps256_mask_round ((__v8sf) __A,
							    (__v8sf) __B,
							    (__v8sf) __D,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmsubadd_round_ps (__m256 __A, __m256 __B, __m256 __D,
				__mmask8 __U, const int __R)
{
  return (__m256) __builtin_ia32_vfmsubaddps256_mask3_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v8sf) __D,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmsubadd_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
				__m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfmsubaddps256_maskz_round ((__v8sf) __A,
							     (__v8sf) __B,
							     (__v8sf) __D,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmul_round_pch (__m256h __B, __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmulcph256_round ((__v16hf) __B,
						     (__v16hf) __D,
						     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmul_round_pch (__m256h __A, __mmask8 __U, __m256h __B,
			    __m256h __D, const int __R)
{
  return (__m256h) __builtin_ia32_vfmulcph256_mask_round ((__v16hf) __B,
							  (__v16hf) __D,
							  (__v16hf) __A,
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmul_round_pch (__mmask8 __U, __m256h __B, __m256h __D,
			     const int __R)
{
  return (__m256h) __builtin_ia32_vfmulcph256_mask_round ((__v16hf) __B,
							  (__v16hf) __D,
							  (__v16hf)
							  _mm256_setzero_ph (),
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmadd_round_pd (__m256d __A, __m256d __B, __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfnmaddpd256_mask_round ((__v4df) __A,
							   (__v4df) __B,
							   (__v4df) __D,
							   (__mmask8) -1,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmadd_round_pd (__m256d __A, __mmask8 __U, __m256d __B,
			     __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfnmaddpd256_mask_round ((__v4df) __A,
							   (__v4df) __B,
							   (__v4df) __D,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmadd_round_pd (__m256d __A, __m256d __B, __m256d __D,
			      __mmask8 __U, const int __R)
{
  return (__m256d) __builtin_ia32_vfnmaddpd256_mask3_round ((__v4df) __A,
							    (__v4df) __B,
							    (__v4df) __D,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmadd_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			      __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfnmaddpd256_maskz_round ((__v4df) __A,
							    (__v4df) __B,
							    (__v4df) __D,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmadd_round_ph (__m256h __A, __m256h __B, __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfnmaddph256_mask_round ((__v16hf) __A,
					    (__v16hf) __B,
					    (__v16hf) __D,
					    (__mmask16) -1,
					    __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmadd_round_ph (__m256h __A, __mmask16 __U, __m256h __B,
			     __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfnmaddph256_mask_round ((__v16hf) __A,
					    (__v16hf) __B,
					    (__v16hf) __D,
					    (__mmask16) __U,
					    __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmadd_round_ph (__m256h __A, __m256h __B, __m256h __D,
			      __mmask16 __U, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfnmaddph256_mask3_round ((__v16hf) __A,
					     (__v16hf) __B,
					     (__v16hf) __D,
					     (__mmask16) __U,
					     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmadd_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			      __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfnmaddph256_maskz_round ((__v16hf) __A,
					     (__v16hf) __B,
					     (__v16hf) __D,
					     (__mmask16) __U,
					     __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmadd_round_ps (__m256 __A, __m256 __B, __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfnmaddps256_mask_round ((__v8sf) __A,
							  (__v8sf) __B,
							  (__v8sf) __D,
							  (__mmask8) -1,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmadd_round_ps (__m256 __A, __mmask8 __U, __m256 __B,
			     __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfnmaddps256_mask_round ((__v8sf) __A,
							  (__v8sf) __B,
							  (__v8sf) __D,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmadd_round_ps (__m256 __A, __m256 __B, __m256 __D,
			      __mmask8 __U, const int __R)
{
  return (__m256) __builtin_ia32_vfnmaddps256_mask3_round ((__v8sf) __A,
							   (__v8sf) __B,
							   (__v8sf) __D,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmadd_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			      __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfnmaddps256_maskz_round ((__v8sf) __A,
							   (__v8sf) __B,
							   (__v8sf) __D,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmsub_round_pd (__m256d __A, __m256d __B, __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfnmsubpd256_mask_round ((__v4df) __A,
							   (__v4df) __B,
							   (__v4df) __D,
							   (__mmask8) -1,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmsub_round_pd (__m256d __A, __mmask8 __U, __m256d __B,
			     __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfnmsubpd256_mask_round ((__v4df) __A,
							   (__v4df) __B,
							   (__v4df) __D,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmsub_round_pd (__m256d __A, __m256d __B, __m256d __D,
			      __mmask8 __U, const int __R)
{
  return (__m256d) __builtin_ia32_vfnmsubpd256_mask3_round ((__v4df) __A,
							    (__v4df) __B,
							    (__v4df) __D,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmsub_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			      __m256d __D, const int __R)
{
  return (__m256d) __builtin_ia32_vfnmsubpd256_maskz_round ((__v4df) __A,
							    (__v4df) __B,
							    (__v4df) __D,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmsub_round_ph (__m256h __A, __m256h __B, __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfnmsubph256_mask_round ((__v16hf) __A,
					    (__v16hf) __B,
					    (__v16hf) __D,
					    (__mmask16) -1,
					    __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmsub_round_ph (__m256h __A, __mmask16 __U, __m256h __B,
			     __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfnmsubph256_mask_round ((__v16hf) __A,
					    (__v16hf) __B,
					    (__v16hf) __D,
					    (__mmask16) __U,
					    __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmsub_round_ph (__m256h __A, __m256h __B, __m256h __D,
			      __mmask16 __U, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfnmsubph256_mask3_round ((__v16hf) __A,
					     (__v16hf) __B,
					     (__v16hf) __D,
					     (__mmask16) __U,
					     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmsub_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			      __m256h __D, const int __R)
{
  return (__m256h)
    __builtin_ia32_vfnmsubph256_maskz_round ((__v16hf) __A,
					     (__v16hf) __B,
					     (__v16hf) __D,
					     (__mmask16) __U,
					     __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmsub_round_ps (__m256 __A, __m256 __B, __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfnmsubps256_mask_round ((__v8sf) __A,
							  (__v8sf) __B,
							  (__v8sf) __D,
							  (__mmask8) -1,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmsub_round_ps (__m256 __A, __mmask8 __U, __m256 __B,
			     __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfnmsubps256_mask_round ((__v8sf) __A,
							  (__v8sf) __B,
							  (__v8sf) __D,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmsub_round_ps (__m256 __A, __m256 __B, __m256 __D,
			      __mmask8 __U, const int __R)
{
  return (__m256) __builtin_ia32_vfnmsubps256_mask3_round ((__v8sf) __A,
							   (__v8sf) __B,
							   (__v8sf) __D,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmsub_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			      __m256 __D, const int __R)
{
  return (__m256) __builtin_ia32_vfnmsubps256_maskz_round ((__v8sf) __A,
							   (__v8sf) __B,
							   (__v8sf) __D,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getexp_round_pd (__m256d __A, const int __R)
{
  return
    (__m256d) __builtin_ia32_getexppd256_mask_round ((__v4df) __A,
						     (__v4df)
						     _mm256_undefined_pd (),
						     (__mmask8) -1,
						     __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_getexp_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			     const int __R)
{
  return (__m256d) __builtin_ia32_getexppd256_mask_round ((__v4df) __A,
							  (__v4df) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getexp_round_pd (__mmask8 __U, __m256d __A, const int __R)
{
  return (__m256d) __builtin_ia32_getexppd256_mask_round ((__v4df) __A,
							  (__v4df)
							  _mm256_setzero_pd (),
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getexp_round_ph (__m256h __A, const int __R)
{
  return (__m256h) __builtin_ia32_getexpph256_mask_round ((__v16hf) __A,
							  (__v16hf)
							  _mm256_setzero_ph (),
							  (__mmask16) -1,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_getexp_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			     const int __R)
{
  return (__m256h) __builtin_ia32_getexpph256_mask_round ((__v16hf) __A,
							  (__v16hf) __W,
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getexp_round_ph (__mmask16 __U, __m256h __A, const int __R)
{
  return (__m256h) __builtin_ia32_getexpph256_mask_round ((__v16hf) __A,
							  (__v16hf)
							  _mm256_setzero_ph (),
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getexp_round_ps (__m256 __A, const int __R)
{
  return (__m256) __builtin_ia32_getexpps256_mask_round ((__v8sf) __A,
							 (__v8sf)
							 _mm256_undefined_ps (),
							 (__mmask8) -1,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_getexp_round_ps (__m256 __W, __mmask8 __U, __m256 __A,
			     const int __R)
{
  return (__m256) __builtin_ia32_getexpps256_mask_round ((__v8sf) __A,
							 (__v8sf) __W,
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getexp_round_ps (__mmask8 __U, __m256 __A, const int __R)
{
  return (__m256) __builtin_ia32_getexpps256_mask_round ((__v8sf) __A,
							 (__v8sf)
							 _mm256_setzero_ps (),
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getmant_round_pd (__m256d __A, _MM_MANTISSA_NORM_ENUM __B,
			 _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return
    (__m256d) __builtin_ia32_getmantpd256_mask_round ((__v4df) __A,
						      (__C << 2) | __B,
						      _mm256_undefined_pd (),
						      (__mmask8) -1, __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_getmant_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			      _MM_MANTISSA_NORM_ENUM __B,
			      _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return (__m256d) __builtin_ia32_getmantpd256_mask_round ((__v4df) __A,
							   (__C << 2) | __B,
							   (__v4df) __W, __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getmant_round_pd (__mmask8 __U, __m256d __A,
			       _MM_MANTISSA_NORM_ENUM __B,
			       _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return (__m256d) __builtin_ia32_getmantpd256_mask_round ((__v4df) __A,
							   (__C << 2) | __B,
							   (__v4df)
							   _mm256_setzero_pd (),
							   __U, __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getmant_round_ph (__m256h __A, _MM_MANTISSA_NORM_ENUM __B,
			 _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return
    (__m256h) __builtin_ia32_getmantph256_mask_round ((__v16hf) __A,
						      (__C << 2) | __B,
						      _mm256_undefined_ph (),
						      (__mmask16) -1, __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_getmant_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			      _MM_MANTISSA_NORM_ENUM __B,
			      _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return (__m256h) __builtin_ia32_getmantph256_mask_round ((__v16hf) __A,
							   (__C << 2) | __B,
							   (__v16hf) __W, __U,
							   __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getmant_round_ph (__mmask8 __U, __m256h __A,
			       _MM_MANTISSA_NORM_ENUM __B,
			       _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return (__m256h) __builtin_ia32_getmantph256_mask_round ((__v16hf) __A,
							   (__C << 2) | __B,
							   (__v16hf)
							   _mm256_setzero_ph (),
							   __U, __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getmant_round_ps (__m256 __A, _MM_MANTISSA_NORM_ENUM __B,
			 _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return
    (__m256) __builtin_ia32_getmantps256_mask_round ((__v8sf) __A,
						     (__C << 2) | __B,
						     _mm256_undefined_ps (),
						     (__mmask8) -1, __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_getmant_round_ps (__m256 __W, __mmask8 __U, __m256 __A,
			      _MM_MANTISSA_NORM_ENUM __B,
			      _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return (__m256) __builtin_ia32_getmantps256_mask_round ((__v8sf) __A,
							  (__C << 2) | __B,
							  (__v8sf) __W, __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getmant_round_ps (__mmask8 __U, __m256 __A,
			       _MM_MANTISSA_NORM_ENUM __B,
			       _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return (__m256) __builtin_ia32_getmantps256_mask_round ((__v8sf) __A,
							  (__C << 2) | __B,
							  (__v8sf)
							  _mm256_setzero_ps (),
							  __U, __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_max_round_pd (__m256d __A, __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_maxpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_undefined_pd (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_max_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			  __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_maxpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df) __W,
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_max_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			   const int __R)
{
  return (__m256d) __builtin_ia32_maxpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_setzero_pd (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_max_round_ph (__m256h __A, __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_maxph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_undefined_ph (),
						       (__mmask16) -1,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_max_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			  __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_maxph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf) __W,
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_max_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			   const int __R)
{
  return (__m256h) __builtin_ia32_maxph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_max_round_ps (__m256 __A, __m256 __B, const int __R)
{
  return (__m256) __builtin_ia32_maxps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_undefined_ps (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_max_round_ps (__m256 __W, __mmask8 __U, __m256 __A, __m256 __B,
			  const int __R)
{
  return (__m256) __builtin_ia32_maxps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf) __W,
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_max_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			   const int __R)
{
  return (__m256) __builtin_ia32_maxps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_setzero_ps (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_min_round_pd (__m256d __A, __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_minpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_undefined_pd (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_min_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			  __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_minpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df) __W,
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_min_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			   const int __R)
{
  return (__m256d) __builtin_ia32_minpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_setzero_pd (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_min_round_ph (__m256h __A, __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_minph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_undefined_ph (),
						       (__mmask16) -1,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_min_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			  __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_minph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf) __W,
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_min_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			   const int __R)
{
  return (__m256h) __builtin_ia32_minph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_min_round_ps (__m256 __A, __m256 __B, const int __R)
{
  return (__m256) __builtin_ia32_minps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_undefined_ps (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_min_round_ps (__m256 __W, __mmask8 __U, __m256 __A, __m256 __B,
			  const int __R)
{
  return (__m256) __builtin_ia32_minps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf) __W,
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_min_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			   const int __R)
{
  return (__m256) __builtin_ia32_minps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_setzero_ps (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mul_round_pd (__m256d __A, __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_mulpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_undefined_pd (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_mul_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			  __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_mulpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df) __W,
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_mul_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			   const int __R)
{
  return (__m256d) __builtin_ia32_mulpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_setzero_pd (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mul_round_ph (__m256h __A, __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_mulph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_undefined_ph (),
						       (__mmask16) -1,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_mul_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			  __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_mulph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf) __W,
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_mul_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			   const int __R)
{
  return (__m256h) __builtin_ia32_mulph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mul_round_ps (__m256 __A, __m256 __B, const int __R)
{
  return (__m256) __builtin_ia32_mulps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_undefined_ps (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_mul_round_ps (__m256 __W, __mmask8 __U, __m256 __A, __m256 __B,
			  const int __R)
{
  return (__m256) __builtin_ia32_mulps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf) __W,
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_mul_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			   const int __R)
{
  return (__m256) __builtin_ia32_mulps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_setzero_ps (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_range_round_pd (__m256d __A, __m256d __B, const int __C,
		       const int __R)
{
  return (__m256d) __builtin_ia32_rangepd256_mask_round ((__v4df) __A,
							 (__v4df) __B,
							 __C,
							 (__v4df)
							 _mm256_setzero_pd (),
							 (__mmask8) -1,
							 __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_range_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			    __m256d __B, const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_rangepd256_mask_round ((__v4df) __A,
							 (__v4df) __B,
							 __C,
							 (__v4df) __W,
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_range_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			     const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_rangepd256_mask_round ((__v4df) __A,
							 (__v4df) __B,
							 __C,
							 (__v4df)
							 _mm256_setzero_pd (),
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_range_round_ps (__m256 __A, __m256 __B, const int __C, const int __R)
{
  return (__m256) __builtin_ia32_rangeps256_mask_round ((__v8sf) __A,
							(__v8sf) __B,
							__C,
							(__v8sf)
							_mm256_setzero_ps (),
							(__mmask8) -1,
							__R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_range_round_ps (__m256 __W, __mmask8 __U, __m256 __A,
			    __m256 __B, const int __C, const int __R)
{
  return (__m256) __builtin_ia32_rangeps256_mask_round ((__v8sf) __A,
							(__v8sf) __B,
							__C,
							(__v8sf) __W,
							(__mmask8) __U,
							__R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_range_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			     const int __C, const int __R)
{
  return (__m256) __builtin_ia32_rangeps256_mask_round ((__v8sf) __A,
							(__v8sf) __B,
							__C,
							(__v8sf)
							_mm256_setzero_ps (),
							(__mmask8) __U,
							__R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_reduce_round_pd (__m256d __A, const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_reducepd256_mask_round ((__v4df) __A,
							  __C,
							  (__v4df)
							  _mm256_setzero_pd (),
							  (__mmask8) -1,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_reduce_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			     const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_reducepd256_mask_round ((__v4df) __A,
							  __C,
							  (__v4df) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_reduce_round_pd (__mmask8 __U, __m256d __A, const int __C,
			      const int __R)
{
  return (__m256d) __builtin_ia32_reducepd256_mask_round ((__v4df) __A,
							  __C,
							  (__v4df)
							  _mm256_setzero_pd (),
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_reduce_round_ph (__m256h __A, const int __C, const int __R)
{
  return (__m256h) __builtin_ia32_reduceph256_mask_round ((__v16hf) __A,
							  __C,
							  (__v16hf)
							  _mm256_setzero_ph (),
							  (__mmask16) -1,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_reduce_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			     const int __C, const int __R)
{
  return (__m256h) __builtin_ia32_reduceph256_mask_round ((__v16hf) __A,
							  __C,
							  (__v16hf) __W,
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_reduce_round_ph (__mmask16 __U, __m256h __A, const int __C,
			      const int __R)
{
  return (__m256h) __builtin_ia32_reduceph256_mask_round ((__v16hf) __A,
							  __C,
							  (__v16hf)
							  _mm256_setzero_ph (),
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_reduce_round_ps (__m256 __A, const int __C, const int __R)
{
  return (__m256) __builtin_ia32_reduceps256_mask_round ((__v8sf) __A,
							 __C,
							 (__v8sf)
							 _mm256_setzero_ps (),
							 (__mmask8) -1,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_reduce_round_ps (__m256 __W, __mmask8 __U, __m256 __A,
			     const int __C, const int __R)
{
  return (__m256) __builtin_ia32_reduceps256_mask_round ((__v8sf) __A,
							 __C,
							 (__v8sf) __W,
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_reduce_round_ps (__mmask8 __U, __m256 __A, const int __C,
			      const int __R)
{
  return (__m256) __builtin_ia32_reduceps256_mask_round ((__v8sf) __A,
							 __C,
							 (__v8sf)
							 _mm256_setzero_ps (),
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_roundscale_round_pd (__m256d __A, const int __C, const int __R)
{
  return
    (__m256d) __builtin_ia32_rndscalepd256_mask_round ((__v4df) __A,
						       __C,
						       (__v4df)
						       _mm256_undefined_pd (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_roundscale_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
				 const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_rndscalepd256_mask_round ((__v4df) __A,
							    __C,
							    (__v4df) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_roundscale_round_pd (__mmask8 __U, __m256d __A, const int __C,
				  const int __R)
{
  return
    (__m256d) __builtin_ia32_rndscalepd256_mask_round ((__v4df) __A,
						       __C,
						       (__v4df)
						       _mm256_setzero_pd (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_roundscale_round_ph (__m256h __A, const int __C, const int __R)
{
  return
    (__m256h) __builtin_ia32_rndscaleph256_mask_round ((__v16hf) __A,
						       __C,
						       (__v16hf)
						       _mm256_undefined_ph (),
						       (__mmask16) -1,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_roundscale_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
				 const int __C, const int __R)
{
  return (__m256h) __builtin_ia32_rndscaleph256_mask_round ((__v16hf) __A,
							    __C,
							    (__v16hf) __W,
							    (__mmask16) __U,
							    __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_roundscale_round_ph (__mmask16 __U, __m256h __A, const int __C,
				  const int __R)
{
  return
    (__m256h) __builtin_ia32_rndscaleph256_mask_round ((__v16hf) __A,
						       __C,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_roundscale_round_ps (__m256 __A, const int __C, const int __R)
{
  return
    (__m256) __builtin_ia32_rndscaleps256_mask_round ((__v8sf) __A,
						      __C,
						      (__v8sf)
						      _mm256_undefined_ps (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_roundscale_round_ps (__m256 __W, __mmask8 __U, __m256 __A,
				 const int __C, const int __R)
{
  return (__m256) __builtin_ia32_rndscaleps256_mask_round ((__v8sf) __A,
							   __C,
							   (__v8sf) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_roundscale_round_ps (__mmask8 __U, __m256 __A, const int __C,
				  const int __R)
{
  return (__m256) __builtin_ia32_rndscaleps256_mask_round ((__v8sf) __A,
							   __C,
							   (__v8sf)
							   _mm256_setzero_ps (),
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_scalef_round_pd (__m256d __A, __m256d __B, const int __R)
{
  return
    (__m256d) __builtin_ia32_scalefpd256_mask_round ((__v4df) __A,
						     (__v4df) __B,
						     (__v4df)
						     _mm256_undefined_pd (),
						     (__mmask8) -1,
						     __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_scalef_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			     __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_scalefpd256_mask_round ((__v4df) __A,
							  (__v4df) __B,
							  (__v4df) __W,
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_scalef_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			      const int __R)
{
  return (__m256d) __builtin_ia32_scalefpd256_mask_round ((__v4df) __A,
							  (__v4df) __B,
							  (__v4df)
							  _mm256_setzero_pd (),
							  (__mmask8) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_scalef_round_ph (__m256h __A, __m256h __B, const int __R)
{
  return
    (__m256h) __builtin_ia32_scalefph256_mask_round ((__v16hf) __A,
						     (__v16hf) __B,
						     (__v16hf)
						     _mm256_undefined_ph (),
						     (__mmask16) -1,
						     __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_scalef_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			     __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_scalefph256_mask_round ((__v16hf) __A,
							  (__v16hf) __B,
							  (__v16hf) __W,
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_scalef_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			      const int __R)
{
  return (__m256h) __builtin_ia32_scalefph256_mask_round ((__v16hf) __A,
							  (__v16hf) __B,
							  (__v16hf)
							  _mm256_setzero_ph (),
							  (__mmask16) __U,
							  __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_scalef_round_ps (__m256 __A, __m256 __B, const int __R)
{
  return (__m256) __builtin_ia32_scalefps256_mask_round ((__v8sf) __A,
							 (__v8sf) __B,
							 (__v8sf)
							 _mm256_undefined_ps (),
							 (__mmask8) -1,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_scalef_round_ps (__m256 __W, __mmask8 __U, __m256 __A,
			     __m256 __B, const int __R)
{
  return (__m256) __builtin_ia32_scalefps256_mask_round ((__v8sf) __A,
							 (__v8sf) __B,
							 (__v8sf) __W,
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_scalef_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			      const int __R)
{
  return (__m256) __builtin_ia32_scalefps256_mask_round ((__v8sf) __A,
							 (__v8sf) __B,
							 (__v8sf)
							 _mm256_setzero_ps (),
							 (__mmask8) __U,
							 __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sqrt_round_pd (__m256d __A, const int __R)
{
  return (__m256d) __builtin_ia32_sqrtpd256_mask_round ((__v4df) __A,
							(__v4df)
							_mm256_undefined_pd (),
							(__mmask8) -1,
							__R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sqrt_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			   const int __R)
{
  return (__m256d) __builtin_ia32_sqrtpd256_mask_round ((__v4df) __A,
							(__v4df) __W,
							(__mmask8) __U,
							__R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sqrt_round_pd (__mmask8 __U, __m256d __A, const int __R)
{
  return (__m256d) __builtin_ia32_sqrtpd256_mask_round ((__v4df) __A,
							(__v4df)
							_mm256_setzero_pd (),
							(__mmask8) __U,
							__R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sqrt_round_ph (__m256h __A, const int __R)
{
  return (__m256h) __builtin_ia32_sqrtph256_mask_round ((__v16hf) __A,
							(__v16hf)
							_mm256_undefined_ph (),
							(__mmask16) -1,
							__R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sqrt_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			   const int __R)
{
  return (__m256h) __builtin_ia32_sqrtph256_mask_round ((__v16hf) __A,
							(__v16hf) __W,
							(__mmask16) __U,
							__R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sqrt_round_ph (__mmask16 __U, __m256h __A, const int __R)
{
  return (__m256h) __builtin_ia32_sqrtph256_mask_round ((__v16hf) __A,
							(__v16hf)
							_mm256_setzero_ph (),
							(__mmask16) __U,
							__R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sqrt_round_ps (__m256 __A, const int __R)
{
  return (__m256) __builtin_ia32_sqrtps256_mask_round ((__v8sf) __A,
						       (__v8sf)
						       _mm256_undefined_ps (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sqrt_round_ps (__m256 __W, __mmask8 __U, __m256 __A,
			   const int __R)
{
  return (__m256) __builtin_ia32_sqrtps256_mask_round ((__v8sf) __A,
						       (__v8sf) __W,
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sqrt_round_ps (__mmask8 __U, __m256 __A, const int __R)
{
  return (__m256) __builtin_ia32_sqrtps256_mask_round ((__v8sf) __A,
						       (__v8sf)
						       _mm256_setzero_ps (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sub_round_pd (__m256d __A, __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_subpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_undefined_pd (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sub_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			  __m256d __B, const int __R)
{
  return (__m256d) __builtin_ia32_subpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df) __W,
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sub_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			   const int __R)
{
  return (__m256d) __builtin_ia32_subpd256_mask_round ((__v4df) __A,
						       (__v4df) __B,
						       (__v4df)
						       _mm256_setzero_pd (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sub_round_ph (__m256h __A, __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_subph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_undefined_ph (),
						       (__mmask16) -1,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sub_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			  __m256h __B, const int __R)
{
  return (__m256h) __builtin_ia32_subph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf) __W,
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sub_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			   const int __R)
{
  return (__m256h) __builtin_ia32_subph256_mask_round ((__v16hf) __A,
						       (__v16hf) __B,
						       (__v16hf)
						       _mm256_setzero_ph (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sub_round_ps (__m256 __A, __m256 __B, const int __R)
{
  return (__m256) __builtin_ia32_subps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_undefined_ps (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sub_round_ps (__m256 __W, __mmask8 __U, __m256 __A, __m256 __B,
			  const int __R)
{
  return (__m256) __builtin_ia32_subps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf) __W,
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sub_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			   const int __R)
{
  return (__m256) __builtin_ia32_subps256_mask_round ((__v8sf) __A,
						      (__v8sf) __B,
						      (__v8sf)
						      _mm256_setzero_ps (),
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

#define _mm256_cvt_roundpd_epi32(A, R) \
  ((__m128i) __builtin_ia32_cvtpd2dq256_mask_round ((__v4df) (A), \
						    (__v4si) \
						    (_mm_undefined_si128 ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_cvt_roundpd_epi32(W, U, A, R) \
  ((__m128i) __builtin_ia32_cvtpd2dq256_mask_round ((__v4df) (A), \
						    (__v4si) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_cvt_roundpd_epi32(U, A, R)\
  ((__m128i) __builtin_ia32_cvtpd2dq256_mask_round ((__v4df) (A), \
						    (__v4si) \
						    (_mm_setzero_si128 ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_cvt_roundpd_epi64(A, R) \
  ((__m256i) __builtin_ia32_cvtpd2qq256_mask_round ((__v4df) (A), \
						    (__v4di) \
						    (_mm256_setzero_si256 ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_cvt_roundpd_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvtpd2qq256_mask_round ((__v4df) (A), \
						    (__v4di) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_cvt_roundpd_epi64(U, A, R) \
  ((__m256i) __builtin_ia32_cvtpd2qq256_mask_round ((__v4df) (A), \
						    (__v4di) \
						    (_mm256_setzero_si256 ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_cvt_roundpd_epu32(A, R) \
  ((__m128i) __builtin_ia32_cvtpd2udq256_mask_round ((__v4df) (A), \
						     (__v4si) \
						     (_mm_undefined_si128 ()), \
						     (__mmask8) (-1),  \
						     (R)))

#define _mm256_mask_cvt_roundpd_epu32(W, U, A, R) \
  ((__m128i) __builtin_ia32_cvtpd2udq256_mask_round ((__v4df) (A), \
						     (__v4si) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundpd_epu32(U, A, R) \
  ((__m128i) __builtin_ia32_cvtpd2udq256_mask_round ((__v4df) (A), \
						     (__v4si) \
						     (_mm_setzero_si128 ()), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvt_roundpd_epu64(A, R) \
  ((__m256i) __builtin_ia32_cvtpd2uqq256_mask_round ((__v4df) (A), \
						     (__v4di) \
						     (_mm256_setzero_si256 ()),\
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvt_roundpd_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvtpd2uqq256_mask_round ((__v4df) (A), \
						     (__v4di) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundpd_epu64(U, A, R) \
  ((__m256i) __builtin_ia32_cvtpd2uqq256_mask_round ((__v4df) (A), \
						     (__v4di) \
						     (_mm256_setzero_si256 ()),\
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvt_roundph_epi32(A, R) \
  ((__m256i) __builtin_ia32_vcvtph2dq256_mask_round ((__v8hf) (A), \
						     (__v8si) \
						     (_mm256_setzero_si256 ()),\
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvt_roundph_epi32(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvtph2dq256_mask_round ((__v8hf) (A), \
						     (__v8si) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundph_epi32(U, A, R) \
  ((__m256i) __builtin_ia32_vcvtph2dq256_mask_round ((__v8hf) (A), \
						     (__v8si) \
						     (_mm256_setzero_si256 ()),\
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvt_roundph_pd(A, R) \
  ((__m256d) __builtin_ia32_vcvtph2pd256_mask_round ((__v8hf) (A), \
						     (__v4df) \
						     (_mm256_setzero_pd ()), \
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvt_roundph_pd(W, U, A, R) \
  ((__m256d) __builtin_ia32_vcvtph2pd256_mask_round ((__v8hf) (A), \
						     (__v4df) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundph_pd(U, A, R) \
  ((__m256d) __builtin_ia32_vcvtph2pd256_mask_round ((__v8hf) (A), \
						     (__v4df) \
						     (_mm256_setzero_pd ()), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvt_roundph_ps(A, R) \
  ((__m256) __builtin_ia32_vcvtph2ps256_mask_round ((__v8hf) (A),  \
						    (__v8sf) \
						    (_mm256_undefined_ps ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_cvt_roundph_ps(W, U, A, R) \
  ((__m256) __builtin_ia32_vcvtph2ps256_mask_round ((__v8hf) (A), \
						    (__v8sf) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_cvt_roundph_ps(U, A, R) \
  ((__m256) __builtin_ia32_vcvtph2ps256_mask_round ((__v8hf) (A), \
						    (__v8sf) \
						    (_mm256_setzero_ps ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_cvtx_roundph_ps(A, R) \
  ((__m256) __builtin_ia32_vcvtph2psx256_mask_round ((__v8hf) (A), \
						     (__v8sf) \
						     (_mm256_setzero_ps ()), \
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvtx_roundph_ps(W, U, A, R) \
  ((__m256) __builtin_ia32_vcvtph2psx256_mask_round ((__v8hf) (A), \
						     (__v8sf) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvtx_roundph_ps(U, A, R) \
  ((__m256) __builtin_ia32_vcvtph2psx256_mask_round ((__v8hf) (A), \
						     (__v8sf) \
						     (_mm256_setzero_ps ()), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvt_roundph_epi64(A, R) \
  ((__m256i) __builtin_ia32_vcvtph2qq256_mask_round ((__v8hf) (A), \
						     (__v4di) \
						     (_mm256_setzero_si256 ()),\
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvt_roundph_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvtph2qq256_mask_round ((__v8hf) (A), \
						     (__v4di) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundph_epi64(U, A, R) \
  ((__m256i) __builtin_ia32_vcvtph2qq256_mask_round ((__v8hf) (A), \
						     (__v4di) \
						     (_mm256_setzero_si256 ()),\
						     (__mmask8) (U),  \
						     (R)))

#define _mm256_cvt_roundph_epu32(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvtph2udq256_mask_round ((__v8hf) (A), \
					    (__v8si) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvt_roundph_epu32(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvtph2udq256_mask_round ((__v8hf) (A), \
						      (__v8si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvt_roundph_epu32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_vcvtph2udq256_mask_round ((__v8hf) (A), \
					    (__v8si) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U),  \
					    (R)))

#define _mm256_cvt_roundph_epu64(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvtph2uqq256_mask_round ((__v8hf) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (-1),  \
					    (R)))

#define _mm256_mask_cvt_roundph_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvtph2uqq256_mask_round ((__v8hf) (A), \
						      (__v4di) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvt_roundph_epu64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_vcvtph2uqq256_mask_round ((__v8hf) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvt_roundph_epu16(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvtph2uw256_mask_round ((__v16hf) (A), \
					   (__v16hi) \
					   (_mm256_undefined_si256 ()), \
					   (__mmask16) (-1), \
					   (R)))

#define _mm256_mask_cvt_roundph_epu16(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvtph2uw256_mask_round ((__v16hf) (A), \
						     (__v16hi) (W), \
						     (__mmask16) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundph_epu16(U, A, R) \
  ((__m256i) \
   __builtin_ia32_vcvtph2uw256_mask_round ((__v16hf) (A), \
					   (__v16hi) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask16) (U), \
					   (R)))

#define _mm256_cvt_roundph_epi16(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvtph2w256_mask_round ((__v16hf) (A), \
					  (__v16hi) \
					  (_mm256_undefined_si256 ()), \
					  (__mmask16) (-1), \
					  (R)))

#define _mm256_mask_cvt_roundph_epi16(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvtph2w256_mask_round ((__v16hf) (A), \
						    (__v16hi) (W), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_maskz_cvt_roundph_epi16(U, A, R) \
  ((__m256i) __builtin_ia32_vcvtph2w256_mask_round ((__v16hf) (A), \
						    (__v16hi) \
						    (_mm256_setzero_si256 ()), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_cvt_roundps_pd(A, R) \
  ((__m256d) __builtin_ia32_vcvtps2pd256_mask_round ((__v4sf) (A), \
						     (__v4df) \
						     (_mm256_undefined_pd ()), \
						     (__mmask8) (-1),  \
						     (R)))

#define _mm256_mask_cvt_roundps_pd(W, U, A, R) \
  ((__m256d) __builtin_ia32_vcvtps2pd256_mask_round ((__v4sf) (A), \
						     (__v4df) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundps_pd(U, A, R) \
  ((__m256d) __builtin_ia32_vcvtps2pd256_mask_round ((__v4sf) (A), \
						     (__v4df) \
						     (_mm256_setzero_pd ()), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvtx_roundps_ph(A, R) \
  ((__m128h) __builtin_ia32_vcvtps2phx256_mask_round ((__v8sf) (A), \
						      (__v8hf) \
						      (_mm_setzero_ph ()), \
						      (__mmask8) (-1), \
						      (R)))

#define _mm256_mask_cvtx_roundps_ph(W, U, A, R) \
  ((__m128h) __builtin_ia32_vcvtps2phx256_mask_round ((__v8sf) (A), \
						      (__v8hf) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtx_roundps_ph(U, A, R) \
  ((__m128h) __builtin_ia32_vcvtps2phx256_mask_round ((__v8sf) (A), \
						      (__v8hf) \
						      (_mm_setzero_ph ()), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_cvt_roundps_epi32(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvtps2dq256_mask_round ((__v8sf) (A), \
					   (__v8si) \
					   (_mm256_undefined_si256 ()), \
					   (__mmask8) (-1), \
					   (R)))

#define _mm256_mask_cvt_roundps_epi32(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvtps2dq256_mask_round ((__v8sf) (A), \
						     (__v8si) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundps_epi32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_vcvtps2dq256_mask_round ((__v8sf) (A), \
					   (__v8si) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask8) (U), \
					   (R)))

#define _mm256_cvt_roundps_epi64(A, R) \
  ((__m256i) __builtin_ia32_cvtps2qq256_mask_round ((__v4sf) (A), \
						    (__v4di) \
						    (_mm256_setzero_si256 ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_cvt_roundps_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvtps2qq256_mask_round ((__v4sf) (A), \
						    (__v4di) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_cvt_roundps_epi64(U, A, R) \
  ((__m256i) __builtin_ia32_cvtps2qq256_mask_round ((__v4sf) (A), \
						    (__v4di) \
						    (_mm256_setzero_si256 ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_cvt_roundps_epu32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvtps2udq256_mask_round ((__v8sf) (A), \
					   (__v8si) \
					   (_mm256_undefined_si256 ()), \
					   (__mmask8) (-1),  \
					   (R)))

#define _mm256_mask_cvt_roundps_epu32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvtps2udq256_mask_round ((__v8sf) (A), \
						     (__v8si) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundps_epu32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvtps2udq256_mask_round ((__v8sf) (A), \
					   (__v8si) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask8) (U), \
					   (R)))

#define _mm256_cvt_roundps_epu64(B, R) \
  ((__m256i) \
   __builtin_ia32_cvtps2uqq256_mask_round ((__v4sf) (B), \
					   (__v4di) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask8) (-1), \
					   (R)))

#define _mm256_mask_cvt_roundps_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvtps2uqq256_mask_round ((__v4sf) (A), \
						     (__v4di) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundps_epu64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvtps2uqq256_mask_round ((__v4sf) (A), \
					   (__v4di) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask8) (U), \
					   (R)))

#define  _mm256_cvt_roundepi64_pd(A, R) \
  ((__m256d) __builtin_ia32_cvtqq2pd256_mask_round ((__v4di) (A), \
						    (__v4df) \
						    (_mm256_setzero_pd ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_cvt_roundepi64_pd(W, U, A, R) \
  ((__m256d) __builtin_ia32_cvtqq2pd256_mask_round ((__v4di) (A), \
						    (__v4df) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_cvt_roundepi64_pd(U, A, R) \
  ((__m256d) __builtin_ia32_cvtqq2pd256_mask_round ((__v4di) (A), \
						    (__v4df) \
						    (_mm256_setzero_pd ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_cvt_roundepi64_ph(A, R) \
  ((__m128h) __builtin_ia32_vcvtqq2ph256_mask_round ((__v4di) (A), \
						     (__v8hf) \
						     (_mm_setzero_ph ()), \
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvt_roundepi64_ph(W, U, A, R) \
  ((__m128h) __builtin_ia32_vcvtqq2ph256_mask_round ((__v4di) (A), \
						     (__v8hf) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundepi64_ph(U, A, R) \
  ((__m128h) __builtin_ia32_vcvtqq2ph256_mask_round ((__v4di) (A), \
						     (__v8hf) \
						     (_mm_setzero_ph ()), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvt_roundepi64_ps(A, R) \
  ((__m128) __builtin_ia32_cvtqq2ps256_mask_round ((__v4di) (A), \
						   (__v4sf) \
						   (_mm_setzero_ps ()), \
						   (__mmask8) (-1), \
						   (R)))

#define  _mm256_mask_cvt_roundepi64_ps(W, U, A, R) \
  ((__m128) __builtin_ia32_cvtqq2ps256_mask_round ((__v4di) (A), \
						   (__v4sf) (W), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_maskz_cvt_roundepi64_ps(U, A, R) \
  ((__m128) __builtin_ia32_cvtqq2ps256_mask_round ((__v4di) (A), \
						   (__v4sf) \
						   (_mm_setzero_ps ()), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_cvtt_roundpd_epi32(A, R) \
  ((__m128i) __builtin_ia32_cvttpd2dq256_mask_round ((__v4df) (A), \
						     (__v4si) \
						     (_mm_undefined_si128 ()), \
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvtt_roundpd_epi32(W, U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2dq256_mask_round ((__v4df) (A), \
						     (__v4si) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvtt_roundpd_epi32(U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2dq256_mask_round ((__v4df) (A), \
						     (__v4si) \
						     (_mm_setzero_si128 ()), \
						     (__mmask8) (U),  \
						     (R)))

#define _mm256_cvtt_roundpd_epi64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2qq256_mask_round ((__v4df) (A), \
					   (__v4di) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask8) (-1), \
					   (R)))

#define _mm256_mask_cvtt_roundpd_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttpd2qq256_mask_round ((__v4df) (A), \
						     (__v4di) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvtt_roundpd_epi64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2qq256_mask_round ((__v4df) (A), \
					   (__v4di) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask8) (U), \
					   (R)))

#define _mm256_cvtt_roundpd_epu32(A, R) \
  ((__m128i) \
   __builtin_ia32_cvttpd2udq256_mask_round ((__v4df) (A), \
					    (__v4si) \
					    (_mm_undefined_si128 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtt_roundpd_epu32(W, U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2udq256_mask_round ((__v4df) (A), \
						      (__v4si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtt_roundpd_epu32(U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2udq256_mask_round ((__v4df) (A), \
						      (__v4si) \
						      (_mm_setzero_si128 ()), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_cvtt_roundpd_epu64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2uqq256_mask_round ((__v4df) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtt_roundpd_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttpd2uqq256_mask_round ((__v4df) (A), \
						      (__v4di) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtt_roundpd_epu64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2uqq256_mask_round ((__v4df) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtt_roundph_epi32(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2dq256_mask_round ((__v8hf) (A), \
					    (__v8si) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtt_roundph_epi32(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvttph2dq256_mask_round ((__v8hf) (A), \
						      (__v8si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtt_roundph_epi32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2dq256_mask_round ((__v8hf) (A), \
					    (__v8si) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtt_roundph_epi64(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2qq256_mask_round ((__v8hf) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtt_roundph_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvttph2qq256_mask_round ((__v8hf) (A), \
						      (__v4di) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtt_roundph_epi64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2qq256_mask_round ((__v8hf) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtt_roundph_epu32(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2udq256_mask_round ((__v8hf) (A), \
					     (__v8si) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtt_roundph_epu32(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvttph2udq256_mask_round ((__v8hf) (A), \
						       (__v8si) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtt_roundph_epu32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2udq256_mask_round ((__v8hf) (A), \
					     (__v8si) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (U), \
					     (R)))

#define _mm256_cvtt_roundph_epu64(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2uqq256_mask_round ((__v8hf) (A), \
					     (__v4di) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtt_roundph_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvttph2uqq256_mask_round ((__v8hf) (A), \
						       (__v4di) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtt_roundph_epu64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2uqq256_mask_round ((__v8hf) (A), \
					     (__v4di) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (U), \
					     (R)))

#define _mm256_cvtt_roundph_epu16(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2uw256_mask_round ((__v16hf) (A), \
					    (__v16hi) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask16) (-1), \
					    (R)))

#define _mm256_mask_cvtt_roundph_epu16(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvttph2uw256_mask_round ((__v16hf) (A), \
						      (__v16hi) (W), \
						      (__mmask16) (U), \
						      (R)))

#define _mm256_maskz_cvtt_roundph_epu16(U, A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2uw256_mask_round ((__v16hf) (A), \
					    (__v16hi) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask16) (U), \
					    (R)))

#define _mm256_cvtt_roundph_epi16(A, R) \
  ((__m256i) \
   __builtin_ia32_vcvttph2uw256_mask_round ((__v16hf) (A), \
					    (__v16hi) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask16) (-1),  \
					    (R)))

#define _mm256_mask_cvtt_roundph_epi16(W, U, A, R) \
  ((__m256i) __builtin_ia32_vcvttph2uw256_mask_round ((__v16hf) (A), \
						      (__v16hi) (W), \
						      (__mmask16) (U), \
						      (R)))

#define _mm256_maskz_cvtt_roundph_epi16(U, A, R)\
  ((__m256i) \
   __builtin_ia32_vcvttph2uw256_mask_round ((__v16hf) (A), \
					    (__v16hi) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask16) (U), \
					    (R)))

#define _mm256_cvtt_roundps_epi32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2dq256_mask_round ((__v8sf) (A), \
					   (__v8si) \
					   (_mm256_undefined_si256 ()), \
					   (__mmask8) (-1), \
					   (R)))

#define _mm256_mask_cvtt_roundps_epi32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2dq256_mask_round ((__v8sf) (A), \
						     (__v8si) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvtt_roundps_epi32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2dq256_mask_round ((__v8sf) (A), \
					   (__v8si) \
					   (_mm256_setzero_si256 ()),  \
					   (__mmask8) (U),  \
					   (R)))

#define _mm256_cvtt_roundps_epi64(A, R) \
  ((__m256i) __builtin_ia32_cvttps2qq256_mask_round ((__v4sf) (A), \
						     (__v4di) \
						     (_mm256_setzero_si256 ()),\
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_cvtt_roundps_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2qq256_mask_round ((__v4sf) (A), \
						     (__v4di) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_cvtt_roundps_epi64(U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2qq256_mask_round ((__v4sf) (A), \
						     (__v4di) \
						     (_mm256_setzero_si256 ()),\
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvtt_roundps_epu32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2udq256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtt_roundps_epu32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2udq256_mask_round ((__v8sf) (A), \
						      (__v8si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtt_roundps_epu32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2udq256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtt_roundps_epu64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2uqq256_mask_round ((__v4sf) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtt_roundps_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2uqq256_mask_round ((__v4sf) (A), \
						      (__v4di) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtt_roundps_epu64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2uqq256_mask_round ((__v4sf) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U),  \
					    (R)))

#define _mm256_cvt_roundepu32_ph(A, R) \
  ((__m128h) __builtin_ia32_vcvtudq2ph256_mask_round ((__v8si) (A), \
						      (__v8hf) \
						      (_mm_setzero_ph ()), \
						      (__mmask8) (-1), \
						      (R)))

#define _mm256_mask_cvt_roundepu32_ph(W, U, A, R) \
  ((__m128h) __builtin_ia32_vcvtudq2ph256_mask_round ((__v8si) (A), \
						      (__v8hf) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvt_roundepu32_ph(U, A, R) \
  ((__m128h) __builtin_ia32_vcvtudq2ph256_mask_round ((__v8si) (A), \
						      (__v8hf) \
						      (_mm_setzero_ph ()), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_cvt_roundepu32_ps(A, R) \
  ((__m256) __builtin_ia32_cvtudq2ps256_mask_round ((__v8si) (A), \
						    (__v8sf) \
						    (_mm256_undefined_ps ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_cvt_roundepu32_ps(W, U, A, R) \
  ((__m256) __builtin_ia32_cvtudq2ps256_mask_round ((__v8si) (A), \
						    (__v8sf) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_cvt_roundepu32_ps(U, A, R) \
  ((__m256) __builtin_ia32_cvtudq2ps256_mask_round ((__v8si) (A), \
						    (__v8sf) \
						    (_mm256_setzero_ps ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_cvt_roundepu64_pd(A, R) \
  ((__m256d) __builtin_ia32_cvtuqq2pd256_mask_round ((__v4di) (A), \
						     (__v4df) \
						     (_mm256_setzero_pd ()), \
						     (__mmask8) (-1),  \
						     (R)))

#define _mm256_mask_cvt_roundepu64_pd(W, U, A, R) \
  ((__m256d) __builtin_ia32_cvtuqq2pd256_mask_round ((__v4di) (A), \
						     (__v4df) (W),  \
						     (__mmask8) (U),  \
						     (R)))

#define _mm256_maskz_cvt_roundepu64_pd(U, A, R) \
  ((__m256d) __builtin_ia32_cvtuqq2pd256_mask_round ((__v4di) (A), \
						     (__v4df) \
						     (_mm256_setzero_pd ()), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_cvt_roundepu64_ph(A, R) \
  ((__m128h) __builtin_ia32_vcvtuqq2ph256_mask_round ((__v4di) (A), \
						      (__v8hf) \
						      (_mm_setzero_ph ()), \
						      (__mmask8) (-1), \
						      (R)))

#define _mm256_mask_cvt_roundepu64_ph(W, U, A, R) \
  ((__m128h) __builtin_ia32_vcvtuqq2ph256_mask_round ((__v4di) (A), \
						      (__v8hf) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvt_roundepu64_ph(U, A, R) \
  ((__m128h) __builtin_ia32_vcvtuqq2ph256_mask_round ((__v4di) (A), \
						      (__v8hf) \
						      (_mm_setzero_ph ()), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_cvt_roundepu64_ps(A, R) \
  ((__m128) __builtin_ia32_cvtuqq2ps256_mask_round ((__v4di) (A), \
						    (__v4sf) \
						    (_mm_setzero_ps ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_cvt_roundepu64_ps(W, U, A, R) \
  ((__m128) __builtin_ia32_cvtuqq2ps256_mask_round ((__v4di) (A), \
						    (__v4sf) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_cvt_roundepu64_ps(U, A, R) \
  ((__m128) __builtin_ia32_cvtuqq2ps256_mask_round ((__v4di) (A), \
						    (__v4sf) \
						    (_mm_setzero_ps ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_cvt_roundepu16_ph(A, R) \
  ((__m256h) __builtin_ia32_vcvtuw2ph256_mask_round ((__v16hi) (A), \
						     (__v16hf) \
						     (_mm256_setzero_ph ()), \
						     (__mmask16) (-1), \
						     (R)))

#define _mm256_mask_cvt_roundepu16_ph(W, U, A, R) \
  ((__m256h) __builtin_ia32_vcvtuw2ph256_mask_round ((__v16hi) (A), \
						     (__v16hf) (W), \
						     (__mmask16) (U), \
						     (R)))

#define _mm256_maskz_cvt_roundepu16_ph(U, A, R) \
  ((__m256h) __builtin_ia32_vcvtuw2ph256_mask_round ((__v16hi) (A), \
						     (__v16hf) \
						     (_mm256_setzero_ph ()), \
						     (__mmask16) (U), \
						     (R)))

#define _mm256_cvt_roundepi16_ph(A, R) \
  ((__m256h) __builtin_ia32_vcvtw2ph256_mask_round ((__v16hi) (A), \
						    (__v16hf) \
						    (_mm256_setzero_ph ()), \
						    (__mmask16) (-1), \
						    (R)))

#define _mm256_mask_cvt_roundepi16_ph(W, U, A, R) \
  ((__m256h) __builtin_ia32_vcvtw2ph256_mask_round ((__v16hi) (A), \
						    (__v16hf) (W), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_maskz_cvt_roundepi16_ph(U, A, R) \
  ((__m256h) __builtin_ia32_vcvtw2ph256_mask_round ((__v16hi) (A), \
						    (__v16hf) \
						    (_mm256_setzero_ph ()), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_div_round_pd(A, B, R) \
  ((__m256d) __builtin_ia32_divpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_undefined_pd ()), \
						 (__mmask8) (-1), \
						 (R)))

#define _mm256_mask_div_round_pd(W, U, A, B, R) \
  ((__m256d) __builtin_ia32_divpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) (W), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_maskz_div_round_pd(U, A, B, R) \
  ((__m256d) __builtin_ia32_divpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_setzero_pd ()), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_div_round_ph(A, B, R) \
  ((__m256h) __builtin_ia32_divph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_setzero_ph ()), \
						 (__mmask16) (-1), \
						 (R)))

#define _mm256_mask_div_round_ph(W, U, A, B, R) \
  ((__m256h) __builtin_ia32_divph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) (W), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_maskz_div_round_ph(U, A, B, R) \
  ((__m256h) __builtin_ia32_divph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_setzero_ph ()), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_div_round_ps(A, B, R) \
  ((__m256) __builtin_ia32_divps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_undefined_ps ()), \
						(__mmask8) (-1), \
						(R)))

#define _mm256_mask_div_round_ps(W, U, A, B, R) \
  ((__m256) __builtin_ia32_divps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) (W), \
						(__mmask8) (U), \
						(R)))

#define _mm256_maskz_div_round_ps(U, A, B, R) \
  ((__m256) __builtin_ia32_divps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_setzero_ps ()), \
						(__mmask8) (U), \
						(R)))

#define _mm256_fcmadd_round_pch(A, B, D, R) \
  (__m256h) __builtin_ia32_vfcmaddcph256_round ((A), (B), (D), (R))

#define _mm256_mask_fcmadd_round_pch(A, U, B, D, R) \
  ((__m256h) __builtin_ia32_vfcmaddcph256_mask_round ((__v16hf)(A), \
						      (__v16hf)(B), \
						      (__v16hf)(D), \
						      (U), (R)))

#define _mm256_mask3_fcmadd_round_pch(A, B, D, U, R) \
  ((__m256h) __builtin_ia32_vfcmaddcph256_mask3_round ((A), (B), (D), (U), (R)))

#define _mm256_maskz_fcmadd_round_pch(U, A, B, D, R) \
  ((__m256h) __builtin_ia32_vfcmaddcph256_maskz_round ((A), (B), (D), (U), (R)))

#define _mm256_fcmul_round_pch(A, B, R) \
  ((__m256h) __builtin_ia32_vfcmulcph256_round ((__v16hf) (A), \
						(__v16hf) (B), \
						(R)))

#define _mm256_mask_fcmul_round_pch(W, U, A, B, R) \
  ((__m256h) __builtin_ia32_vfcmulcph256_mask_round ((__v16hf) (A), \
						     (__v16hf) (B), \
						     (__v16hf) (W), \
						     (__mmask16) (U), \
						     (R)))

#define _mm256_maskz_fcmul_round_pch(U, A, B, R) \
  ((__m256h) __builtin_ia32_vfcmulcph256_mask_round ((__v16hf) (A), \
						     (__v16hf) (B), \
						     (__v16hf) \
						     (_mm256_setzero_ph ()), \
						     (__mmask16) (U), \
						     (R)))

#define _mm256_fixupimm_round_pd(A, B, D, C, R) \
  ((__m256d) __builtin_ia32_fixupimmpd256_mask_round ((__v4df) (A), \
						      (__v4df) (B), \
						      (__v4di) (D), \
						      (C), \
						      (__mmask8) (-1), \
						      (R)))

#define _mm256_mask_fixupimm_round_pd(A, U, B, D, C, R)\
  ((__m256d) __builtin_ia32_fixupimmpd256_mask_round ((__v4df) (A),  \
						      (__v4df) (B),  \
						      (__v4di) (D),  \
						      (C),  \
						      (__mmask8) (U),  \
						      (R)))

#define _mm256_maskz_fixupimm_round_pd(U, A, B, D, C, R)\
  ((__m256d) __builtin_ia32_fixupimmpd256_maskz_round ((__v4df) (A),  \
						       (__v4df) (B),  \
						       (__v4di) (D),  \
						       (C),  \
						       (__mmask8) (U),  \
						       (R)))

#define _mm256_fixupimm_round_ps(A, B, D, C, R)\
  ((__m256) __builtin_ia32_fixupimmps256_mask_round ((__v8sf) (A),  \
						     (__v8sf) (B),  \
						     (__v8si) (D),  \
						     (C),  \
						     (__mmask8) (-1),  \
						     (R)))

#define _mm256_mask_fixupimm_round_ps(A, U, B, D, C, R)\
  ((__m256) __builtin_ia32_fixupimmps256_mask_round ((__v8sf) (A),  \
						     (__v8sf) (B),  \
						     (__v8si) (D),  \
						     (C),  \
						     (__mmask8) (U),  \
						     (R)))

#define _mm256_maskz_fixupimm_round_ps(U, A, B, D, C, R)\
  ((__m256) __builtin_ia32_fixupimmps256_maskz_round ((__v8sf) (A),  \
						      (__v8sf) (B),  \
						      (__v8si) (D),  \
						      (C),  \
						      (__mmask8) (U),  \
						      (R)))

#define _mm256_fmadd_round_pd(A, B, D, R) \
  ((__m256d) __builtin_ia32_vfmaddpd256_mask_round (A, B, D, -1, R))

#define _mm256_mask_fmadd_round_pd(A, U, B, D, R) \
  ((__m256d) __builtin_ia32_vfmaddpd256_mask_round (A, B, D, U, R))

#define _mm256_mask3_fmadd_round_pd(A, B, D, U, R) \
  ((__m256d) __builtin_ia32_vfmaddpd256_mask3_round (A, B, D, U, R))

#define _mm256_maskz_fmadd_round_pd(U, A, B, D, R) \
  ((__m256d) __builtin_ia32_vfmaddpd256_maskz_round (A, B, D, U, R))

#define _mm256_fmadd_round_ph(A, B, D, R) \
  ((__m256h) __builtin_ia32_vfmaddph256_mask_round (A, B, D, -1, R))

#define _mm256_mask_fmadd_round_ph(A, U, B, D, R) \
  ((__m256h) __builtin_ia32_vfmaddph256_mask_round (A, B, D, U, R))

#define _mm256_mask3_fmadd_round_ph(A, B, D, U, R) \
  ((__m256h) __builtin_ia32_vfmaddph256_mask3_round (A, B, D, U, R))

#define _mm256_maskz_fmadd_round_ph(U, A, B, D, R) \
  ((__m256h) __builtin_ia32_vfmaddph256_maskz_round (A, B, D, U, R))

#define _mm256_fmadd_round_ps(A, B, D, R) \
  ((__m256)__builtin_ia32_vfmaddps256_mask_round (A, B, D, -1, R))

#define _mm256_mask_fmadd_round_ps(A, U, B, D, R)    \
  ((__m256)__builtin_ia32_vfmaddps256_mask_round (A, B, D, U, R))

#define _mm256_mask3_fmadd_round_ps(A, B, D, U, R)   \
  ((__m256)__builtin_ia32_vfmaddps256_mask3_round (A, B, D, U, R))

#define _mm256_maskz_fmadd_round_ps(U, A, B, D, R)   \
  ((__m256)__builtin_ia32_vfmaddps256_maskz_round (A, B, D, U, R))

#define _mm256_fmadd_round_pch(A, B, D, R)	\
  (__m256h) __builtin_ia32_vfmaddcph256_round ((A), (B), (D), (R))

#define _mm256_mask_fmadd_round_pch(A, U, B, D, R) \
  ((__m256h) __builtin_ia32_vfmaddcph256_mask_round ((__v16hf) (A), \
						     (__v16hf) (B), \
						     (__v16hf) (D), \
						     (U), (R)))

#define _mm256_mask3_fmadd_round_pch(A, B, D, U, R)	\
  (__m256h) __builtin_ia32_vfmaddcph256_mask3_round ((A), (B), (D), (U), (R))

#define _mm256_maskz_fmadd_round_pch(U, A, B, D, R)	\
  (__m256h) __builtin_ia32_vfmaddcph256_maskz_round ((A), (B), (D), (U), (R))

#define _mm256_fmaddsub_round_pd(A, B, D, R)		\
  (__m256d) __builtin_ia32_vfmaddsubpd256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fmaddsub_round_pd(A, U, B, D, R)    \
  (__m256d) __builtin_ia32_vfmaddsubpd256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fmaddsub_round_pd(A, B, D, U, R)   \
  (__m256d)__builtin_ia32_vfmaddsubpd256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fmaddsub_round_pd(U, A, B, D, R)   \
  (__m256d)__builtin_ia32_vfmaddsubpd256_maskz_round (A, B, D, U, R)

#define _mm256_fmaddsub_round_ph(A, B, D, R)		  \
  ((__m256h)__builtin_ia32_vfmaddsubph256_mask_round ((A), (B), (D), -1, (R)))

#define _mm256_mask_fmaddsub_round_ph(A, U, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfmaddsubph256_mask_round ((A), (B), (D), (U), (R)))

#define _mm256_mask3_fmaddsub_round_ph(A, B, D, U, R)	  \
  ((__m256h)__builtin_ia32_vfmaddsubph256_mask3_round ((A), (B), (D), (U), (R)))

#define _mm256_maskz_fmaddsub_round_ph(U, A, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfmaddsubph256_maskz_round ((A), (B), (D), (U), (R)))

#define _mm256_fmaddsub_round_ps(A, B, D, R)		\
  (__m256)__builtin_ia32_vfmaddsubps256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fmaddsub_round_ps(A, U, B, D, R)    \
  (__m256)__builtin_ia32_vfmaddsubps256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fmaddsub_round_ps(A, B, D, U, R)   \
  (__m256)__builtin_ia32_vfmaddsubps256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fmaddsub_round_ps(U, A, B, D, R)   \
  (__m256)__builtin_ia32_vfmaddsubps256_maskz_round (A, B, D, U, R)

#define _mm256_fmsub_round_pd(A, B, D, R)	    \
  (__m256d)__builtin_ia32_vfmsubpd256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fmsub_round_pd(A, U, B, D, R)    \
  (__m256d)__builtin_ia32_vfmsubpd256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fmsub_round_pd(A, B, D, U, R)   \
  (__m256d)__builtin_ia32_vfmsubpd256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fmsub_round_pd(U, A, B, D, R)   \
  (__m256d)__builtin_ia32_vfmsubpd256_maskz_round (A, B, D, U, R)

#define _mm256_fmsub_round_ph(A, B, D, R)	      \
  ((__m256h)__builtin_ia32_vfmsubph256_mask_round ((A), (B), (D), -1, (R)))

#define _mm256_mask_fmsub_round_ph(A, U, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfmsubph256_mask_round ((A), (B), (D), (U), (R)))

#define _mm256_mask3_fmsub_round_ph(A, B, D, U, R)	  \
  ((__m256h)__builtin_ia32_vfmsubph256_mask3_round ((A), (B), (D), (U), (R)))

#define _mm256_maskz_fmsub_round_ph(U, A, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfmsubph256_maskz_round ((A), (B), (D), (U), (R)))

#define _mm256_fmsub_round_ps(A, B, D, R)	    \
  (__m256)__builtin_ia32_vfmsubps256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fmsub_round_ps(A, U, B, D, R)    \
  (__m256)__builtin_ia32_vfmsubps256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fmsub_round_ps(A, B, D, U, R)   \
  (__m256)__builtin_ia32_vfmsubps256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fmsub_round_ps(U, A, B, D, R)   \
  (__m256)__builtin_ia32_vfmsubps256_maskz_round (A, B, D, U, R)

#define _mm256_fmsubadd_round_pd(A, B, D, R)		\
  (__m256d)__builtin_ia32_vfmsubaddpd256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fmsubadd_round_pd(A, U, B, D, R)    \
  (__m256d)__builtin_ia32_vfmsubaddpd256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fmsubadd_round_pd(A, B, D, U, R)   \
  (__m256d)__builtin_ia32_vfmsubaddpd256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fmsubadd_round_pd(U, A, B, D, R)   \
  (__m256d)__builtin_ia32_vfmsubaddpd256_maskz_round (A, B, D, U, R)

#define _mm256_fmsubadd_round_ph(A, B, D, R)	    \
  ((__m256h)__builtin_ia32_vfmsubaddph256_mask_round ((A), (B), (D), -1, (R)))

#define _mm256_mask_fmsubadd_round_ph(A, U, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfmsubaddph256_mask_round ((A), (B), (D), (U), (R)))

#define _mm256_mask3_fmsubadd_round_ph(A, B, D, U, R)	  \
  ((__m256h)__builtin_ia32_vfmsubaddph256_mask3_round ((A), (B), (D), (U), (R)))

#define _mm256_maskz_fmsubadd_round_ph(U, A, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfmsubaddph256_maskz_round ((A), (B), (D), (U), (R)))

#define _mm256_fmsubadd_round_ps(A, B, D, R)		\
  (__m256)__builtin_ia32_vfmsubaddps256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fmsubadd_round_ps(A, U, B, D, R)    \
  (__m256)__builtin_ia32_vfmsubaddps256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fmsubadd_round_ps(A, B, D, U, R)   \
  (__m256)__builtin_ia32_vfmsubaddps256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fmsubadd_round_ps(U, A, B, D, R)   \
  (__m256)__builtin_ia32_vfmsubaddps256_maskz_round (A, B, D, U, R)

#define _mm256_fmul_round_pch(B, D, R) \
  ((__m256h) __builtin_ia32_vfmulcph256_round ((__v16hf) (B), \
					       (__v16hf) (D), \
					       (R)))

#define _mm256_mask_fmul_round_pch(A, U, B, D, R) \
  ((__m256h) __builtin_ia32_vfmulcph256_mask_round ((__v16hf) (B), \
						    (__v16hf) (D), \
						    (__v16hf) (A), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_maskz_fmul_round_pch(U, B, D, R) \
  ((__m256h) __builtin_ia32_vfmulcph256_mask_round ((__v16hf) (B), \
						    (__v16hf) (D), \
						    (__v16hf) \
						    (_mm256_setzero_ph ()), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_fnmadd_round_pd(A, B, D, R)	      \
  (__m256d)__builtin_ia32_vfnmaddpd256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fnmadd_round_pd(A, U, B, D, R)    \
  (__m256d)__builtin_ia32_vfnmaddpd256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fnmadd_round_pd(A, B, D, U, R)   \
  (__m256d)__builtin_ia32_vfnmaddpd256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fnmadd_round_pd(U, A, B, D, R)   \
  (__m256d)__builtin_ia32_vfnmaddpd256_maskz_round (A, B, D, U, R)

#define _mm256_fnmadd_round_ph(A, B, D, R)	    \
  ((__m256h)__builtin_ia32_vfnmaddph256_mask_round ((A), (B), (D), -1, (R)))

#define _mm256_mask_fnmadd_round_ph(A, U, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfnmaddph256_mask_round ((A), (B), (D), (U), (R)))

#define _mm256_mask3_fnmadd_round_ph(A, B, D, U, R)	  \
  ((__m256h)__builtin_ia32_vfnmaddph256_mask3_round ((A), (B), (D), (U), (R)))

#define _mm256_maskz_fnmadd_round_ph(U, A, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfnmaddph256_maskz_round ((A), (B), (D), (U), (R)))

#define _mm256_fnmadd_round_ps(A, B, D, R)	      \
  (__m256)__builtin_ia32_vfnmaddps256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fnmadd_round_ps(A, U, B, D, R)    \
  (__m256)__builtin_ia32_vfnmaddps256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fnmadd_round_ps(A, B, D, U, R)   \
  (__m256)__builtin_ia32_vfnmaddps256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fnmadd_round_ps(U, A, B, D, R)   \
  (__m256)__builtin_ia32_vfnmaddps256_maskz_round (A, B, D, U, R)

#define _mm256_fnmsub_round_pd(A, B, D, R)	      \
  (__m256d)__builtin_ia32_vfnmsubpd256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fnmsub_round_pd(A, U, B, D, R)    \
  (__m256d)__builtin_ia32_vfnmsubpd256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fnmsub_round_pd(A, B, D, U, R)   \
  (__m256d)__builtin_ia32_vfnmsubpd256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fnmsub_round_pd(U, A, B, D, R)   \
  (__m256d)__builtin_ia32_vfnmsubpd256_maskz_round (A, B, D, U, R)

#define _mm256_fnmsub_round_ph(A, B, D, R)	    \
  ((__m256h)__builtin_ia32_vfnmsubph256_mask_round ((A), (B), (D), -1, (R)))

#define _mm256_mask_fnmsub_round_ph(A, U, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfnmsubph256_mask_round ((A), (B), (D), (U), (R)))

#define _mm256_mask3_fnmsub_round_ph(A, B, D, U, R)	  \
  ((__m256h)__builtin_ia32_vfnmsubph256_mask3_round ((A), (B), (D), (U), (R)))

#define _mm256_maskz_fnmsub_round_ph(U, A, B, D, R)	  \
  ((__m256h)__builtin_ia32_vfnmsubph256_maskz_round ((A), (B), (D), (U), (R)))

#define _mm256_fnmsub_round_ps(A, B, D, R)	      \
  (__m256)__builtin_ia32_vfnmsubps256_mask_round (A, B, D, -1, R)

#define _mm256_mask_fnmsub_round_ps(A, U, B, D, R)    \
  (__m256)__builtin_ia32_vfnmsubps256_mask_round (A, B, D, U, R)

#define _mm256_mask3_fnmsub_round_ps(A, B, D, U, R)   \
  (__m256)__builtin_ia32_vfnmsubps256_mask3_round (A, B, D, U, R)

#define _mm256_maskz_fnmsub_round_ps(U, A, B, D, R)   \
  (__m256)__builtin_ia32_vfnmsubps256_maskz_round (A, B, D, U, R)

#define _mm256_getexp_round_pd(A, R) \
  ((__m256d) __builtin_ia32_getexppd256_mask_round ((__v4df) (A), \
						    (__v4df) \
						    (_mm256_undefined_pd ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_getexp_round_pd(W, U, A, R) \
  ((__m256d) __builtin_ia32_getexppd256_mask_round ((__v4df) (A), \
						    (__v4df) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_getexp_round_pd(U, A, R) \
  ((__m256d) __builtin_ia32_getexppd256_mask_round ((__v4df) (A), \
						    (__v4df) \
						    (_mm256_setzero_pd ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_getexp_round_ph(A, R)\
  ((__m256h) __builtin_ia32_getexpph256_mask_round ((__v16hf) (A), \
						    (__v16hf) \
						    (_mm256_setzero_ph ()), \
						    (__mmask16) (-1), \
						    (R)))

#define _mm256_mask_getexp_round_ph(W, U, A, R)\
  ((__m256h) __builtin_ia32_getexpph256_mask_round ((__v16hf) (A), \
						    (__v16hf) (W), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_maskz_getexp_round_ph(U, A, R)\
  ((__m256h) __builtin_ia32_getexpph256_mask_round ((__v16hf) (A), \
						    (__v16hf) \
						    (_mm256_setzero_ph ()), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_getexp_round_ps(A, R)\
  ((__m256) __builtin_ia32_getexpps256_mask_round ((__v8sf) (A), \
						   (__v8sf) \
						   (_mm256_undefined_ps ()), \
						   (__mmask8) (-1), \
						   (R)))

#define _mm256_mask_getexp_round_ps(W, U, A, R)\
  ((__m256) __builtin_ia32_getexpps256_mask_round ((__v8sf) (A), \
						   (__v8sf) (W), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_maskz_getexp_round_ps(U, A, R)\
  ((__m256) __builtin_ia32_getexpps256_mask_round ((__v8sf) (A), \
						   (__v8sf) \
						   (_mm256_setzero_ps ()), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_getmant_round_pd(A, B, C, R) \
  ((__m256d)__builtin_ia32_getmantpd256_mask_round ((__v4df) (__m256d) (A), \
						    (int) (((C) << 2) | (B)), \
						    (__v4df) (__m256d) \
						    _mm256_undefined_pd (), \
						    (__mmask8)-1, \
						    (R)))

#define _mm256_mask_getmant_round_pd(W, U, A, B, C, R) \
  ((__m256d)__builtin_ia32_getmantpd256_mask_round ((__v4df) (__m256d) (A),    \
						    (int) (((C) << 2) | (B)), \
						    (__v4df) (__m256d) (W),    \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_getmant_round_pd(U, A, B, C, R) \
  ((__m256d)__builtin_ia32_getmantpd256_mask_round ((__v4df) (__m256d) (A), \
						    (int) (((C) << 2) | (B)), \
						    (__v4df) (__m256d) \
						    _mm256_setzero_pd (), \
						    (__mmask8) (U), \
						    (R)))


#define _mm256_getmant_round_ph(A, B, C, R) \
  ((__m256h)__builtin_ia32_getmantph256_mask_round ((__v16hf) (__m256h) (A), \
						    (int) (((C)<<2) | (B)), \
						    (__v16hf) (__m256h) \
						    _mm256_undefined_ph (), \
						    (__mmask16)-1, \
						    (R)))

#define _mm256_mask_getmant_round_ph(W, U, A, B, C, R) \
  ((__m256h)__builtin_ia32_getmantph256_mask_round ((__v16hf) (__m256h) (A), \
						    (int) (((C)<<2) | (B)), \
						    (__v16hf) (__m256h) (W), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_maskz_getmant_round_ph(U, A, B, C, R) \
  ((__m256h)__builtin_ia32_getmantph256_mask_round ((__v16hf) (__m256h) (A), \
						    (int) (((C)<<2) | (B)), \
						    (__v16hf) (__m256h) \
						    _mm256_setzero_ph (), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_getmant_round_ps(A, B, C, R) \
  ((__m256)__builtin_ia32_getmantps256_mask_round ((__v8sf) (__m256) (A), \
						   (int) (((C)<<2) | (B)), \
						   (__v8sf) (__m256) \
						   _mm256_undefined_ps (), \
						   (__mmask8)-1, \
						   (R)))

#define _mm256_mask_getmant_round_ps(W, U, A, B, C, R) \
  ((__m256)__builtin_ia32_getmantps256_mask_round ((__v8sf) (__m256) (A), \
						   (int) (((C)<<2) | (B)), \
						   (__v8sf) (__m256) (W), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_maskz_getmant_round_ps(U, A, B, C, R) \
  ((__m256)__builtin_ia32_getmantps256_mask_round ((__v8sf) (__m256) (A), \
						   (int) (((C)<<2) | (B)), \
						   (__v8sf) (__m256) \
						   _mm256_setzero_ps (), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_max_round_pd(A, B, R) \
  ((__m256d) __builtin_ia32_maxpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_undefined_pd ()), \
						 (__mmask8) (-1), \
						 (R)))

#define _mm256_mask_max_round_pd(W, U, A, B, R) \
  ((__m256d) __builtin_ia32_maxpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) (W), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_maskz_max_round_pd(U, A, B, R) \
  ((__m256d) __builtin_ia32_maxpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_setzero_pd ()), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_max_round_ph(A, B, R) \
  ((__m256h) __builtin_ia32_maxph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_undefined_ph ()), \
						 (__mmask16) (-1), \
						 (R)))

#define _mm256_mask_max_round_ph(W, U, A, B, R) \
  ((__m256h) __builtin_ia32_maxph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) (W), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_maskz_max_round_ph(U, A, B, R) \
  ((__m256h) __builtin_ia32_maxph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_setzero_ph ()), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_max_round_ps(A, B, R) \
  ((__m256) __builtin_ia32_maxps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_undefined_ps ()), \
						(__mmask8) (-1), \
						(R)))

#define _mm256_mask_max_round_ps(W, U, A, B, R) \
  ((__m256) __builtin_ia32_maxps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) (W), \
						(__mmask8) (U), \
						(R)))

#define _mm256_maskz_max_round_ps(U, A, B, R) \
  ((__m256) __builtin_ia32_maxps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_setzero_ps ()), \
						(__mmask8) (U), \
						(R)))

#define _mm256_min_round_pd(A, B, R) \
  ((__m256d) __builtin_ia32_minpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_undefined_pd ()), \
						 (__mmask8) (-1), \
						 (R)))

#define _mm256_mask_min_round_pd(W, U, A, B, R) \
  ((__m256d) __builtin_ia32_minpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) (W), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_maskz_min_round_pd(U, A, B, R) \
  ((__m256d) __builtin_ia32_minpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_setzero_pd ()), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_min_round_ph(A, B, R) \
  ((__m256h) __builtin_ia32_minph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_undefined_ph ()), \
						 (__mmask16) (-1), \
						 (R)))

#define _mm256_mask_min_round_ph(W, U, A, B, R) \
  ((__m256h) __builtin_ia32_minph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) (W), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_maskz_min_round_ph(U, A, B, R) \
  ((__m256h) __builtin_ia32_minph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_setzero_ph ()), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_min_round_ps(A, B, R) \
  ((__m256) __builtin_ia32_minps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_undefined_ps ()), \
						(__mmask8) (-1), \
						(R)))

#define _mm256_mask_min_round_ps(W, U, A, B, R) \
  ((__m256) __builtin_ia32_minps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) (W), \
						(__mmask8) (U), \
						(R)))

#define _mm256_maskz_min_round_ps(U, A, B, R) \
  ((__m256) __builtin_ia32_minps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_setzero_ps ()), \
						(__mmask8) (U), \
						(R)))

#define _mm256_mul_round_pd(A, B, R) \
  ((__m256d) __builtin_ia32_mulpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_undefined_pd ()), \
						 (__mmask8) (-1), \
						 (R)))

#define _mm256_mask_mul_round_pd(W, U, A, B, R) \
  ((__m256d) __builtin_ia32_mulpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) (W), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_maskz_mul_round_pd(U, A, B, R) \
  ((__m256d) __builtin_ia32_mulpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_setzero_pd ()), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_mul_round_ph(A, B, R) \
  ((__m256h) __builtin_ia32_mulph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_undefined_ph ()), \
						 (__mmask16) (-1), \
						 (R)))

#define _mm256_mask_mul_round_ph(W, U, A, B, R) \
  ((__m256h) __builtin_ia32_mulph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) (W), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_maskz_mul_round_ph(U, A, B, R) \
  ((__m256h) __builtin_ia32_mulph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_setzero_ph ()), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_mul_round_ps(A, B, R) \
  ((__m256) __builtin_ia32_mulps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_undefined_ps ()), \
						(__mmask8) (-1), \
						(R)))

#define _mm256_mask_mul_round_ps(W, U, A, B, R) \
  ((__m256) __builtin_ia32_mulps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) (W), \
						(__mmask8) (U), \
						(R)))

#define _mm256_maskz_mul_round_ps(U, A, B, R) \
  ((__m256) __builtin_ia32_mulps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_setzero_ps ()), \
						(__mmask8) (U), \
						(R)))

#define _mm256_range_round_pd(A, B, C, R) \
  ((__m256d) __builtin_ia32_rangepd256_mask_round ((__v4df) (A), \
						   (__v4df) (B), \
						   (C), \
						   (__v4df) \
						   (_mm256_setzero_pd ()), \
						   (__mmask8) (-1), \
						   (R)))

#define _mm256_mask_range_round_pd(W, U, A, B, C, R) \
  ((__m256d) __builtin_ia32_rangepd256_mask_round ((__v4df) (A), \
						   (__v4df) (B), \
						   (C), \
						   (__v4df) (W), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_maskz_range_round_pd(U, A, B, C, R) \
  ((__m256d) __builtin_ia32_rangepd256_mask_round ((__v4df) (A), \
						   (__v4df) (B), \
						   (C), \
						   (__v4df) \
						   (_mm256_setzero_pd ()), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_range_round_ps(A, B, C, R) \
  ((__m256) __builtin_ia32_rangeps256_mask_round ((__v8sf) (A), \
						  (__v8sf) (B), \
						  (C), \
						  (__v8sf) \
						  (_mm256_setzero_ps ()), \
						  (__mmask8) (-1), \
						  (R)))

#define _mm256_mask_range_round_ps(W, U, A, B, C, R) \
  ((__m256) __builtin_ia32_rangeps256_mask_round ((__v8sf) (A), \
						  (__v8sf) (B), \
						  (C), \
						  (__v8sf) (W), \
						  (__mmask8) (U), \
						  (R)))

#define _mm256_maskz_range_round_ps(U, A, B, C, R) \
  ((__m256) __builtin_ia32_rangeps256_mask_round ((__v8sf) (A), \
						  (__v8sf) (B), \
						  (C), \
						  (__v8sf) \
						  (_mm256_setzero_ps ()), \
						  (__mmask8) (U), \
						  (R)))

#define _mm256_reduce_round_pd(A, C, R) \
  ((__m256d) __builtin_ia32_reducepd256_mask_round ((__v4df) (A), \
						    (C), \
						    (__v4df) \
						    (_mm256_setzero_pd ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_reduce_round_pd(W, U, A, C, R) \
  ((__m256d) __builtin_ia32_reducepd256_mask_round ((__v4df) (A), \
						    (C), \
						    (__v4df) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_reduce_round_pd(U, A, C, R) \
  ((__m256d) __builtin_ia32_reducepd256_mask_round ((__v4df) (A), \
						    (C), \
						    (__v4df) \
						    (_mm256_setzero_pd ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_reduce_round_ph(A, C, R) \
  ((__m256h) __builtin_ia32_reduceph256_mask_round ((__v16hf) (A), \
						    (C), \
						    (__v16hf) \
						    (_mm256_setzero_ph ()), \
						    (__mmask16) (-1), \
						    (R)))

#define _mm256_mask_reduce_round_ph(W, U, A, C, R) \
  ((__m256h) __builtin_ia32_reduceph256_mask_round ((__v16hf) (A), \
						    (C), \
						    (__v16hf) (W), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_maskz_reduce_round_ph(U, A, C, R) \
  ((__m256h) __builtin_ia32_reduceph256_mask_round ((__v16hf) (A), \
						    (C), \
						    (__v16hf) \
						    (_mm256_setzero_ph ()), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_reduce_round_ps(A, C, R) \
  ((__m256) __builtin_ia32_reduceps256_mask_round ((__v8sf) (A), \
						   (C), \
						   (__v8sf) \
						   (_mm256_setzero_ps ()), \
						   (__mmask8) (-1), \
						   (R)))

#define _mm256_mask_reduce_round_ps(W, U, A, C, R) \
  ((__m256) __builtin_ia32_reduceps256_mask_round ((__v8sf) (A), \
						   (C), \
						   (__v8sf) (W), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_maskz_reduce_round_ps(U, A, C, R) \
  ((__m256) __builtin_ia32_reduceps256_mask_round ((__v8sf) (A), \
						   (C), \
						   (__v8sf) \
						   (_mm256_setzero_ps ()), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_roundscale_round_pd(A, C, R) \
  ((__m256d) \
   __builtin_ia32_rndscalepd256_mask_round ((__v4df) (A), \
					    (C), \
					    (__v4df) \
					    (_mm256_undefined_pd ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_roundscale_round_pd(W, U, A, C, R) \
  ((__m256d) __builtin_ia32_rndscalepd256_mask_round ((__v4df) (A), \
						      (C), \
						      (__v4df) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_roundscale_round_pd(U, A, C, R) \
  ((__m256d) __builtin_ia32_rndscalepd256_mask_round ((__v4df) (A), \
						      (C), \
						      (__v4df) \
						      (_mm256_setzero_pd ()), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_roundscale_round_ph(A, C, R) \
  ((__m256h) \
   __builtin_ia32_rndscaleph256_mask_round ((__v16hf) (A), \
					    (C), \
					    (__v16hf) \
					    (_mm256_undefined_ph ()), \
					    (__mmask16) (-1), \
					    (R)))

#define _mm256_mask_roundscale_round_ph(W, U, A, C, R) \
  ((__m256h) __builtin_ia32_rndscaleph256_mask_round ((__v16hf) (A), \
						      (C), \
						      (__v16hf) (W), \
						      (__mmask16) (U), \
						      (R)))

#define _mm256_maskz_roundscale_round_ph(U, A, C, R) \
  ((__m256h) __builtin_ia32_rndscaleph256_mask_round ((__v16hf) (A), \
						      (C), \
						      (__v16hf) \
						      (_mm256_setzero_ph ()), \
						      (__mmask16) (U), \
						      (R)))

#define _mm256_roundscale_round_ps(A, C, R) \
  ((__m256) __builtin_ia32_rndscaleps256_mask_round ((__v8sf) (A), \
						     (C), \
						     (__v8sf) \
						     (_mm256_undefined_ps ()), \
						     (__mmask8) (-1), \
						     (R)))

#define _mm256_mask_roundscale_round_ps(W, U, A, C, R) \
  ((__m256) __builtin_ia32_rndscaleps256_mask_round ((__v8sf) (A), \
						     (C), \
						     (__v8sf) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_roundscale_round_ps(U, A, C, R) \
  ((__m256) __builtin_ia32_rndscaleps256_mask_round ((__v8sf) (A), \
						     (C), \
						     (__v8sf) \
						     (_mm256_setzero_ps ()), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_scalef_round_pd(A, B, R) \
  ((__m256d) __builtin_ia32_scalefpd256_mask_round ((__v4df) (A), \
						    (__v4df) (B), \
						    (__v4df) \
						    (_mm256_undefined_pd ()), \
						    (__mmask8) (-1), \
						    (R)))

#define _mm256_mask_scalef_round_pd(W, U, A, B, R) \
  ((__m256d) __builtin_ia32_scalefpd256_mask_round ((__v4df) (A), \
						    (__v4df) (B), \
						    (__v4df) (W), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_maskz_scalef_round_pd(U, A, B, R) \
  ((__m256d) __builtin_ia32_scalefpd256_mask_round ((__v4df) (A), \
						    (__v4df) (B), \
						    (__v4df) \
						    (_mm256_setzero_pd ()), \
						    (__mmask8) (U), \
						    (R)))

#define _mm256_scalef_round_ph(A, B, R) \
  ((__m256h) __builtin_ia32_scalefph256_mask_round ((__v16hf) (A), \
						    (__v16hf) (B), \
						    (__v16hf) \
						    (_mm256_undefined_ph ()), \
						    (__mmask16) (-1), \
						    (R)))

#define _mm256_mask_scalef_round_ph(W, U, A, B, R) \
  ((__m256h) __builtin_ia32_scalefph256_mask_round ((__v16hf) (A), \
						    (__v16hf) (B), \
						    (__v16hf) (W), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_maskz_scalef_round_ph(U, A, B, R) \
  ((__m256h) __builtin_ia32_scalefph256_mask_round ((__v16hf) (A), \
						    (__v16hf) (B), \
						    (__v16hf) \
						    (_mm256_setzero_ph ()), \
						    (__mmask16) (U), \
						    (R)))

#define _mm256_scalef_round_ps(A, B, R) \
  ((__m256) __builtin_ia32_scalefps256_mask_round ((__v8sf) (A), \
						   (__v8sf) (B), \
						   (__v8sf) \
						   (_mm256_undefined_ps ()), \
						   (__mmask8) (-1), \
						   (R)))

#define _mm256_mask_scalef_round_ps(W, U, A, B, R) \
  ((__m256) __builtin_ia32_scalefps256_mask_round ((__v8sf) (A), \
						   (__v8sf) (B), \
						   (__v8sf) (W), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_maskz_scalef_round_ps(U, A, B, R) \
  ((__m256) __builtin_ia32_scalefps256_mask_round ((__v8sf) (A), \
						   (__v8sf) (B), \
						   (__v8sf) \
						   (_mm256_setzero_ps ()), \
						   (__mmask8) (U), \
						   (R)))

#define _mm256_sqrt_round_pd(A, R) \
  ((__m256d) __builtin_ia32_sqrtpd256_mask_round ((__v4df) (A), \
						  (__v4df) \
						  (_mm256_undefined_pd ()), \
						  (__mmask8) (-1), \
						  (R)))

#define _mm256_mask_sqrt_round_pd(W, U, A, R) \
  ((__m256d) __builtin_ia32_sqrtpd256_mask_round ((__v4df) (A), \
						  (__v4df) (W), \
						  (__mmask8) (U), \
						  (R)))

#define _mm256_maskz_sqrt_round_pd(U, A, R) \
  ((__m256d) __builtin_ia32_sqrtpd256_mask_round ((__v4df) (A), \
						  (__v4df) \
						  (_mm256_setzero_pd ()), \
						  (__mmask8) (U), \
						  (R)))

#define _mm256_sqrt_round_ph(A, R) \
  ((__m256h) __builtin_ia32_sqrtph256_mask_round ((__v16hf) (A), \
						  (__v16hf) \
						  (_mm256_undefined_ph ()), \
						  (__mmask16) (-1), \
						  (R)))

#define _mm256_mask_sqrt_round_ph(W, U, A, R) \
  ((__m256h) __builtin_ia32_sqrtph256_mask_round ((__v16hf) (A), \
						  (__v16hf) (W), \
						  (__mmask16) (U), \
						  (R)))

#define _mm256_maskz_sqrt_round_ph(U, A, R) \
  ((__m256h) __builtin_ia32_sqrtph256_mask_round ((__v16hf) (A), \
						  (__v16hf) \
						  (_mm256_setzero_ph ()), \
						  (__mmask16) (U), \
						  (R)))

#define _mm256_sqrt_round_ps(A, R) \
  ((__m256) __builtin_ia32_sqrtps256_mask_round ((__v8sf) (A), \
						 (__v8sf) \
						 (_mm256_undefined_ps ()), \
						 (__mmask8) (-1), \
						 (R)))

#define _mm256_mask_sqrt_round_ps(W, U, A, R) \
  ((__m256) __builtin_ia32_sqrtps256_mask_round ((__v8sf) (A), \
						 (__v8sf) (W), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_maskz_sqrt_round_ps(U, A, R) \
  ((__m256) __builtin_ia32_sqrtps256_mask_round ((__v8sf) (A), \
						 (__v8sf) \
						 (_mm256_setzero_ps ()), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_sub_round_pd(A, B, R) \
  ((__m256d) __builtin_ia32_subpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_undefined_pd ()), \
						 (__mmask8) (-1), \
						 (R)))

#define _mm256_mask_sub_round_pd(W, U, A, B, R) \
  ((__m256d) __builtin_ia32_subpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) (W), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_maskz_sub_round_pd(U, A, B, R) \
  ((__m256d) __builtin_ia32_subpd256_mask_round ((__v4df) (A), \
						 (__v4df) (B), \
						 (__v4df) \
						 (_mm256_setzero_pd ()), \
						 (__mmask8) (U), \
						 (R)))

#define _mm256_sub_round_ph(A, B, R) \
  ((__m256h) __builtin_ia32_subph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_undefined_ph ()), \
						 (__mmask16) (-1), \
						 (R)))

#define _mm256_mask_sub_round_ph(W, U, A, B, R) \
  ((__m256h) __builtin_ia32_subph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) (W), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_maskz_sub_round_ph(U, A, B, R) \
  ((__m256h) __builtin_ia32_subph256_mask_round ((__v16hf) (A), \
						 (__v16hf) (B), \
						 (__v16hf) \
						 (_mm256_setzero_ph ()), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_sub_round_ps(A, B, R) \
  ((__m256) __builtin_ia32_subps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_undefined_ps ()), \
						(__mmask8) (-1), \
						(R)))

#define _mm256_mask_sub_round_ps(W, U, A, B, R) \
  ((__m256) __builtin_ia32_subps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) (W), \
						(__mmask8) (U), \
						(R)))

#define _mm256_maskz_sub_round_ps(U, A, B, R) \
  ((__m256) __builtin_ia32_subps256_mask_round ((__v8sf) (A), \
						(__v8sf) (B), \
						(__v8sf) \
						(_mm256_setzero_ps ()), \
						(__mmask8) (U), \
						(R)))
#endif

#define _mm256_cmul_round_pch(A, B, R) _mm256_fcmul_round_pch ((A), (B), (R))
#define _mm256_mask_cmul_round_pch(W, U, A, B, R)		      \
  _mm256_mask_fcmul_round_pch ((W), (U), (A), (B), (R))
#define _mm256_maskz_cmul_round_pch(U, A, B, R)			      \
  _mm256_maskz_fcmul_round_pch ((U), (A), (B), (R))

#define _mm256_mul_round_pch(A, B, R) _mm256_fmul_round_pch ((A), (B), (R))
#define _mm256_mask_mul_round_pch(W, U, A, B, R)		      \
  _mm256_mask_fmul_round_pch ((W), (U), (A), (B), (R))
#define _mm256_maskz_mul_round_pch(U, A, B, R)			      \
  _mm256_maskz_fmul_round_pch ((U), (A), (B), (R))

#ifdef __DISABLE_AVX10_2_256__
#undef __DISABLE_AVX10_2_256__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2_256__ */

#endif /* _AVX10_2ROUNDINGINTRIN_H_INCLUDED */
